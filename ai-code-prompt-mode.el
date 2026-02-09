;;; ai-code-prompt-mode.el --- Unified interface for multiple AI coding CLI tool -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Code:

(require 'org)
(require 'magit)

(defvar yas-snippet-dirs)

(declare-function magit-toplevel "magit" (&optional dir))

(defvar ai-code-use-gptel-headline)
(defvar ai-code-prompt-suffix)
(defvar ai-code-auto-test-type)
(defvar ai-code-auto-test-suffix)
(defvar ai-code-use-prompt-suffix)

(declare-function yas-load-directory "yasnippet" (dir))
(declare-function yas-minor-mode "yasnippet")
(declare-function ai-code-cli-send-command "ai-code-backends" (command))
(declare-function ai-code-cli-switch-to-buffer "ai-code-backends" ())
(declare-function gptel-request "gptel" (prompt &rest args))
(declare-function gptel-abort "gptel" (buffer))
(declare-function ai-code--git-repo-recent-modified-files "ai-code-git" (base-dir limit))
(declare-function ai-code--git-ignored-repo-file-p "ai-code-git" (file root))
(declare-function ai-code--hash-completion-target-file "ai-code-input" (&optional end-pos))
(declare-function ai-code--choose-symbol-from-file "ai-code-input" (file))
(declare-function ai-code-current-backend-label "ai-code-backends" ())

(defcustom ai-code-prompt-preprocess-filepaths t
  "When non-nil, preprocess the prompt to replace file paths.
If a word in the prompt is a file path within the current git repository,
it will be replaced with a relative path prefixed with '@'."
  :type 'boolean
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-prompt-file-name ".ai.code.prompt.org"
  "File name that will automatically enable `ai-code-prompt-mode` when opened.
This is the file name without path."
  :type 'string
  :group 'ai-code)

(defun ai-code--setup-snippets ()
  "Setup YASnippet directories for `ai-code-prompt-mode`."
  (condition-case _err
      (when (require 'yasnippet nil t)
        (let ((snippet-dir (expand-file-name "snippets"
                                             (file-name-directory (file-truename (locate-library "ai-code"))))))
          (when (file-directory-p snippet-dir)
            (unless (boundp 'yas-snippet-dirs)
              (setq yas-snippet-dirs nil))
            (add-to-list 'yas-snippet-dirs snippet-dir t)
            (ignore-errors (yas-load-directory snippet-dir)))))
    (error nil))) ;; Suppress all errors

;;;###autoload
(defun ai-code-open-prompt-file ()
  "Open AI prompt file under .ai.code.files/ directory.
If file doesn't exist, create it with sample prompt."
  (interactive)
  (let* ((files-dir (ai-code--ensure-files-directory))
         (prompt-file (expand-file-name ai-code-prompt-file-name files-dir)))
    (find-file-other-window prompt-file)
    (unless (file-exists-p prompt-file)
      ;; Insert initial content for new file
      (insert "# AI Prompt File\n")
      (insert "# This file is for storing AI prompts and instructions\n")
      (insert "# Use this file to save reusable prompts for your AI assistant\n\n")
      (insert "* Sample prompt:\n\n")
      (insert "Explain the architecture of this codebase\n")
      (save-buffer))))

(defun ai-code--get-ai-code-prompt-file-path ()
  "Get the path to the AI prompt file in the .ai.code.files/ directory."
  (let ((files-dir (ai-code--get-files-directory)))
    (expand-file-name ai-code-prompt-file-name files-dir)))

(defun ai-code--execute-command (command)
  "Execute COMMAND directly without saving to prompt file."
  (message "Executing command: %s" command)
  (ignore-errors (ai-code-cli-send-command command))
  (ai-code-cli-switch-to-buffer))

(defun ai-code--generate-prompt-headline (prompt-text)
  "Generate and insert a headline for PROMPT-TEXT."
  (insert "** ")
  (if (and ai-code-use-gptel-headline (require 'gptel nil t))
      (condition-case nil
          (let ((headline (ai-code-call-gptel-sync (concat "Create a 5-10 word action-oriented headline for this AI prompt that captures the main task. Use keywords like: refactor, implement, fix, optimize, analyze, document, test, review, enhance, add, remove, improve, integrate, task. Example: 'Optimize database queries' or 'Implement error handling'.\n\nPrompt: " prompt-text))))
            (insert headline " ")
            (org-insert-time-stamp (current-time) t t))
        (error (org-insert-time-stamp (current-time) t t)))
    (org-insert-time-stamp (current-time) t t))
  (insert "\n"))

(defun ai-code-call-gptel-sync (question)
  "Get an answer from gptel synchronously for a given QUESTION.
This function blocks until a response is received or a timeout occurs.
Only works when gptel package is installed, otherwise shows error message."
  (unless (featurep 'gptel)
    (user-error "GPTel package is required for AI command generation. Please install gptel package"))
  (let ((answer nil)
        (done nil)
        (error-info nil)
        (start-time (float-time))
        (temp-buffer (generate-new-buffer " *gptel-sync*")))
    (unwind-protect
        (progn
          (gptel-request question
                         :buffer temp-buffer
                         :stream nil
                         :callback (lambda (response info)
                                     (cond
                                      ((stringp response)
                                       (setq answer response))
                                      ((eq response 'abort)
                                       (setq error-info "Request aborted."))
                                      (t
                                       (setq error-info (or (plist-get info :status) "Unknown error"))))
                                     (setq done t)))
          ;; Block until 'done' is true or timeout is reached
          (while (not done)
            (when (> (- (float-time) start-time) 30) ;; timeout after 30 seconds
              ;; Try to abort any running processes
              (gptel-abort temp-buffer)
              (setq done t
                    error-info (format "Request timed out after %d seconds" 30)))
            ;; Use sit-for to process events and allow interruption
            (sit-for 0.1)))
      ;; Clean up temp buffer
      (when (buffer-live-p temp-buffer)
        (kill-buffer temp-buffer)))
    (if error-info
        (error "ai-code-call-gptel-sync failed: %s" error-info)
      answer)))

(defun ai-code--format-and-insert-prompt (prompt-text)
  "Insert PROMPT-TEXT into the current buffer without suffix."
  (insert prompt-text)
  (unless (bolp)
    (insert "\n"))
  prompt-text)

(defun ai-code--get-prompt-buffer (prompt-file)
  "Get the buffer for PROMPT-FILE, without selecting it."
  (find-file-noselect prompt-file))

(defun ai-code--append-prompt-to-buffer (prompt-text)
  "Append formatted PROMPT-TEXT to the end of the current buffer.
This includes generating a headline and formatting the prompt.
Returns the full prompt text with suffix for sending to AI."
  (goto-char (point-max))
  (insert "\n\n")
  (ai-code--generate-prompt-headline prompt-text)
  (ai-code--format-and-insert-prompt prompt-text))

(defun ai-code--send-prompt (full-prompt)
  "Send FULL-PROMPT to AI."
  (ai-code-cli-send-command full-prompt)
  (ai-code-cli-switch-to-buffer))

(defun ai-code--write-prompt-to-file-and-send (prompt-text)
  "Write PROMPT-TEXT to the AI prompt file."
  (let* ((suffix-parts (delq nil (list ai-code-prompt-suffix
                                       (when ai-code-auto-test-type
                                         ai-code-auto-test-suffix))))
         (suffix (when (and ai-code-use-prompt-suffix suffix-parts)
                   (mapconcat #'identity suffix-parts "\n")))
         (full-prompt (concat (if suffix
                                  (concat prompt-text "\n" suffix)
                                prompt-text) "\n"))
         (prompt-file (ai-code--get-ai-code-prompt-file-path))
         (original-default-directory default-directory))
    (if prompt-file
      (let ((buffer (ai-code--get-prompt-buffer prompt-file)))
        (with-current-buffer buffer
          (ai-code--append-prompt-to-buffer prompt-text)
          (save-buffer)
          (message "Prompt added to %s" prompt-file))
        (let ((default-directory original-default-directory))
          (ai-code--send-prompt full-prompt)))
      (ai-code--send-prompt full-prompt))))

(defun ai-code--process-word-for-filepath (word git-root-truename)
  "Process a single WORD, converting it to relative path with @ prefix.
WORD is the text to process.
GIT-ROOT-TRUENAME is the true name of the git repository root.
If WORD is a file path, it's converted to a relative path."
  (if (or (string= word ".") (string= word ".."))
      word
    (let* ((expanded-word (expand-file-name word))
           (expanded-word-truename (file-truename expanded-word)))
      (if (and (file-exists-p expanded-word)
               (string-prefix-p git-root-truename expanded-word-truename))
          (concat "@" (file-relative-name expanded-word-truename git-root-truename))
        word))))

(defun ai-code--preprocess-prompt-text (prompt-text)
  "Preprocess PROMPT-TEXT to replace file paths with relative paths.
The function splits the prompt by whitespace, checks if each part is a file
path within the current git repository, and if so, replaces it with a
relative path prefixed with @.
NOTE: This does not handle file paths containing spaces."
  (if-let* ((git-root (magit-toplevel)))
      (let ((git-root-truename (file-truename git-root)))
        (mapconcat
         (lambda (word) (ai-code--process-word-for-filepath word git-root-truename))
         (split-string prompt-text "[ \t\n]+" t) ; split by whitespace and remove empty strings
         " "))
    ;; Not in a git repo, return original prompt
    prompt-text))

(defun ai-code--file-in-git-repo-p (file git-root-truename)
  "Return non-nil when FILE is a regular file under GIT-ROOT-TRUENAME."
  (when (and file (file-exists-p file))
    (let ((truename (file-truename file)))
      (and (file-regular-p truename)
           (string-prefix-p git-root-truename truename)))))

(defun ai-code--relative-filepath (file git-root-truename)
  "Return FILE relative to GIT-ROOT-TRUENAME, prefixed with '@'."
  (concat "@" (file-relative-name (file-truename file) git-root-truename)))

(defun ai-code--normalize-path (file)
  "Return normalized absolute path for FILE.
If FILE exists, return its truename. Otherwise return expanded path."
  (let ((full (expand-file-name file)))
    (if (file-exists-p full)
        (file-truename full)
      full)))

(defun ai-code--candidate-path (file git-root-truename)
  "Return completion candidate for FILE.
Return '@'-prefixed path relative to GIT-ROOT-TRUENAME when FILE is under
that root, otherwise return the absolute path."
  (let ((full-truename (ai-code--normalize-path file)))
    (if (string-prefix-p git-root-truename full-truename)
        (ai-code--relative-filepath full-truename git-root-truename)
      full-truename)))

(defun ai-code--current-frame-dired-paths (git-root-truename)
  "Return dired directory candidates from current frame under GIT-ROOT-TRUENAME."
  (let ((paths '()))
    (dolist (win (window-list nil 'no-minibuffer))
      (with-current-buffer (window-buffer win)
        (when (derived-mode-p 'dired-mode)
          (let ((dir (if (fboundp 'dired-current-directory)
                         (dired-current-directory)
                       default-directory)))
            (when (and dir
                       (file-directory-p dir)
                       (string-prefix-p git-root-truename
                                        (file-truename dir))
                       (not (ai-code--git-ignored-repo-file-p
                             dir
                             git-root-truename)))
              (push (ai-code--relative-filepath dir
                                                git-root-truename)
                    paths))))))
    (nreverse (delete-dups paths))))

(defun ai-code--visible-window-files ()
  "Return files from visible windows in current frame."
  (let ((files '())
        (selected (selected-window)))
    (dolist (win (cons selected
                       (delq selected (window-list nil 'no-minibuffer))))
      (let ((file (buffer-file-name (window-buffer win))))
        (when file
          (push file files))))
    (nreverse (delete-dups files))))

(defun ai-code--recent-buffer-paths (git-root-truename)
  "Return candidate paths for most recent 5 visited buffer files or directories."
  (let ((files '())
        (count 0))
    (dolist (buf (buffer-list))
      (when (< count 5)
        (with-current-buffer buf
          (if (derived-mode-p 'dired-mode)
              (let ((dir (if (fboundp 'dired-current-directory)
                             (dired-current-directory)
                           default-directory)))
                (when dir
                  (push dir files)
                  (setq count (1+ count))))
            (let ((file (buffer-file-name buf)))
              (when file
                (push file files)
                (setq count (1+ count))))))))
    (mapcar (lambda (file)
              (ai-code--candidate-path file git-root-truename))
            (nreverse files))))

(defun ai-code--buffer-file-list (git-root-truename &optional skip-files)
  "Return buffer file list under GIT-ROOT-TRUENAME, skipping SKIP-FILES."
  (let ((files '()))
    (dolist (buf (buffer-list))
      (let ((file (buffer-file-name buf)))
        (when (and (ai-code--file-in-git-repo-p file git-root-truename)
                   (not (ai-code--git-ignored-repo-file-p file git-root-truename))
                   (not (member (file-truename file) skip-files)))
          (push file files))))
    (nreverse files)))

(defun ai-code--repo-recent-files (git-root)
  "Return top 1000 most recently modified files under GIT-ROOT."
  (ai-code--git-repo-recent-modified-files git-root 1000))

(defun ai-code--dedupe-preserve-order (items)
  "Return ITEMS with duplicates removed while preserving order."
  (let ((seen (make-hash-table :test #'equal))
        (result '()))
    (dolist (item items)
      (unless (gethash item seen)
        (puthash item t seen)
        (push item result)))
    (nreverse result)))

(defun ai-code--prompt-filepath-candidates ()
  "Return file path candidates for prompt completion."
  (when-let ((git-root (magit-toplevel)))
    (let* ((git-root-truename (file-truename git-root))
           (current-file (buffer-file-name (current-buffer)))
           (current-frame-dired-paths
            (ai-code--current-frame-dired-paths git-root-truename))
           (visible-files (ai-code--visible-window-files))
           (skip-files (mapcar #'ai-code--normalize-path visible-files))
           (buffer-files (ai-code--buffer-file-list git-root-truename skip-files))
           (recent-files (ai-code--repo-recent-files git-root-truename))
           (ignore-prefix (concat "@" ai-code-files-dir-name "/"))
           (visible-paths (mapcar (lambda (file)
                                    (ai-code--candidate-path file git-root-truename))
                                  visible-files))
           (recent-buffer-paths
            (ai-code--recent-buffer-paths git-root-truename))
           (buffer-paths (mapcar (lambda (file)
                                   (ai-code--candidate-path file git-root-truename))
                                 buffer-files))
           (recent-paths (mapcar (lambda (file)
                                   (ai-code--candidate-path file git-root-truename))
                                 recent-files))
           (combined (append current-frame-dired-paths
                             visible-paths
                             recent-buffer-paths
                             buffer-paths
                             recent-paths))
           (deduped (ai-code--dedupe-preserve-order combined))
           (filtered '()))
      (dolist (item deduped)
        (unless (or (string-prefix-p ignore-prefix item)
                    (and current-file
                         (string= item (ai-code--relative-filepath current-file git-root-truename))))
          (push item filtered)))
      (nreverse filtered))))

(defun ai-code--prompt-filepath-capf ()
  "Provide completion candidates for @file paths in prompt buffer."
  (when (and (not (minibufferp)) (magit-toplevel))
    (let ((end (point))
          (start (save-excursion
                   (skip-chars-backward "A-Za-z0-9_./-")
                   (when (eq (char-before) ?@)
                     (1- (point))))))
      (when start
        (let ((candidates (ai-code--prompt-filepath-candidates)))
          (when candidates
            (list start end candidates :exclusive 'no)))))))

(defun ai-code--prompt-auto-trigger-filepath-completion ()
  "Auto trigger file path/symbol completion when '@' or '#' is inserted."
  (when (not (minibufferp))
    (pcase (char-before)
      (?@
       (let ((candidates (ai-code--prompt-filepath-candidates)))
         (when candidates
           (let ((choice (completing-read "File: " candidates nil nil)))
             (when (and choice (not (string-empty-p choice)))
               (delete-char -1)  ; Remove the '@' we just typed
               (insert choice))))))
      (?#
       (require 'ai-code-input nil t)
       (when (and (fboundp 'ai-code--hash-completion-target-file)
                  (fboundp 'ai-code--choose-symbol-from-file))
         (when-let* ((file (ai-code--hash-completion-target-file (1- (point))))
                     (symbol (ai-code--choose-symbol-from-file file)))
           (when (not (string-empty-p symbol))
             (delete-char -1)  ; Remove the '#' we just typed
             (insert (concat "#" symbol)))))))))

(defun ai-code--insert-prompt (prompt-text)
  "Preprocess and insert PROMPT-TEXT into the AI prompt file.
If PROMPT-TEXT is a command (starts with /), execute it directly instead."
  (let ((processed-prompt (if ai-code-prompt-preprocess-filepaths
                              (ai-code--preprocess-prompt-text prompt-text)
                            prompt-text)))
    (if (and (string-prefix-p "/" processed-prompt)
             (not (string-match-p " " processed-prompt)))
        (ai-code--execute-command processed-prompt)
      (ai-code--write-prompt-to-file-and-send processed-prompt))))

;; Define the AI Prompt Mode (derived from org-mode)
;;;###autoload
(define-derived-mode ai-code-prompt-mode org-mode "AI Prompt"
  "Major mode derived from `org-mode` for editing AI prompt files.
Special commands:
\{ai-code-prompt-mode-map}"
  ;; Basic setup
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local truncate-lines nil)  ; Disable line truncation, allowing lines to wrap
  (define-key ai-code-prompt-mode-map (kbd "C-c C-c") #'ai-code-prompt-send-block)
  (add-hook 'completion-at-point-functions #'ai-code--prompt-filepath-capf nil t)
  (add-hook 'post-self-insert-hook #'ai-code--prompt-auto-trigger-filepath-completion nil t)
  ;; YASnippet support
  (when (require 'yasnippet nil t)
    (yas-minor-mode 1)
    (ai-code--setup-snippets)))

;;;###autoload
(defun ai-code-prompt-send-block ()
  "Send the current text block (paragraph) to the AI service.
The block is the text separated by blank lines.
It trims leading/trailing whitespace."
  (interactive)
  (let* ((block-text (thing-at-point 'paragraph))
         (trimmed-text (when block-text (string-trim block-text))))
    (if (and trimmed-text (string-match-p "\\S-" trimmed-text))
        (progn
          (ai-code-cli-send-command trimmed-text)
          (ai-code-cli-switch-to-buffer))
      (message "No text in the current block to send."))))

;; ai coding task feature

;;;###autoload
(defconst ai-code-files-dir-name ".ai.code.files"
  "Directory name for storing AI task files.")

;;;###autoload
(defcustom ai-code-task-use-gptel-filename nil
  "Whether to use GPTel to generate filename for task files.
If non-nil, call `ai-code-call-gptel-sync` to generate a smart filename
based on the task name. Otherwise, use cleaned-up task name directly."
  :type 'boolean
  :group 'ai-code)

(defun ai-code--get-files-directory ()
  "Get the task directory path.
If in a git repository, return `.ai.code.files/` under git root.
Otherwise, return the current `default-directory`."
  (let ((git-root (magit-toplevel)))
    (if git-root
        (expand-file-name ai-code-files-dir-name git-root)
      default-directory)))

(defun ai-code--ensure-files-directory ()
  "Ensure the task directory exists and return its path."
  (let ((ai-code-files-dir (ai-code--get-files-directory)))
    (unless (file-directory-p ai-code-files-dir)
      (make-directory ai-code-files-dir t))
    ai-code-files-dir))

(defun ai-code--generate-task-filename (task-name)
  "Generate a task filename from TASK-NAME.
If `ai-code-task-use-gptel-filename` is non-nil, use GPTel to generate
a smart filename. Otherwise, use cleaned-up task name directly.
If TASK-NAME contains `rdar://ID`, use `rdar_ID_` as prefix.
Otherwise, use `task_YYYYMMDD_` as prefix.
Returns a filename with .org suffix."
  (let* ((radar-id (when (string-match "rdar://\\([0-9]+\\)" task-name)
                     (match-string 1 task-name)))
         (prefix (if radar-id
                     (format "rdar_%s_" radar-id)
                   (format "task_%s_" (format-time-string "%Y%m%d"))))
         (generated-name
          (if ai-code-task-use-gptel-filename
              ;; Use GPTel to generate filename
              (condition-case nil
                  (ai-code-call-gptel-sync
                   (format "Generate a short English filename (max 60 chars, lowercase, use underscores for spaces, no extension) for this task: %s" task-name))
                (error (replace-regexp-in-string "[^a-z0-9_]" "_" (downcase task-name))))
            ;; Use task name directly (cleaned up)
            (replace-regexp-in-string "[^a-z0-9_]" "_" (downcase task-name)))))
    ;; Clean up the generated name
    (setq generated-name (replace-regexp-in-string "[^a-z0-9_]" "_" (downcase generated-name)))
    (setq generated-name (replace-regexp-in-string "_+" "_" generated-name))
    (setq generated-name (replace-regexp-in-string "^_\\|_$" "" generated-name))
    ;; Ensure reasonable length
    (when (> (length generated-name) 60)
      (setq generated-name (substring generated-name 0 60)))
    (concat prefix generated-name ".org")))

;;;###autoload
(defun ai-code-create-or-open-task-file ()
  "Create or open an AI task file.
Prompts for a task name. If empty, opens the task directory.
If non-empty, optionally prompts for a URL, generates a filename
using GPTel, and creates the task file."
  (interactive)
  (let ((task-name (read-string "Task name (empty to open task directory): ")))
    (if (string-empty-p task-name)
        ;; Open the task directory
        (let ((ai-code-files-dir (ai-code--ensure-files-directory)))
          (dired-other-window ai-code-files-dir)
          (message "Opened task directory: %s" ai-code-files-dir))
      ;; Create a new task file
      (let* ((task-url (read-string "URL (optional, press Enter to skip): "))
             (ai-code-files-dir (ai-code--ensure-files-directory))
             (generated-filename (ai-code--generate-task-filename task-name))
             (confirmed-filename (read-string "Confirm task filename: " generated-filename))
             (task-file (expand-file-name confirmed-filename ai-code-files-dir)))
        ;; Ensure filename has .org extension
        (unless (string-suffix-p ".org" confirmed-filename)
          (setq task-file (concat task-file ".org")))
        (find-file-other-window task-file)
        (unless (file-exists-p task-file)
          ;; Initialize new task file
          (insert (format "#+TITLE: %s\n" task-name))
          (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d")))
          (unless (string-empty-p task-url)
            (insert (format "#+URL: %s\n" task-url)))
          (let ((label (ai-code-current-backend-label)))
            (insert (format "#+AGENT: %s\n" label))
            (insert "#+SESSION_ID: <Usually you can get the session id with /status or /stat in AI coding window>\n"))
          (insert "\n* Task Description\n\n")
          (insert task-name)
          (insert "\n\n* Investigation\n\n")
          (insert "# Enter your prompts here. After that,\n# Select them and use C-c a SPC (ai-code-send-command) to send to AI\n\n")
          (insert "# Use C-c a n (ai-code-take-notes) to copy notes back from AI session\n\n")
          (insert "\n\n* Code Change\n\n"))
        (message "Opened task file: %s" task-file)))))

;;;###autoload
(add-to-list 'auto-mode-alist
             `(,(concat "^"
                        (regexp-quote
                         (file-name-as-directory
                          (expand-file-name ai-code-files-dir-name)))
                        ".*\\.org\\'")
               . ai-code-prompt-mode))

(provide 'ai-code-prompt-mode)

;;; ai-code-prompt-mode.el ends here
