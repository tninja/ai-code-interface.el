;;; ai-code-prompt-mode.el --- Unified interface for multiple AI coding CLI tool -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Prompt file management, prompt history, and file completion helpers for
;; ai-code sessions.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'magit)
(require 'ai-code-utils)

(defvar yas-snippet-dirs)

(defvar ai-code-use-gptel-headline nil)
(defvar ai-code-prompt-suffix)
(defvar ai-code-use-prompt-suffix)
(defvar ai-code-selected-backend)
(defvar ai-code-backends-infra--session-terminal-backend nil
  "Buffer-local terminal backend symbol for an AI session buffer, or nil.
This is set by `ai-code-backends-infra.el' for terminal-managed sessions
such as `vterm' and `eat'.  A nil value means the buffer is not managed by
the terminal backend infrastructure.")

(defvar ai-code-backends-infra-use-paste-backends)
(defvar ai-code-backends-infra--session-prefix)

(declare-function yas-load-directory "yasnippet" (dir))
(declare-function yas-minor-mode "yasnippet")
(declare-function ai-code-implement-todo "ai-code-change" (arg &optional default-action))
(declare-function ai-code-cli-send-command "ai-code-backends" (command))
(declare-function ai-code-cli-switch-to-buffer "ai-code-backends" ())
(declare-function gptel-request "gptel" (prompt &rest args))
(declare-function gptel-abort "gptel" (buffer))
(declare-function ai-code--git-repo-recent-modified-files "ai-code-git" (base-dir limit))
(declare-function ai-code--git-ignored-repo-file-p "ai-code-git" (file root))
(declare-function ai-code--hash-completion-target-file "ai-code-input" (&optional end-pos))
(declare-function ai-code--choose-symbol-from-file "ai-code-input" (file))
(declare-function ai-code-read-string "ai-code-input" (prompt &optional initial-input candidate-list))
(declare-function ai-code--confirm-and-send "ai-code-input" (prompt-label initial-prompt))
(declare-function ai-code--worktree-main-repo-root "ai-code-utils" ())
(declare-function ai-code-current-backend-label "ai-code-backends" ())
(declare-function ai-code-backends-infra--session-buffer-p "ai-code-backends-infra" (buffer))
(declare-function ai-code-backends-infra--session-buffer-matches-directory-p "ai-code-backends-infra" (buffer directory))
(declare-function ai-code-backends-infra--terminal-send-string
  "ai-code-backends-infra" (string &optional paste))
(declare-function ai-code-backends-infra--terminal-send-return "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra--display-buffer-in-side-window "ai-code-backends-infra" (buffer))

(cl-defstruct (ai-code-prompt-context
               (:constructor ai-code--make-prompt-context))
  "Context shared by prompt suffix providers."
  prompt-text
  origin-command
  backend
  (cache (make-hash-table :test #'eq)))

(defcustom ai-code-prompt-suffix-functions nil
  "Ordered abnormal hook that returns suffixes for a prompt context.
Each function receives one `ai-code-prompt-context` and returns either a
non-empty string or nil.  Provider errors abort the send."
  :type 'hook
  :group 'ai-code)

(defvar ai-code--prompt-origin-command nil
  "Originating interactive command for the current prompt request.")

(defun ai-code-prompt-context-memoize (context key producer)
  "Return the cached value for KEY in CONTEXT, calling PRODUCER once."
  (let* ((cache (ai-code-prompt-context-cache context))
         (missing (make-symbol "missing"))
         (value (gethash key cache missing)))
    (if (eq value missing)
        (let ((produced (funcall producer)))
          (puthash key produced cache)
          produced)
      value)))

(defun ai-code--prompt-suffix-from-provider (provider context)
  "Return the validated suffix from PROVIDER for CONTEXT."
  (condition-case err
      (let ((suffix (funcall provider context)))
        (cond
         ((null suffix) nil)
         ((not (stringp suffix))
          (error "Returned %S instead of a string or nil" suffix))
         ((string-empty-p suffix) nil)
         (t suffix)))
    (error
     (user-error "Prompt suffix provider %S failed: %s"
                 provider (error-message-string err)))))

(defun ai-code--collect-prompt-suffixes (context)
  "Return non-empty suffix strings produced for CONTEXT in hook order."
  (let (suffixes)
    (run-hook-wrapped
     'ai-code-prompt-suffix-functions
     (lambda (provider prompt-context)
       (when-let ((suffix (ai-code--prompt-suffix-from-provider
                           provider prompt-context)))
         (push suffix suffixes))
       nil)
     context)
    (nreverse suffixes)))

(defun ai-code--prompt-context-for-text (prompt-text)
  "Return a prompt suffix context for PROMPT-TEXT."
  (ai-code--make-prompt-context
   :prompt-text prompt-text
   :origin-command (or ai-code--prompt-origin-command this-command)
   :backend (and (boundp 'ai-code-selected-backend)
                 ai-code-selected-backend)))

(defun ai-code--apply-prompt-suffixes (prompt-text)
  "Return PROMPT-TEXT with all enabled prompt suffixes appended."
  (let* ((context (ai-code--prompt-context-for-text prompt-text))
         (suffixes (ai-code--collect-prompt-suffixes context)))
    (if suffixes
        (concat prompt-text "\n" (mapconcat #'identity suffixes "\n"))
      prompt-text)))

(defun ai-code--custom-prompt-suffix-provider (_context)
  "Return the configured custom prompt suffix when it is enabled."
  (when (and (bound-and-true-p ai-code-use-prompt-suffix)
             (boundp 'ai-code-prompt-suffix)
             (stringp ai-code-prompt-suffix)
             (not (string-empty-p ai-code-prompt-suffix)))
    ai-code-prompt-suffix))

(add-hook 'ai-code-prompt-suffix-functions
          #'ai-code--custom-prompt-suffix-provider 10)

(defcustom ai-code-prompt-preprocess-filepaths t
  "When non-nil, preprocess the prompt to replace file paths.
If a word in the prompt is a file path within the current git repository,
it will be replaced with a relative path prefixed with '@'."
  :type 'boolean
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-gptel-sync-timeout 15
  "Timeout in seconds for synchronous gptel requests.
Used by `ai-code-call-gptel-sync`."
  :type 'integer
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-gptel-sync-max-length 1000
  "Maximum length of characters to send to gptel synchronously.
If the prompt exceeds this limit, it will be truncated to prevent
long request times."
  :type 'integer
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
  "Generate an Org headline for PROMPT-TEXT."
  (insert "* ")
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
    (user-error "GPTel package is required for AI command generation; please install gptel package"))
  (let ((truncated-question (if (and ai-code-gptel-sync-max-length
                                      (> (length question) ai-code-gptel-sync-max-length))
                                 (substring question 0 ai-code-gptel-sync-max-length)
                               question))
        (retry t)
        (result nil))
    (while retry
      (let ((answer nil)
            (done nil)
            (error-info nil)
            (start-time (float-time))
            (temp-buffer (generate-new-buffer " *gptel-sync*")))
        (unwind-protect
            (progn
              (gptel-request truncated-question
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
                (when quit-flag
                  (keyboard-quit))
                (when (> (- (float-time) start-time) ai-code-gptel-sync-timeout)
                  ;; Try to abort any running processes
                  (gptel-abort temp-buffer)
                  (setq done t
                        error-info (format "Request timed out after %d seconds" ai-code-gptel-sync-timeout)))
                ;; Use sit-for to process events and allow interruption
                (sit-for 0.1)))
          ;; Clean up temp buffer
          (when (buffer-live-p temp-buffer)
            (kill-buffer temp-buffer)))
        (cond
         ((null error-info)
          (setq result answer
                retry nil))
         ((and error-info (string-match-p "timed out" error-info))
          (if (y-or-n-p (format "%s. Do you want to retry? " error-info))
              (message "Retrying...")
            (setq retry nil
                  result (cons 'error error-info))))
         (t
          (setq retry nil
                result (cons 'error error-info))))))
    (if (and (listp result) (eq (car result) 'error))
        (error "Ai-code-call-gptel-sync failed: %s" (cdr result))
      result)))

(defun ai-code--format-and-insert-prompt (prompt-text)
  "Insert PROMPT-TEXT into the current buffer without suffix."
  (insert prompt-text)
  (unless (bolp)
    (insert "\n"))
  prompt-text)

(defun ai-code--get-prompt-buffer (prompt-file)
  "Get the buffer for PROMPT-FILE, without selecting it."
  (find-file-noselect prompt-file))

(defun ai-code--insert-backend-label-drawer ()
  "Insert an Org drawer recording the current AI backend label."
  (let ((label (condition-case nil
                   (ai-code-current-backend-label)
                (error "Unknown"))))
    (insert ":PROPERTIES:\n")
    (insert (format ":AGENT: %s\n" label))
    (insert ":END:\n")))

(defun ai-code--append-prompt-to-buffer (stored-prompt-text)
  "Append formatted STORED-PROMPT-TEXT to the end of the current buffer.
This includes generating a headline and formatting the prompt text
that should be recorded in the prompt history file."
  (goto-char (point-max))
  (insert "\n\n")
  (ai-code--generate-prompt-headline stored-prompt-text)
  (ai-code--insert-backend-label-drawer)
  (ai-code--format-and-insert-prompt stored-prompt-text))

(defun ai-code--find-visible-session-buffer ()
  "Return a visible terminal-managed AI session buffer in the current frame."
  (cl-some
   (lambda (win)
     (let ((buf (window-buffer win)))
       (when (and (buffer-live-p buf)
                  (ai-code-backends-infra--session-buffer-p buf)
                  (buffer-local-value
                   'ai-code-backends-infra--session-terminal-backend buf))
         buf)))
   (window-list nil 'no-minibuffer)))

(defun ai-code--find-project-session-buffers ()
  "Return terminal-managed AI session buffers associated with the current project."
  (when-let ((git-root (ai-code--git-root)))
    (cl-remove-if-not
     (lambda (buf)
       (and (ai-code-backends-infra--session-buffer-p buf)
            (buffer-local-value
             'ai-code-backends-infra--session-terminal-backend buf)
            (ai-code-backends-infra--session-buffer-matches-directory-p buf git-root)))
     (buffer-list))))

(defun ai-code--prompt-choose-target-session ()
  "Choose AI session buffer to send prompt to.
Return a session buffer, or nil for default backend dispatch."
  (when-let ((visible-session (ai-code--find-visible-session-buffer)))
    (let* ((project-sessions (ai-code--find-project-session-buffers))
           (visible-is-project-session (memq visible-session project-sessions))
           (competing-sessions
            (cl-remove visible-session project-sessions)))
      (cond
       (visible-is-project-session nil)
       ((null competing-sessions)
        visible-session)
       (t
        (let* ((choice-alist (mapcar (lambda (buf)
                                       (cons (buffer-name buf) buf))
                                     (cons visible-session competing-sessions)))
               (selection (completing-read
                           "Multiple AI sessions available.  Send to: "
                           (mapcar #'car choice-alist)
                           nil t nil nil (buffer-name visible-session))))
          (cdr (assoc selection choice-alist))))))))

(defun ai-code--send-prompt-to-session-buffer (prompt buffer)
  "Send PROMPT directly to session BUFFER and display it."
  (with-current-buffer buffer
    (if (and (string-match-p "\n" prompt)
             (bound-and-true-p ai-code-backends-infra-use-paste-backends)
             (member ai-code-backends-infra--session-prefix
                     ai-code-backends-infra-use-paste-backends))
        (ai-code-backends-infra--terminal-send-string prompt t)
      (ai-code-backends-infra--terminal-send-string prompt))
    (sit-for 0.5)
    (ai-code-backends-infra--terminal-send-return))
  (if-let ((window (get-buffer-window buffer)))
      (select-window window)
    (ai-code-backends-infra--display-buffer-in-side-window buffer)))

(defun ai-code--send-prompt (full-prompt)
  "Send FULL-PROMPT to AI.
When a visible AI session buffer is detected in the current frame,
send the prompt directly to it instead of going through the default
backend dispatch."
  (if-let ((target (ai-code--prompt-choose-target-session)))
      (ai-code--send-prompt-to-session-buffer full-prompt target)
    (ai-code-cli-send-command full-prompt)
    (ai-code-cli-switch-to-buffer)))

(defun ai-code--write-prompt-to-file-and-send (prompt-text)
  "Write PROMPT-TEXT to the AI prompt file."
  (let* ((full-prompt (concat prompt-text "\n"))
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

(defun ai-code--filter-prompt-suffix-args (args)
  "Return ARGS with prompt suffix providers applied to its prompt text."
  (list (ai-code--apply-prompt-suffixes (car args))))

(unless (advice-member-p #'ai-code--filter-prompt-suffix-args
                         'ai-code--write-prompt-to-file-and-send)
  (advice-add 'ai-code--write-prompt-to-file-and-send
              :filter-args
              #'ai-code--filter-prompt-suffix-args))

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
The function checks each non-whitespace token in the prompt; if a token is a
file path within the current git repository it is replaced with a relative
path prefixed with @.  Original whitespace is preserved.
NOTE: This does not handle file paths containing spaces."
  (if-let* ((git-root-truename (ai-code--git-root)))
      (replace-regexp-in-string
       "[^ \t\n]+"
       (lambda (word) (ai-code--process-word-for-filepath word git-root-truename))
       prompt-text t t)
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
If FILE exists, return its truename.  Otherwise return expanded path."
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
  "Return Dired directory candidates from current frame under GIT-ROOT-TRUENAME."
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
  "Return candidate paths for most recent 5 visited buffers.
GIT-ROOT-TRUENAME is the normalized Git root."
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
  (when-let ((git-root-truename (ai-code--git-root)))
    (let* ((current-file (buffer-file-name (current-buffer)))
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
  (when (and (not (minibufferp)) (ai-code--git-root))
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
            (insert "#" symbol))))))))

(defun ai-code--direct-command-p (prompt-text)
  "Return non-nil when PROMPT-TEXT is a single-token slash command."
  (and (string-prefix-p "/" prompt-text)
       (not (string-match-p "[[:space:]]" prompt-text))))

(defun ai-code--insert-prompt (prompt-text)
  "Preprocess and insert PROMPT-TEXT into the AI prompt file.
If PROMPT-TEXT is a command (starts with /), execute it directly instead."
  (let ((processed-prompt (if ai-code-prompt-preprocess-filepaths
                              (ai-code--preprocess-prompt-text prompt-text)
                            prompt-text)))
    (if (ai-code--direct-command-p processed-prompt)
        (ai-code--execute-command processed-prompt)
      (let* ((append-summary-p (and (derived-mode-p 'org-mode)
                                    (ignore-errors (save-excursion (org-back-to-heading t) t))
                                    (y-or-n-p "Append result summary to current section? ")))
             (final-prompt (if append-summary-p
                               (concat processed-prompt
                                       (format "\n\nAfter completing, append a concise result summary as a sub-heading at the end of the current section in file %s near line %d."
                                               buffer-file-name (line-number-at-pos)))
                             processed-prompt)))
        (ai-code--write-prompt-to-file-and-send final-prompt)))))

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
  "Send context-aware action in prompt mode.
Following issue #404 behavior:
1. If cursor is on an Org section headline, call `ai-code-implement-todo`.
2. If there is a selected region, send the selected region to the AI session.
3. Otherwise, fallback to the existing `org-mode` `C-c C-c` action (`org-ctrl-c-ctrl-c`)."
  (interactive)
  (cond
   ((and (derived-mode-p 'org-mode)
         (org-at-heading-p))
    (call-interactively #'ai-code-implement-todo))
   ((use-region-p)
    (let* ((block-text (buffer-substring-no-properties (region-beginning)
                                                       (region-end)))
           (trimmed-text (when block-text (string-trim block-text))))
      (if (and trimmed-text (string-match-p "\\S-" trimmed-text))
          (if (and buffer-file-name
                   (string= (file-name-nondirectory buffer-file-name)
                            ai-code-prompt-file-name))
              (ai-code--send-prompt trimmed-text)
            (when-let ((edited-prompt
                        (ai-code-read-string "Confirm and edit prompt before sending: "
                                             trimmed-text)))
              (ai-code--insert-prompt edited-prompt)))
        (message "No text in the selected region to send."))))
   (t
    (if (fboundp 'org-ctrl-c-ctrl-c)
        (call-interactively #'org-ctrl-c-ctrl-c)
      (user-error "org-ctrl-c-ctrl-c is not defined")))))

(defun ai-code--mark-prompt-block ()
  "Mark the current prompt block.
A prompt block is multiple non-empty lines surrounded by empty lines."
  (interactive)
  (let ((start (point))
        (end (point)))
    (save-excursion
      (while (and (not (bobp)) (not (looking-at-p "^$")))
        (forward-line -1))
      (unless (bobp)
        (forward-line 1))
      (setq start (point)))
    (save-excursion
      (while (and (not (eobp)) (not (looking-at-p "^$")))
        (forward-line 1))
      (setq end (point)))
    (goto-char start)
    (set-mark (point))
    (goto-char end)
    (message "Code block marked from line %d to line %d"
             (line-number-at-pos start)
             (line-number-at-pos end))))
;;;###autoload
(defcustom ai-code-note-search-additional-paths nil
  "Additional paths to offer when searching notes with AI.
Each entry may be a path string or a symbol whose value is a path string,
for example `org-roam-directory'."
  :type '(repeat
          (choice
           (directory :tag "Directory")
           (string :tag "Path")
           (symbol :tag "Variable")))
  :group 'ai-code)

(defun ai-code--resolve-note-search-path (entry)
  "Resolve note search ENTRY to an existing absolute path, or nil.
ENTRY may be a path string or a symbol whose value is a path string."
  (let* ((raw-value
          (cond
           ((symbolp entry)
            (and (boundp entry) (symbol-value entry)))
           ((stringp entry) entry)))
         (path (and (stringp raw-value)
                    (not (string-empty-p raw-value))
                    (expand-file-name raw-value))))
    (when (and path (file-exists-p path))
      path)))

(defun ai-code--note-search-scope-candidates (ai-code-files-dir)
  "Return candidate note search scopes rooted at AI-CODE-FILES-DIR."
  (cl-remove-duplicates
   (delq nil
         (cons ai-code-files-dir
               (mapcar #'ai-code--resolve-note-search-path
                       ai-code-note-search-additional-paths)))
   :test #'string-equal))

(defun ai-code--note-search-additional-scope-candidates (ai-code-files-dir)
  "Return additional note search scopes beyond AI-CODE-FILES-DIR."
  (cl-remove ai-code-files-dir
             (ai-code--note-search-scope-candidates ai-code-files-dir)
             :test #'string-equal))

(defun ai-code--read-note-search-scopes (ai-code-files-dir)
  "Return note search scopes rooted at AI-CODE-FILES-DIR.
Always include AI-CODE-FILES-DIR.  When configured additional note paths
exist, prompt once to optionally include them as well."
  (let ((additional-scopes
         (ai-code--note-search-additional-scope-candidates ai-code-files-dir)))
    (if (and additional-scopes
             (y-or-n-p
              (format
               "Include additional note search paths from `ai-code-note-search-additional-paths` along with %s? "
               ai-code-files-dir)))
        (cons ai-code-files-dir additional-scopes)
      (list ai-code-files-dir))))

(defun ai-code--build-note-search-prompt (scopes search-description)
  "Build a prompt for searching SCOPES for SEARCH-DESCRIPTION."
  (let ((git-root-truename
         (when-let ((git-root (ai-code--git-root)))
           (file-truename git-root))))
    (format
     "Search my notes and related files for: %s\n\
Search scope paths:\n%s\n\
Use the available search tools to inspect the selected paths.\n\
Focus on relevant information inside files, not just file names.\n\
Return the most relevant paths, matched excerpts, and a concise answer."
     search-description
     (mapconcat
      (lambda (scope)
        (format "- %s"
                 (if git-root-truename
                     (ai-code--candidate-path scope git-root-truename)
                   scope)))
       scopes "\n"))))

;;;###autoload
(defun ai-code-search-notes-with-ai (&optional arg)
  "Ask AI to search task files and configured note paths.
With \\[universal-argument], append clipboard contents as context.
If a region is active, include the selected text as context.

ARG is the prefix argument."
  (interactive "P")
  ;; DONE: this function should support context:
  ;; If there is selected region, content of it should automatically included
  ;; C-u should include clipboard as context, just like ai-code-ask-question
  (let* ((region-text (when (use-region-p)
                        (buffer-substring-no-properties (region-beginning)
                                                       (region-end))))
         (clipboard-context (when arg (ai-code--get-clipboard-text)))
         (ai-code-files-dir (ai-code--ensure-files-directory))
         (scopes (ai-code--read-note-search-scopes ai-code-files-dir))
         (search-description (ai-code-read-string "Search notes for: "))
         (default-prompt (concat
                          (ai-code--build-note-search-prompt scopes search-description)
                          (when region-text
                            (concat "\nContext:\n" region-text))
                          (when (and clipboard-context
                                     (string-match-p "\\S-" clipboard-context))
                            (concat "\n\nClipboard context:\n" clipboard-context))))
         (confirmed-prompt (ai-code-read-string "Confirm search prompt: " default-prompt)))
    (ai-code--insert-prompt confirmed-prompt)))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("/\\.ai\\.code\\.files/.*\\.org\\'" . ai-code-prompt-mode))

(provide 'ai-code-prompt-mode)

;;; ai-code-prompt-mode.el ends here
