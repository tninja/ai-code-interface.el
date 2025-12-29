;;; ai-code-prompt-mode.el --- AI code prompt mode for editing AI prompt files -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides a major mode for editing AI prompt files.

;;; Code:

(require 'org)
(require 'magit)

(defvar yas-snippet-dirs)

(declare-function magit-toplevel "magit" (&optional dir))

(defvar ai-code-use-gptel-headline)
(defvar ai-code-prompt-suffix)
(defvar ai-code-use-prompt-suffix)

(declare-function yas-load-directory "yasnippet" (dir))
(declare-function yas-minor-mode "yasnippet")
(declare-function ai-code-cli-send-command "ai-code-interface" (command))
(declare-function ai-code-cli-switch-to-buffer "ai-code-interface" ())
(declare-function gptel-request "gptel" (prompt &rest args))
(declare-function gptel-abort "gptel" (buffer))

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
                                             (file-name-directory (file-truename (locate-library "ai-code-interface"))))))
          (when (file-directory-p snippet-dir)
            (unless (boundp 'yas-snippet-dirs)
              (setq yas-snippet-dirs nil))
            (add-to-list 'yas-snippet-dirs snippet-dir t)
            (ignore-errors (yas-load-directory snippet-dir)))))
    (error nil))) ;; Suppress all errors

;;;###autoload
(defun ai-code-open-prompt-file ()
  "Open AI prompt file under git repo root.
If file doesn't exist, create it with sample prompt."
  (interactive)
  (let* ((git-root (magit-toplevel))
         (prompt-file (when git-root
                        (expand-file-name ai-code-prompt-file-name git-root))))
    (if prompt-file
        (progn
          (find-file-other-window prompt-file)
          (unless (file-exists-p prompt-file)
            ;; Insert initial content for new file
            (insert "# AI Prompt File\n")
            (insert "# This file is for storing AI prompts and instructions\n")
            (insert "# Use this file to save reusable prompts for your AI assistant\n\n")
            (insert "* Sample prompt:\n\n")
            (insert "Explain the architecture of this codebase\n")
            (save-buffer)))
      (message "Not in a git repository"))))

(defun ai-code--get-ai-code-prompt-file-path ()
  "Get the path to the AI prompt file in the current git repository."
  (let* ((git-root (magit-toplevel)))
    (when git-root
      (expand-file-name ai-code-prompt-file-name git-root))))

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
  (let* ((full-prompt (concat (if (and ai-code-use-prompt-suffix ai-code-prompt-suffix)
                                  (concat prompt-text "\n" ai-code-prompt-suffix)
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
  (if-let ((git-root (magit-toplevel)))
      (let ((git-root-truename (file-truename git-root)))
        (mapconcat
         (lambda (word) (ai-code--process-word-for-filepath word git-root-truename))
         (split-string prompt-text "[ \t\n]+" t) ; split by whitespace and remove empty strings
         " "))
    ;; Not in a git repo, return original prompt
    prompt-text))

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

;;;###autoload
(add-to-list 'auto-mode-alist
             `(,(concat "/" (regexp-quote ai-code-prompt-file-name) "\'") . ai-code-prompt-mode))

(provide 'ai-code-prompt-mode)

;;; ai-code-prompt-mode.el ends here
