;;; ai-code-file.el --- File operations for AI code interface -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides file operation functionality for the AI Code Interface package.

;;; Code:

(require 'magit)
(require 'dired)

(require 'ai-code-input)
(require 'ai-code-prompt-mode)

(declare-function ai-code-read-string "ai-code-input")
(declare-function ai-code--process-word-for-filepath "ai-code-prompt-mode" (word git-root-truename))
(declare-function ai-code-call-gptel-sync "ai-code-prompt-mode" (prompt))

;; Variables that will be defined in ai-code-interface.el
(defvar ai-code-use-prompt-suffix)
(defvar ai-code-prompt-suffix)
(defvar ai-code-cli)

;;;###autoload
(defun ai-code-copy-buffer-file-name-to-clipboard (&optional arg)
  "Copy the current buffer's file path or selected text to clipboard.
If in a magit status buffer, copy the current branch name.
If in a dired buffer, copy the file at point or directory path.
If in a regular file buffer with selected text, copy text with file path.
Otherwise, copy the file path of the current buffer.
With prefix argument ARG (C-u), always return full path instead of processed path.
File paths are processed to relative paths with @ prefix if within git repo."
  (interactive "P")
  (let ((path-to-copy
         (cond
          ;; If current buffer is a magit status buffer
          ((eq major-mode 'magit-status-mode)
           (magit-get-current-branch))
          ;; If current buffer is a file, use existing logic
          ((buffer-file-name)
           (let* ((git-root (magit-toplevel))
                  (git-root-truename (when git-root (file-truename git-root))))
             (if (use-region-p)
                 (let ((processed-file (if (and git-root-truename (not arg))
                                           (ai-code--process-word-for-filepath (buffer-file-name) git-root-truename)
                                         (buffer-file-name))))
                   (format "%s in %s"
                           (buffer-substring-no-properties (region-beginning) (region-end))
                           processed-file))
               (if (and git-root-truename (not arg))
                   (ai-code--process-word-for-filepath (buffer-file-name) git-root-truename)
                 (buffer-file-name)))))
          ;; If current buffer is a dired buffer
          ((eq major-mode 'dired-mode)
           (let* ((file-at-point (ignore-errors (dired-get-file-for-visit)))
                  (git-root (magit-toplevel))
                  (git-root-truename (when git-root (file-truename git-root))))
             (if file-at-point
                 ;; If there's a file under cursor, copy its processed path
                 (if (and git-root-truename (not arg))
                     (ai-code--process-word-for-filepath file-at-point git-root-truename)
                   file-at-point)
               ;; If no file under cursor, copy the dired directory path
               (let ((dir-path (dired-current-directory)))
                 (if (and git-root-truename (not arg))
                     (ai-code--process-word-for-filepath dir-path git-root-truename)
                   dir-path)))))
          ;; For other buffer types, return nil
          (t nil))))
    (if path-to-copy
        (progn
          (kill-new path-to-copy)
          (message (format "copied %s to clipboard" path-to-copy)))
      (message "No file path available to copy"))))

;;;###autoload
(defun ai-code-open-clipboard-file-path-as-dired ()
  "Open the file or directory path from clipboard in dired.
If the clipboard contains a valid file path, open its directory in dired in another window
and move the cursor to that file.
If the clipboard contains a directory path, open it directly in dired in another window."
  (interactive)
  (let ((path (current-kill 0)))
    (if (and path (file-exists-p path))
        (if (file-directory-p path)
            (dired-other-window path)
          (let* ((dir (file-name-directory path))
                 (file (file-name-nondirectory path))
                 (dired-buffer (dired-other-window dir)))
            (with-current-buffer dired-buffer
              (goto-char (point-min))
              (when (search-forward (regexp-quote file) nil t)
                (goto-char (match-beginning 0))))))
      (message "Clipboard does not contain a valid file or directory path"))))

(defvar ai-code-run-file-history nil
  "History list for ai-code-run-current-file commands.")

;;;###autoload
(defun ai-code-run-current-file ()
  "Generate command to run current script file (.py or .sh).
Let user modify the command before running it in a compile buffer.
Maintains a dedicated history list for this command."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (file-ext (when current-file (file-name-extension current-file)))
         (file-name (when current-file (file-name-nondirectory current-file)))
         (last-command (when ai-code-run-file-history (car ai-code-run-file-history)))
         (default-command
           (cond
            ;; Check if current file is in the last run command
            ((and last-command file-name (string-match-p (regexp-quote file-name) last-command))
             last-command)
            ;; Generate default command based on file extension
            ((string= file-ext "py")
             (format "python %s" file-name))
            ((string= file-ext "sh")
             (format "bash %s" file-name))
            (t nil))))
    (unless current-file
      (user-error "Current buffer is not visiting a file"))
    (unless default-command
      (user-error "Current file is not a .py or .sh file"))
      (let ((command (read-string (format "Run command for %s: " file-name)
                                  default-command
                                  'ai-code-run-file-history)))
        (let* ((default-directory (file-name-directory current-file))
               (buffer-name (format "*ai-code-run-current-file: %s*"
                                    (file-name-base file-name))))
          (compilation-start
           command
           nil
           (lambda (_mode)
             (generate-new-buffer-name buffer-name)))))))

;;;###autoload
(defun ai-code-apply-prompt-on-current-file ()
  "Apply a user prompt to the current file and send to an AI CLI tool.
The file can be the one in the current buffer or the one at point in a dired buffer.
It constructs a shell command: sed \"1i <prompt>: \" <file> | <ai-code-cli>
and runs it in a compilation buffer."
  (interactive)
  (let* ((prompt (ai-code-read-string "Prompt: "))
         (prompt-with-suffix (if (and ai-code-use-prompt-suffix ai-code-prompt-suffix)
                                 (concat prompt ", " ai-code-prompt-suffix)
                                 prompt))
         (file-name (cond
                     ((eq major-mode 'dired-mode)
                      (dired-get-filename))
                     ((buffer-file-name)
                      (buffer-file-name))
                     (t (user-error "Cannot determine the file name"))))
         (command (format "sed \"1i %s: \" %s | %s"
                          (shell-quote-argument prompt-with-suffix)
                          (shell-quote-argument file-name)
                          ai-code-cli)))
    (when file-name
      (let* ((default-directory (file-name-directory file-name))
             (buffer-name (format "*ai-code-apply-prompt: %s*"
                                  (file-name-base file-name))))
        (compilation-start
         command
         nil
         (lambda (_mode)
           (generate-new-buffer-name buffer-name)))))))

(defun ai-code--generate-shell-command (&optional initial-input)
  "Generate shell command from user input or AI assistance.
Read initial command from user with INITIAL-INPUT as default.
If command starts with ':', treat as prompt for AI to generate command.
Return the final command string."
  (let* ((initial-command (ai-code-read-string "Shell command: " initial-input))
         ;; if current buffer is dired buffer, replace the * character
         ;; inside initial-command with file base name under cursor,
         ;; or marked files, separate with space
         (initial-command
          (if (and (eq major-mode 'dired-mode)
                   (string-match-p "\\*" initial-command))
              (let* ((files (ignore-errors (dired-get-marked-files)))
                     (file-names (when files
                                   (mapcar #'file-name-nondirectory files))))
                (if file-names
                    (replace-regexp-in-string
                     (regexp-quote "*")
                     (mapconcat #'identity file-names " ")
                     initial-command
                     nil
                     t)
                  initial-command))
            initial-command))
         (command 
          (if (string-prefix-p ":" initial-command)
              ;; If command starts with :, treat as prompt for AI
              (let ((prompt (concat "Generate a shell command (pure command, no fense) for: " (substring initial-command 1))))
                (condition-case err
                    (let ((ai-generated (ai-code-call-gptel-sync prompt)))
                      (when ai-generated
                        ;; Ask user to confirm/edit the AI-generated command
                        (read-string "Shell command (AI generated): " (string-trim ai-generated))))
                  (error 
                   (message "Failed to generate command with AI: %s" err)
                   initial-command)))
            ;; Regular command, use as-is
            initial-command)))
    command))

;;;###autoload
(defun ai-code-shell-cmd (&optional initial-input)
  "Run shell command in dired directory or insert command in shell buffers.
If current buffer is a dired buffer, get user input shell command with read-string,
then run it under the directory of dired buffer, in a buffer with name as *ai-code-shell-cmd: <current-dir>*.
If current buffer is shell-mode, eshell-mode or sh-mode, get input and insert command under cursor, do not run it.
If the command starts with ':', it means it is a prompt. In this case, ask gptel to generate 
the corresponding shell command, and call ai-code-shell-cmd with that command as candidate.
INITIAL-INPUT is the initial text to populate the shell command prompt."
  (interactive)
  (cond
   ;; Handle shell modes: insert command without running
   ((memq major-mode '(shell-mode eshell-mode sh-mode vterm-mode))
    (let ((command (ai-code--generate-shell-command initial-input)))
      (when (and command (not (string= command "")))
        (insert command))))
   ;; Handle other modes: run command in compilation buffer
   (t
    (let* ((current-dir (cond
                         ((eq major-mode 'dired-mode)
                          (dired-current-directory))
                         (initial-input
                          default-directory)
                         (t nil))))
      (unless current-dir
        (user-error "Cannot determine working directory: requires either a dired buffer or initial input."))
      (let* ((command (ai-code--generate-shell-command initial-input))
             (buffer-name (format "*ai-code-shell-cmd: %s*" (directory-file-name current-dir))))
        (when (and command (not (string= command "")))
          (let ((default-directory current-dir))
            (compilation-start
             command
             nil
             (lambda (_mode)
               (generate-new-buffer-name buffer-name))))))))))

;;;###autoload
(defun ai-code-run-current-file-or-shell-cmd ()
  "Run current file or shell command based on buffer state.
Call `ai-code-shell-cmd` when in dired mode, shell modes or a region is active; otherwise run the current file."
  (interactive)
  (cond
   ((eq major-mode 'dired-mode)
    (ai-code-shell-cmd))
   ((memq major-mode '(shell-mode eshell-mode sh-mode vterm-mode))
    (ai-code-shell-cmd))
   ((use-region-p)
    (let ((initial-input (string-trim (buffer-substring-no-properties (region-beginning)
                                                                      (region-end)))))
      (ai-code-shell-cmd initial-input)))
   (t
    (ai-code-run-current-file))))

(provide 'ai-code-file)

;;; ai-code-file.el ends here 
