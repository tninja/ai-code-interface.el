;;; ai-code-file.el --- File operations for AI code interface -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides file operation functionality for the AI Code Interface package.

;;; Code:

(require 'magit)
(require 'dired)
(require 'comint)
(require 'subr-x)

(require 'ai-code-input)
(require 'ai-code-prompt-mode)

(declare-function ai-code-read-string "ai-code-input")
(declare-function ai-code--get-context-files-string "ai-code-input")
(declare-function ai-code--insert-prompt "ai-code-prompt-mode" (prompt-text))
(declare-function ai-code--process-word-for-filepath "ai-code-prompt-mode" (word git-root-truename))
(declare-function ai-code-call-gptel-sync "ai-code-prompt-mode" (prompt))
(declare-function ai-code-backends-infra--session-buffer-p "ai-code-backends-infra" (buffer))
(declare-function projectile-project-root "projectile")
(declare-function ai-code-run-test "ai-code-agile")

(defcustom ai-code-sed-command "sed"
  "GNU sed command used to apply prompts to files."
  :type 'string
  :group 'ai-code)

;; Variables that will be defined in ai-code.el
(defvar ai-code-use-prompt-suffix)
(defvar ai-code-prompt-suffix)
(defvar ai-code-auto-test-type)
(defvar ai-code-auto-test-suffix)
(defvar ai-code-cli)

;;;###autoload
(defun ai-code-copy-buffer-file-name-to-clipboard (&optional arg)
  "Copy the current buffer's file path or selected text to clipboard.
If in a magit status buffer, copy the current branch name.
If in a Dired buffer, copy the file at point or directory path.
If in a regular file buffer with selected text, copy text with file path.
Otherwise, copy the file path of the current buffer.
With prefix argument ARG \[universal-argument], always return full path
instead of processed path.  File paths are processed to relative paths
with @ prefix if within git repo."
  (interactive "P")
  (let ((path-to-copy
         (cond
          ;; If current buffer is a magit status buffer
          ((derived-mode-p 'magit-status-mode)
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
          ((derived-mode-p 'dired-mode)
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
          (message "copied %s to clipboard" path-to-copy))
      (message "No file path available to copy"))))

;;;###autoload
(defun ai-code-open-clipboard-file-path-as-dired ()
  "Open the file or directory path from clipboard in Dired.
If the clipboard contains a valid file path, open its directory in Dired
in another window and move the cursor to that file.  If the clipboard
contains a directory path, open it directly in Dired in another window."
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
  "History list for `ai-code-run-current-file' commands.")

(defun ai-code--run-command-in-comint (command run-directory display-name)
  "Run COMMAND in a comint buffer using RUN-DIRECTORY and DISPLAY-NAME.
COMMAND is parsed into a program and its SWITCHES using
`split-string-and-unquote'.  RUN-DIRECTORY is used as the default
directory for the process.  DISPLAY-NAME is the buffer name applied to
the comint session.  This function is a programmatic wrapper around
`comint-run'."
  (let* ((command-parts (split-string-and-unquote command))
         (program (car command-parts))
         (switches (cdr command-parts)))
    (unless program
      (user-error "Command cannot be empty"))
    (let* ((origin-window (selected-window))
           (origin-buffer (window-buffer origin-window))
           (effective-directory (or run-directory default-directory))
           (comint-buffer-name (format "*%s*" (file-name-nondirectory program))))
      (when-let* ((existing (get-buffer display-name)))
        (unless (yes-or-no-p (format "Kill existing session %s? " display-name))
          (user-error "Aborted running command: %s" command))
        (when-let* ((proc (get-buffer-process existing)))
          (delete-process proc))
        (kill-buffer existing))
      (let ((default-directory effective-directory))
        (apply #'make-comint "Run" program nil switches))
      (when (window-live-p origin-window)
        (set-window-buffer origin-window origin-buffer))
      (when-let* ((buffer (get-buffer comint-buffer-name)))
        (with-current-buffer buffer
          (rename-buffer display-name t)
          (setq-local default-directory effective-directory))
        (let ((session-window
               (display-buffer buffer '((display-buffer-pop-up-window)
                                        (inhibit-same-window . t)))))
          (when (window-live-p session-window)
            (select-window session-window))))
      (get-buffer display-name))))

;;;###autoload
(defun ai-code-run-current-file ()
  "Generate command to run current script file (.py, .js, .ts, or .sh).
Let user modify the command before running it in a comint buffer.
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
            ((string= file-ext "js")
             (format "node %s" file-name))
            ((string= file-ext "ts")
             (format "npx tsx %s" file-name))
            ((string= file-ext "sh")
             (format "bash %s" file-name))
            (t nil))))
    (unless current-file
      (user-error "Current buffer is not visiting a file"))
    (unless default-command
      (user-error "Current file is not a .py, .js, .ts, or .sh file"))
    (let ((command (read-string (format "Run command for %s: " file-name)
                                default-command
                                'ai-code-run-file-history)))
      (let* ((run-directory (file-name-directory current-file))
             (display-name (format "*ai-code-run-current-file: %s*"
                                   (file-name-base file-name))))
        (ai-code--run-command-in-comint command run-directory display-name)))))

;;;###autoload
(defun ai-code-apply-prompt-on-current-file ()
  "Apply a user prompt to the current file and send to an AI CLI tool.
The file can be the one in the current buffer or at point in a Dired
buffer.  It constructs a shell command:
sed \"1i <prompt>: \" <file> | <ai-code-cli>
and runs it in a compilation buffer."
  (interactive)
  (let* ((prompt (ai-code-read-string "Prompt: "))
         (suffix-parts (delq nil (list ai-code-prompt-suffix
                                       (when ai-code-auto-test-type
                                         ai-code-auto-test-suffix))))
         (suffix (when (and ai-code-use-prompt-suffix suffix-parts)
                   (string-join suffix-parts ", ")))
         (prompt-with-suffix (if suffix
                                 (concat prompt ", " suffix)
                               prompt))
         (file-name (cond
                     ((derived-mode-p 'dired-mode)
                      (dired-get-filename))
                     ((buffer-file-name)
                      (buffer-file-name))
                     (t (user-error "Cannot determine the file name"))))
         (command (format "%s \"1i %s: \" %s | %s"
                          ai-code-sed-command
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
         ;; if current buffer is Dired buffer, replace the * character
         ;; inside initial-command with file base name under cursor,
         ;; or marked files, separate with space
         (initial-command
          (if (and (derived-mode-p 'dired-mode)
                   (string-match-p "\\*" initial-command))
              (let* ((files (ignore-errors (dired-get-marked-files)))
                     (file-names (when files
                                   (delete-dups (mapcar #'file-name-nondirectory files)))))
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
              (let* ((base-prompt (concat "Generate a shell command (pure command, no fense, no duplicate, in one line) for: " (substring initial-command 1)))
                     (prompt (if (derived-mode-p 'dired-mode)
                                 (let* ((files (ignore-errors (dired-get-marked-files)))
                                        (file-names (when files (delete-dups (mapcar #'file-name-nondirectory files)))))
                                   (if file-names
                                       (concat (format "For these files %s, " (mapconcat #'identity file-names " "))
                                               base-prompt)
                                     base-prompt))
                               base-prompt)))
                (condition-case err
                    ;; DONE: if ai-generated has more than one line,
                    ;; only take the first line
                    (let* ((ai-generated (ai-code-call-gptel-sync prompt))
                           (first-line (when ai-generated
                                         (car (split-string ai-generated "\n" t)))))
                      (when first-line
                        ;; Ask user to confirm/edit the AI-generated command
                        (read-string "Shell command (AI generated): " (string-trim first-line))))
                  (error
                   (message "Failed to generate command with AI: %s" err)
                   "")))
            ;; Regular command, use as-is
            initial-command)))
    command))

;;;###autoload
(defun ai-code-shell-cmd (&optional initial-input)
  "Run shell command in Dired directory or insert command in shell buffers.
If current buffer is a Dired buffer, get user input shell command with
`read-string', then run it under the directory of Dired buffer, in a
buffer with name as *ai-code-shell-cmd: <current-dir>*.  If current
buffer is `shell-mode', `eshell-mode' or `sh-mode', get input and insert
command under cursor, do not run it.  If the command starts with
\\=':\\=', it means it is a prompt.  In this case, ask gptel to generate
the corresponding shell command, and call `ai-code-shell-cmd' with that
command as candidate.  INITIAL-INPUT is the initial text to populate the
shell command prompt."
  (interactive)
  (cond
   ;; Handle shell modes: insert command without running
   ((memq major-mode '(shell-mode eshell-mode))
    (let ((command (ai-code--generate-shell-command initial-input)))
      (when (and command (not (string= command "")))
        (insert command))))
   ;; Handle other modes: run command in compilation buffer
   (t
    (let* ((current-dir (cond
                         ((derived-mode-p 'dired-mode)
                          (dired-current-directory))
                         (initial-input
                          default-directory)
                         (t nil))))
      (unless current-dir
        (user-error "Cannot determine working directory: requires either a Dired buffer or initial input"))
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
Call `ai-code-shell-cmd` when in Dired mode, shell modes or a region
is active; otherwise run the current file."
  (interactive)
  (cond
   ((derived-mode-p 'dired-mode)
    (ai-code-shell-cmd))
   ((memq major-mode '(shell-mode eshell-mode))
    (ai-code-shell-cmd))
   ((use-region-p)
    (let ((initial-input (string-trim (buffer-substring-no-properties (region-beginning)
                                                                      (region-end)))))
      (ai-code-shell-cmd initial-input)))
   (t
    (ai-code-run-current-file))))

;;;###autoload
(defun ai-code-build-or-test-project ()
  "Build or test the current project based on user choice.
If user chooses to build, check for build.sh in the project root
and send to AI for execution. Otherwise, ask AI to generate a build command.
If user chooses to test, call `ai-code-run-test'."
  (interactive)
  (let ((action (completing-read "Choose action: " '("Build project" "Test on scope") nil t)))
    (cond
     ((string= action "Build project") (ai-code-build-project))
     ((string= action "Test on scope") (ai-code-run-test)))))

;;;###autoload
(defun ai-code-build-project ()
  "Build the current project.
Check for build.sh in the project root and send to AI for execution.
Otherwise, ask AI to generate a build command."
  (interactive)
  (let* ((proj-root (or (and (fboundp 'projectile-project-root)
                             (ignore-errors (projectile-project-root)))
                        (magit-toplevel)))
         (build-script (when proj-root (expand-file-name "build.sh" proj-root)))
         (repo-context (ai-code--format-repo-context-info))
         (initial-input
          (if (and proj-root build-script (file-exists-p build-script))
              ;; build.sh exists, ask AI to run it
              (concat "Run the build script and report the results. "
                      (format "\nProject root: %s" proj-root)
                      (format "\nBuild script: %s" build-script)
                      (when repo-context (concat "\n" repo-context)))
            ;; No build.sh, ask AI to generate build command
            (concat "Build the current project. Provide the build command and execute it if possible. "
                    (when proj-root (format "\nProject root: %s" proj-root))
                    (when repo-context (concat "\n" repo-context)))))
         (prompt (ai-code-read-string "Send to AI: " initial-input)))
    (ai-code--insert-prompt prompt)))

(defvar ai-code--repo-context-info (make-hash-table :test #'equal)
  "Hash table storing context info lists per Git repository root.")

(defun ai-code--store-context-entry (repo-root context)
  "Store CONTEXT string for REPO-ROOT in `ai-code--repo-context-info'."
  (let ((current (gethash repo-root ai-code--repo-context-info)))
    (unless (member context current)
      (setq current (cons context current)))
    (puthash repo-root current ai-code--repo-context-info)))

;;;###autoload
(defun ai-code-add-context ()
  "Capture current buffer context and store it per Git repository.
When no region is selected, use the full file path and current function
\(if any).  When a region is active, use the file path with line range
in the form filepath#Lstart-Lend."
  (interactive)
  (let* ((current-root (magit-toplevel))
         (all-roots (let ((roots '()))
                      (walk-windows
                       (lambda (w)
                         (with-current-buffer (window-buffer w)
                           (let ((root (magit-toplevel)))
                             (when (and root (not (member root roots)))
                               (push root roots)))))
                       nil 'current-frame)
                      (when (fboundp 'ai-code-backends-infra--session-buffer-p)
                        (dolist (buf (buffer-list))
                          (when (ai-code-backends-infra--session-buffer-p buf)
                            (with-current-buffer buf
                              (let ((root (ignore-errors (magit-toplevel))))
                                (when (and root (not (member root roots)))
                                  (push root roots)))))))
                      (nreverse roots)))
         (ordered-roots (if (and current-root (member current-root all-roots))
                            (cons current-root (remove current-root all-roots))
                          all-roots))
         (repo-root (cond
                     ((null ordered-roots)
                      (user-error "Not inside a Git repository"))
                     ((= (length ordered-roots) 1)
                      (car ordered-roots))
                     (t
                      (completing-read "Select Git repository for context: "
                                       ordered-roots
                                       nil t nil nil (car ordered-roots))))))
    (if (derived-mode-p 'dired-mode)
        (let* ((all-marked (dired-get-marked-files))
               (file-at-point (dired-get-filename nil t))
               (has-marks (and all-marked
                               (not (and (= (length all-marked) 1)
                                         file-at-point
                                         (equal (car all-marked) file-at-point)))))
               (targets (cond
                         (has-marks all-marked)
                         (file-at-point (list file-at-point))
                         (t nil))))
          (unless targets
            (user-error "No file or directory selected in Dired"))
          (dolist (path targets)
            (ai-code--store-context-entry repo-root path))
          (message "Added context for %s: %s"
                   repo-root
                   (mapconcat #'identity targets ", ")))
      (unless buffer-file-name
        (user-error "Current buffer is not visiting a file"))
      (let ((context
             (if (use-region-p)
                 (let* ((start (region-beginning))
                        (end (region-end))
                        (start-line (line-number-at-pos (min start end)))
                        (end-line (line-number-at-pos (max start end))))
                   (format "%s#L%d-L%d" buffer-file-name start-line end-line))
               (let ((function-name (when (fboundp 'which-function)
                                      (which-function))))
                 (if (and function-name
                          (stringp function-name)
                          (not (string-empty-p function-name)))
                     (format "%s#%s" buffer-file-name function-name)
                   buffer-file-name)))))
        (ai-code--store-context-entry repo-root context)
        (message "Added context for %s: %s" repo-root context)))))

(defun ai-code-list-context ()
  "Display stored context entries grouped by Git repository."
  (interactive)
  (if (= (hash-table-count ai-code--repo-context-info) 0)
      (message "No stored context info.")
    (let ((sections '()))
      (maphash
       (lambda (repo contexts)
         (let ((entries (if contexts
                            (mapconcat (lambda (ctx)
                                         (concat "  - " ctx))
                                       (reverse contexts)
                                       "\n")
                          "  (no entries)")))
           (push (format "Repo: %s\n%s" repo entries) sections)))
       ai-code--repo-context-info)
      (message "%s" (string-join (nreverse sections) "\n\n")))))

(defun ai-code-clear-context (&optional arg)
  "Clear stored context entries.
Without ARG, clear entries for the current Git repository.
With prefix ARG, clear all repositories."
  (interactive "P")
  (if arg
      (progn
        (clrhash ai-code--repo-context-info)
        (message "Cleared all stored context info."))
    (let ((repo-root (or (magit-toplevel)
                         (user-error "Not inside a Git repository"))))
      (if (gethash repo-root ai-code--repo-context-info)
          (progn
            (remhash repo-root ai-code--repo-context-info)
            (message "Cleared context info for %s." repo-root))
        (message "No context info stored for %s." repo-root)))))

(defun ai-code-context-action (arg)
  "Add or clear context entries depending on ARG.
Without prefix ARG, add context and immediately list all stored entries.
With prefix ARG \[universal-argument], clear context; clearing the
current repo or all repos is delegated to `ai-code-clear-context'."
  (interactive "P")
  (if arg
      (call-interactively #'ai-code-clear-context)
    (call-interactively #'ai-code-add-context)
    (ai-code-list-context)))

(defun ai-code--format-repo-context-info ()
  "Return formatted repository context string or nil.
Includes stored context entries for the current Git repository if available."
  (when (and (boundp 'ai-code--repo-context-info)
             ai-code--repo-context-info)
    (let ((repo-root (condition-case nil
                         (magit-toplevel)
                       (error nil))))
      (when repo-root
        (let ((entries (gethash repo-root ai-code--repo-context-info)))
          (when entries
            (concat "\nStored repository context:\n"
                    (mapconcat (lambda (ctx)
                                 (concat "  - " ctx))
                               (reverse entries)
                               "\n"))))))))

;;;###autoload
(defun ai-code-toggle-current-buffer-dedicated (arg)
  "Toggle the dedicated state of the current buffer's window.
When a window is dedicated, Emacs will not automatically reuse it for
displaying other buffers.  With prefix ARG \[universal-argument],
toggle dedication for every window in the current frame."
  ;; DONE: if C-u pressed, toggle dedicate to all buffers inside current window
  (interactive "P")
  (let* ((targets (if arg
                      (window-list nil 'no-minibuf)
                    (list (selected-window))))
         (results '()))
    (dolist (window targets)
      (let ((buffer (window-buffer window)))
        (when (buffer-file-name buffer)
          (let ((new-status (not (window-dedicated-p window))))
            (set-window-dedicated-p window new-status)
            (push (format "%s: %s" (buffer-name buffer) (if new-status "dedicated" "free")) results)))))
    ;; DONE: show each affected buffer name and dedicate status
    (if results
        (message "Window dedication updated: %s" (mapconcat #'identity (nreverse results) ", "))
      (message "No file buffers found in target windows"))))

(provide 'ai-code-file)

;;; ai-code-file.el ends here
