;;; ai-code-interface.el --- AI code interface for editing AI prompt files -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Version: 0.20
;; Package-Requires: ((emacs "26.1") (transient "0.8.0") (magit "2.1.0"))

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides a major mode for editing AI prompt files.

;;; Code:

(require 'org)
(require 'which-func)
(require 'magit)
(require 'transient)
(require 'seq)

(require 'ai-code-backends)
(require 'ai-code-input)
(require 'ai-code-prompt-mode)
(require 'ai-code-agile)
(require 'ai-code-git)
(require 'ai-code-change)
(require 'ai-code-discussion)
(require 'ai-code-codex-cli)
(require 'ai-code-github-copilot-cli)

;; Forward declarations for dynamically defined backend functions
(declare-function ai-code-cli-start "ai-code-backends")
(declare-function ai-code-cli-switch-to-buffer "ai-code-backends")
(declare-function ai-code-cli-send-command "ai-code-backends" (command))
(declare-function ai-code-current-backend-label "ai-code-backends")
(declare-function ai-code-open-backend-config "ai-code-backends")

(declare-function ai-code--process-word-for-filepath "ai-code-prompt-mode" (word git-root-truename))

;; Default aliases are set when a backend is applied via `ai-code-select-backend`.

;;;###autoload
(defcustom ai-code-use-gptel-headline nil
  "Whether to use GPTel to generate headlines for prompt sections.
If non-nil, call `gptel-get-answer` from gptel-assistant.el to generate
headlines instead of using the current time string."
  :type 'boolean
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-prompt-suffix nil
  "Suffix text to append to prompts after a new line.
If non-nil, this text will be appended to the end of each prompt
with a newline separator."
  :type '(choice (const nil) string)
  :group 'ai-code)

(defcustom ai-code-use-prompt-suffix t
  "When non-nil, append `ai-code-prompt-suffix` where supported."
  :type 'boolean
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-cli "claude"
  "The command-line AI tool to use for `ai-code-apply-prompt-on-current-file`."
  :type 'string
  :group 'ai-code)

;;;###autoload
(defun ai-code-send-command ()
  "Read a prompt from the user and send it to the AI service."
  (interactive)
  (let ((initial-input (when (use-region-p)
                         (string-trim-right
                          (buffer-substring-no-properties (region-beginning)
                                                          (region-end))
                          "\n"))))
    (when-let ((prompt (ai-code-read-string "Send to AI: " initial-input)))
      (ai-code--insert-prompt prompt))))

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
If the clipboard contains a valid file path, open its directory in
dired in another window and move the cursor to that file.
If the clipboard contains a directory path, open it directly in
dired in another window."
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
         (default-command (cond
                          ;; Check if current file is in the last run command
                          ((and last-command file-name 
                                (string-match-p (regexp-quote file-name) last-command))
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
    (let ((command (read-string 
                   (format "Run command for %s: " file-name)
                   default-command
                   'ai-code-run-file-history)))
      (let ((default-directory (file-name-directory current-file)))
        (compile command)))))

;;;###autoload
(defun ai-code-apply-prompt-on-current-file ()
  "Apply a user prompt to the current file and send to an AI CLI tool.
The file can be the one in the current buffer or the one at point in a dired buffer.
It constructs a shell command:
sed \"1i <prompt>: \" <file> | <ai-code-cli>
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
      (let ((default-directory (file-name-directory file-name)))
        (compile command)))))

;;;###autoload
(defun ai-code-cli-switch-to-buffer-or-hide ()
  "Hide current buffer if its name starts and ends with '*', otherwise switch to AI CLI buffer."
  (interactive)
  (if (and (string-prefix-p "*" (buffer-name))
           (string-suffix-p "*" (buffer-name)))
      (quit-window)
    (ai-code-cli-switch-to-buffer)))

(defclass ai-code--use-prompt-suffix-type (transient-lisp-variable)
  ((variable :initform 'ai-code-use-prompt-suffix)
   (format :initform "%k %d %v")
   (reader :initform #'transient-lisp-variable--read-value))
  "Toggle helper for `ai-code-use-prompt-suffix`.")

(transient-define-infix ai-code--infix-toggle-suffix ()
  "Toggle `ai-code-use-prompt-suffix`."
  :class 'ai-code--use-prompt-suffix-type
  :key "^"
  :description "Use prompt suffix:"
  :reader (lambda (_prompt _initial-input _history)
            (not ai-code-use-prompt-suffix)))

(defun ai-code--select-backend-description (&rest _)
  "Dynamic description for the Select Backend menu item.
Shows the current backend label to the right."
  (format "Select Backend (%s)" (ai-code-current-backend-label)))

;;;###autoload
(transient-define-prefix ai-code-menu ()
  "Transient menu for AI Code Interface interactive functions."
  ["AI Code Commands"
   ["AI CLI session"
    ("a" "Start AI CLI" ai-code-cli-start)
    ("R" "Resume AI CLI" ai-code-cli-resume)
    ("z" "Switch to AI CLI" ai-code-cli-switch-to-buffer-or-hide)
    ("s" ai-code--select-backend-description ai-code-select-backend)
    ("g" "Open backend config (eg. add mcp)" ai-code-open-backend-config)
    ("p" "Open prompt file" ai-code-open-prompt-file)
    ("b" "Send prompt block to AI" ai-code-prompt-send-block)
    ]
   ["AI Code Actions"
    (ai-code--infix-toggle-suffix)
    ("c" "Code change (C-u: global)" ai-code-code-change)
    ("i" "Implement TODO" ai-code-implement-todo)
    ("q" "Ask question (C-u: global)" ai-code-ask-question)
    ("x" "Explain code" ai-code-explain)
    ("<SPC>" "Send command to AI" ai-code-send-command)
    ]
   ["AI Agile Development"
    ("r" "Refactor Code"               ai-code-refactor-book-method)
    ("t" "Test Driven Development"     ai-code-tdd-cycle)
    ("v" "Pull or Review Code Change"  ai-code-pull-or-review-diff-file)
    ("!" "Run Current File" ai-code-run-current-file)
    ]
   ["Other Tools"
    ("e" "Debug exception (C-u: global)" ai-code-investigate-exception)
    ("f" "Fix Flycheck errors in scope" ai-code-flycheck-fix-errors-in-scope)
    ("k" "Copy Cur File Name (C-u: full)" ai-code-copy-buffer-file-name-to-clipboard)
    ("o" "Open Clipboard file dir" ai-code-open-clipboard-file-path-as-dired)
    ("|" "Apply prompt on file" ai-code-apply-prompt-on-current-file)
    ]
   ])

;;;###autoload
(global-set-key (kbd "C-c a") #'ai-code-menu)

;; When in a special buffer (e.g., *claude-code*) and using evil-mode,
;; pressing SPC in normal state will send the prompt.

;; following code is buggy
(defvar ai-code--original-spc-command-in-evil-normal-state nil
  "Original command for SPC in `evil-normal-state`.")

(defun ai-code-spc-command-for-special-buffer-in-evil ()
  "In special buffers (*...*), run `ai-code-send-command`.
Otherwise, run the original command for SPC."
  (interactive)
  (if (and (string-prefix-p "*" (buffer-name))
           (string-suffix-p "*" (buffer-name)))
      (call-interactively #'ai-code-send-command)
    (when ai-code--original-spc-command-in-evil-normal-state
      (call-interactively ai-code--original-spc-command-in-evil-normal-state))))

(with-eval-after-load 'evil
  (when (boundp 'evil-normal-state-map)
    (unless ai-code--original-spc-command-in-evil-normal-state
      (setq ai-code--original-spc-command-in-evil-normal-state
            (lookup-key evil-normal-state-map (kbd "SPC"))))
    (when ai-code--original-spc-command-in-evil-normal-state
      (define-key evil-normal-state-map (kbd "SPC")
        #'ai-code-spc-command-for-special-buffer-in-evil))))

(provide 'ai-code-interface)

;;; ai-code-interface.el ends here
