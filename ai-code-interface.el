;;; ai-code-interface.el --- AI code interface for editing AI prompt files -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Version: 0.61
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
(require 'ai-code-opencode)
(require 'ai-code-file)
(require 'ai-code-ai)

;; Forward declarations for dynamically defined backend functions
(declare-function ai-code-cli-start "ai-code-backends")
(declare-function ai-code-cli-resume "ai-code-backends")
(declare-function ai-code-cli-switch-to-buffer "ai-code-backends")
(declare-function ai-code-cli-send-command "ai-code-backends" (command))
(declare-function ai-code-current-backend-label "ai-code-backends")
(declare-function ai-code-set-backend "ai-code-backends")
(declare-function ai-code-select-backend "ai-code-backends")
(declare-function ai-code-open-backend-config "ai-code-backends")
(declare-function ai-code-upgrade-backend "ai-code-backends")

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

(defun ai-code--get-clipboard-text ()
  "Return the current clipboard contents as a plain string, or nil if unavailable."
  (let* ((selection (when (fboundp 'gui-get-selection)
                      (or (let ((text (gui-get-selection 'CLIPBOARD 'UTF8_STRING)))
                            (and (stringp text) (not (string-empty-p text)) text))
                          (let ((text (gui-get-selection 'CLIPBOARD 'STRING)))
                            (and (stringp text) (not (string-empty-p text)) text)))))
         (kill-text (condition-case nil
                        (current-kill 0 t)
                      (error nil))))
    (let ((text (or selection kill-text)))
      (when (stringp text)
        (substring-no-properties text)))))

;;;###autoload
(defun ai-code-send-command (arg)
  "Read a prompt from the user and send it to the AI service.
With a prefix argument (\[universal-argument]), append the clipboard contents as context."
  (interactive "P")
  (let* ((initial-input (when (use-region-p)
                          (string-trim-right
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end))
                           "\n")))
         (clipboard-context (when arg (ai-code--get-clipboard-text)))
         (prompt-label (if (and clipboard-context
                                (string-match-p "\\S-" clipboard-context))
                           "Send to AI (clipboard context): "
                         "Send to AI: ")))
    (when-let ((prompt (ai-code-read-string prompt-label initial-input)))
      (let ((final-prompt (if (and clipboard-context
                                   (string-match-p "\\S-" clipboard-context))
                              (concat prompt
                                      "\n\nClipboard context:\n"
                                      clipboard-context)
                            prompt)))
        (ai-code--insert-prompt final-prompt)))))

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
    ;; Use plist style to provide a dynamic description function.
    ("s" ai-code-select-backend :description ai-code--select-backend-description)
    ("u" "Upgrade AI CLI" ai-code-upgrade-backend)
    ("g" "Open backend config (eg. add mcp)" ai-code-open-backend-config)
    ("|" "Apply prompt on file" ai-code-apply-prompt-on-current-file)
    ]
   ["AI Code Actions"
    (ai-code--infix-toggle-suffix)
    ("c" "Code change (C-u: clipboard)" ai-code-code-change)
    ("i" "Implement TODO (C-u: clipboard)" ai-code-implement-todo)
    ("q" "Ask question (C-u: clipboard)" ai-code-ask-question)
    ("x" "Explain code" ai-code-explain)
    ("<SPC>" "Send command (C-u: clipboard)" ai-code-send-command)
    ("@" "Add context (C-u: clear)" ai-code-context-action)
    ]
   ["AI Agile Development"
    ("r" "Refactor Code"               ai-code-refactor-book-method)
    ("t" "Test Driven Development"     ai-code-tdd-cycle)
    ("v" "Pull or Review Code Change"  ai-code-pull-or-review-diff-file)
    ("p" "Open prompt file" ai-code-open-prompt-file)
    ("b" "Send prompt block to AI" ai-code-prompt-send-block)
    ("!" "Run Current File or Command" ai-code-run-current-file-or-shell-cmd)
    ]
   ["Other Tools"
    ("." "Init projectile and tags" ai-code-init-project)
    ("e" "Debug exception (C-u: clipboard)" ai-code-investigate-exception)
    ("f" "Fix Flycheck errors in scope" ai-code-flycheck-fix-errors-in-scope)
    ("k" "Copy Cur File Name (C-u: full)" ai-code-copy-buffer-file-name-to-clipboard)
    ("o" "Open Clipboard file dir" ai-code-open-clipboard-file-path-as-dired)
    ("m" "Debug python MCP server" ai-code-debug-mcp)
    ("n" "Take notes from AI session region" ai-code-take-notes)
    ]
   ])

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
