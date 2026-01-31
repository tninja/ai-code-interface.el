;;; ai-code.el --- Unified interface for AI coding CLI such as Claude Code, Codex CLI, Gemini CLI, Copilot CLI, Opencode, Grok CLI, etc -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Version: 1.22
;; Package-Requires: ((emacs "28.1") (transient "0.8.0") (magit "2.1.0"))
;; URL: https://github.com/tninja/ai-code-interface.el

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This package provides a uniform Emacs interface for various AI-assisted software
;; development CLI tools. Its purpose is to offer a consistent user experience
;; across different AI backends, providing context-aware code actions, and integrating
;; seamlessly with AI-driven agile development workflows.
;;
;; URL: https://github.com/tninja/ai-code-interface.el
;;
;; Supported AI coding CLIs include:
;;   - Claude Code
;;   - Gemini CLI
;;   - OpenAI Codex
;;   - GitHub Copilot CLI
;;   - Opencode
;;   - Grok CLI
;;   - Cursor CLI
;;   - Kiro CLI
;;
;; Basic configuration example:
;;
;; (use-package ai-code
;;   :config
;;   ;; use codex as backend, other options are 'gemini, 'github-copilot-cli, 'opencode, 'grok, 'claude-code-ide, 'claude-code-el, 'claude-code, 'cursor, 'kiro
;;   (ai-code-set-backend 'codex) ;; set your preferred backend
;;   (global-set-key (kbd "C-c a") #'ai-code-menu)
;;   (global-auto-revert-mode 1)
;;   (setq auto-revert-interval 1)) ;; set to 1 second for faster update
;;
;; Key features:
;;   - Transient-driven Hub (C-c a) for all AI capabilities.
;;   - One key switching to different AI backend (C-c a s).
;;   - Context-aware code actions (change code, implement TODOs, explain code).
;;   - Agile development workflows (TDD cycle, refactoring navigator, review helper).
;;   - Seamless prompt management using Org-mode.
;;   - AI-assisted bash commands and productivity utilities.
;;   - Multiple AI coding sessions management.
;;
;; Many features are ported from aider.el, making it a powerful alternative for
;; developers who wish to switch between modern AI coding CLIs while keeping
;; the same interface and agile tools.

;;; Code:

(require 'org)
(require 'which-func)
(require 'magit)
(require 'transient)
(require 'seq)

(require 'ai-code-backends)
(require 'ai-code-backends-infra)
(require 'ai-code-input)
(require 'ai-code-prompt-mode)
(require 'ai-code-agile)
(require 'ai-code-git)
(require 'ai-code-change)
(require 'ai-code-discussion)
(require 'ai-code-codex-cli)
(require 'ai-code-github-copilot-cli)
(require 'ai-code-opencode)
(require 'ai-code-grok-cli)
(require 'ai-code-file)
(require 'ai-code-ai)
(require 'ai-code-notifications)

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
(declare-function ai-code-backends-infra--session-buffer-p "ai-code-backends-infra" (buffer))

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
With \\[universal-argument], append files and repo context.
With \\[universal-argument] \\[universal-argument], also append clipboard context.
ARG is the prefix argument."
  ;; Prefix levels control whether files/repo and clipboard context are included,
  ;; and the prompt label reflects the selected context.
  (interactive "P")
  (let* ((initial-input (when (use-region-p)
                          (string-trim-right
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end))
                           "\n")))
         (prefix-value (when arg (prefix-numeric-value arg)))
         (include-files-and-repo (and arg (>= prefix-value 4)))
         (include-clipboard (and arg (>= prefix-value 16)))
         (files-context-string (when include-files-and-repo
                                 (ai-code--get-context-files-string)))
         (repo-context-string (when include-files-and-repo
                                (ai-code--format-repo-context-info)))
         (clipboard-context (when include-clipboard
                              (ai-code--get-clipboard-text)))
         (prompt-label
          (cond
           (include-clipboard "Send to AI (files/repo/clipboard context): ")
           (include-files-and-repo "Send to AI (files/repo context): ")
           (t "Send to AI: "))))
    (when-let* ((prompt (ai-code-read-string prompt-label initial-input)))
      (let ((final-prompt
             (concat prompt
                     (or files-context-string "")
                     (or repo-context-string "")
                     (when (and clipboard-context
                                (string-match-p "\\S-" clipboard-context))
                       (concat "\n\nClipboard context:\n"
                               clipboard-context)))))
        (ai-code--insert-prompt final-prompt)))))

;;;###autoload
(defun ai-code-cli-switch-to-buffer-or-hide ()
  "Hide current buffer if its name starts and ends with '*'.
Otherwise switch to AI CLI buffer."
  (interactive)
  (if (and current-prefix-arg
           (ai-code-backends-infra--session-buffer-p (current-buffer)))
      (quit-window)
    ;; Try with argument first; fall back to no-arg call if function doesn't accept it
    (condition-case nil
        (ai-code-cli-switch-to-buffer t)
      (wrong-number-of-arguments ;; will be triggered during calling corresponding function in external backends such as claude-code-ide.el, claude-code.el, since the corresponding function doesn't have parameter
       (ai-code-cli-switch-to-buffer)))))

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
    ("a" "Start AI CLI (C-u: args)" ai-code-cli-start)
    ("R" "Resume AI CLI" ai-code-cli-resume)
    ("z" "Switch to AI CLI (C-u: hide)" ai-code-cli-switch-to-buffer-or-hide)
    ;; Use plist style to provide a dynamic description function.
    ("s" ai-code-select-backend :description ai-code--select-backend-description)
    ("u" "Install / Upgrade AI CLI" ai-code-upgrade-backend)
    ("g" "Open backend config (eg. add mcp)" ai-code-open-backend-config)
    ("|" "Apply prompt on file" ai-code-apply-prompt-on-current-file)]

   ["AI Code Actions"
    (ai-code--infix-toggle-suffix)
    ("c" "Code change (C-u: clipboard)" ai-code-code-change)
    ("i" "Implement TODO (C-u: clipboard)" ai-code-implement-todo)
    ("q" "Ask question (C-u: clipboard)" ai-code-ask-question)
    ("x" "Explain code in scope" ai-code-explain)
    ("<SPC>" "Send command (C-u: context)" ai-code-send-command)
    ("@" "Add context (C-u: clear)" ai-code-context-action)
    ("K" "Create or open task file" ai-code-create-or-open-task-file)]

   ["AI Agile Development"
    ("r" "Refactor Code"               ai-code-refactor-book-method)
    ("t" "Test Driven Development"     ai-code-tdd-cycle)
    ("v" "Pull or Review Code Change"  ai-code-pull-or-review-diff-file)
    ;; ("b" "Send prompt block to AI" ai-code-prompt-send-block)
    ("!" "Run Current File or Command" ai-code-run-current-file-or-shell-cmd)
    ("b" "Build project"               ai-code-build-project)
    ("p" "Open prompt history file" ai-code-open-prompt-file)
    ("I" "Insert function name at point" ai-code-insert-function-at-point)]

   ["Other Tools"
    ("." "Init projectile and gtags" ai-code-init-project)
    ("e" "Debug exception (C-u: clipboard)" ai-code-investigate-exception)
    ("f" "Fix Flycheck errors in scope" ai-code-flycheck-fix-errors-in-scope)
    ("k" "Copy Cur File Name (C-u: full)" ai-code-copy-buffer-file-name-to-clipboard)
    ("d" "Toggle current buffer dedicated" ai-code-toggle-current-buffer-dedicated)
    ;; ("o" "Open Clipboard file dir" ai-code-open-clipboard-file-path-as-dired)
    ("m" "Debug python MCP server" ai-code-debug-mcp)
    ("n" "Take notes from AI session region" ai-code-take-notes)
    ("N" "Toggle notifications" ai-code-notifications-toggle)]])



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

;;;###autoload
(defun ai-code-evil-setup ()
  "Setup AI Code integration with Evil mode.
This function configures SPC key binding in Evil normal state for
special AI Code buffers.  Call this function after Evil is loaded,
typically in your Emacs configuration with:
  (with-eval-after-load \\='evil (ai-code-evil-setup))"
  (interactive)
  (when (and (featurep 'evil) (boundp 'evil-normal-state-map))
    (unless ai-code--original-spc-command-in-evil-normal-state
      (setq ai-code--original-spc-command-in-evil-normal-state
            (lookup-key evil-normal-state-map (kbd "SPC"))))
    (when ai-code--original-spc-command-in-evil-normal-state
      (define-key evil-normal-state-map (kbd "SPC")
        #'ai-code-spc-command-for-special-buffer-in-evil))))

(provide 'ai-code)

;;; ai-code.el ends here
