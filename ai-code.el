;;; ai-code.el --- Unified interface for AI coding CLI such as Claude Code, Codex CLI, Gemini CLI, Copilot CLI, Opencode, Grok CLI, Aider CLI, etc -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Version: 1.42
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
;;   - CodeBuddy Code CLI
;;   - Aider CLI
;;
;; Basic configuration example:
;;
;; (use-package ai-code
;;   :config
;;   ;; use codex as backend, other options are 'gemini, 'github-copilot-cli, 'opencode, 'grok, 'claude-code-ide, 'claude-code-el, 'claude-code, 'cursor, 'kiro, 'codebuddy, 'aider
;;   (ai-code-set-backend 'codex) ;; set your preferred backend
;;   (global-set-key (kbd "C-c a") #'ai-code-menu)
;;   ;; Optional: Enable @ file completion in comments and AI sessions
;;   (ai-code-prompt-filepath-completion-mode 1)
;;   ;; Optional: Ask AI to run test after code changes, for a tighter build-test loop
;;   (setq ai-code-auto-test-type 'test-after-change)
;;   ;; Optional: In the AI session buffer (Evil normal state), SPC triggers the prompt entry UI
;;   (with-eval-after-load 'evil (ai-code-backends-infra-evil-setup))
;;   (global-auto-revert-mode 1)
;;   (setq auto-revert-interval 1) ;; set to 1 second for faster update
;;   )
;;
;; Key features:
;;   - Transient-driven Hub (C-c a) for all AI capabilities.
;;   - One key switching to different AI backend (C-c a s).
;;   - Context-aware code actions (change code, implement TODOs, explain code, @ completion).
;;   - Agile development workflows (TDD cycle, refactoring navigator, review helper, Build / Test feedback loop).
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

(defgroup ai-code nil
  "Unified interface for multiple AI coding CLIs."
  :group 'tools)

(require 'ai-code-backends)
(require 'ai-code-backends-infra)
(require 'ai-code-input)
(require 'ai-code-prompt-mode)
(require 'ai-code-agile)
(require 'ai-code-git)
(require 'ai-code-change)
(require 'ai-code-discussion)
(require 'ai-code-codex-cli)
(require 'ai-code-aider-cli)
(require 'ai-code-github-copilot-cli)
(require 'ai-code-opencode)
(require 'ai-code-grok-cli)
(require 'ai-code-codebuddy-cli)
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
(declare-function ai-code-open-backend-agent-file "ai-code-backends")
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

;;;###autoload
(defcustom ai-code-use-prompt-suffix t
  "When non-nil, append `ai-code-prompt-suffix` where supported."
  :type 'boolean
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-test-after-code-change-suffix
  "If any program code changes, run unit-tests and follow up on the test-result (fix code if there is an error)."
  "User-provided prompt suffix for test-after-code-change."
  :type '(choice (const nil) string)
  :group 'ai-code)

;;;###autoload
(defvar ai-code-auto-test-suffix ai-code-test-after-code-change-suffix
  "Default prompt suffix to request running tests after code changes.")

(defun ai-code--test-after-code-change--resolve-tdd-suffix ()
  "Return the TDD-style suffix for test-after-code-change prompts."
  (let ((pattern (when (and (boundp 'ai-code--tdd-test-pattern-instruction)
                            (stringp ai-code--tdd-test-pattern-instruction))
                   ai-code--tdd-test-pattern-instruction)))
    (concat "Follow TDD principles - write the failing test first, then implement the minimal code to make it pass. Only update test and source code. Run the tests and follow up with the test result (fix code if there is error)."
            (or pattern ""))))

(defun ai-code--test-after-code-change--set (symbol value)
  "Set SYMBOL to VALUE and update related suffix behavior."
  (set-default symbol value)
  (set symbol value)
  (pcase value
    ('test-after-change
     (setq ai-code-auto-test-suffix
           ai-code-test-after-code-change-suffix))
    ('tdd
     (setq ai-code-auto-test-suffix
           (ai-code--test-after-code-change--resolve-tdd-suffix)))
    (_
     (setq ai-code-auto-test-suffix nil))))

(defun ai-code--apply-auto-test-type (value)
  "Set `ai-code-auto-test-type` and refresh related suffix."
  (setq ai-code-auto-test-type value)
  (ai-code--test-after-code-change--set 'ai-code-auto-test-type value)
  value)

(defcustom ai-code-auto-test-type nil
  "Select how prompts request tests after code changes."
  :type '(choice (const :tag "Use test after code change prompt" test-after-change)
                 (const :tag "Use TDD Red+Green prompt" tdd)
                 (const :tag "Off" nil))
  :set #'ai-code--test-after-code-change--set
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

(defclass ai-code--code-change-auto-test-type (transient-lisp-variable)
  ((variable :initform 'ai-code-auto-test-type)
   (format :initform "%k %d %v")
   (reader :initform #'transient-lisp-variable--read-value))
  "Selection helper for `ai-code-auto-test-type`.")

(transient-define-infix ai-code--infix-select-code-change-auto-test ()
  "Select `ai-code-auto-test-type` mode."
  :class 'ai-code--code-change-auto-test-type
  :key "T"
  :description "Auto test type:"
  :reader (lambda (_prompt _initial-input _history)
            (let* ((choices '(("Use test after code change prompt" . test-after-change)
                              ("Use TDD Red+Green prompt" . tdd)
                              ("Off" . nil)))
                   (choice (completing-read "Test after code change: "
                                            (mapcar #'car choices)
                                            nil t)))
              (let ((value (cdr (assoc choice choices))))
                (ai-code--apply-auto-test-type value)
                (message "Auto test type set to %s; prompt suffix is now %s"
                         (or value "off")
                         (or ai-code-auto-test-suffix "cleared"))
                value))))

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
    ("R" "Resume AI CLI (C-u: args)" ai-code-cli-resume)
    ("z" "Switch to AI CLI (C-u: hide)" ai-code-cli-switch-to-buffer-or-hide)
    ;; Use plist style to provide a dynamic description function.
    ("s" ai-code-select-backend :description ai-code--select-backend-description)
    ("u" "Install / Upgrade AI CLI" ai-code-upgrade-backend)
    ("g" "Open backend config (eg. add mcp)" ai-code-open-backend-config)
    ("G" "Open backend repo agent file" ai-code-open-backend-agent-file)
    ("|" "Apply prompt on file" ai-code-apply-prompt-on-current-file)
    ]

   ["AI Code Actions"
    (ai-code--infix-toggle-suffix)
    ("c" "Code change (C-u: clipboard)" ai-code-code-change)
    ("i" "Implement TODO (C-u: clipboard)" ai-code-implement-todo)
    ("q" "Ask question (C-u: clipboard)" ai-code-ask-question)
    ("x" "Explain code in scope" ai-code-explain)
    ("<SPC>" "Send command (C-u: context)" ai-code-send-command)
    ("@" "Add context (C-u: clear)" ai-code-context-action)
    ]

   ["AI Agile Development"
    (ai-code--infix-select-code-change-auto-test)
    ("r" "Refactor Code"               ai-code-refactor-book-method)
    ("t" "Test Driven Development"     ai-code-tdd-cycle)
    ("v" "Pull or Review Code Change"  ai-code-pull-or-review-diff-file)
    ;; ("b" "Send prompt block to AI" ai-code-prompt-send-block)
    ("!" "Run Current File or Command" ai-code-run-current-file-or-shell-cmd)
    ("b" "Build / Test project"               ai-code-build-or-test-project)
    ;; ("I" "Insert function name at point" ai-code-insert-function-at-point)
    ("K" "Create or open task file" ai-code-create-or-open-task-file)
    ("n" "Take notes from AI session region" ai-code-take-notes)
    ]

   ["Other Tools"
    ("." "Init projectile and gtags" ai-code-init-project)
    ("e" "Debug exception (C-u: clipboard)" ai-code-investigate-exception)
    ("f" "Fix Flycheck errors in scope" ai-code-flycheck-fix-errors-in-scope)
    ("k" "Copy Cur File Name (C-u: full)" ai-code-copy-buffer-file-name-to-clipboard)
    ;; ("d" "Toggle current buffer dedicated" ai-code-toggle-current-buffer-dedicated)
    ("o" "Open recent file (C-u: insert)" ai-code-git-repo-recent-modified-files)
    ;; ("o" "Open Clipboard file dir" ai-code-open-clipboard-file-path-as-dired)
    ("p" "Open prompt history file" ai-code-open-prompt-file)
    ("m" "Debug python MCP server" ai-code-debug-mcp)
    ("N" "Toggle notifications" ai-code-notifications-toggle)]])



;; DONE: When in an ai-coding session (e.g., with -backends-infra.el code) and using evil-mode,
;; pressing SPC in normal state will trigger ai-code-send-command.
;; Fixed implementation moved to ai-code-backends-infra.el:
;;   - `ai-code-backends-infra--evil-spc-command' - intercepts SPC in AI session buffers
;;   - `ai-code-backends-infra-evil-setup' - call with (with-eval-after-load 'evil ...)

;; (with-eval-after-load 'evil (ai-code-backends-infra-evil-setup))

(provide 'ai-code)

;;; ai-code.el ends here
