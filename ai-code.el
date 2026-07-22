;;; ai-code.el --- Unified interface for AI coding backends such as Codex CLI, Antigravity CLI, Claude Code, Opencode, etc -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Assisted-by:
;; - CodexCLI:GPT-5.5
;; - GithubCopilotCLI:GPT-5.4
;; - ClaudeCode:Opus-4.8
;; - GeminiCLI:gemini-flash-3.5
;;
;; Version: 1.900
;; Package-Requires: ((emacs "29.1") (transient "0.9.0") (magit "2.1.0"))
;; URL: https://github.com/tninja/ai-code-interface.el

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This package provides a uniform Emacs interface for various
;; AI-assisted software development CLI tools.  Its purpose is to
;; offer a consistent user experience across different AI backends,
;; providing context-aware code actions, and integrating seamlessly
;; with AI-driven agile development workflows.
;;
;; URL: https://github.com/tninja/ai-code-interface.el
;;
;; Supported AI coding CLIs include:
;;   - OpenAI Codex
;;   - Pi
;;   - Antigravity CLI
;;   - Opencode
;;   - Claude Code
;;   - GitHub Copilot CLI
;;   - Gemini CLI
;;   - Kilo
;;   - Grok CLI
;;   - Cursor CLI
;;   - Kiro CLI
;;   - Open Interpreter CLI (Codex-compatible)
;;   - CodeBuddy Code CLI
;;   - Aider CLI
;;   - agent-shell
;;   - ECA (Editor Code Assistant)
;;
;; New User Quick Start:
;;   1) Minimal setup:
;;
;;      (use-package ai-code
;;        :config
;;        (ai-code-set-backend 'codex)
;;        ;; Optional: use a narrower transient menu on smaller frames.
;;        ;; (setq ai-code-menu-layout 'two-columns)
;;        (global-set-key (kbd "C-c a") #'ai-code-menu))
;;
;;   2) First 60 seconds:
;;      - C-c a a : Start the selected AI CLI session
;;      - C-c a c : Ask AI to change current function/region
;;      - C-c a q : Ask question only (no code change)
;;      - C-c a z : Jump back to active AI session buffer
;;
;; Basic configuration example:
;;
;; (use-package ai-code
;;   :config
;;   ;; use codex as backend, other options are 'pi, 'gemini, 'github-copilot-cli, 'open-interpreter, 'opencode, 'kilo, 'grok, 'claude-code-ide, 'claude-code-el, 'claude-code, 'cursor, 'kiro, 'codebuddy, 'aider, 'agent-shell, 'eca
;;   (ai-code-set-backend 'codex) ;; set your preferred backend
;;   ;; Optional: use a narrower transient menu on smaller frames
;;   ;; (setq ai-code-menu-layout 'two-columns)
;;   (global-set-key (kbd "C-c a") #'ai-code-menu)
;;   ;; Optional: Try ghostel or eat as an backend infra
;;   ;; (setq ai-code-backends-infra-terminal-backend 'ghostel) ;; 'eat is another option
;;   ;; Optional: Disable @ file completion in comments and AI sessions
;;   ;; (ai-code-prompt-filepath-completion-mode -1)
;;   ;; Optional: Configure AI test prompting mode (e.g., ask about running tests/TDD) for a tighter build-test loop
;;   (setq ai-code-auto-test-type 'ask-me)
;;   ;; Optional: Disable numbered next steps for discussion prompts at send time
;;   ;; (setq ai-code-discussion-auto-follow-up-enabled nil)
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
(require 'subr-x)

(defgroup ai-code nil
  "Unified interface for multiple AI coding CLIs."
  :group 'tools)

;;;###autoload
(defcustom ai-code-menu-layout 'default
  "Layout used by `ai-code-menu`.
`default' keeps the original wide multi-column transient.
`two-columns' uses a narrower two-column transient with the same commands."
  :type '(choice (const :tag "Default multi-column menu" default)
                 (const :tag "Narrower two-column menu" two-columns))
  :group 'ai-code)

(require 'ai-code-backends)
(require 'ai-code-backends-infra)
(require 'ai-code-session)
(require 'ai-code-input)
(require 'ai-code-task)
(require 'ai-code-prompt-mode)
(require 'ai-code-send)
(require 'ai-code-agile)
(require 'ai-code-git)
(require 'ai-code-github)
(require 'ai-code-change)
(require 'ai-code-discussion)
(require 'ai-code-codex-cli)
(require 'ai-code-aider-cli)
(require 'ai-code-github-copilot-cli)
(require 'ai-code-opencode)
(require 'ai-code-kilo)
(require 'ai-code-grok-cli)
(require 'ai-code-codebuddy-cli)
(require 'ai-code-file)
(require 'ai-code-doc)
(require 'ai-code-harness)
(require 'ai-code-ai)
(require 'ai-code-mcp-server)
(require 'ai-code-notifications)
(require 'ai-code-onboarding)

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

(defvar ai-code-mcp-agent-enabled-backends)
(declare-function ai-code-install-backend-skills "ai-code-backends")
(declare-function ai-code-backends-infra--session-buffer-p "ai-code-backends-infra" (buffer))
(declare-function ai-code-search-notes-with-ai "ai-code-prompt-mode")

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
(defcustom ai-code-quick-prompts nil
  "List of pre-defined prompt strings for quick sending to AI.
Each entry is a string that can be selected via `ai-code-send-quick-prompt`
and sent to the current AI session."
  :type '(repeat string)
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-cli "claude"
  "The command-line AI tool to use for `ai-code-apply-prompt-on-current-file`."
  :type 'string
  :group 'ai-code)

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
(defun ai-code-send-quick-prompt ()
  "Select and send a pre-defined prompt from `ai-code-quick-prompts`."
  ;; DONE: use ai-code-read-string to allow user to modify the prompt before sending
  (interactive)
  (unless ai-code-quick-prompts
    (user-error "No quick prompts configured; customize `ai-code-quick-prompts' first"))
  (let ((selected (completing-read "Quick prompt: " ai-code-quick-prompts nil t)))
    (ai-code--confirm-and-send "Edit quick prompt: " selected)))

(defconst ai-code-session-checkpoint-prompt
  (concat
   "Please stop and output a CHECKPOINT:\n"
   "- Goal\n"
   "- Files changed\n"
   "- Current hypothesis\n"
   "- Tests/build result\n"
   "- Blockers\n"
   "- Recommended next action\n"
   "Do not continue editing after this checkpoint")
  "Prompt sent by `ai-code-session-checkpoint'.")

;;;###autoload
(defun ai-code-session-checkpoint ()
  "Ask the active AI session to summarize its current state and stop editing."
  (interactive)
  ;; DONE: let user to edit and confirm the prompt before sending, with read-string function
  (when-let* ((prompt (read-string "Edit checkpoint prompt: "
                                   ai-code-session-checkpoint-prompt)))
    (ai-code--insert-prompt prompt)))

(defun ai-code--emacs-runtime-debug-prompt (description eval-available-p
                                                       &optional region-text
                                                       region-location-info
                                                       buffer-scope
                                                       function-name
                                                       files-context-string
                                                       repo-context-string
                                                       clipboard-context)
  "Return an Emacs runtime debugging prompt from DESCRIPTION.
EVAL-AVAILABLE-P reports whether `eval_elisp' is globally enabled.
Optional REGION-TEXT and REGION-LOCATION-INFO add selected-region context.
BUFFER-SCOPE, FUNCTION-NAME, FILES-CONTEXT-STRING, REPO-CONTEXT-STRING,
and CLIPBOARD-CONTEXT add broader debugging context."
  (let ((scope-string
         (ai-code--emacs-runtime-debug-scope-string
          buffer-scope function-name files-context-string))
        (context-string
         (ai-code--emacs-runtime-debug-context-string
          repo-context-string clipboard-context)))
    (format
     "Use the Emacs MCP tools available in this session to debug my Emacs runtime.\n\
The issue may involve an interactive function or a key binding.\n\
%s\n\n\
Inspect the relevant runtime state first: keymaps, command metadata,\n\
variables, recent messages, load state, and the last backtrace when useful.\n\
Explain what you find, then recommend the smallest fix or next step.\n\n\
Runtime issue description:\n\
%s%s%s%s"
     (if eval-available-p
         "eval_elisp is enabled in your Emacs MCP config."
       "eval_elisp is disabled in your Emacs MCP config, so rely on non-eval inspection tools unless you first enable ai-code-mcp-debug-tools-enable-eval-elisp.")
     description
     (if (string-empty-p scope-string)
         ""
       (concat "\n\nScope:\n" scope-string))
     (if region-text
         (concat
          "\n\nSelected region:\n"
          (when region-location-info
            (concat region-location-info "\n"))
          region-text)
       "")
     (if (string-empty-p context-string)
         ""
       (concat "\n\nContext:\n" context-string)))))

(defun ai-code--emacs-runtime-debug-scope-string (buffer-scope function-name
                                                              files-context-string)
  "Return scope text for one Emacs runtime debug prompt.
BUFFER-SCOPE describes the current file or buffer.  FUNCTION-NAME and
FILES-CONTEXT-STRING are optional additional scope levels."
  (string-trim
   (concat
    (or buffer-scope "")
    (when function-name
      (format "\nFunction: %s" function-name))
    (or files-context-string ""))))

(defun ai-code--emacs-runtime-debug-context-string (repo-context-string
                                                    clipboard-context)
  "Return context text for one Emacs runtime debug prompt.
REPO-CONTEXT-STRING is stored repository context.  CLIPBOARD-CONTEXT is
optional runtime/debugging text from the clipboard."
  (string-trim
   (concat
    (or repo-context-string "")
    (when (and repo-context-string
               clipboard-context)
      "\n\n")
    (when clipboard-context
      (concat "Clipboard context:\n" clipboard-context)))))

;;;###autoload
(defun ai-code-debug-emacs-runtime ()
  "Assemble and send an Emacs runtime debugging prompt for the current AI session."
  (interactive)
  ;; DONE: similar to ai-code-investigate-exception, this function should support multiple level of context: file, function, selected region, clipboard, etc
  (unless (bound-and-true-p ai-code-mcp-debug-tools-enabled)
    (user-error
     "Enable ai-code-mcp-debug-tools-enabled before using Emacs runtime debugging"))
  (let* ((description
          (ai-code-read-string
           "Describe the Emacs runtime issue (it can be an interactive function or a key binding): "))
         (region-text (when (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (region-location-info (when region-text
                                 (ai-code--get-region-location-info
                                  (region-beginning)
                                  (region-end))))
         (buffer-scope (if buffer-file-name
                           (format "Current file: %s" buffer-file-name)
                         (format "Current buffer: %s" (buffer-name))))
         (function-name (which-function))
         (files-context-string (ai-code--get-context-files-string))
         (repo-context-string (ai-code--format-repo-context-info))
         (clipboard-context (when current-prefix-arg
                              (let ((text (ai-code--get-clipboard-text)))
                                (when (and text
                                           (string-match-p "\\S-" text))
                                  text))))
         (eval-available-p
          (bound-and-true-p ai-code-mcp-debug-tools-enable-eval-elisp)))
    (if eval-available-p
        (message
         "eval_elisp is enabled in your Emacs MCP config. Emacs can use it for debugging.")
      (message
       "eval_elisp is disabled in your Emacs MCP config. It is better to turn it on to improve debugging capability."))
    (when description
      (ai-code--confirm-and-send
       "Confirm and edit Emacs runtime debug prompt: "
       (ai-code--emacs-runtime-debug-prompt
        description
        eval-available-p
        region-text
        region-location-info
        buffer-scope
        function-name
        files-context-string
        repo-context-string
        clipboard-context)))))

;;;###autoload
(defun ai-code-cli-switch-to-buffer-or-hide ()
  "Hide the current buffer when its name both begins and ends with '*'.
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
            (let ((next-val (ai-code--cycle-auto-test-type-value ai-code-auto-test-type)))
              (ai-code--apply-auto-test-type next-val)
              (message "Auto test type set to %s; prompt suffix is now %s"
                       (or next-val "off")
                       (if (eq next-val 'ask-me)
                           "ask each send"
                         (or ai-code-auto-test-suffix "cleared")))
              next-val)))

(defclass ai-code--discussion-auto-follow-up-enabled-type (transient-lisp-variable)
  ((variable :initform 'ai-code-discussion-auto-follow-up-enabled)
   (format :initform "%k %d %v")
   (reader :initform #'transient-lisp-variable--read-value))
  "Selection helper for `ai-code-discussion-auto-follow-up-enabled`.")

(transient-define-infix ai-code--infix-toggle-auto-follow-up ()
  "Toggle `ai-code-discussion-auto-follow-up-enabled`."
  :class 'ai-code--discussion-auto-follow-up-enabled-type
  :key "F"
  :description "Discussion follow-up:"
  :reader (lambda (_prompt _initial-input _history)
            (ai-code--apply-discussion-auto-follow-up-enabled
             (ai-code--cycle-discussion-auto-follow-up-value
              ai-code-discussion-auto-follow-up-enabled))))

(defun ai-code--select-backend-description (&rest _)
  "Dynamic description for the Select Backend menu item.
Shows the current backend label to the right."
  (format "Select Backend (%s)" (ai-code-current-backend-label)))

(defconst ai-code--terminal-backend-choices
  '(("vterm" . vterm)
    ("eat" . eat)
    ("ghostel" . ghostel))
  "Display choices for `ai-code-backends-infra-terminal-backend'.")

(defun ai-code--ordered-terminal-backend-choices ()
  "Return terminal backend choices with the current backend first."
  (let ((current-label
         (car (seq-find
               (lambda (it)
                 (eq (cdr it) ai-code-backends-infra-terminal-backend))
               ai-code--terminal-backend-choices))))
    (if current-label
        (let ((current (assoc current-label ai-code--terminal-backend-choices)))
          (cons current
                (seq-remove (lambda (it)
                              (equal (car it) current-label))
                            ai-code--terminal-backend-choices)))
      ai-code--terminal-backend-choices)))

(defun ai-code-select-terminal ()
  "Interactively select the terminal backend for AI sessions."
  (interactive)
  (let* ((ordered-choices (ai-code--ordered-terminal-backend-choices))
         (current-label (caar ordered-choices))
         (choice (completing-read "Select terminal: "
                                  (mapcar #'car ordered-choices)
                                  nil t nil nil current-label))
         (backend (cdr (assoc choice ordered-choices))))
    (setq ai-code-backends-infra-terminal-backend backend)
    (ai-code-backends-infra--sync-reflow-filter-advice)
    (message "AI Code terminal backend switched to: %s" choice)))

(defun ai-code--select-terminal-description (&rest _)
  "Dynamic description for the Select Terminal menu item."
  (format "Select Terminal (%s)"
          (symbol-name ai-code-backends-infra-terminal-backend)))



;; Mirror aider.el's reusable-section approach using `transient-define-group`.
(transient-define-group ai-code--menu-ai-cli-session
  ("a" "Start AI CLI (C-u: args)" ai-code-cli-start)
  ("R" "Resume AI CLI (C-u: args)" ai-code-cli-resume)
  ("z" "Switch to AI CLI (C-u: hide)" ai-code-cli-switch-to-buffer-or-hide)
  ("s" ai-code-select-backend :description ai-code--select-backend-description)
  ("j" "Session dashboard" ai-code-session-dashboard)
  ;; DONE: similar to ai-code-select-backend, add ai-code-select-terminal, it will use ai-code-backends-infra-terminal-backend to select between different terminal emulators for AI sessions, such as vterm, eat, and ghostel.
  ("u" "Install / Upgrade AI CLI" ai-code-upgrade-backend)
  ("S" "(Un)Install skills for backend" ai-code-install-backend-skills)
  ("g" "Open backend config (eg. add mcp)" ai-code-open-backend-config)
  ("G" "Open backend repo agent file" ai-code-open-backend-agent-file)
  ("l" ai-code-select-terminal :description ai-code--select-terminal-description))

(transient-define-group ai-code--menu-actions-with-context
  (ai-code--infix-toggle-suffix)
  ("c" "Code change (C-u: clipboard)" ai-code-code-change)
  ("i" "Implement TODO (C-u: clipboard)" ai-code-implement-todo)
  ("q" "Ask question (C-u: clipboard)" ai-code-ask-question)
  ("x" "Explain code in scope" ai-code-explain)
  ("<SPC>" "Send command (C-u: context)" ai-code-send-command)
  ("I" "Insert to session..." ai-code-insert-menu)
  ("@" "Context (add/show/clear)" ai-code-context-action)
  ("C" "Create file or dir with AI" ai-code-create-file-or-dir)
  (":" "Speech to text input" ai-code-speech-to-text-input)
  ("w" "New worktree branch (C-u: status)" ai-code-git-worktree-action))

;;;###autoload
(transient-define-prefix ai-code-insert-menu ()
  "Insert files and editor selections into an AI Code session or viewport."
  ["Insert"
   ["Files" :class transient-column
    :setup-children ai-code-send-setup-menu-children
    ("f" "File" ai-code-send-file)
    ("F" "File to..." ai-code-send-file-to)
    ("c" "Current file" ai-code-send-current-file)
    ("o" "Other file" ai-code-send-other-file)]
   ["Code" :class transient-column
    :setup-children ai-code-send-setup-menu-children
    ("r" "Region" ai-code-send-region)
    ("R" "Region to..." ai-code-send-region-to)
    ("d" "DWIM" ai-code-send-dwim)
    ("D" "DWIM to..." ai-code-send-dwim-to)]
   ["Attachments" :class transient-column
    :setup-children ai-code-send-setup-menu-children
    ("s" "Screenshot" ai-code-send-screenshot)
    ("S" "Screenshot to..." ai-code-send-screenshot-to)
    ("i" "Clipboard image" ai-code-send-clipboard-image)
    ("I" "Clipboard image to..." ai-code-send-clipboard-image-to)]]
  (interactive)
  (ai-code-send-prepare-menu)
  (transient-setup 'ai-code-insert-menu))

(transient-define-group ai-code--menu-agile-development
  (ai-code--infix-select-code-change-auto-test)
  ("r" "Refactor Code" ai-code-refactor-book-method)
  ("t" "Test Driven Development" ai-code-tdd-cycle)
  ("v" "GitHub PR AI Action" ai-code-pull-or-review-diff-file)
  ;; DONE: Move ai-code-derive-architecture-guardrails ai-code-file.el. Add a new menu item: "Derive architecture document", bind to D. It let user choose from complet-reading: Derive Architecture Guardrails, and Derive DDD Context for Repo. No need to keep other two separate menu items
  ("A" "Derive architecture document" ai-code-derive-architecture-document)
  ("!" "Run Current File or Command" ai-code-run-current-file-or-shell-cmd)
  ("b" "Build/Test/Lint (AI follow-up)" ai-code-build-or-test-project)
  ("K" "Create/Open task file" ai-code-create-or-open-task-file)
  ("H" "Agent handoff (C-u: whole task)" ai-code-agent-handoff)
  ("/" "Search notes with AI" ai-code-search-notes-with-ai)
  ("n" "Take notes from AI session" ai-code-take-notes))

(transient-define-group ai-code--menu-other-tools
  (ai-code--infix-toggle-auto-follow-up)
  ("." "Init projectile and gtags" ai-code-init-project)
  ("P" "AI session checkpoint" ai-code-session-checkpoint)
  ("e" "Investigate exception (C-u: clipboard)" ai-code-investigate-exception)
  ("f" "Fix Flycheck errors in scope" ai-code-flycheck-fix-errors-in-scope)
  ("k" "Copy Cur File Name (C-u: full)" ai-code-copy-buffer-file-name-to-clipboard)
  ;; ("o" "Open recent file (C-u: insert)" ai-code-git-repo-recent-modified-files)
  ("p" "Open prompt history file" ai-code-open-prompt-file)
  ;; ("m" "Debug python MCP server" ai-code-debug-mcp)
  ;; ("N" "Toggle notifications" ai-code-notifications-toggle)
  ("d" "Debug Emacs runtime" ai-code-debug-emacs-runtime)
  ;; DONE: Add a menu item here: Given a new customized variable, which suppose to be a list of strings, by default it is nil. User can choose one from it, probably with complet-reading, and it will be sent to AI session with ai-code--insert-prompt. This is useful for user to quickly send some pre-defined prompt templates or instructions to AI, like a shortcut.
  ("Q" "Send quick prompt" ai-code-send-quick-prompt)
  ("|" "Apply prompt on file" ai-code-apply-prompt-on-current-file)
  ("h" "Help / Quick Start" ai-code-onboarding-open-quickstart))

(transient-define-prefix ai-code-menu-default ()
  "Default transient menu for AI Code Interface interactive functions."
  ["AI Code Commands"
   ["AI CLI session" ai-code--menu-ai-cli-session]
   ["AI Code Actions With Context" ai-code--menu-actions-with-context]
   ["AI Agile Development With Harness" ai-code--menu-agile-development]
   ["Other Tools" ai-code--menu-other-tools]])

(transient-define-prefix ai-code-menu-2-columns ()
  "Narrower two-column transient menu for AI Code Interface interactive functions."
  ["AI Code Commands"
   ["AI CLI session" ai-code--menu-ai-cli-session]
   ["AI Code Actions With Context" ai-code--menu-actions-with-context]]
  [["AI Agile Development With Harness" ai-code--menu-agile-development]
   ["Other Tools" ai-code--menu-other-tools]])

(defun ai-code--menu-prefix-command ()
  "Return the transient prefix command selected by `ai-code-menu-layout`."
  (pcase ai-code-menu-layout
    ('two-columns #'ai-code-menu-2-columns)
    ('default #'ai-code-menu-default)
    (_ #'ai-code-menu-default)))

;;;###autoload
(defun ai-code-menu ()
  "Show the AI Code transient menu selected by `ai-code-menu-layout`."
  (interactive)
  (ai-code-onboarding-maybe-show-quickstart)
  (call-interactively (ai-code--menu-prefix-command)))


(provide 'ai-code)

;;; ai-code.el ends here
