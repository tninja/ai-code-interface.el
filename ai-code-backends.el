;;; ai-code-backends.el --- Backend selection support for ai-code -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Backend selection support extracted from ai-code.el.

;;; Code:

(require 'seq)

(require 'ai-code-git)

(defvar ai-code-cli)
(defvar claude-code-terminal-backend)

(declare-function claude-code--do-send-command "claude-code" (cmd))
(declare-function claude-code--term-send-string "claude-code" (backend string))
(declare-function ai-code--validate-git-repository "ai-code-git" ())
(declare-function ai-code--git-root "ai-code-file" (&optional dir))
(declare-function ai-code-onboarding-show-backend-switch-hint "ai-code-onboarding" ())
(declare-function ai-code-read-string "ai-code-input" (prompt &optional initial-input candidate-list))

(defvar ai-code--cli-start-fn #'ai-code--unsupported-start)
(defvar ai-code--cli-resume-fn #'ai-code--unsupported-resume)
(defvar ai-code--cli-switch-fn #'ai-code--unsupported-switch-to-buffer)
(defvar ai-code--cli-send-fn #'ai-code--unsupported-send-command)

(defvar ai-code--repo-backend-alist nil
  "Alist of (GIT-ROOT . BACKEND) to keep backend affinity per repository.")

(defun ai-code--normalize-git-root (git-root)
  "Return normalized GIT-ROOT path, or nil when invalid."
  (when (and (stringp git-root) (> (length git-root) 0))
    (file-truename git-root)))

(defun ai-code--current-git-root ()
  "Return normalized current git root for backend affinity, or nil."
  (let ((git-root
         (cond
          ((fboundp 'ai-code--git-root)
           (ai-code--git-root))
          ((fboundp 'magit-toplevel)
           (condition-case nil
               (magit-toplevel)
             (error nil)))
          (t nil))))
    (ai-code--normalize-git-root git-root)))

(defun ai-code--repo-backend-for-root (git-root)
  "Return remembered backend for GIT-ROOT, or nil."
  (cdr (assoc git-root ai-code--repo-backend-alist)))

(defun ai-code--remember-repo-backend (git-root backend)
  "Remember BACKEND for GIT-ROOT."
  (when (and git-root backend)
    (setq ai-code--repo-backend-alist
          (cons (cons git-root backend)
                (seq-remove (lambda (it)
                              (string= (car it) git-root))
                            ai-code--repo-backend-alist)))))

(defun ai-code--effective-backend ()
  "Return backend for current context, preferring repo-local affinity."
  (let* ((git-root (ai-code--current-git-root))
         (repo-backend (and git-root (ai-code--repo-backend-for-root git-root))))
    (if (and repo-backend (ai-code--backend-spec repo-backend))
        repo-backend
      ai-code-selected-backend)))

(defun ai-code--activate-effective-backend ()
  "Switch active backend to the effective backend for current context."
  (let ((effective (ai-code--effective-backend)))
    (when (and effective (not (eq effective ai-code-selected-backend)))
      (ai-code-set-backend effective))))

(defun ai-code--remember-current-backend-for-repo ()
  "Remember current backend for current git repository."
  (let ((git-root (ai-code--current-git-root)))
    (when git-root
      (ai-code--remember-repo-backend git-root ai-code-selected-backend))))

(defun ai-code--unsupported-start (&optional _arg)
  "Signal that the current backend does not support start.
Argument _ARG is ignored."
  (interactive "P")
  (user-error "Backend '%s' does not support start"
              (ai-code-current-backend-label)))

(defun ai-code--unsupported-switch-to-buffer (&optional _arg)
  "Signal that the current backend does not support switching.
Argument _ARG is ignored."
  (interactive "P")
  (user-error "Backend '%s' does not support switching buffers"
              (ai-code-current-backend-label)))

(defun ai-code--unsupported-send-command (&optional _command)
  "Signal that the current backend does not support sending commands.
Argument _COMMAND is ignored."
  (interactive)
  (user-error "Backend '%s' does not support sending commands"
              (ai-code-current-backend-label)))

(defun ai-code--unsupported-resume (&optional _arg)
  "Signal that the current backend does not support resume.
Argument _ARG is ignored."
  (interactive "P")
  (user-error "Backend '%s' does not support resume"
              (ai-code-current-backend-label)))

;;;###autoload
(defun ai-code-cli-start (&optional arg)
  "Start the current backend's CLI session when supported.
Argument ARG is passed to the backend's start function."
  (interactive "P")
  (ai-code--activate-effective-backend)
  (prog1
      (if (called-interactively-p 'interactive)
          (call-interactively ai-code--cli-start-fn)
        (if arg
            (funcall ai-code--cli-start-fn arg)
          (funcall ai-code--cli-start-fn)))
    (ai-code--remember-current-backend-for-repo)))

;;;###autoload
(defun ai-code-cli-resume (&optional arg)
  "Resume the current backend's CLI session when supported.
Noninteractive callers pass ARG to the backend resume function.
When called interactively, any prefix argument is forwarded via
`current-prefix-arg', and it is up to the backend how to interpret
it (for example, some backends may use a non-nil prefix to prompt for
additional CLI arguments)."
  (interactive "P")
  (ai-code--activate-effective-backend)
  (prog1
      (if (called-interactively-p 'interactive)
          (call-interactively ai-code--cli-resume-fn)
        (if arg
            (funcall ai-code--cli-resume-fn arg)
          (funcall ai-code--cli-resume-fn)))
    (ai-code--remember-current-backend-for-repo)))

;;;###autoload
(defun ai-code-cli-switch-to-buffer (&optional arg)
  "Switch to the current backend's CLI buffer when supported.
Argument ARG is passed to the backend's switch function."
  (interactive "P")
  (ai-code--activate-effective-backend)
  (if (called-interactively-p 'interactive)
      (call-interactively ai-code--cli-switch-fn)
    (if arg
        (funcall ai-code--cli-switch-fn arg)
      (funcall ai-code--cli-switch-fn))))

;;;###autoload
(defun ai-code-cli-send-command (&optional command)
  "Send COMMAND to the current backend when supported.
When called interactively, prompt for COMMAND.
Noninteractive callers must supply COMMAND."
  (interactive)
  (ai-code--activate-effective-backend)
  (if (called-interactively-p 'interactive)
      (call-interactively ai-code--cli-send-fn)
    (if (null command)
        (user-error "COMMAND is required for noninteractive calls")
      (funcall ai-code--cli-send-fn command))))

;;;###autoload
(defun ai-code-claude-code-el-send-command (cmd)
  "Send CMD to claude-code programmatically or interactively.
This wrapper function works around the signature change in
`claude-code-send-command' which no longer accepts a command parameter.
When called interactively, prompts for the command.
When called from Lisp code, sends CMD directly without prompting."
  (interactive "sClaude command: ")
  (claude-code--do-send-command cmd))

(defun ai-code-claude-code-install-skills ()
  "Install skills for Claude Code by prompting for a skills repo URL.
Ask the Claude Code CLI to clone and set up the skills from the given
repository.  Claude Code manages skills as files under ~/.claude/,
so the CLI itself handles the installation details."
  (let* ((url (read-string
               "Skills repo URL for Claude Code: "
               nil nil "https://github.com/obra/superpowers"))
         (default-prompt
          (format
           "Install the skill from %s for this Claude Code CLI. Read the repository README to understand the installation instructions and follow them. Set up the skill files under the appropriate directory (e.g. ~/.claude/ or the project .claude/ directory) so they are available in future sessions."
           url))
         (prompt (if (called-interactively-p 'interactive)
                     (ai-code-read-string
                      "Edit install-skills prompt for Claude Code: "
                      default-prompt)
                   default-prompt)))
    (ai-code-cli-send-command prompt)))

;;;###autoload
(defcustom ai-code-backends
  '((claude-code
     :label "Claude Code"
     :require ai-code-claude-code
     :start   ai-code-claude-code
     :switch  ai-code-claude-code-switch-to-buffer
     :send    ai-code-claude-code-send-command
     :resume  ai-code-claude-code-resume
     :config  "~/.claude.json"
     :agent-file "CLAUDE.md"
     :upgrade "npm install -g @anthropic-ai/claude-code@latest"
     :install-skills nil
     :cli     "claude")
    (gemini
     :label "Gemini CLI"
     :require ai-code-gemini-cli
     :start   ai-code-gemini-cli
     :switch  ai-code-gemini-cli-switch-to-buffer
     :send    ai-code-gemini-cli-send-command
     :resume  ai-code-gemini-cli-resume
     :config  "~/.gemini/settings.json"
     :agent-file "GEMINI.md"
     :upgrade "npm install -g @google/gemini-cli"
     :install-skills nil
     :cli     "gemini")
    (github-copilot-cli
     :label "GitHub Copilot CLI"
     :require ai-code-github-copilot-cli
     :start   ai-code-github-copilot-cli
     :switch  ai-code-github-copilot-cli-switch-to-buffer
     :send    ai-code-github-copilot-cli-send-command
     :resume  ai-code-github-copilot-cli-resume
     :config  "~/.copilot/mcp-config.json"
     :agent-file nil
     :upgrade "npm install -g @github/copilot"
     :install-skills nil
     :cli     "copilot")
    (codex
     :label "OpenAI Codex CLI"
     :require ai-code-codex-cli
     :start   ai-code-codex-cli
     :switch  ai-code-codex-cli-switch-to-buffer
     :send    ai-code-codex-cli-send-command
     :resume  ai-code-codex-cli-resume
     :config  "~/.codex/config.toml"
     :agent-file "AGENTS.md"
     :upgrade "npm install -g @openai/codex@latest"
     :install-skills nil
     :cli     "codex")
    (opencode
     :label "Opencode"
     :require ai-code-opencode
     :start   ai-code-opencode
     :switch  ai-code-opencode-switch-to-buffer
     :send    ai-code-opencode-send-command
     :resume  ai-code-opencode-resume
     :config  "~/.config/opencode/opencode.jsonc"
     :agent-file nil
     :upgrade "npm i -g opencode-ai@latest"
     :install-skills nil
     :cli     "opencode")
    (grok
     :label "Grok CLI"
     :require ai-code-grok-cli
     :start   ai-code-grok-cli
     :switch  ai-code-grok-cli-switch-to-buffer
     :send    ai-code-grok-cli-send-command
     :resume  ai-code-grok-cli-resume
     :config  "~/.config/grok/config.json"
     :agent-file nil
     :upgrade "bun add -g @vibe-kit/grok-cli"
     :install-skills nil
     :cli     "grok")
    (cursor
     :label "Cursor CLI"
     :require ai-code-cursor-cli
     :start   ai-code-cursor-cli
     :switch  ai-code-cursor-cli-switch-to-buffer
     :send    ai-code-cursor-cli-send-command
     :resume  ai-code-cursor-cli-resume
     :config  "~/.cursor"
     :agent-file nil
     :upgrade "cursor-agent update"
     :install-skills nil
     :cli     "cursor-agent")
    (kiro
     :label "Kiro CLI"
     :require ai-code-kiro-cli
     :start   ai-code-kiro-cli
     :switch  ai-code-kiro-cli-switch-to-buffer
     :send    ai-code-kiro-cli-send-command
     :resume  ai-code-kiro-cli-resume
     :config  "~/.kiro/settings/cli.json"
     :agent-file nil
     :upgrade "kiro-cli update"
     :install-skills nil
     :cli     "kiro-cli")
    (codebuddy
     :label "CodeBuddy Code"
     :require ai-code-codebuddy-cli
     :start   ai-code-codebuddy-cli
     :switch  ai-code-codebuddy-cli-switch-to-buffer
     :send    ai-code-codebuddy-cli-send-command
     :resume  ai-code-codebuddy-cli-resume
     :config  "~/.codebuddy"
     :agent-file nil
     :upgrade "codebuddy update"
     :install-skills nil
     :cli     "codebuddy")
    (aider
     :label "Aider CLI"
     :require ai-code-aider-cli
     :start   ai-code-aider-cli
     :switch  ai-code-aider-cli-switch-to-buffer
     :send    ai-code-aider-cli-send-command
     :resume  nil
     :config  "~/.aider.conf.yml"
     :agent-file nil
     :upgrade nil
     :install-skills nil
     :cli     "aider")
    (eca              ; external backend, requires eca package
     :label "ECA (Editor Code Assistant)"
     :require ai-code-eca
     :start   ai-code-eca-start
     :switch  ai-code-eca-switch
     :send    ai-code-eca-send
     :resume  ai-code-eca-resume
     :config  "~/.config/eca/config.json"
     :agent-file "AGENTS.md"
     :upgrade ai-code-eca-upgrade
     :install-skills ai-code-eca-install-skills
     :cli     nil)
    (agent-shell      ; external backend, requires agent-shell package
     :label "agent-shell"
     :require ai-code-agent-shell
     :start   ai-code-agent-shell
     :switch  ai-code-agent-shell-switch-to-buffer
     :send    ai-code-agent-shell-send-command
     :resume  ai-code-agent-shell-resume
     :config  nil
     :agent-file nil
     :upgrade nil
     :install-skills nil
     :cli     "agent-shell")
    (gptel-agent      ; external backend, requires gptel-agent package
     :label "GPTel Agent"
     :require ai-code-gptel-agent
     :start   ai-code-gptel-agent
     :switch  ai-code-gptel-agent-switch-to-buffer
     :send    ai-code-gptel-agent-send-command
     :resume  nil
     :config  nil
     :agent-file nil
     :upgrade nil
     :install-skills nil
     :cli     nil)
    (claude-code-ide ; external backend, requires claude-code-ide.el package
     :label "claude-code-ide.el"
     :require claude-code-ide
     :start   claude-code-ide--start-if-no-session
     :switch  claude-code-ide-switch-to-buffer
     :send    claude-code-ide-send-prompt
     :resume  claude-code-ide-resume
     :config  "~/.claude.json"
     :agent-file "CLAUDE.md"
     :upgrade "npm install -g @anthropic-ai/claude-code@latest"
     :install-skills nil
     :cli     "claude")
    (claude-code-el ; external backend, requires claude-code.el package
     :label "claude-code.el"
     :require claude-code
     :start   claude-code
     :switch  claude-code-switch-to-buffer
     :send    ai-code-claude-code-el-send-command
     :resume  claude-code-resume
     :config  "~/.claude.json"
     :agent-file "CLAUDE.md"
     :upgrade "npm install -g @anthropic-ai/claude-code@latest"
     :install-skills nil
     :cli     "claude"))
  "Available AI backends and how to integrate with them.
Each entry is (KEY :label STRING :require FEATURE :start FN :switch FN
:send FN :resume FN-or-nil :upgrade STRING-or-nil :cli STRING
:agent-file STRING-or-nil :install-skills STRING-or-SYMBOL-or-nil).
The :upgrade property can be either a string shell command or nil.
The :install-skills property can be a string shell command, a function symbol, or nil."
  :type '(repeat (list (symbol :tag "Key")
                       (const :label) (string :tag "Label")
                       (const :require) (symbol :tag "Feature to require")
                       (const :start) (symbol :tag "Start function")
                       (const :switch) (symbol :tag "Switch function")
                       (const :send) (symbol :tag "Send function")
                       (const :resume) (choice (symbol :tag "Resume function")
                                               (const :tag "Not supported" nil))
                       (const :upgrade) (choice (string :tag "Upgrade command")
                                                (const :tag "Not supported" nil))
                       (const :cli) (string :tag "CLI name")
                       (const :agent-file) (choice (string :tag "Agent file name")
                                                   (const :tag "Not supported" nil))
                       (const :install-skills) (choice (string :tag "Install skills command")
                                                       (symbol :tag "Install skills function")
                                                       (const :tag "Not supported" nil))))
  :group 'ai-code)

(defvar ai-code-selected-backend 'claude-code
  "Currently selected backend key from `ai-code-backends'.")

(defun ai-code-set-backend (new-backend)
  "Set the AI backend to NEW-BACKEND."
  (unless (ai-code--backend-spec new-backend)
    (user-error "Unknown backend: %s" new-backend))
  (setq ai-code-selected-backend new-backend)
  (ai-code--apply-backend new-backend)
  (ai-code--remember-current-backend-for-repo))

(defun ai-code--backend-spec (key)
  "Return backend plist for KEY from `ai-code-backends'."
  (seq-find (lambda (it) (eq (car it) key)) ai-code-backends))

(defun ai-code-current-backend-label ()
  "Return label string of the currently selected backend.
Falls back to symbol name when label is unavailable."
  (let* ((effective-backend (ai-code--effective-backend))
         (spec (ai-code--backend-spec effective-backend))
         (label (when spec (plist-get (cdr spec) :label))))
    (or label (and effective-backend
                   (symbol-name effective-backend)) "<none>")))

(defun ai-code--ensure-backend-loaded (spec)
  "Ensure FEATURE for backend SPEC is loaded, if any."
  (let* ((plist (cdr spec))
         (feature (plist-get plist :require)))
    (when feature (require feature nil t))))

(defun ai-code--apply-backend (key)
  "Apply backend identified by KEY.
Sets backend dispatch functions and updates `ai-code-cli'."
  (let* ((spec (ai-code--backend-spec key)))
    (unless spec
      (user-error "Unknown backend: %s" key))
    (ai-code--ensure-backend-loaded spec)
    (let* ((plist (cdr spec))
           (label  (plist-get plist :label))
           (feature (plist-get plist :require))
           (start  (plist-get plist :start))
           (switch (plist-get plist :switch))
           (send   (plist-get plist :send))
           (resume (plist-get plist :resume))
           (cli    (plist-get plist :cli)))
      ;; If the declared feature is not available after require,
      ;; inform user to install it.
      (when (and feature (not (featurep feature)))
        (user-error
         "Backend '%s' is not available.  Please install the package providing '%s' and try again"
         label (symbol-name feature)))
      (let ((missing-fns (seq-filter (lambda (fn) (not (fboundp fn)))
                                     (list start switch send))))
        (when missing-fns
          (user-error
           "Backend '%s' is not available (missing functions: %s).  Please install the package providing '%s'"
           label
           (mapconcat #'symbol-name missing-fns ", ")
           (symbol-name feature))))
      (when (and resume (not (fboundp resume)))
        (user-error
         "Backend '%s' declares resume function '%s' but it is not callable"
         label (symbol-name resume)))
      (setq ai-code--cli-start-fn start
            ai-code--cli-switch-fn switch
            ai-code--cli-send-fn send
            ai-code--cli-resume-fn (if resume
                                       (lambda (&optional arg)
                                         (interactive "P")
                                         (let ((current-prefix-arg (or arg current-prefix-arg)))
                                           (call-interactively resume)))
                                     #'ai-code--unsupported-resume))
      (setq ai-code-cli cli
            ai-code-selected-backend key)
      (message "AI Code backend switched to: %s" (plist-get plist :label)))))

;;;###autoload
(defun ai-code-cli-start-or-resume (&optional arg)
  "Start or resume the CLI depending on prefix argument.
If called with \\[universal-argument] (raw prefix ARG \\='(4)),
invoke `ai-code-cli-resume'; otherwise call `ai-code-cli-start'."
  (interactive "P")
  (if arg
      (call-interactively #'ai-code-cli-resume)
    (call-interactively #'ai-code-cli-start)))

;;;###autoload
(defun ai-code-select-backend ()
  "Interactively select and apply an AI backend from `ai-code-backends'."
  (interactive)
  (let* ((choices (mapcar (lambda (it)
                            (let* ((key (car it))
                                   (label (plist-get (cdr it) :label)))
                              (cons (format "%s" label) key)))
                          ai-code-backends))
         (effective-backend (ai-code--effective-backend))
         (current-label (car (seq-find (lambda (it)
                                         (eq (cdr it) effective-backend))
                                       choices)))
         (ordered-choices (if current-label
                              (let ((current (assoc current-label choices)))
                                (cons current
                                      (seq-remove (lambda (it)
                                                    (equal (car it) current-label))
                                                  choices)))
                            choices))
         (choice (completing-read "Select backend: "
                                  (mapcar #'car ordered-choices)
                                  nil t nil nil current-label))
         (key (cdr (assoc choice ordered-choices))))
    (ai-code-set-backend key)
    (when (fboundp 'ai-code-onboarding-show-backend-switch-hint)
      (ai-code-onboarding-show-backend-switch-hint))))

;;;###autoload
(defun ai-code-open-backend-config ()
  "Open the current backend's configuration file in another window."
  (interactive)
  (let* ((spec (ai-code--backend-spec ai-code-selected-backend)))
    (if (not spec)
        (user-error "No backend is currently selected")
      (let* ((plist  (cdr spec))
             (label  (or (plist-get plist :label)
                         (symbol-name ai-code-selected-backend)))
             (config (plist-get plist :config)))
        (if (not config)
            (user-error "Backend '%s' does not declare a config file" label)
          (let ((file (expand-file-name config)))
            (find-file-other-window file)
            (message "Opened %s config: %s" label file)))))))

;;;###autoload
(defun ai-code-open-backend-agent-file ()
  "Open the current backend's agent file from the git repository root."
  (interactive)
  (let* ((spec (ai-code--backend-spec ai-code-selected-backend)))
    (if (not spec)
        (user-error "No backend is currently selected")
      (let* ((plist (cdr spec))
             (label (or (plist-get plist :label)
                        (symbol-name ai-code-selected-backend)))
             (agent-file (plist-get plist :agent-file)))
        (if (not agent-file)
            (user-error "Backend '%s' does not declare an agent file" label)
          (let* ((git-root (ai-code--validate-git-repository))
                 (file (expand-file-name agent-file git-root)))
            (find-file-other-window file)
            (message "Opened %s agent file: %s" label file)))))))

;;;###autoload
(defun ai-code-upgrade-backend (&optional arg)
  "Run the upgrade command for the currently selected backend.
If the backend defines an :upgrade property, use it:
  - string: run as a shell command via `compile'.
  - symbol: call the function with prefix arg forwarded.
ARG is the prefix argument to pass to the upgrade function."
  (interactive "P")
  (let* ((spec (ai-code--backend-spec ai-code-selected-backend)))
    (if (not spec)
        (user-error "No backend is currently selected")
      (let* ((plist   (cdr spec))
             (upgrade (plist-get plist :upgrade))
             (label   (ai-code-current-backend-label)))
        (cond
         ((stringp upgrade)
          (compile upgrade)
          (message "Running upgrade command for %s" label))
         ((and upgrade (symbolp upgrade) (fboundp upgrade))
          (let ((current-prefix-arg arg))
            (call-interactively upgrade))
          (message "Running upgrade for %s" label))
         ((and upgrade (symbolp upgrade) (not (fboundp upgrade)))
          (user-error "Backend '%s' declares :upgrade function '%s' but it is not callable"
                      label upgrade))
         (t
          (user-error "Upgrade command for backend '%s' is not defined"
                      label)))))))

(defun ai-code--install-backend-skills-fallback (label)
  "Fallback skills installation for backend LABEL.
Prompt user for a skills repository URL and ask the AI CLI session
to read the repo README and install the skills."
  (ai-code--manage-backend-skills-fallback label 'install))

(defun ai-code--manage-backend-skills-fallback (label action)
  "Fallback backend skills management for LABEL and ACTION.
ACTION should be the symbol `install' or `uninstall'."
  (let* ((action-name
          (pcase action
            ('install "install")
            ('uninstall "uninstall")
            (_ (user-error
                "Invalid backend skills action: %S; expected `install' or `uninstall'"
                action))))
         (url (read-string
               (format "Skills repo URL for %s %s: " label action-name)
               nil nil "https://github.com/obra/superpowers"))
         (default-prompt
          (if (eq action 'uninstall)
              (format
               "Please read the README of %s and uninstall/remove the skills described there for %s. Follow the repository instructions to remove any installed skill files and cleanup related configuration."
               url label)
            (format
             "Please read the README of %s and install/setup the skills described there for %s. Follow the installation instructions in the README."
             url label)))
         (prompt (if (called-interactively-p 'interactive)
                     (ai-code-read-string
                      (format "Edit %s-skills prompt for %s: " action-name label)
                      default-prompt)
                   default-prompt)))
    (ai-code-cli-send-command prompt)))

;;;###autoload
(defun ai-code-install-backend-skills ()
  "Install skills for the currently selected backend.
If the backend defines an :install-skills property, use it:
  - string: run as a shell command via `compile'.
  - symbol: call the function.
Otherwise fall back to prompting the AI session to install from a
skills repository URL."
  ;; DONE: firstly ask user if it is install or uninstall, then ask for the repo URL if needed, and finally run install / uninstall using corresponding prompt.
  (interactive)
  (let* ((spec (ai-code--backend-spec ai-code-selected-backend)))
    (if (not spec)
        (user-error "No backend is currently selected")
      (ai-code--ensure-backend-loaded spec)
      (let* ((plist (cdr spec))
             (install-skills (plist-get plist :install-skills))
             (label (ai-code-current-backend-label))
             (action-choice (completing-read
                             (format "Manage skills for %s: " label)
                             '("install" "uninstall")
                             nil t nil nil "install"))
             (action (if (string= action-choice "uninstall")
                         'uninstall
                       'install)))
        (cond
         ((eq action 'uninstall)
          (ai-code--manage-backend-skills-fallback label 'uninstall))
         ((stringp install-skills)
          (compile install-skills)
          (message "Running skills installation for %s" label))
         ((and install-skills (symbolp install-skills) (fboundp install-skills))
          (funcall install-skills)
          (message "Running skills installation for %s" label))
         ((and install-skills (symbolp install-skills) (not (fboundp install-skills)))
          (user-error "Backend '%s' declares :install-skills function '%s' but it is not callable"
                      label install-skills))
         (t
          (ai-code--install-backend-skills-fallback label)))))))

(provide 'ai-code-backends)

;;; ai-code-backends.el ends here
