;;; ai-code-backends.el --- Backend selection support for ai-code -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Backend selection support extracted from ai-code-interface.el.

;;; Code:

(require 'seq)

(defvar ai-code-cli)

(defun ai-code--unsupported-resume (&optional _arg)
  (interactive "P")
  (user-error "Backend '%s' does not support resume" (ai-code-current-backend-label)))

;;;###autoload
(defun ai-code-cli-resume (&optional arg)
  "Resume the current backend's CLI session when supported."
  (interactive "P")
  (ai-code--unsupported-resume arg))

;;;###autoload
(defcustom ai-code-backends
  '((claude-code
     :label "claude-code.el"
     :require claude-code
     :start   claude-code
     :switch  claude-code-switch-to-buffer
     :send    claude-code-send-command
     :resume  claude-code-resume
     :config  "~/.claude.json"
     :upgrade nil
     :cli     "claude")
    (claude-code-ide
     :label "claude-code-ide.el"
     :require claude-code-ide
     :start   claude-code-ide--start-if-no-session
     :switch  claude-code-ide-switch-to-buffer
     :send    claude-code-ide-send-prompt
     :resume  claude-code-ide-resume
     :config  "~/.claude.json"
     :upgrade nil
     :cli     "claude")
    (gemini
     :label "gemini-cli.el"
     :require gemini-cli
     :start   gemini-cli
     :switch  gemini-cli-switch-to-buffer
     :send    gemini-cli-send-command
     :resume  nil
     :config  "~/.gemini/settings.json"
     :upgrade "npm install -g @google/gemini-cli"
     :cli     "gemini")
    (github-copilot-cli
     :label "ai-code-github-copilot-cli.el"
     :require ai-code-github-copilot-cli
     :start   github-copilot-cli
     :switch  github-copilot-cli-switch-to-buffer
     :send    github-copilot-cli-send-command
     :resume  nil
     :config  "~/.config/mcp-config.json" ;; https://docs.github.com/en/copilot/how-tos/use-copilot-agents/use-copilot-cli
     :upgrade "npm install -g @github/copilot"
     :cli     "copilot")
    (codex
     :label "ai-code-codex-cli.el"
     :require ai-code-codex-cli
     :start   codex-cli
     :switch  codex-cli-switch-to-buffer
     :send    codex-cli-send-command
     :resume  codex-cli-resume
     :config  "~/.codex/config.toml"
     :upgrade "npm install -g @openai/codex@latest"
     :cli     "codex"))
  "Available AI backends and how to integrate with them.
Each entry is (KEY :label STRING :require FEATURE :start FN :switch FN :send FN :resume FN-or-nil :upgrade STRING-or-nil :cli STRING).
The :upgrade property can be either a string shell command or nil."
  :type '(repeat (list (symbol :tag "Key")
                       (const :label) (string :tag "Label")
                       (const :require) (symbol :tag "Feature to require")
                       (const :start) (symbol :tag "Start function")
                       (const :switch) (symbol :tag "Switch function")
                       (const :send) (symbol :tag "Send function")
                       (const :resume) (choice (symbol :tag "Resume function") (const :tag "Not supported" nil))
                       (const :upgrade) (choice (string :tag "Upgrade command") (const :tag "Not supported" nil))
                       (const :cli) (string :tag "CLI name")))
  :group 'ai-code)

(defvar ai-code-selected-backend 'claude-code
  "Currently selected backend key from `ai-code-backends'.")

(defun ai-code-set-backend (new-backend)
  (unless (ai-code--backend-spec new-backend)
    (user-error "Unknown backend: %s" new-backend))
  (setq ai-code-selected-backend new-backend)
  (ai-code--apply-backend new-backend))

(defun ai-code--backend-spec (key)
  "Return backend plist for KEY from `ai-code-backends'."
  (seq-find (lambda (it) (eq (car it) key)) ai-code-backends))

(defun ai-code-current-backend-label ()
  "Return label string of the currently selected backend.
Falls back to symbol name when label is unavailable."
  (let* ((spec (ai-code--backend-spec ai-code-selected-backend))
         (label (when spec (plist-get (cdr spec) :label))))
    (or label (and ai-code-selected-backend (symbol-name ai-code-selected-backend)) "<none>")))

(defun ai-code--ensure-backend-loaded (spec)
  "Ensure FEATURE for backend SPEC is loaded, if any."
  (let* ((plist (cdr spec))
         (feature (plist-get plist :require)))
    (when feature (require feature nil t))))

(defun ai-code--apply-backend (key)
  "Apply backend identified by KEY.
Sets `ai-code-cli-*' defaliases and updates `ai-code-cli'."
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
      ;; If the declared feature is not available after require, inform user to install it.
      (when (and feature (not (featurep feature)))
        (user-error "Backend '%s' is not available. Please install the package providing '%s' and try again."
                    label (symbol-name feature)))
      (unless (and (fboundp start) (fboundp switch) (fboundp send))
        (user-error "Backend '%s' is not available (missing functions). Please install the package providing '%s'."
                    label (symbol-name feature)))
      (defalias 'ai-code-cli-start start)
      (defalias 'ai-code-cli-switch-to-buffer switch)
      (defalias 'ai-code-cli-send-command send)
      (when (and resume (not (fboundp resume)))
        (user-error "Backend '%s' declares resume function '%s' but it is not callable."
                    label (symbol-name resume)))
      (if resume
          (fset 'ai-code-cli-resume
                (lambda (&optional arg)
                  (interactive "P")
                  (let ((current-prefix-arg arg))
                    (call-interactively resume))))
        (fset 'ai-code-cli-resume #'ai-code--unsupported-resume))
      (setq ai-code-cli cli
            ai-code-selected-backend key)
      (message "AI Code backend switched to: %s" (plist-get plist :label)))))

;;;###autoload
(defun ai-code-cli-start-or-resume (&optional arg)
  "Start or resume the CLI depending on prefix argument.
If called with `C-u' (raw prefix ARG '(4)), invoke `ai-code-cli-resume';
otherwise call `ai-code-cli-start'."
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
         (choice (completing-read "Select backend: " (mapcar #'car choices) nil t))
         (key (cdr (assoc choice choices))))
    (ai-code-set-backend key)))

;;;###autoload
(defun ai-code-open-backend-config ()
  "Open the current backend's configuration file in another window."
  (interactive)
  (let* ((spec (ai-code--backend-spec ai-code-selected-backend)))
    (unless spec
      (user-error "No backend is currently selected."))
    (let* ((plist  (cdr spec))
           (label  (or (plist-get plist :label)
                       (symbol-name ai-code-selected-backend)))
           (config (plist-get plist :config)))
      (unless config
        (user-error "Backend '%s' does not declare a config file." label))
      (let ((file (expand-file-name config)))
        (find-file-other-window file)
        (message "Opened %s config: %s" label file)))))

;;;###autoload
(defun ai-code-upgrade-backend ()
  "Run the upgrade command for the currently selected backend."
  (interactive)
  (let* ((spec (ai-code--backend-spec ai-code-selected-backend)))
    (unless spec
      (user-error "No backend is currently selected."))
    (let* ((plist   (cdr spec))
           (upgrade (plist-get plist :upgrade))
           (label   (ai-code-current-backend-label)))
      (if upgrade
          (progn
            (compile upgrade)
            (message "Running upgrade command for %s" label))
        (user-error "Upgrade command for backend '%s' is not defined." label)))))

(provide 'ai-code-backends)

;;; ai-code-backends.el ends here
