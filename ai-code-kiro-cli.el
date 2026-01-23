;;; ai-code-kiro-cli.el --- Thin wrapper for Kiro CLI  -*- lexical-binding: t; -*-

;; Author: Jason Jenkins
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `ai-code-backends-infra' to run Kiro CLI.
;;
;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)

(defgroup ai-code-kiro-cli nil
  "Kiro CLI integration via `ai-code-backends-infra'."
  :group 'tools
  :prefix "ai-code-kiro-cli-")

(defcustom ai-code-kiro-cli-program "kiro-cli"
  "Path to the Kiro CLI executable."
  :type 'string
  :group 'ai-code-kiro-cli)

(defcustom ai-code-kiro-cli-program-switches nil
  "Command line switches to pass to Kiro CLI on startup."
  :type '(repeat string)
  :group 'ai-code-kiro-cli)

(defcustom ai-code-kiro-cli-trust-all-tools nil
  "When non-nil, pass --trust-all-tools to allow commands without confirmation."
  :type 'boolean
  :group 'ai-code-kiro-cli)

(defcustom ai-code-kiro-cli-agent nil
  "Agent/context profile to use. When nil, uses default agent."
  :type '(choice (const :tag "Default" nil)
                 (string :tag "Agent name"))
  :group 'ai-code-kiro-cli)

(defconst ai-code-kiro-cli--session-prefix "kiro"
  "Session prefix used in Kiro CLI buffer names.")

(defvar ai-code-kiro-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping Kiro session keys to processes.")

(defun ai-code-kiro-cli--build-command ()
  "Build the Kiro CLI command string."
  (let ((args (list ai-code-kiro-cli-program "chat")))
    (when ai-code-kiro-cli-trust-all-tools
      (setq args (append args '("--trust-all-tools"))))
    (when ai-code-kiro-cli-agent
      (setq args (append args (list "--agent" ai-code-kiro-cli-agent))))
    (when ai-code-kiro-cli-program-switches
      (setq args (append args ai-code-kiro-cli-program-switches)))
    (mapconcat 'identity args " ")))

;;;###autoload
(defun ai-code-kiro-cli (&optional arg)
  "Start Kiro CLI chat session.
With prefix ARG, prompt for a new instance name."
  (interactive "P")
  (let* ((working-dir (ai-code-backends-infra--session-working-directory))
         (force-prompt (and arg t))
         (command (ai-code-kiro-cli--build-command))
         (instance-name nil))
    (unless force-prompt
      (when-let ((existing-buffer
                  (ai-code-backends-infra--select-session-buffer
                   ai-code-kiro-cli--session-prefix
                   working-dir)))
        (setq instance-name
              (ai-code-backends-infra--session-instance-name
               (buffer-name existing-buffer)
               ai-code-kiro-cli--session-prefix))))
    (ai-code-backends-infra--toggle-or-create-session
     working-dir
     nil
     ai-code-kiro-cli--processes
     command
     #'ai-code-kiro-cli-send-escape
     nil
     instance-name
     ai-code-kiro-cli--session-prefix
     force-prompt)))

;;;###autoload
(defun ai-code-kiro-cli-switch-to-buffer (&optional force-prompt)
  "Switch to the Kiro CLI buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--switch-to-session-buffer
     nil
     "No Kiro session for this project"
     ai-code-kiro-cli--session-prefix
     working-dir
     force-prompt)))

;;;###autoload
(defun ai-code-kiro-cli-send-command (line)
  "Send LINE to Kiro CLI."
  (interactive "sKiro> ")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--send-line-to-session
     nil
     "No Kiro session for this project"
     line
     ai-code-kiro-cli--session-prefix
     working-dir)))

;;;###autoload
(defun ai-code-kiro-cli-send-escape ()
  "Send escape key to Kiro CLI."
  (interactive)
  (ai-code-backends-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-kiro-cli-resume (&optional arg)
  "Resume a previous Kiro CLI session."
  (interactive "P")
  (let ((ai-code-kiro-cli-program-switches (append ai-code-kiro-cli-program-switches '("--resume"))))
    (ai-code-kiro-cli arg)))

(provide 'ai-code-kiro-cli)

;;; ai-code-kiro-cli.el ends here
