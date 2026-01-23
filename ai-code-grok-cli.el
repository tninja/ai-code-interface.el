;;; ai-code-grok-cli.el --- Thin wrapper for Grok CLI -*- lexical-binding: t; -*-

;; Author: richard134, Kang Tu

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Provide Grok CLI integration by reusing `ai-code-backends-infra'.

;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)

(defgroup ai-code-grok-cli nil
  "Grok CLI integration via `ai-code-backends-infra.el'."
  :group 'tools
  :prefix "ai-code-grok-cli-")

(defcustom ai-code-grok-cli-program "grok"
  "Path to the Grok CLI executable."
  :type 'string
  :group 'ai-code-grok-cli)

(defcustom ai-code-grok-cli-program-switches nil
  "Command line switches to pass to Grok CLI on startup."
  :type '(repeat string)
  :group 'ai-code-grok-cli)

(defconst ai-code-grok-cli--session-prefix "grok"
  "Session prefix used in Grok CLI buffer names.")

(defvar ai-code-grok-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping Grok session keys to processes.")

;;;###autoload
(defun ai-code-grok-cli (&optional arg)
  "Start Grok CLI (uses `ai-code-backends-infra' logic).
With prefix ARG, prompt for a new instance name."
  (interactive "P")
  (let* ((working-dir (ai-code-backends-infra--session-working-directory))
         (force-prompt (and arg t))
         (command (concat ai-code-grok-cli-program " "
                          (mapconcat 'identity ai-code-grok-cli-program-switches " "))))
    (ai-code-backends-infra--toggle-or-create-session
     working-dir
     nil
     ai-code-grok-cli--processes
     command
     nil
     nil
     nil
     ai-code-grok-cli--session-prefix
     force-prompt)))

;;;###autoload
(defun ai-code-grok-cli-switch-to-buffer (&optional force-prompt)
  "Switch to the Grok CLI buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--switch-to-session-buffer
     nil
     "No Grok session for this project"
     ai-code-grok-cli--session-prefix
     working-dir
     force-prompt)))

;;;###autoload
(defun ai-code-grok-cli-send-command (line)
  "Send LINE to Grok CLI programmatically or interactively.
When called interactively, prompts for the command.
When called from Lisp code, sends LINE directly without prompting."
  (interactive "sGrok> ")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--send-line-to-session
     nil
     "No Grok session for this project"
     line
     ai-code-grok-cli--session-prefix
     working-dir)))

;;;###autoload
(defun ai-code-grok-cli-resume (&optional arg)
  "Resume the previous Grok CLI session, when supported.
ARG is passed to the underlying start function."
  (interactive "P")
  (let ((ai-code-grok-cli-program-switches
         (append ai-code-grok-cli-program-switches '("resume"))))
    (ai-code-grok-cli arg)
    (let* ((working-dir (ai-code-backends-infra--session-working-directory))
           (buffer (ai-code-backends-infra--select-session-buffer
                    ai-code-grok-cli--session-prefix
                    working-dir)))
      (when buffer
        (with-current-buffer buffer
          (sit-for 0.5)
          (ai-code-backends-infra--terminal-send-string "")
          (goto-char (point-min)))))))

(provide 'ai-code-grok-cli)

;;; ai-code-grok-cli.el ends here
