;;; ai-code-grok-cli.el --- Thin wrapper for Grok CLI -*- lexical-binding: t; -*-

;;; Commentary:
;; Provide Grok CLI integration by reusing `claude-code'.

;;; Code:

(require 'claude-code)
(require 'ai-code-backends)

(declare-function claude-code--start "claude-code" (arg extra-switches &optional force-prompt force-switch-to-buffer))
(declare-function claude-code--term-send-string "claude-code" (backend string))
(declare-function claude-code--do-send-command "claude-code" (cmd))
(defvar claude-code-terminal-backend)

(defgroup ai-code-grok-cli nil
  "Grok CLI integration via `claude-code'."
  :group 'tools
  :prefix "grok-cli-")

(defcustom grok-cli-program "grok"
  "Path to the Grok CLI executable."
  :type 'string
  :group 'ai-code-grok-cli)

(defcustom grok-cli-program-switches nil
  "Command line switches to pass to Grok CLI on startup."
  :type '(repeat string)
  :group 'ai-code-grok-cli)

;;;###autoload
(defun grok-cli (&optional arg)
  "Start Grok CLI by leveraging `claude-code'."
  (interactive "P")
  (let ((claude-code-program grok-cli-program)
        (claude-code-program-switches grok-cli-program-switches))
    (claude-code arg)))

;;;###autoload
(defun grok-cli-switch-to-buffer ()
  "Switch to the Grok CLI buffer."
  (interactive)
  (claude-code-switch-to-buffer))

;;;###autoload
(defun grok-cli-send-command (line)
  "Send LINE to Grok CLI programmatically or interactively.
When called interactively, prompts for the command.
When called from Lisp code, sends LINE directly without prompting."
  (interactive "sGrok> ")
  (claude-code--do-send-command line))

;;;###autoload
(defun grok-cli-resume (&optional arg)
  "Resume the previous Grok CLI session, when supported."
  (interactive "P")
  (let ((claude-code-program grok-cli-program)
        (claude-code-program-switches grok-cli-program-switches))
    (claude-code--start arg '("resume") nil t)
    (claude-code--term-send-string claude-code-terminal-backend "")
    (with-current-buffer claude-code-terminal-backend
      (goto-char (point-min)))))

(provide 'ai-code-grok-cli)

;;; ai-code-grok-cli.el ends here
