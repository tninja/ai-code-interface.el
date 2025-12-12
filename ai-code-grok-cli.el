;;; ai-code-grok-cli.el --- Thin wrapper for Grok CLI -*- lexical-binding: t; -*-

;;; Commentary:
;; Provide Grok CLI integration by reusing `claude-code'.

;;; Code:

;; Package-Requires: ((emacs "26.1") (claude-code "0.1") (ai-code-backends "0.1"))

(defvar claude-code-program)
(defvar claude-code-program-switches)
(defvar claude-code-terminal-backend)
(declare-function claude-code--start "claude" (arg extra-switches &optional force-prompt force-switch-to-buffer))
(declare-function claude-code--term-send-string "claude" (backend string))
(declare-function claude-code--do-send-command "claude" (cmd))
(defconst ai-code-grok-cli--missing-claude-code-msg
  "claude-code.el is required for Grok CLI integration. Install it from https://github.com/stevemolitor/claude-code.el.")

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

(defun ai-code-grok-cli--ensure-claude-code ()
  (unless (require 'claude-code nil t)
    (user-error "%s" ai-code-grok-cli--missing-claude-code-msg)))

;;;###autoload
(defun grok-cli (&optional arg)
  "Start Grok CLI by leveraging `claude-code'."
  (interactive "P")
  (ai-code-grok-cli--ensure-claude-code)
  (let ((claude-code-program grok-cli-program)
        (claude-code-program-switches grok-cli-program-switches))
    (claude-code arg)))

;;;###autoload
(defun grok-cli-switch-to-buffer ()
  "Switch to the Grok CLI buffer."
  (interactive)
  (ai-code-grok-cli--ensure-claude-code)
  (claude-code-switch-to-buffer))

;;;###autoload
(defun grok-cli-send-command (line)
  "Send LINE to Grok CLI programmatically or interactively.
When called interactively, prompts for the command.
When called from Lisp code, sends LINE directly without prompting."
  (interactive "sGrok> ")
  (ai-code-grok-cli--ensure-claude-code)
  (claude-code--do-send-command line))

;;;###autoload
(defun grok-cli-resume (&optional arg)
  "Resume the previous Grok CLI session, when supported."
  (interactive "P")
  (ai-code-grok-cli--ensure-claude-code)
  (let ((claude-code-program grok-cli-program)
        (claude-code-program-switches grok-cli-program-switches))
    (claude-code--start arg '("resume") nil t)
    (claude-code--term-send-string claude-code-terminal-backend "")
    (with-current-buffer claude-code-terminal-backend
      (goto-char (point-min)))))

(provide 'ai-code-grok-cli)

;;; ai-code-grok-cli.el ends here
