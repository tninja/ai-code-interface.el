;;; ai-code-codex-cli.el --- Thin wrapper for Codex CLI  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Thin wrapper that reuses `claude-code' to run Codex CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

;; Package-Requires: ((emacs "26.1") (claude-code "0.1") (ai-code-backends "0.1"))

(defconst ai-code-codex-cli--missing-claude-code-msg
  "claude-code.el is required for Codex CLI integration. Install it from https://github.com/stevemolitor/claude-code.el.")
(defvar claude-code-program)
(defvar claude-code-program-switches)
(defvar claude-code-terminal-backend)
(declare-function claude-code--start "claude-code" (arg extra-switches &optional force-prompt force-switch-to-buffer))
(declare-function claude-code--term-send-string "claude-code" (backend string))
(declare-function claude-code--do-send-command "claude-code" (cmd))


(defgroup ai-code-codex-cli nil
  "Codex CLI integration via `claude-code'."
  :group 'tools
  :prefix "codex-cli-")

(defcustom codex-cli-program "codex"
  "Path to the Codex CLI executable."
  :type 'string
  :group 'ai-code-codex-cli)

(defcustom codex-cli-program-switches nil
  "Command line switches to pass to Codex CLI on startup."
  :type '(repeat string)
  :group 'ai-code-codex-cli)

(defun ai-code-codex-cli--ensure-claude-code ()
  (unless (require 'claude-code nil t)
    (user-error "%s" ai-code-codex-cli--missing-claude-code-msg)))

;;;###autoload
(defun codex-cli (&optional arg)
  "Start Codex (reuses `claude-code' startup logic)."
  (interactive "P")
  (ai-code-codex-cli--ensure-claude-code)
  (let ((claude-code-program codex-cli-program) ; override dynamically
        (claude-code-program-switches codex-cli-program-switches))
    (claude-code arg)))

;;;###autoload
(defun codex-cli-switch-to-buffer ()
  (interactive)
  (ai-code-codex-cli--ensure-claude-code)
  (claude-code-switch-to-buffer))

;;;###autoload
(defun codex-cli-send-command (line)
  "Send LINE to Codex CLI programmatically or interactively.
When called interactively, prompts for the command.
When called from Lisp code, sends LINE directly without prompting."
  (interactive "sCodex> ")
  (ai-code-codex-cli--ensure-claude-code)
  (claude-code--do-send-command line))

;;;###autoload
(defun codex-cli-resume (&optional arg)
  "Resume a previous Codex CLI session."
  (interactive "P")
  (ai-code-codex-cli--ensure-claude-code)
  (let ((claude-code-program codex-cli-program)
        (claude-code-program-switches codex-cli-program-switches))
    (claude-code--start arg '("resume") nil t)
    (claude-code--term-send-string claude-code-terminal-backend "")
    (with-current-buffer claude-code-terminal-backend
      (goto-char (point-min)))))

(provide 'ai-code-codex-cli)

;;; ai-code-codex-cli.el ends here
