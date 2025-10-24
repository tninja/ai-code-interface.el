;;; ai-code-codex-cli.el --- Thin wrapper for Codex CLI  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Thin wrapper that reuses `claude-code' to run Codex CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'claude-code)
(require 'ai-code-backends)

(declare-function claude-code--start "claude-code" (arg extra-switches &optional force-prompt force-switch-to-buffer))
(declare-function claude-code--term-send-string "claude-code" (backend string))
(declare-function ai-code--claude-code-send-command-impl "ai-code-backends" (cmd))
(defvar claude-code-terminal-backend)


(defgroup ai-code-codex-cli nil
  "Codex CLI integration via `claude-code'."
  :group 'tools
  :prefix "codex-cli-")

(defcustom codex-cli-program "codex"
  "Path to the Codex CLI executable."
  :type 'string
  :group 'ai-code-codex-cli)

;;;###autoload
(defun codex-cli (&optional arg)
  "Start Codex (reuses `claude-code' startup logic)."
  (interactive "P")
  (let ((claude-code-program codex-cli-program) ; override dynamically
        (claude-code-program-switches nil))         ; optional e.g.: '("exec" "--non-interactive")
    (claude-code arg)))

;;;###autoload
(defun codex-cli-switch-to-buffer ()
  (interactive)
  (claude-code-switch-to-buffer))

;;;###autoload
(defun codex-cli-send-command (line)
  "Send LINE to Codex CLI programmatically or interactively.
When called interactively, prompts for the command.
When called from Lisp code, sends LINE directly without prompting."
  (interactive "sCodex> ")
  (ai-code--claude-code-send-command-impl line))

;;;###autoload
(defun codex-cli-resume (&optional arg)
  "Resume a previous Codex CLI session."
  (interactive "P")
  (let ((claude-code-program codex-cli-program)
        (claude-code-program-switches nil))
    (claude-code--start arg '("resume") nil t)
    (claude-code--term-send-string claude-code-terminal-backend "")
    (with-current-buffer claude-code-terminal-backend
      (goto-char (point-min)))))

(provide 'ai-code-codex-cli)

;;; ai-code-codex-cli.el ends here
