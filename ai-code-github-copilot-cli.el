;;; ai-code-github-copilot-cli.el --- Thin wrapper for Github Copilot CLI  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Thin wrapper that reuses `claude-code' to run Github Copilot CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'claude-code)
(require 'ai-code-backends)

(defvar claude-code-program)
(defvar claude-code-program-switches)
(defvar claude-code-terminal-backend)
(declare-function claude-code "claude-code" (&optional arg extra-switches force-prompt force-switch-to-buffer))
(declare-function claude-code-resume "claude-code" (&optional arg))
(declare-function claude-code-switch-to-buffer "claude-code" (&optional arg))
(declare-function claude-code--start "claude-code" (arg extra-switches &optional force-prompt force-switch-to-buffer))
(declare-function claude-code--term-send-string "claude-code" (backend string))
(declare-function ai-code--claude-code-send-command-impl "ai-code-backends" (cmd))


(defgroup ai-code-github-copilot-cli nil
  "Github Copilot CLI integration via `claude-code'."
  :group 'tools
  :prefix "github-copilot-cli-")

(defcustom github-copilot-cli-program "copilot"
  "Path to the Github Copilot CLI executable."
  :type 'string
  :group 'ai-code-github-copilot-cli)

;;;###autoload
(defun github-copilot-cli (&optional arg)
  "Start Github Copilot CLI (reuses `claude-code' startup logic)."
  (interactive "P")
  (let ((claude-code-program github-copilot-cli-program) ; override dynamically
        (claude-code-program-switches nil))         ; optional e.g.: '("exec" "--non-interactive")
    (claude-code arg)))

;;;###autoload
(defun github-copilot-cli-switch-to-buffer ()
  (interactive)
  (claude-code-switch-to-buffer))

;;;###autoload
(defun github-copilot-cli-send-command (line)
  "Send LINE to Github Copilot CLI programmatically or interactively.
When called interactively, prompts for the command.
When called from Lisp code, sends LINE directly without prompting."
  (interactive "sCopilot> ")
  (ai-code--claude-code-send-command-impl line))

;;;###autoload
(defun github-copilot-cli-resume (&optional arg)
  "Resume a previous GitHub Copilot CLI session.

This command starts GitHub Copilot CLI with the --resume flag to resume
a specific past session. The CLI will present an interactive list of past
sessions to choose from.

If current buffer belongs to a project, start in the project's root
directory. Otherwise start in the directory of the current buffer file,
or the current value of `default-directory' if no project and no buffer file.

With double prefix ARG (\\[universal-argument] \\[universal-argument]),
prompt for the project directory."
  (interactive "P")
  (let ((claude-code-program github-copilot-cli-program)
        (claude-code-program-switches nil)
        (extra-switches '("--resume")))
    (claude-code--start arg extra-switches nil t)
    ;; Send empty string to trigger terminal processing and ensure CLI session picker appears
    (claude-code--term-send-string claude-code-terminal-backend "")
    ;; Position cursor at beginning to show session list from the top
    (goto-char (point-min))))

(provide 'ai-code-github-copilot-cli)

;;; ai-code-github-copilot-cli.el ends here
