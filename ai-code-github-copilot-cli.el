;;; ai-code-github-copilot-cli.el --- Thin wrapper for GitHub Copilot CLI  -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `claude-code' to run GitHub Copilot CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'ai-code-backends)

(defvar claude-code-program)
(defvar claude-code-program-switches)
(defvar claude-code-terminal-backend)
(declare-function claude-code "claude-code" (&optional arg extra-switches force-prompt force-switch-to-buffer))
(declare-function claude-code-resume "claude-code" (&optional arg))
(declare-function claude-code-switch-to-buffer "claude-code" (&optional arg))
(declare-function claude-code--start "claude-code" (arg extra-switches &optional force-prompt force-switch-to-buffer))
(declare-function claude-code--term-send-string "claude-code" (backend string))
(declare-function claude-code--do-send-command "claude-code" (cmd))


(defgroup ai-code-github-copilot-cli nil
  "GitHub Copilot CLI integration via `claude-code'."
  :group 'tools
  :prefix "ai-code-github-copilot-cli-")

(defcustom ai-code-github-copilot-cli-program "copilot"
  "Path to the GitHub Copilot CLI executable."
  :type 'string
  :group 'ai-code-github-copilot-cli)

(defcustom ai-code-github-copilot-cli-program-switches nil
  "Command line switches to pass to GitHub Copilot CLI on startup."
  :type '(repeat string)
  :group 'ai-code-github-copilot-cli)

;;;###autoload
(defun ai-code-github-copilot-cli (&optional arg)
  "Start GitHub Copilot CLI (reuses `claude-code' startup logic).
ARG is passed to `claude-code'."
  (interactive "P")
  (let ((claude-code-program ai-code-github-copilot-cli-program)
        (claude-code-program-switches ai-code-github-copilot-cli-program-switches))
    (claude-code arg)))

;;;###autoload
(defun ai-code-github-copilot-cli-switch-to-buffer ()
  "Switch to the GitHub Copilot CLI buffer."
  (interactive)
  (claude-code-switch-to-buffer))

;;;###autoload
(defun ai-code-github-copilot-cli-send-command (line)
  "Send LINE to GitHub Copilot CLI programmatically or interactively.
When called interactively, prompts for the command.
When called from Lisp code, sends LINE directly without prompting."
  (interactive "sCopilot> ")
  (claude-code--do-send-command line))

;;;###autoload
(defun ai-code-github-copilot-cli-resume (&optional arg)
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
  (let ((claude-code-program ai-code-github-copilot-cli-program)
        (claude-code-program-switches ai-code-github-copilot-cli-program-switches)
        (extra-switches '("--resume")))
    (claude-code--start arg extra-switches nil t)
    ;; Send empty string to trigger terminal processing and ensure CLI session picker appears
    (claude-code--term-send-string claude-code-terminal-backend "")
    ;; Position cursor at beginning to show session list from the top
    (goto-char (point-min))))

(provide 'ai-code-github-copilot-cli)

;;; ai-code-github-copilot-cli.el ends here

