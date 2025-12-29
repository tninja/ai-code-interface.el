;;; ai-code-codex-cli.el --- Thin wrapper for Codex CLI  -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `claude-code' to run Codex CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'ai-code-backends)

(declare-function claude-code "claude-code" (&optional arg))
(declare-function claude-code--start "claude-code" (arg extra-switches &optional force-prompt force-switch-to-buffer))
(declare-function claude-code--term-send-string "claude-code" (backend string))
(declare-function claude-code--do-send-command "claude-code" (cmd))
(declare-function claude-code-switch-to-buffer "claude-code")
(defvar claude-code-terminal-backend)


(defgroup ai-code-codex-cli nil
  "Codex CLI integration via `claude-code'."
  :group 'tools
  :prefix "ai-code-codex-cli-")

(defcustom ai-code-codex-cli-program "codex"
  "Path to the Codex CLI executable."
  :type 'string
  :group 'ai-code-codex-cli)

(defcustom ai-code-codex-cli-program-switches nil
  "Command line switches to pass to Codex CLI on startup."
  :type '(repeat string)
  :group 'ai-code-codex-cli)

;;;###autoload
(defun ai-code-codex-cli (&optional arg)
  "Start Codex (reuses `claude-code' startup logic).
ARG is passed to `claude-code'."
  (interactive "P")
  (let ((claude-code-program ai-code-codex-cli-program)
        (claude-code-program-switches ai-code-codex-cli-program-switches))
    (claude-code arg)))

;;;###autoload
(defun ai-code-codex-cli-switch-to-buffer ()
  "Switch to the Codex CLI buffer."
  (interactive)
  (claude-code-switch-to-buffer))

;;;###autoload
(defun ai-code-codex-cli-send-command (line)
  "Send LINE to Codex CLI programmatically or interactively.
When called interactively, prompts for the command.
When called from Lisp code, sends LINE directly without prompting."
  (interactive "sCodex> ")
  (claude-code--do-send-command line))

;;;###autoload
(defun ai-code-codex-cli-resume (&optional arg)
  "Resume a previous Codex CLI session.
ARG is passed to the underlying start function."
  (interactive "P")
  (let ((claude-code-program ai-code-codex-cli-program)
        (claude-code-program-switches ai-code-codex-cli-program-switches))
    (claude-code--start arg '("resume") nil t)
    (claude-code--term-send-string claude-code-terminal-backend "")
    (with-current-buffer claude-code-terminal-backend
      (goto-char (point-min)))))

(provide 'ai-code-codex-cli)

;;; ai-code-codex-cli.el ends here

