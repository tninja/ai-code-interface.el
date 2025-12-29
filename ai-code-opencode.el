;;; ai-code-opencode.el --- Thin wrapper for Opencode  -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `claude-code' to run Opencode.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;; Opencode is an open-source alternative to Claude Code that provides
;; HTTP server APIs and customization features (LSP, custom LLM providers, etc.)
;; See: https://opencode.ai/
;;
;;; Code:

(declare-function claude-code "claude-code" (&optional arg))
(declare-function claude-code--start "claude-code" (arg extra-switches &optional force-prompt force-switch-to-buffer))
(declare-function claude-code--term-send-string "claude-code" (backend string))
(declare-function claude-code-switch-to-buffer "claude-code")
(declare-function claude-code-send-command "claude-code" (line))
(defvar claude-code-terminal-backend)


(defgroup ai-code-opencode nil
  "Opencode integration via `claude-code'."
  :group 'tools
  :prefix "ai-code-opencode-")

(defcustom ai-code-opencode-program "opencode"
  "Path to the Opencode executable."
  :type 'string
  :group 'ai-code-opencode)

(defcustom ai-code-opencode-program-switches nil
  "Command line switches to pass to Opencode on startup."
  :type '(repeat string)
  :group 'ai-code-opencode)

;;;###autoload
(defun ai-code-opencode (&optional arg)
  "Start Opencode (reuses `claude-code' startup logic).
ARG is passed to `claude-code'."
  (interactive "P")
  (let ((claude-code-program ai-code-opencode-program)
        (claude-code-program-switches ai-code-opencode-program-switches))
    (claude-code arg)))

;;;###autoload
(defun ai-code-opencode-switch-to-buffer ()
  "Switch to the Opencode buffer."
  (interactive)
  (claude-code-switch-to-buffer))

;;;###autoload
(defun ai-code-opencode-send-command (line)
  "Send LINE to Opencode.
When called interactively, prompts for the command."
  (interactive "sOpencode> ")
  (claude-code-send-command line))

;;;###autoload
(defun ai-code-opencode-resume (&optional arg)
  "Resume a previous Opencode session.

This command starts Opencode with the --resume flag to resume
a specific past session. The CLI will present an interactive list of past
sessions to choose from.

If current buffer belongs to a project, start in the project's root
directory. Otherwise start in the directory of the current buffer file,
or the current value of `default-directory' if no project and no buffer file.

With double prefix ARG (\\[universal-argument] \\[universal-argument]),
prompt for the project directory."
  (interactive "P")
  (let ((claude-code-program ai-code-opencode-program)
        (claude-code-program-switches ai-code-opencode-program-switches))
    (claude-code--start arg '("resume") nil t)
    (claude-code--term-send-string claude-code-terminal-backend "")
    (with-current-buffer claude-code-terminal-backend
      (goto-char (point-min)))))

(provide 'ai-code-opencode)

;;; ai-code-opencode.el ends here

