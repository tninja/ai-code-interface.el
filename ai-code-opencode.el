;;; ai-code-opencode.el --- Thin wrapper for Opencode  -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `ai-code-backends-infra' to run Opencode.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;; Opencode is an open-source alternative to Claude Code that provides
;; HTTP server APIs and customization features (LSP, custom LLM providers, etc.)
;; See: https://opencode.ai/
;;
;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)


(defgroup ai-code-opencode nil
  "Opencode integration via `ai-code-backends-infra.el'."
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

(defconst ai-code-opencode--session-prefix "opencode"
  "Session prefix used in Opencode buffer names.")

(defvar ai-code-opencode--processes (make-hash-table :test 'equal)
  "Hash table mapping Opencode session keys to processes.")

;;;###autoload
(defun ai-code-opencode (&optional arg)
  "Start Opencode (uses `ai-code-backends-infra' logic).
With prefix ARG, prompt for a new instance name."
  (interactive "P")
  (let* ((working-dir (ai-code-backends-infra--session-working-directory))
         (force-prompt (and arg t))
         (command (concat ai-code-opencode-program " "
                          (mapconcat 'identity ai-code-opencode-program-switches " "))))
    (ai-code-backends-infra--toggle-or-create-session
     working-dir
     nil
     ai-code-opencode--processes
     command
     nil
     nil
     nil
     ai-code-opencode--session-prefix
     force-prompt)))

;;;###autoload
(defun ai-code-opencode-switch-to-buffer (&optional force-prompt)
  "Switch to the Opencode buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--switch-to-session-buffer
     nil
     "No Opencode session for this project"
     ai-code-opencode--session-prefix
     working-dir
     force-prompt)))

;;;###autoload
(defun ai-code-opencode-send-command (line)
  "Send LINE to Opencode.
When called interactively, prompts for the command."
  (interactive "sOpencode> ")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--send-line-to-session
     nil
     "No Opencode session for this project"
     line
     ai-code-opencode--session-prefix
     working-dir)))

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
  (let ((ai-code-opencode-program-switches
         (append ai-code-opencode-program-switches '("--continue"))))
    (ai-code-opencode arg)
    (let* ((working-dir (ai-code-backends-infra--session-working-directory))
           (buffer (ai-code-backends-infra--select-session-buffer
                    ai-code-opencode--session-prefix
                    working-dir)))
      (when buffer
        (with-current-buffer buffer
          (sit-for 0.5)
          (ai-code-backends-infra--terminal-send-string "")
          (goto-char (point-min)))))))

(provide 'ai-code-opencode)

;;; ai-code-opencode.el ends here
