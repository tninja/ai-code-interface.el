;;; ai-code-codebuddy-cli.el --- Thin wrapper for CodeBuddy Code CLI  -*- lexical-binding: t; -*-

;; Author: liaohanqin
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `ai-code-backends-infra' to run CodeBuddy Code CLI.
;; Provides interactive commands and aliases for AI Code suite.
;;
;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)

(defgroup ai-code-codebuddy-cli nil
  "CodeBuddy Code CLI integration via `ai-code-backends-infra'."
  :group 'tools
  :prefix "ai-code-codebuddy-cli-")

(defcustom ai-code-codebuddy-cli-program "codebuddy"
  "Path to CodeBuddy Code CLI executable."
  :type 'string
  :group 'ai-code-codebuddy-cli)

(defcustom ai-code-codebuddy-cli-program-switches nil
  "Command line switches to pass to CodeBuddy CLI on startup."
  :type '(repeat string)
  :group 'ai-code-codebuddy-cli)

(defconst ai-code-codebuddy-cli--session-prefix "codebuddy"
  "Session prefix used in CodeBuddy CLI buffer names.")

(defvar ai-code-codebuddy-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping CodeBuddy session keys to processes.")

;;;###autoload
(defun ai-code-codebuddy-cli (&optional arg)
  "Start CodeBuddy (uses `ai-code-backends-infra' logic).
With prefix ARG, prompt for CLI args using
`ai-code-codebuddy-cli-program-switches' as the default input."
  (interactive "P")
  (let* ((working-dir (ai-code-backends-infra--session-working-directory))
         (resolved (ai-code-backends-infra--resolve-start-command
                    ai-code-codebuddy-cli-program
                    ai-code-codebuddy-cli-program-switches
                    arg
                    "CodeBuddy"))
         (command (plist-get resolved :command)))
    (ai-code-backends-infra--toggle-or-create-session
     working-dir
     nil
     ai-code-codebuddy-cli--processes
     command
     #'ai-code-codebuddy-cli-send-escape
     nil
     nil
     ai-code-codebuddy-cli--session-prefix
     nil)))

;;;###autoload
(defun ai-code-codebuddy-cli-switch-to-buffer (&optional force-prompt)
  "Switch to the CodeBuddy CLI buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--switch-to-session-buffer
     nil
     "No CodeBuddy session for this project"
     ai-code-codebuddy-cli--session-prefix
     working-dir
     force-prompt)))

;;;###autoload
(defun ai-code-codebuddy-cli-send-command (line)
  "Send LINE to the CodeBuddy CLI."
  (interactive "sCodeBuddy> ")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--send-line-to-session
     nil
     "No CodeBuddy session for this project"
     line
     ai-code-codebuddy-cli--session-prefix
     working-dir)))

;;;###autoload
(defun ai-code-codebuddy-cli-send-escape ()
  "Send escape key to the CodeBuddy CLI."
  (interactive)
  (ai-code-backends-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-codebuddy-cli-resume (&optional arg)
  "Resume a previous CodeBuddy CLI session.
Argument ARG is passed to the start command."
  (interactive "P")
  (let ((ai-code-codebuddy-cli-program-switches (append ai-code-codebuddy-cli-program-switches '("-c"))))
    (ai-code-codebuddy-cli arg)))

(provide 'ai-code-codebuddy-cli)

;;; ai-code-codebuddy-cli.el ends here
