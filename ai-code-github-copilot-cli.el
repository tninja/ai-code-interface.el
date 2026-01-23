;;; ai-code-github-copilot-cli.el --- Thin wrapper for GitHub Copilot CLI  -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `ai-code-backends-infra' to run GitHub Copilot CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)

(defgroup ai-code-github-copilot-cli nil
  "GitHub Copilot CLI integration via `ai-code-backends-infra'."
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

(defconst ai-code-github-copilot-cli--session-prefix "copilot"
  "Session prefix used in GitHub Copilot CLI buffer names.")

(defvar ai-code-github-copilot-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping Copilot session keys to processes.")

;;;###autoload
(defun ai-code-github-copilot-cli (&optional arg)
  "Start GitHub Copilot CLI (uses `ai-code-backends-infra' logic).
With prefix ARG, prompt for a new instance name."
  (interactive "P")
  (let* ((working-dir (ai-code-backends-infra--session-working-directory))
         (force-prompt (and arg t))
         (command (concat ai-code-github-copilot-cli-program " "
                          (mapconcat 'identity ai-code-github-copilot-cli-program-switches " "))))
    (ai-code-backends-infra--toggle-or-create-session
     working-dir
     nil
     ai-code-github-copilot-cli--processes
     command
     #'ai-code-github-copilot-cli-send-escape
     nil
     nil
     ai-code-github-copilot-cli--session-prefix
     force-prompt)))

;;;###autoload
(defun ai-code-github-copilot-cli-switch-to-buffer (&optional force-prompt)
  "Switch to the GitHub Copilot CLI buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--switch-to-session-buffer
     nil
     "No Copilot session for this project"
     ai-code-github-copilot-cli--session-prefix
     working-dir
     force-prompt)))

;;;###autoload
(defun ai-code-github-copilot-cli-send-command (line)
  "Send LINE to GitHub Copilot CLI."
  (interactive "sCopilot> ")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--send-line-to-session
     nil
     "No Copilot session for this project"
     line
     ai-code-github-copilot-cli--session-prefix
     working-dir)))

;;;###autoload
(defun ai-code-github-copilot-cli-send-escape ()
  "Send escape key to GitHub Copilot CLI."
  (interactive)
  (ai-code-backends-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-github-copilot-cli-resume (&optional arg)
  "Resume a previous GitHub Copilot CLI session."
  (interactive "P")
  (let ((ai-code-github-copilot-cli-program-switches (append ai-code-github-copilot-cli-program-switches '("--resume"))))
    (ai-code-github-copilot-cli arg)
    ;; Send empty string to trigger terminal processing and ensure CLI session picker appears
    (let* ((working-dir (ai-code-backends-infra--session-working-directory))
           (buffer (ai-code-backends-infra--select-session-buffer
                    ai-code-github-copilot-cli--session-prefix
                    working-dir)))
      (when buffer
        (with-current-buffer buffer
          (sit-for 0.5)
          (ai-code-backends-infra--terminal-send-string "")
          (goto-char (point-min)))))))

(provide 'ai-code-github-copilot-cli)

;;; ai-code-github-copilot-cli.el ends here
