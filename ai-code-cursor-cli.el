;;; ai-code-cursor-cli.el --- Thin wrapper for Cursor CLI  -*- lexical-binding: t; -*-

;; Author: donneyluck <donneyluck@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `ai-code-backends-infra' to run Cursor CLI (cursor-agent).
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)

(defgroup ai-code-cursor-cli nil
  "Cursor CLI integration via `ai-code-backends-infra'."
  :group 'tools
  :prefix "ai-code-cursor-cli-")

(defcustom ai-code-cursor-cli-program "cursor-agent"
  "Path to the Cursor CLI executable (cursor-agent)."
  :type 'string
  :group 'ai-code-cursor-cli)

(defcustom ai-code-cursor-cli-program-switches nil
  "Command line switches to pass to Cursor CLI on startup."
  :type '(repeat string)
  :group 'ai-code-cursor-cli)

(defconst ai-code-cursor-cli--session-prefix "cursor"
  "Session prefix used in Cursor CLI buffer names.")

(defvar ai-code-cursor-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping Cursor session keys to processes.")

;;;###autoload
(defun ai-code-cursor-cli (&optional arg)
  "Start Cursor CLI (uses `ai-code-backends-infra' logic).
With prefix ARG, prompt for a new instance name."
  (interactive "P")
  (let* ((working-dir (ai-code-backends-infra--session-working-directory))
         (force-prompt (and arg t))
         (command (concat ai-code-cursor-cli-program " "
                          (mapconcat 'identity ai-code-cursor-cli-program-switches " "))))
    (ai-code-backends-infra--toggle-or-create-session
     working-dir
     nil
     ai-code-cursor-cli--processes
     command
     #'ai-code-cursor-cli-send-escape
     nil
     nil
     ai-code-cursor-cli--session-prefix
     force-prompt)))

;;;###autoload
(defun ai-code-cursor-cli-switch-to-buffer (&optional force-prompt)
  "Switch to the Cursor CLI buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--switch-to-session-buffer
     nil
     "No Cursor session for this project"
     ai-code-cursor-cli--session-prefix
     working-dir
     force-prompt)))

;;;###autoload
(defun ai-code-cursor-cli-send-command (line)
  "Send LINE to Cursor CLI."
  (interactive "sCursor> ")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--send-line-to-session
     nil
     "No Cursor session for this project"
     line
     ai-code-cursor-cli--session-prefix
     working-dir)))

;;;###autoload
(defun ai-code-cursor-cli-send-escape ()
  "Send escape key to Cursor CLI."
  (interactive)
  (ai-code-backends-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-cursor-cli-resume (&optional arg)
  "Resume a previous Cursor CLI session."
  (interactive "P")
  (let ((ai-code-cursor-cli-program-switches (append ai-code-cursor-cli-program-switches '("resume"))))
    (ai-code-cursor-cli arg)
    ;; Send empty string to trigger terminal processing and ensure CLI session picker appears
    (let* ((working-dir (ai-code-backends-infra--session-working-directory))
           (buffer (ai-code-backends-infra--select-session-buffer
                    ai-code-cursor-cli--session-prefix
                    working-dir)))
      (when buffer
        (with-current-buffer buffer
          (sit-for 0.5)
          (ai-code-backends-infra--terminal-send-string "")
          (goto-char (point-min)))))))

(provide 'ai-code-cursor-cli)

;;; ai-code-cursor-cli.el ends here
