;;; ai-code-gemini-cli.el --- Thin wrapper for Gemini CLI  -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `ai-code-backends-infra' to run Gemini CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)

(defgroup ai-code-gemini-cli nil
  "Gemini CLI integration via `ai-code-backends-infra'."
  :group 'tools
  :prefix "ai-code-gemini-cli-")

(defcustom ai-code-gemini-cli-program "gemini"
  "Path to the Gemini CLI executable."
  :type 'string
  :group 'ai-code-gemini-cli)

(defcustom ai-code-gemini-cli-program-switches nil
  "Command line switches to pass to Gemini CLI on startup."
  :type '(repeat string)
  :group 'ai-code-gemini-cli)

(defvar ai-code-gemini-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping directory roots to their Gemini processes.")

;;;###autoload
(defun ai-code-gemini-cli (&optional arg)
  "Start Gemini (uses `ai-code-backends-infra' logic).
ARG is currently unused but kept for compatibility."
  (interactive "P")
  (let* ((working-dir (ai-code-backends-infra--session-working-directory))
         (buffer-name (ai-code-backends-infra--session-buffer-name "gemini" working-dir))
         (command (concat ai-code-gemini-cli-program " "
                          (mapconcat 'identity ai-code-gemini-cli-program-switches " "))))
    (ai-code-backends-infra--toggle-or-create-session
     working-dir
     buffer-name
     ai-code-gemini-cli--processes
     command
     #'ai-code-gemini-cli-send-escape
     (lambda ()
       (ai-code-backends-infra--cleanup-session
        working-dir
        buffer-name
        ai-code-gemini-cli--processes)))))

;;;###autoload
(defun ai-code-gemini-cli-switch-to-buffer ()
  "Switch to the Gemini CLI buffer."
  (interactive)
  (let* ((working-dir (ai-code-backends-infra--session-working-directory))
         (buffer-name (ai-code-backends-infra--session-buffer-name "gemini" working-dir)))
    (ai-code-backends-infra--switch-to-session-buffer
     buffer-name
     "No Gemini session for this project")))

;;;###autoload
(defun ai-code-gemini-cli-send-command (line)
  "Send LINE to Gemini CLI."
  (interactive "sGemini> ")
  (let* ((working-dir (ai-code-backends-infra--session-working-directory))
         (buffer-name (ai-code-backends-infra--session-buffer-name "gemini" working-dir)))
    (ai-code-backends-infra--send-line-to-session
     buffer-name
     "No Gemini session for this project"
     line)))

;;;###autoload
(defun ai-code-gemini-cli-send-escape ()
  "Send escape key to Gemini CLI."
  (interactive)
  (ai-code-backends-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-gemini-cli-resume (&optional arg)
  "Resume a previous Gemini CLI session."
  (interactive "P")
  (let ((ai-code-gemini-cli-program-switches (append ai-code-gemini-cli-program-switches '("--resume"))))
    (ai-code-gemini-cli arg)))

(provide 'ai-code-gemini-cli)

;;; ai-code-gemini-cli.el ends here
