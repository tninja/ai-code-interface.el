;;; ai-code-codex-cli.el --- Thin wrapper for Codex CLI  -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `ai-code-backends-infra' to run Codex CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)

(defgroup ai-code-codex-cli nil
  "Codex CLI integration via `ai-code-backends-infra'."
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

(defvar ai-code-codex-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping directory roots to their Codex processes.")

;;;###autoload
(defun ai-code-codex-cli (&optional arg)
  "Start Codex (uses `ai-code-backends-infra' logic).
ARG is currently unused but kept for compatibility."
  (interactive "P")
  (let* ((working-dir (ai-code-backends-infra--session-working-directory))
         (buffer-name (ai-code-backends-infra--session-buffer-name "codex" working-dir))
         (command (concat ai-code-codex-cli-program " "
                          (mapconcat 'identity ai-code-codex-cli-program-switches " "))))
    (ai-code-backends-infra--toggle-or-create-session
     working-dir
     buffer-name
     ai-code-codex-cli--processes
     command
     #'ai-code-codex-cli-send-escape
     (lambda ()
       (ai-code-backends-infra--cleanup-session
        working-dir
        buffer-name
        ai-code-codex-cli--processes)))))

;;;###autoload
(defun ai-code-codex-cli-switch-to-buffer ()
  "Switch to the Codex CLI buffer."
  (interactive)
  (let* ((working-dir (ai-code-backends-infra--session-working-directory))
         (buffer-name (ai-code-backends-infra--session-buffer-name "codex" working-dir)))
    (ai-code-backends-infra--switch-to-session-buffer
     buffer-name
     "No Codex session for this project")))

;;;###autoload
(defun ai-code-codex-cli-send-command (line)
  "Send LINE to Codex CLI."
  (interactive "sCodex> ")
  (let* ((working-dir (ai-code-backends-infra--session-working-directory))
         (buffer-name (ai-code-backends-infra--session-buffer-name "codex" working-dir)))
    (ai-code-backends-infra--send-line-to-session
     buffer-name
     "No Codex session for this project"
     line)))

;;;###autoload
(defun ai-code-codex-cli-send-escape ()
  "Send escape key to Codex CLI."
  (interactive)
  (ai-code-backends-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-codex-cli-resume (&optional arg)
  "Resume a previous Codex CLI session."
  (interactive "P")
  (let ((ai-code-codex-cli-program-switches (append ai-code-codex-cli-program-switches '("resume"))))
    (ai-code-codex-cli arg)))

(provide 'ai-code-codex-cli)

;;; ai-code-codex-cli.el ends here
