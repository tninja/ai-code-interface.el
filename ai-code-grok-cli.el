;;; ai-code-grok-cli.el --- Thin wrapper for Grok CLI -*- lexical-binding: t; -*-

;; Author: richard134, Kang Tu

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Provide Grok CLI integration by reusing `claude-code-ide-infra'.

;;; Code:

(require 'ai-code-backends)
(require 'claude-code-ide-infra)

(defgroup ai-code-grok-cli nil
  "Grok CLI integration via `claude-code'."
  :group 'tools
  :prefix "ai-code-grok-cli-")

(defcustom ai-code-grok-cli-program "grok"
  "Path to the Grok CLI executable."
  :type 'string
  :group 'ai-code-grok-cli)

(defcustom ai-code-grok-cli-program-switches nil
  "Command line switches to pass to Grok CLI on startup."
  :type '(repeat string)
  :group 'ai-code-grok-cli)

(defvar ai-code-grok-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping directory roots to their Grok processes.")

;;;###autoload
(defun ai-code-grok-cli (&optional arg)
  "Start Grok CLI (uses `claude-code-ide-infra' logic).
ARG is currently unused but kept for compatibility."
  (interactive "P")
  (let* ((working-dir (claude-code-ide-infra--session-working-directory))
         (buffer-name (claude-code-ide-infra--session-buffer-name "grok" working-dir))
         (command (concat ai-code-grok-cli-program " "
                          (mapconcat 'identity ai-code-grok-cli-program-switches " "))))
    (claude-code-ide-infra--toggle-or-create-session
     working-dir
     buffer-name
     ai-code-grok-cli--processes
     command
     nil
     (lambda ()
       (claude-code-ide-infra--cleanup-session
        working-dir
        buffer-name
        ai-code-grok-cli--processes)))))

;;;###autoload
(defun ai-code-grok-cli-switch-to-buffer ()
  "Switch to the Grok CLI buffer."
  (interactive)
  (let* ((working-dir (claude-code-ide-infra--session-working-directory))
         (buffer-name (claude-code-ide-infra--session-buffer-name "grok" working-dir)))
    (claude-code-ide-infra--switch-to-session-buffer
     buffer-name
     "No Grok session for this project")))

;;;###autoload
(defun ai-code-grok-cli-send-command (line)
  "Send LINE to Grok CLI programmatically or interactively.
When called interactively, prompts for the command.
When called from Lisp code, sends LINE directly without prompting."
  (interactive "sGrok> ")
  (let* ((working-dir (claude-code-ide-infra--session-working-directory))
         (buffer-name (claude-code-ide-infra--session-buffer-name "grok" working-dir)))
    (claude-code-ide-infra--send-line-to-session
     buffer-name
     "No Grok session for this project"
     line)))

;;;###autoload
(defun ai-code-grok-cli-resume (&optional arg)
  "Resume the previous Grok CLI session, when supported.
ARG is passed to the underlying start function."
  (interactive "P")
  (let ((ai-code-grok-cli-program-switches
         (append ai-code-grok-cli-program-switches '("resume"))))
    (ai-code-grok-cli arg)
    (let* ((working-dir (claude-code-ide-infra--session-working-directory))
           (buffer-name (claude-code-ide-infra--session-buffer-name "grok" working-dir))
           (buffer (get-buffer buffer-name)))
      (when buffer
        (with-current-buffer buffer
          (sit-for 0.5)
          (claude-code-ide-infra--terminal-send-string "")
          (goto-char (point-min)))))))

(provide 'ai-code-grok-cli)

;;; ai-code-grok-cli.el ends here
