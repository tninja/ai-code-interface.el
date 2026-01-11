;;; ai-code-github-copilot-cli.el --- Thin wrapper for GitHub Copilot CLI  -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `claude-code-ide-infra' to run GitHub Copilot CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'ai-code-backends)
(require 'claude-code-ide-infra)

(defgroup ai-code-github-copilot-cli nil
  "GitHub Copilot CLI integration via `claude-code-ide-infra'."
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

(defvar ai-code-github-copilot-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping directory roots to their Copilot processes.")

;;;###autoload
(defun ai-code-github-copilot-cli (&optional arg)
  "Start GitHub Copilot CLI (uses `claude-code-ide-infra' logic).
ARG is currently unused but kept for compatibility."
  (interactive "P")
  (let* ((working-dir (claude-code-ide-infra--session-working-directory))
         (buffer-name (claude-code-ide-infra--session-buffer-name "copilot" working-dir))
         (command (concat ai-code-github-copilot-cli-program " "
                          (mapconcat 'identity ai-code-github-copilot-cli-program-switches " "))))
    (claude-code-ide-infra--toggle-or-create-session
     working-dir
     buffer-name
     ai-code-github-copilot-cli--processes
     command
     #'ai-code-github-copilot-cli-send-escape
     (lambda ()
       (claude-code-ide-infra--cleanup-session
        working-dir
        buffer-name
        ai-code-github-copilot-cli--processes)))))

;;;###autoload
(defun ai-code-github-copilot-cli-switch-to-buffer ()
  "Switch to the GitHub Copilot CLI buffer."
  (interactive)
  (let* ((working-dir (claude-code-ide-infra--session-working-directory))
         (buffer-name (claude-code-ide-infra--session-buffer-name "copilot" working-dir)))
    (claude-code-ide-infra--switch-to-session-buffer
     buffer-name
     "No Copilot session for this project")))

;;;###autoload
(defun ai-code-github-copilot-cli-send-command (line)
  "Send LINE to GitHub Copilot CLI."
  (interactive "sCopilot> ")
  (let* ((working-dir (claude-code-ide-infra--session-working-directory))
         (buffer-name (claude-code-ide-infra--session-buffer-name "copilot" working-dir)))
    (claude-code-ide-infra--send-line-to-session
     buffer-name
     "No Copilot session for this project"
     line)))

;;;###autoload
(defun ai-code-github-copilot-cli-send-escape ()
  "Send escape key to GitHub Copilot CLI."
  (interactive)
  (claude-code-ide-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-github-copilot-cli-resume (&optional arg)
  "Resume a previous GitHub Copilot CLI session."
  (interactive "P")
  (let ((ai-code-github-copilot-cli-program-switches (append ai-code-github-copilot-cli-program-switches '("--resume"))))
    (ai-code-github-copilot-cli arg)
    ;; Send empty string to trigger terminal processing and ensure CLI session picker appears
    (let* ((working-dir (claude-code-ide-infra--session-working-directory))
           (buffer-name (claude-code-ide-infra--session-buffer-name "copilot" working-dir))
           (buffer (get-buffer buffer-name)))
      (when buffer
        (with-current-buffer buffer
          (sit-for 0.5)
          (claude-code-ide-infra--terminal-send-string "")
          (goto-char (point-min)))))))

(provide 'ai-code-github-copilot-cli)

;;; ai-code-github-copilot-cli.el ends here
