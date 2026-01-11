;;; ai-code-codex-cli.el --- Thin wrapper for Codex CLI  -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `claude-code-ide-infra' to run Codex CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'ai-code-backends)
(require 'claude-code-ide-infra)

(defgroup ai-code-codex-cli nil
  "Codex CLI integration via `claude-code-ide-infra'."
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

(defun ai-code-codex-cli--get-working-directory ()
  "Get the current working directory."
  (if-let ((project (project-current)))
      (expand-file-name (project-root project))
    (expand-file-name default-directory)))

(defun ai-code-codex-cli--get-buffer-name (directory)
  "Generate buffer name for Codex in DIRECTORY."
  (format "*codex[%s]*"
          (file-name-nondirectory (directory-file-name directory))))

(defun ai-code-codex-cli--cleanup-on-exit (directory)
  "Clean up Codex session for DIRECTORY."
  (remhash directory ai-code-codex-cli--processes)
  (let ((buffer-name (ai-code-codex-cli--get-buffer-name directory)))
    (when-let ((buffer (get-buffer buffer-name)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;;###autoload
(defun ai-code-codex-cli (&optional arg)
  "Start Codex (uses `claude-code-ide-infra' logic).
ARG is currently unused but kept for compatibility."
  (interactive "P")
  (claude-code-ide-infra--cleanup-dead-processes ai-code-codex-cli--processes)
  (let* ((working-dir (ai-code-codex-cli--get-working-directory))
         (buffer-name (ai-code-codex-cli--get-buffer-name working-dir))
         (existing-process (gethash working-dir ai-code-codex-cli--processes)))

    (if (and existing-process (process-live-p existing-process))
        (let ((buffer (get-buffer buffer-name)))
          (if (get-buffer-window buffer)
              (delete-window (get-buffer-window buffer))
            (claude-code-ide-infra--display-buffer-in-side-window buffer)))
      ;; Start new session
      (let* ((command (concat ai-code-codex-cli-program " "
                              (mapconcat 'identity ai-code-codex-cli-program-switches " ")))
             (buffer-and-process (claude-code-ide-infra--create-terminal-session
                                  buffer-name working-dir command nil))
             (buffer (car buffer-and-process))
             (process (cdr buffer-and-process)))
        (puthash working-dir process ai-code-codex-cli--processes)
        (set-process-sentinel process
                              (lambda (_proc _event)
                                (ai-code-codex-cli--cleanup-on-exit working-dir)))
        (with-current-buffer buffer
          (local-set-key (kbd "C-<escape>") #'ai-code-codex-cli-send-escape))
        (sleep-for claude-code-ide-infra-terminal-initialization-delay)
        (claude-code-ide-infra--display-buffer-in-side-window buffer)))))

;;;###autoload
(defun ai-code-codex-cli-switch-to-buffer ()
  "Switch to the Codex CLI buffer."
  (interactive)
  (let* ((working-dir (ai-code-codex-cli--get-working-directory))
         (buffer-name (ai-code-codex-cli--get-buffer-name working-dir)))
    (if-let ((buffer (get-buffer buffer-name)))
        (if-let ((window (get-buffer-window buffer)))
            (select-window window)
          (claude-code-ide-infra--display-buffer-in-side-window buffer))
      (user-error "No Codex session for this project"))))

;;;###autoload
(defun ai-code-codex-cli-send-command (line)
  "Send LINE to Codex CLI."
  (interactive "sCodex> ")
  (let* ((working-dir (ai-code-codex-cli--get-working-directory))
         (buffer-name (ai-code-codex-cli--get-buffer-name working-dir)))
    (if-let ((buffer (get-buffer buffer-name)))
        (with-current-buffer buffer
          (claude-code-ide-infra--terminal-send-string line)
          (sit-for 0.1)
          (claude-code-ide-infra--terminal-send-return))
      (user-error "No Codex session for this project"))))

;;;###autoload
(defun ai-code-codex-cli-send-escape ()
  "Send escape key to Codex CLI."
  (interactive)
  (claude-code-ide-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-codex-cli-resume (&optional arg)
  "Resume a previous Codex CLI session."
  (interactive "P")
  (let ((ai-code-codex-cli-program-switches (append ai-code-codex-cli-program-switches '("resume"))))
    (ai-code-codex-cli arg)))

(provide 'ai-code-codex-cli)

;;; ai-code-codex-cli.el ends here