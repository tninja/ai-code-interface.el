;;; ai-code-backends-infra-ghostel.el --- Ghostel support for AI Code terminals  -*- lexical-binding: t; -*-

;; Author: Kang Tu, AI Agent
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Ghostel-specific support for `ai-code-backends-infra'.

;;; Code:

(require 'ai-code-session-link)

;; Prefer `ghostel-exec' for Ghostel backend startup when available, as
;; it simplifies process startup integration.

(declare-function ai-code-backends-infra--configure-session-input-shortcuts
                  "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra--install-navigation-cursor-sync
                  "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra--note-meaningful-output
                  "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra--output-meaningful-p
                  "ai-code-backends-infra" (output))
(declare-function ai-code-backends-infra--set-session-directory
                  "ai-code-backends-infra" (buffer directory))
(declare-function ai-code-backends-infra--sync-terminal-cursor
                  "ai-code-backends-infra" ())
(declare-function ghostel-exec "ghostel" (buffer program &optional args))
(declare-function ghostel-send-key "ghostel" (key-name &optional mods))
(declare-function ghostel-send-string "ghostel" (string))
(declare-function ghostel--window-adjust-process-window-size
                  "ghostel" (process windows))

(defvar ai-code-backends-infra--session-terminal-backend)
(defvar ghostel--copy-mode-active nil)
(defvar ghostel-set-title-function nil)

(defun ai-code-backends-infra-ghostel-ensure-backend ()
  "Ensure the Ghostel backend is available."
  (unless (featurep 'ghostel) (require 'ghostel nil t))
  (unless (featurep 'ghostel)
    (user-error "The package ghostel is not installed")))

(defun ai-code-backends-infra-ghostel-navigation-mode-p ()
  "Return non-nil when the current Ghostel buffer is in copy mode."
  (bound-and-true-p ghostel--copy-mode-active))

(defun ai-code-backends-infra-ghostel-install-navigation-cursor-sync ()
  "Install cursor synchronization for Ghostel navigation mode."
  (add-hook 'post-command-hook
            #'ai-code-backends-infra--sync-terminal-cursor nil t))

(defun ai-code-backends-infra-ghostel-send-string (string)
  "Send STRING to the current Ghostel process."
  (ghostel-send-string string))

(defun ai-code-backends-infra-ghostel-send-escape ()
  "Send escape to the current Ghostel process."
  (ghostel-send-key "escape"))

(defun ai-code-backends-infra-ghostel-send-return ()
  "Send return to the current Ghostel process."
  (ghostel-send-key "return"))

(defun ai-code-backends-infra-ghostel-send-backspace ()
  "Send backspace to the current Ghostel process."
  (ghostel-send-key "backspace"))

(defun ai-code-backends-infra-ghostel-resize-handler ()
  "Return the Ghostel resize handler."
  #'ghostel--window-adjust-process-window-size)

(defun ai-code-backends-infra--configure-ghostel-buffer ()
  "Configure the current Ghostel buffer for AI Code sessions."
  (setq-local ghostel-set-title-function nil)
  (ai-code-backends-infra--configure-session-input-shortcuts)
  (ai-code-backends-infra--install-navigation-cursor-sync))

(defun ai-code-backends-infra--start-ghostel-process (buffer command)
  "Start a Ghostel session in BUFFER for COMMAND."
  (with-current-buffer buffer
    (ai-code-backends-infra--configure-ghostel-buffer)
    (let* ((argv (split-string-shell-command command))
           (program (car argv))
           (args (cdr argv)))
      (cond
       ((not program) nil)
       ((fboundp 'ghostel-exec)
        (ghostel-exec buffer program args))
       (t
        (user-error
         "Ghostel backend requires a Ghostel version that provides `ghostel-exec`"))))))

(defun ai-code-backends-infra-ghostel-create-session (buffer-name working-dir command env-vars)
  "Create a Ghostel session named BUFFER-NAME in WORKING-DIR.
COMMAND is the shell command to run and ENV-VARS are extra environment
variables for the terminal process."
  (let* ((working-dir (file-name-as-directory (expand-file-name working-dir)))
         (buffer (get-buffer-create buffer-name))
         (process-environment (append env-vars process-environment)))
    (ai-code-backends-infra--set-session-directory buffer working-dir)
    (with-current-buffer buffer
      (setq-local default-directory working-dir)
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (let ((default-directory working-dir)
            (proc (ai-code-backends-infra--start-ghostel-process buffer command)))
        (when (processp proc)
          (ignore-errors
            (set-process-query-on-exit-flag proc nil))
          (let ((orig-filter (process-filter proc)))
            (set-process-filter
             proc
             (lambda (process output)
               (when (buffer-live-p buffer)
                 (when orig-filter
                   (funcall orig-filter process output))
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (when (ai-code-backends-infra--output-meaningful-p output)
                       (ai-code-backends-infra--note-meaningful-output))
                     (ai-code-session-link--linkify-recent-output output))))))))
        (cons buffer proc)))))

(provide 'ai-code-backends-infra-ghostel)
;;; ai-code-backends-infra-ghostel.el ends here
