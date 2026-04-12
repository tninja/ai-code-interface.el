;;; ai-code-backends-infra-eat.el --- Eat support for AI Code terminals -*- lexical-binding: t; -*-

;; Author: Yoav Orot, Kang Tu, Silex, Steve Molitor, AI Agent
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This library contains eat-specific helpers for `ai-code-backends-infra'.

;;; Code:

;; Silence native-compiler warnings.
(declare-function ai-code-backends-infra--configure-session-input-shortcuts "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra--install-navigation-cursor-sync "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra--note-meaningful-output "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra--output-meaningful-p "ai-code-backends-infra" (output))
(declare-function ai-code-backends-infra--set-session-directory "ai-code-backends-infra" (buffer directory))
(declare-function ai-code-session-link--linkify-recent-output "ai-code-session-link" (output))
(declare-function eat-term-send-string "eat" (&rest args))
(declare-function eat--adjust-process-window-size "eat" (&rest args))
(declare-function eat-mode "eat" ())
(declare-function eat-exec "eat" (&rest args))

(defvar ai-code-backends-infra--session-terminal-backend)
(defvar eat-terminal)

(defun ai-code-backends-infra--ensure-eat-backend ()
  "Ensure the eat backend is available."
  (unless (featurep 'eat)
    (require 'eat nil t))
  (unless (featurep 'eat)
    (user-error "The package eat is not installed")))

(defun ai-code-backends-infra--eat-send-string (string)
  "Send STRING to the current Eat terminal."
  (when (bound-and-true-p eat-terminal)
    (eat-term-send-string eat-terminal string)))

(defun ai-code-backends-infra--eat-send-escape ()
  "Send escape to the current Eat terminal."
  (when (bound-and-true-p eat-terminal)
    (eat-term-send-string eat-terminal "\e")))

(defun ai-code-backends-infra--eat-send-return ()
  "Send return to the current Eat terminal."
  (when (bound-and-true-p eat-terminal)
    (eat-term-send-string eat-terminal "\r")))

(defun ai-code-backends-infra--eat-send-backspace ()
  "Send backspace to the current Eat terminal."
  (when (bound-and-true-p eat-terminal)
    (eat-term-send-string eat-terminal "\177")))

(defun ai-code-backends-infra--eat-resize-handler ()
  "Return the resize handler used by Eat."
  #'eat--adjust-process-window-size)

(defun ai-code-backends-infra--create-eat-terminal-session (buffer-name working-dir command env-vars)
  "Create an Eat session using BUFFER-NAME, WORKING-DIR, COMMAND, and ENV-VARS."
  (let* ((default-directory working-dir)
         (buffer (get-buffer-create buffer-name))
         (parts (split-string-shell-command command))
         (program (car parts))
         (args (cdr parts)))
    (ai-code-backends-infra--set-session-directory buffer working-dir)
    (with-current-buffer buffer
      (setq-local ai-code-backends-infra--session-terminal-backend 'eat)
      (unless (eq major-mode 'eat-mode)
        (eat-mode))
      (ai-code-backends-infra--configure-session-input-shortcuts)
      (ai-code-backends-infra--install-navigation-cursor-sync)
      (setq-local process-environment (append env-vars process-environment))
      (eat-exec buffer buffer-name program nil args)
      (when-let ((proc (get-buffer-process buffer)))
        (let ((orig-filter (process-filter proc)))
          (set-process-filter
           proc
           (lambda (process output)
             (when orig-filter
               (funcall orig-filter process output))
             (with-current-buffer (process-buffer process)
               (when (ai-code-backends-infra--output-meaningful-p output)
                 (ai-code-backends-infra--note-meaningful-output))
               (ai-code-session-link--linkify-recent-output output))))))
      (cons buffer (get-buffer-process buffer)))))

(provide 'ai-code-backends-infra-eat)

;;; ai-code-backends-infra-eat.el ends here
