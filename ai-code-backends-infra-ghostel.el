;;; ai-code-backends-infra-ghostel.el --- Ghostel support for AI Code terminals -*- lexical-binding: t; -*-

;; Author: Kang Tu
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This library contains ghostel-specific helpers for `ai-code-backends-infra'.

;;; Code:

;; Silence native-compiler warnings.
(declare-function ai-code-backends-infra--configure-session-input-shortcuts "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra--note-meaningful-output "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra--output-meaningful-p "ai-code-backends-infra" (output))
(declare-function ai-code-backends-infra--set-session-directory "ai-code-backends-infra" (buffer directory))
(declare-function ai-code-backends-infra--sync-terminal-cursor "ai-code-backends-infra" ())
(declare-function ai-code-session-link--linkify-recent-output "ai-code-session-link" (output))
(declare-function ghostel-mode "ghostel" ())
(declare-function ghostel--new "ghostel" (rows cols scrollback-bytes))
(declare-function ghostel--start-process "ghostel" ())
(declare-function ghostel--window-adjust-process-window-size "ghostel" (process windows))

(defvar ai-code-backends-infra--session-terminal-backend)
(defvar ghostel-enable-title-tracking t)
(defvar ghostel-max-scrollback nil)
(defvar ghostel--process nil)
(defvar ghostel--term nil)
(defvar ghostel--term-rows nil)
(defvar ghostel--copy-mode-active nil)

(defun ai-code-backends-infra--ensure-ghostel-backend ()
  "Ensure the ghostel backend is available."
  (unless (featurep 'ghostel)
    (require 'ghostel nil t))
  (unless (featurep 'ghostel)
    (user-error "The package ghostel is not installed")))

(defun ai-code-backends-infra--configure-ghostel-buffer ()
  "Configure the current Ghostel buffer for AI Code sessions."
  (unless (eq major-mode 'ghostel-mode)
    (ghostel-mode))
  ;; Keep AI session names stable so remembered sessions can still be
  ;; resolved through the conventional *backend[project]* buffer title.
  (setq-local ghostel-enable-title-tracking nil)
  (if-let ((window (get-buffer-window (current-buffer) t)))
      (ai-code-backends-infra--initialize-ghostel-term window)
    (add-hook 'window-configuration-change-hook
              #'ai-code-backends-infra--initialize-ghostel-when-displayed
              nil t))
  (ai-code-backends-infra--configure-session-input-shortcuts)
  (ai-code-backends-infra--ghostel-install-navigation-cursor-sync))

(defun ai-code-backends-infra--initialize-ghostel-term (window)
  "Initialize the current Ghostel terminal state for WINDOW."
  (let ((height (max 1 (window-body-height window)))
        (width (max 1 (window-max-chars-per-line window))))
    (setq-local ghostel--term
                (ghostel--new height width ghostel-max-scrollback))
    (setq-local ghostel--term-rows height)))

(defun ai-code-backends-infra--initialize-ghostel-when-displayed ()
  "Initialize the current Ghostel buffer once it has a live window."
  (when (and (eq major-mode 'ghostel-mode)
             (not (bound-and-true-p ghostel--term)))
    (when-let ((window (get-buffer-window (current-buffer) t)))
      (ai-code-backends-infra--initialize-ghostel-term window)
      (remove-hook 'window-configuration-change-hook
                   #'ai-code-backends-infra--initialize-ghostel-when-displayed
                   t))))

(defun ai-code-backends-infra--ghostel-send-string (string)
  "Send STRING to the current Ghostel process."
  (when (and (bound-and-true-p ghostel--process)
             (process-live-p ghostel--process))
    (process-send-string ghostel--process string)))

(defun ai-code-backends-infra--ghostel-send-escape ()
  "Send escape to the current Ghostel process."
  (ai-code-backends-infra--ghostel-send-string "\e"))

(defun ai-code-backends-infra--ghostel-send-return ()
  "Send return to the current Ghostel process."
  (ai-code-backends-infra--ghostel-send-string "\r"))

(defun ai-code-backends-infra--ghostel-send-backspace ()
  "Send backspace to the current Ghostel process."
  (ai-code-backends-infra--ghostel-send-string "\177"))

(defun ai-code-backends-infra--ghostel-navigation-mode-p ()
  "Return non-nil when the current Ghostel buffer is in copy mode."
  (bound-and-true-p ghostel--copy-mode-active))

(defun ai-code-backends-infra--ghostel-install-navigation-cursor-sync ()
  "Install Ghostel-specific hooks for cursor handoff."
  (add-hook 'post-command-hook
            #'ai-code-backends-infra--sync-terminal-cursor nil t))

(defun ai-code-backends-infra--ghostel-resize-handler ()
  "Return the resize handler used by Ghostel."
  #'ghostel--window-adjust-process-window-size)

(defun ai-code-backends-infra--create-ghostel-terminal-session (buffer-name working-dir command env-vars)
  "Create a Ghostel session using BUFFER-NAME, WORKING-DIR, COMMAND, and ENV-VARS."
  (let* ((default-directory working-dir)
         (buffer (get-buffer-create buffer-name))
         (process-environment (append env-vars process-environment)))
    (ai-code-backends-infra--set-session-directory buffer working-dir)
    (with-current-buffer buffer
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (ai-code-backends-infra--configure-ghostel-buffer)
      (let ((proc (ghostel--start-process)))
        (when (processp proc)
          (set-process-query-on-exit-flag proc nil)
          (let ((orig-filter (process-filter proc)))
            (set-process-filter
             proc
             (lambda (process output)
               (when orig-filter
                 (funcall orig-filter process output))
               (with-current-buffer (process-buffer process)
                 (when (ai-code-backends-infra--output-meaningful-p output)
                   (ai-code-backends-infra--note-meaningful-output))
                 (ai-code-session-link--linkify-recent-output output)))))
          (when (and command (> (length command) 0))
            (process-send-string proc (concat command "\r"))))
        (cons buffer proc)))))

(provide 'ai-code-backends-infra-ghostel)

;;; ai-code-backends-infra-ghostel.el ends here
