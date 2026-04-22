;;; ai-code-backends-infra-eat.el --- Eat support for AI Code terminals  -*- lexical-binding: t; -*-

;; Author: Yoav Orot, Kang Tu, Silex, Steve Molitor, AI Agent
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Eat-specific support for `ai-code-backends-infra'.

;;; Code:

(require 'ai-code-session-link)

(defcustom ai-code-backends-infra-eat-preserve-position t
  "Maintain terminal scroll position when switching windows in eat.
When enabled, prevents the eat terminal from jumping to the top
when you switch focus to other windows and return.  This provides
a more stable viewing experience when working with multiple windows."
  :type 'boolean
  :group 'ai-code-backends-infra)

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
(declare-function ai-code-backends-infra--strip-alternate-screen-sequences
                  "ai-code-backends-infra" (str))
(declare-function ai-code-backends-infra--sync-terminal-cursor
                  "ai-code-backends-infra" ())
(declare-function eat-term-send-string "eat" (&rest args))
(declare-function eat--adjust-process-window-size "eat" (&rest args))
(declare-function eat-mode "eat" ())
(declare-function eat-exec "eat" (&rest args))
(declare-function eat-term-display-cursor "eat" (terminal))

(defvar ai-code-backends-infra--session-terminal-backend)
(defvar eat--semi-char-mode)
(defvar eat--synchronize-scroll-function)
(defvar eat-term-name)
(defvar eat-terminal)

(defun ai-code-backends-infra-eat-ensure-backend ()
  "Ensure the Eat backend is available."
  (unless (featurep 'eat) (require 'eat nil t))
  (unless (featurep 'eat)
    (user-error "The package eat is not installed")))

(defun ai-code-backends-infra-eat-navigation-mode-p ()
  "Return non-nil when the current Eat buffer is in navigation mode."
  (and (bound-and-true-p eat-terminal)
       (or buffer-read-only
           (not (bound-and-true-p eat--semi-char-mode)))))

(defun ai-code-backends-infra-eat-install-navigation-cursor-sync ()
  "Install cursor synchronization for Eat navigation mode."
  (add-hook 'post-command-hook
            #'ai-code-backends-infra--sync-terminal-cursor nil t))

(defun ai-code-backends-infra-eat-send-string (string)
  "Send STRING to the current Eat terminal."
  (when (bound-and-true-p eat-terminal)
    (eat-term-send-string eat-terminal string)))

(defun ai-code-backends-infra-eat-send-escape ()
  "Send escape to the current Eat terminal."
  (when (bound-and-true-p eat-terminal)
    (eat-term-send-string eat-terminal "\e")))

(defun ai-code-backends-infra-eat-send-return ()
  "Send return to the current Eat terminal."
  (when (bound-and-true-p eat-terminal)
    (eat-term-send-string eat-terminal "\r")))

(defun ai-code-backends-infra-eat-send-backspace ()
  "Send backspace to the current Eat terminal."
  (when (bound-and-true-p eat-terminal)
    (eat-term-send-string eat-terminal "\177")))

(defun ai-code-backends-infra-eat-resize-handler ()
  "Return the Eat resize handler."
  #'eat--adjust-process-window-size)

(defun ai-code-backends-infra--eat-terminal-position-keeper (window-list)
  "Maintain stable terminal view position across window switch events.
WINDOW-LIST contains windows requiring position synchronization.
Implements intelligent scroll management to preserve user context
when navigating between terminal and other buffers."
  (dolist (win window-list)
    (if (eq win 'buffer)
        (goto-char (eat-term-display-cursor eat-terminal))
      (unless buffer-read-only
        (let ((terminal-point (eat-term-display-cursor eat-terminal)))
          (set-window-point win terminal-point)
          (cond
           ((>= terminal-point (- (point-max) 2))
            (with-selected-window win
              (goto-char terminal-point)
              (recenter -1)))
           ((not (pos-visible-in-window-p terminal-point win))
            (with-selected-window win
              (goto-char terminal-point)
              (recenter)))))))))

(defun ai-code-backends-infra-eat-create-session (buffer-name working-dir command env-vars)
  "Create an Eat session named BUFFER-NAME in WORKING-DIR.
COMMAND is the shell command to run and ENV-VARS are extra environment
variables for the terminal process."
  (let* ((working-dir (file-name-as-directory (expand-file-name working-dir)))
         (buffer (get-buffer-create buffer-name))
         (eat-term-name "xterm-256color")
         (parts (split-string-shell-command command))
         (program (car parts))
         (args (cdr parts)))
    (ai-code-backends-infra--set-session-directory buffer working-dir)
    (with-current-buffer buffer
      (setq-local default-directory working-dir)
      (setq-local ai-code-backends-infra--session-terminal-backend 'eat)
      (unless (eq major-mode 'eat-mode) (eat-mode))
      (when ai-code-backends-infra-eat-preserve-position
        (setq-local eat--synchronize-scroll-function
                    #'ai-code-backends-infra--eat-terminal-position-keeper))
      (ai-code-backends-infra--configure-session-input-shortcuts)
      (ai-code-backends-infra--install-navigation-cursor-sync)
      (setq-local process-environment (append env-vars process-environment))
      (let ((default-directory working-dir))
        (eat-exec buffer buffer-name program nil args))
      (when-let ((proc (get-buffer-process buffer)))
        (let ((orig-filter (process-filter proc)))
          (set-process-filter
           proc
           (lambda (process output)
             (when (buffer-live-p buffer)
               (let ((filtered-output
                      (with-current-buffer buffer
                        (ai-code-backends-infra--strip-alternate-screen-sequences output))))
                 (when orig-filter
                   (funcall orig-filter process filtered-output))
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (when (ai-code-backends-infra--output-meaningful-p filtered-output)
                       (ai-code-backends-infra--note-meaningful-output))
                     (ai-code-session-link--linkify-recent-output filtered-output)))))))))
      (cons buffer (get-buffer-process buffer)))))

(provide 'ai-code-backends-infra-eat)
;;; ai-code-backends-infra-eat.el ends here
