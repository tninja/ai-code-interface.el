;;; ai-code-backends-infra-vterm.el --- Vterm support for AI Code terminals  -*- lexical-binding: t; -*-

;; Author: Yoav Orot, Kang Tu, Silex, Steve Molitor, AI Agent
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Vterm-specific support for `ai-code-backends-infra'.

;;; Code:

(require 'cl-lib)
(require 'ai-code-session-link)

(defcustom ai-code-backends-infra-vterm-anti-flicker t
  "Enable intelligent flicker reduction for vterm display."
  :type 'boolean
  :group 'ai-code-backends-infra)

(defcustom ai-code-backends-infra-vterm-render-delay 0.01
  "Rendering optimization delay for batched terminal updates."
  :type 'number
  :group 'ai-code-backends-infra)

(declare-function ai-code-backends-infra--configure-session-input-shortcuts
                  "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra--install-navigation-cursor-sync
                  "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra--note-meaningful-output
                  "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra--output-meaningful-p
                  "ai-code-backends-infra" (output))
(declare-function ai-code-backends-infra--session-buffer-p
                  "ai-code-backends-infra" (buffer))
(declare-function ai-code-backends-infra--set-session-directory
                  "ai-code-backends-infra" (buffer directory))
(declare-function ai-code-backends-infra--strip-alternate-screen-sequences
                  "ai-code-backends-infra" (str))
(declare-function ai-code-backends-infra--sync-terminal-cursor
                  "ai-code-backends-infra" ())
(declare-function ai-code-notifications-response-ready
                  "ai-code-notifications" (&optional backend-name))
(declare-function vterm "vterm" (&optional buffer-name))
(declare-function vterm-send-string "vterm" (&rest args))
(declare-function vterm-send-escape "vterm" ())
(declare-function vterm-send-return "vterm" ())
(declare-function vterm--window-adjust-process-window-size "vterm" (&rest args))
(declare-function vterm--filter "vterm" (&rest args))

(defvar ai-code-backends-infra-strip-alternate-screen)
(defvar ai-code-backends-infra--session-terminal-backend)
(defvar vterm-copy-mode)
(defvar vterm-copy-mode-hook)
(defvar vterm-environment)
(defvar vterm-kill-buffer-on-exit)
(defvar vterm-scroll-to-bottom-on-output)
(defvar vterm-shell)
(defvar ai-code-backends-infra--last-meaningful-output-time)
(defvar ai-code-backends-infra--response-seen)
(defvar vterm--redraw-immididately)

(defconst ai-code-backends-infra--vterm-redraw-regexp
  "\033\\[[0-9;?]*[A-GJKMH]"
  "Regexp to detect ANSI terminal redraw or movement sequences.
Standalone carriage returns are intentionally excluded so simple CR-based
updates are handled separately via carriage return counting.")

(defvar-local ai-code-backends-infra--vterm-render-queue nil)
(defvar-local ai-code-backends-infra--vterm-render-timer nil)

(defvar ai-code-backends-infra--vterm-advices-installed nil
  "Flag indicating whether vterm filter advices have been installed globally.")

(defun ai-code-backends-infra-vterm-ensure-backend ()
  "Ensure the vterm backend is available."
  (unless (featurep 'vterm) (require 'vterm nil t))
  (unless (featurep 'vterm)
    (user-error "The package vterm is not installed")))

(defun ai-code-backends-infra-vterm-navigation-mode-p ()
  "Return non-nil when the current vterm buffer is in copy mode."
  (bound-and-true-p vterm-copy-mode))

(defun ai-code-backends-infra-vterm-install-navigation-cursor-sync ()
  "Install cursor synchronization for vterm copy mode."
  (add-hook 'vterm-copy-mode-hook
            #'ai-code-backends-infra--sync-terminal-cursor nil t))

(defun ai-code-backends-infra-vterm-send-string (string)
  "Send STRING to the current vterm buffer."
  (vterm-send-string string))

(defun ai-code-backends-infra-vterm-send-escape ()
  "Send escape to the current vterm buffer."
  (vterm-send-escape))

(defun ai-code-backends-infra-vterm-send-return ()
  "Send return to the current vterm buffer."
  (vterm-send-return))

(defun ai-code-backends-infra-vterm-send-backspace ()
  "Send backspace to the current vterm buffer."
  (vterm-send-string "\177"))

(defun ai-code-backends-infra-vterm-resize-handler ()
  "Return the vterm resize handler."
  #'vterm--window-adjust-process-window-size)

(defun ai-code-backends-infra--vterm-notification-tracker (orig-fun process input)
  "Track vterm activity for notification purposes, then call ORIG-FUN.
When `ai-code-backends-infra-strip-alternate-screen' is non-nil,
strip alternate screen buffer sequences from INPUT for PROCESS so that TUI
applications write to the normal screen buffer (preserving scrollback)."
  (let ((filtered-input
         (if (ai-code-backends-infra--session-buffer-p (process-buffer process))
             (with-current-buffer (process-buffer process)
               (ai-code-backends-infra--strip-alternate-screen-sequences input))
           input)))
    (when (ai-code-backends-infra--session-buffer-p (process-buffer process))
      (with-current-buffer (process-buffer process)
        (when (ai-code-backends-infra--output-meaningful-p filtered-input)
          (ai-code-backends-infra--note-meaningful-output))))
    (prog1
        (funcall orig-fun process filtered-input)
      (when (ai-code-backends-infra--session-buffer-p (process-buffer process))
        (ai-code-session-link--schedule-linkify-recent-output
         (process-buffer process)
         filtered-input)))))

(defun ai-code-backends-infra--vterm-render-preserving-copy-mode-view (render-fn)
  "Call RENDER-FN while keeping the user's `vterm-copy-mode' viewport stable."
  (if (not (bound-and-true-p vterm-copy-mode))
      (funcall render-fn)
    (let ((point-marker (copy-marker (point) t))
          (window-states
           (mapcar (lambda (window)
                     (list window
                           (copy-marker (window-start window) t)
                           (copy-marker (window-point window) t)))
                   (get-buffer-window-list (current-buffer) nil t))))
      (unwind-protect
          (let ((inhibit-redisplay t))
            (funcall render-fn))
        (dolist (state window-states)
          (pcase-let ((`(,window ,start-marker ,window-point-marker) state))
            (when (window-live-p window)
              (set-window-start window start-marker t)
              (set-window-point window window-point-marker))
            (set-marker start-marker nil)
            (set-marker window-point-marker nil)))
        (goto-char point-marker)
        (set-marker point-marker nil)))))

(defun ai-code-backends-infra--vterm-render-queued-output (orig-fun buffer)
  "Render queued vterm output for BUFFER using ORIG-FUN."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ai-code-backends-infra--vterm-render-timer nil)
      (when ai-code-backends-infra--vterm-render-queue
        (let ((data ai-code-backends-infra--vterm-render-queue))
          (setq ai-code-backends-infra--vterm-render-queue nil)
          (when-let* ((process (get-buffer-process buffer))
                      ((process-live-p process)))
            (ai-code-backends-infra--vterm-render-preserving-copy-mode-view
             (lambda ()
               (funcall orig-fun process data)))))))))

(defun ai-code-backends-infra--vterm-smart-renderer (orig-fun process input)
  "Update vterm display via smart rendering around ORIG-FUN.
Activity tracking for notifications is handled separately by
`ai-code-backends-infra--vterm-notification-tracker'.
When PROCESS receives INPUT in `vterm-copy-mode', rendering preserves
the current viewport so scrollback continues updating without yanking
navigation."
  (if (or (not ai-code-backends-infra-vterm-anti-flicker)
          (not (ai-code-backends-infra--session-buffer-p (process-buffer process))))
      (funcall orig-fun process input)
    (with-current-buffer (process-buffer process)
      (let* ((complex-redraw-detected
              (string-match-p ai-code-backends-infra--vterm-redraw-regexp input))
             (clear-count (1- (length (split-string input "\033\\[K"))))
             (cr-count (cl-count ?\15 input))
             (escape-count (cl-count ?\033 input))
             (input-length (length input))
             (escape-density (if (> input-length 0)
                                 (/ (float escape-count) input-length)
                               0)))
        (if (or complex-redraw-detected
                (>= cr-count 2)
                (and (> escape-density 0.3) (>= clear-count 2))
                ai-code-backends-infra--vterm-render-queue)
            (let ((buffer (current-buffer)))
              (setq ai-code-backends-infra--vterm-render-queue
                    (concat ai-code-backends-infra--vterm-render-queue input))
              (when ai-code-backends-infra--vterm-render-timer
                (cancel-timer ai-code-backends-infra--vterm-render-timer))
              (setq ai-code-backends-infra--vterm-render-timer
                    (run-at-time ai-code-backends-infra-vterm-render-delay nil
                                 #'ai-code-backends-infra--vterm-render-queued-output
                                 orig-fun
                                 buffer)))
          (ai-code-backends-infra--vterm-render-preserving-copy-mode-view
           (lambda ()
             (funcall orig-fun process input))))))))

(defun ai-code-backends-infra--vterm-flush-on-copy-mode-exit ()
  "Flush any pending render queue when exiting `vterm-copy-mode'.
Added buffer-locally to `vterm-copy-mode-hook' so that terminal output
queued while copy mode was active is rendered immediately when the user
returns to normal terminal interaction."
  (unless (bound-and-true-p vterm-copy-mode)
    (when ai-code-backends-infra--vterm-render-queue
      (when-let ((proc (get-buffer-process (current-buffer))))
        (let ((data ai-code-backends-infra--vterm-render-queue))
          (setq ai-code-backends-infra--vterm-render-queue nil)
          (vterm--filter proc data))))))

(defun ai-code-backends-infra--configure-vterm-buffer ()
  "Configure vterm for enhanced performance."
  (setq-local vterm-scroll-to-bottom-on-output nil)
  (when (boundp 'vterm--redraw-immididately)
    (setq-local vterm--redraw-immididately nil))
  (ai-code-backends-infra--configure-session-input-shortcuts)
  (setq-local cursor-in-non-selected-windows nil)
  (setq-local blink-cursor-mode nil)
  (setq-local cursor-type nil)
  (when-let ((proc (get-buffer-process (current-buffer))))
    (set-process-query-on-exit-flag proc nil)
    (when (fboundp 'process-put)
      (process-put proc 'read-output-max 4096)))
  (add-hook 'vterm-copy-mode-hook
            #'ai-code-backends-infra--vterm-flush-on-copy-mode-exit nil t)
  (ai-code-backends-infra--install-navigation-cursor-sync)
  (unless ai-code-backends-infra--vterm-advices-installed
    (advice-add 'vterm--filter :around #'ai-code-backends-infra--vterm-notification-tracker)
    (when ai-code-backends-infra-vterm-anti-flicker
      (advice-add 'vterm--filter :around #'ai-code-backends-infra--vterm-smart-renderer))
    (setq ai-code-backends-infra--vterm-advices-installed t)))

(defun ai-code-backends-infra-vterm-create-session (buffer-name working-dir command env-vars)
  "Create a vterm session named BUFFER-NAME in WORKING-DIR.
COMMAND is the shell command to run and ENV-VARS are extra environment
variables for the terminal process."
  (let ((default-directory working-dir)
        (vterm-shell command)
        (vterm-kill-buffer-on-exit nil)
        (vterm-environment (append env-vars (bound-and-true-p vterm-environment))))
    (let ((buffer (save-window-excursion (vterm buffer-name))))
      (ai-code-backends-infra--set-session-directory buffer working-dir)
      (with-current-buffer buffer
        (setq-local ai-code-backends-infra--session-terminal-backend 'vterm)
        (ai-code-backends-infra--configure-vterm-buffer))
      (cons buffer (get-buffer-process buffer)))))

(provide 'ai-code-backends-infra-vterm)
;;; ai-code-backends-infra-vterm.el ends here
