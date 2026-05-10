;;; ai-code-backends-infra-etc.el --- Side-panel resize extras for AI Code terminals -*- lexical-binding: t; -*-

;; Author: AI Agent
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Extra side-panel resize commands and keybindings for AI Code sessions.

;;; Code:

(require 'cl-lib)

(declare-function ai-code-backends-infra--fit-side-window-body-width
                  "ai-code-backends-infra" (window))
(declare-function ai-code-backends-infra--configure-session-buffer
                  "ai-code-backends-infra" (buffer &optional escape-fn multiline-input-sequence))
(declare-function ai-code-backends-infra--session-buffer-p
                  "ai-code-backends-infra" (buffer))
(declare-function ai-code-backends-infra--sync-terminal-dimensions
                  "ai-code-backends-infra" (buffer window))

(defvar ai-code-backends-infra-window-width)
(defvar ai-code-backends-infra-window-height)

(defconst ai-code-backends-infra-etc--side-window-sides
  '(left right top bottom)
  "Valid side positions for AI side windows.")

(defcustom ai-code-backends-infra-etc-resize-step 5
  "Columns/rows to adjust on each panel resize command."
  :type 'integer
  :group 'ai-code-backends-infra)

(defun ai-code-backends-infra-etc--side-window-p (window)
  "Return non-nil when WINDOW is one of the configured side windows."
  (memq (window-parameter window 'window-side)
        ai-code-backends-infra-etc--side-window-sides))

(defun ai-code-backends-infra-etc--active-side-window ()
  "Return an active AI session side window, or nil when unavailable."
  (let ((current-window (get-buffer-window (current-buffer) t)))
    (cond
     ((and current-window
           (ai-code-backends-infra-etc--side-window-p current-window)
           (ai-code-backends-infra--session-buffer-p (current-buffer)))
      current-window)
     (t
      (cl-find-if
       (lambda (window)
         (let ((buffer (window-buffer window)))
           (and (window-live-p window)
                (ai-code-backends-infra-etc--side-window-p window)
                (buffer-live-p buffer)
                (ai-code-backends-infra--session-buffer-p buffer))))
       (window-list))))))

(defun ai-code-backends-infra-etc--apply-resize-delta (delta)
  "Apply panel resize DELTA to the current AI session side window."
  (let ((window (ai-code-backends-infra-etc--active-side-window)))
    (unless window
      (user-error "No visible AI side panel found"))
    (let ((side (window-parameter window 'window-side))
          (buffer (window-buffer window)))
      (pcase side
        ((or 'left 'right)
         (setq ai-code-backends-infra-window-width
               (max 1 (+ ai-code-backends-infra-window-width delta)))
         (ai-code-backends-infra--fit-side-window-body-width window))
        ((or 'top 'bottom)
         (setq ai-code-backends-infra-window-height
               (max 1 (+ ai-code-backends-infra-window-height delta)))
         (let ((height-delta (- ai-code-backends-infra-window-height
                                (window-body-height window))))
           (unless (zerop height-delta)
             (window-resize window height-delta nil))))
        (_
         (user-error "AI panel is not in a side window")))
      (ai-code-backends-infra--sync-terminal-dimensions buffer window))))

;;;###autoload
(defun ai-code-backends-infra-etc-grow-panel ()
  "Grow AI session side panel by `ai-code-backends-infra-etc-resize-step'."
  (interactive)
  (ai-code-backends-infra-etc--apply-resize-delta
   ai-code-backends-infra-etc-resize-step))

;;;###autoload
(defun ai-code-backends-infra-etc-shrink-panel ()
  "Shrink AI session side panel by `ai-code-backends-infra-etc-resize-step'."
  (interactive)
  (ai-code-backends-infra-etc--apply-resize-delta
   (- ai-code-backends-infra-etc-resize-step)))

(defun ai-code-backends-infra-etc--bind-panel-resize-keys ()
  "Install local resize keybindings in the current AI session buffer."
  (local-set-key (kbd "C-.") #'ai-code-backends-infra-etc-grow-panel)
  (local-set-key (kbd "C-,") #'ai-code-backends-infra-etc-shrink-panel))

(defun ai-code-backends-infra-etc--configure-session-buffer-advice (buffer &rest _args)
  "Bind resize keys after shared session setup for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (ai-code-backends-infra-etc--bind-panel-resize-keys))))

(defun ai-code-backends-infra-etc-activate ()
  "Activate side-panel resize extensions for AI session buffers."
  (unless (advice-member-p #'ai-code-backends-infra-etc--configure-session-buffer-advice
                           #'ai-code-backends-infra--configure-session-buffer)
    (advice-add #'ai-code-backends-infra--configure-session-buffer
                :after
                #'ai-code-backends-infra-etc--configure-session-buffer-advice))
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (ai-code-backends-infra--session-buffer-p buffer))
      (with-current-buffer buffer
        (ai-code-backends-infra-etc--bind-panel-resize-keys)))))

(provide 'ai-code-backends-infra-etc)

;;; ai-code-backends-infra-etc.el ends here
