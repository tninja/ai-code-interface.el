;;; ai-code-session-attention.el --- Show AI sessions that need attention -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Badge every tab-bar tab that shows an AI session with the session's
;; state: idle, working, finished while hidden, or waiting for input.
;; Signals come from the terminal layer, so this works for every AI coding
;; CLI backend.  The session dashboard shows the same state.
;;
;; Enable with (ai-code-session-attention-mode 1).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ai-code-session)

(defvar tab-bar-tab-name-format-functions)

(defgroup ai-code-session-attention nil
  "Show AI sessions that need the user's attention."
  :group 'ai-code)

(defface ai-code-session-attention-waiting
  '((t :inherit warning :weight bold))
  "Face for sessions waiting for user input."
  :group 'ai-code-session-attention)

(defface ai-code-session-attention-done
  '((t :inherit success :weight bold))
  "Face for sessions that finished while hidden."
  :group 'ai-code-session-attention)

(defface ai-code-session-attention-working
  '((t :inherit font-lock-keyword-face))
  "Face for sessions that are working."
  :group 'ai-code-session-attention)

(defface ai-code-session-attention-idle
  '((t :inherit shadow))
  "Face for sessions with nothing going on."
  :group 'ai-code-session-attention)

(defconst ai-code-session-attention--markers
  '((waiting "?" ai-code-session-attention-waiting)
    (done "✓" ai-code-session-attention-done)
    (working "◐" ai-code-session-attention-working)
    (idle "○" ai-code-session-attention-idle))
  "Badge text and face for each session state, most important first.
Symbols mark states that need the user; circles are ambient status.")

;;; State

(defun ai-code-session-attention--state (session)
  "Return the attention state of SESSION, or nil."
  (plist-get (ai-code-session-metadata session) :attention))

(defun ai-code-session-attention--visible-p (buffer)
  "Return non-nil when BUFFER is shown in a live window."
  (and (get-buffer-window-list buffer nil t) t))

(defun ai-code-session-attention--set (session state)
  "Record attention STATE for SESSION and redraw the tab bar."
  (ai-code-session-update-metadata
   (ai-code-session-buffer session)
   (append (list :attention state :attention-since (and state (float-time)))
           (unless state (list :quiet-since (float-time)))))
  ;; This also redraws the tab bar.
  (force-mode-line-update t))

(defun ai-code-session-attention-mark (buffer state)
  "Mark the session in BUFFER as needing attention with STATE.
STATE is `waiting' or `done'.  Visible sessions are ignored, and `done'
never replaces `waiting'."
  (when-let* ((session (ai-code-session-get buffer)))
    (let ((current (ai-code-session-attention--state session)))
      (unless (or (eq current state)
                  (and (eq state 'done) (eq current 'waiting))
                  (ai-code-session-attention--visible-p buffer))
        (ai-code-session-attention--set session state)))))

(defun ai-code-session-attention-clear (buffer &optional state)
  "Clear the attention state of the session in BUFFER.
When STATE is non-nil, clear only that state."
  (when-let* ((session (ai-code-session-get buffer))
              (current (ai-code-session-attention--state session))
              ((or (null state) (eq state current))))
    (ai-code-session-attention--set session nil)))

(defun ai-code-session-attention-set-working (buffer working)
  "Record whether the session in BUFFER is WORKING.
The tab bar is redrawn only when the state changes."
  (when-let* ((session (ai-code-session-get buffer))
              ((not (eq (and working t)
                        (plist-get (ai-code-session-metadata session) :working)))))
    (ai-code-session-update-metadata
     buffer (list :working (and working t)
                  (if working :working-since :quiet-since) (float-time)))
    (force-mode-line-update t)))

(defun ai-code-session-attention--clear-visible (&optional _frame)
  "Clear attention for sessions now shown in a live window."
  (dolist (session (ai-code-session-list))
    (let ((buffer (ai-code-session-buffer session)))
      (when (and (ai-code-session-attention--state session)
                 (buffer-live-p buffer)
                 (ai-code-session-attention--visible-p buffer))
        (ai-code-session-attention--set session nil)))))

(defun ai-code-session-attention--display-state (session)
  "Return the badge state of SESSION: `waiting', `done', `working' or `idle'."
  (or (ai-code-session-attention--state session)
      (and (plist-get (ai-code-session-metadata session) :working) 'working)
      'idle))

(defun ai-code-session-attention--rank (state)
  "Return the importance rank of badge STATE; lower is more important."
  (cl-position state ai-code-session-attention--markers :key #'car))

;;; Display

(defun ai-code-session-attention--marker (state)
  "Return the propertized marker for attention STATE."
  (pcase-let ((`(,_ ,text ,face) (assq state ai-code-session-attention--markers)))
    (propertize text 'face face)))

(defun ai-code-session-attention--tab-has-buffer-p (tab buffer)
  "Return non-nil when TAB shows BUFFER."
  (if (eq (car tab) 'current-tab)
      (get-buffer-window buffer)
    (when-let* ((state (alist-get 'ws tab)))
      (let ((buffers (window-state-buffers state)))
        (or (memq buffer buffers)
            (member (buffer-name buffer) buffers))))))

(defun ai-code-session-attention--tab-state (tab)
  "Return the most important badge state of the sessions TAB shows, or nil."
  (let (best)
    (dolist (session (ai-code-session-list))
      (let ((buffer (ai-code-session-buffer session)))
        (when (and (buffer-live-p buffer)
                   (ai-code-session-attention--tab-has-buffer-p tab buffer))
          (let ((state (ai-code-session-attention--display-state session)))
            (when (or (null best)
                      (< (ai-code-session-attention--rank state)
                         (ai-code-session-attention--rank best)))
              (setq best state))))))
    best))

(defun ai-code-session-attention--tab-name-format (name tab _index)
  "Prefix NAME with a badge when TAB shows an AI session."
  (if-let* ((state (ai-code-session-attention--tab-state tab)))
      (concat (ai-code-session-attention--marker state) " " name)
    name))

;;; Mode

;;;###autoload
(define-minor-mode ai-code-session-attention-mode
  "Badge tab-bar tabs that show AI sessions with the session state.
○ idle, ◐ working, ✓ finished while hidden, ? waiting for input.
Showing a session clears ✓ and ?."
  :global t
  :group 'ai-code-session-attention
  (if ai-code-session-attention-mode
      (progn
        (add-hook 'window-buffer-change-functions
                  #'ai-code-session-attention--clear-visible)
        ;; Load tab-bar first: adding to its unbound defcustom would
        ;; replace the default formatters.
        (when (and (require 'tab-bar nil t)
                   (boundp 'tab-bar-tab-name-format-functions))
          (add-hook 'tab-bar-tab-name-format-functions
                    #'ai-code-session-attention--tab-name-format)))
    (remove-hook 'window-buffer-change-functions
                 #'ai-code-session-attention--clear-visible)
    (when (boundp 'tab-bar-tab-name-format-functions)
      (remove-hook 'tab-bar-tab-name-format-functions
                   #'ai-code-session-attention--tab-name-format)))
  (force-mode-line-update t))

(provide 'ai-code-session-attention)

;;; ai-code-session-attention.el ends here
