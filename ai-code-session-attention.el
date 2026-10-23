;;; ai-code-session-attention.el --- Show AI sessions that need attention -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Track AI sessions that finished or wait for input while hidden and
;; badge the tab-bar tabs that show them.  Signals come from the terminal
;; layer, so this works for every AI coding CLI backend.  The session
;; dashboard shows the same state.
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
  '((t :inherit warning))
  "Face for sessions waiting for user input."
  :group 'ai-code-session-attention)

(defface ai-code-session-attention-done
  '((t :inherit success))
  "Face for sessions that finished while hidden."
  :group 'ai-code-session-attention)

(defconst ai-code-session-attention--markers
  '((waiting "?" ai-code-session-attention-waiting)
    (done "✓" ai-code-session-attention-done))
  "Marker text and face for each attention state, most urgent first.")

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
   (list :attention state :attention-since (and state (float-time))))
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

(defun ai-code-session-attention--clear-visible (&optional _frame)
  "Clear attention for sessions now shown in a live window."
  (dolist (session (ai-code-session-list))
    (let ((buffer (ai-code-session-buffer session)))
      (when (and (ai-code-session-attention--state session)
                 (buffer-live-p buffer)
                 (ai-code-session-attention--visible-p buffer))
        (ai-code-session-attention--set session nil)))))

(defun ai-code-session-attention--rank (session)
  "Return the urgency rank of SESSION; lower is more urgent."
  (cl-position (ai-code-session-attention--state session)
               ai-code-session-attention--markers :key #'car))

(defun ai-code-session-attention--pending ()
  "Return live sessions needing attention, most urgent and oldest first."
  (sort (cl-remove-if-not
         (lambda (session)
           (and (ai-code-session-attention--rank session)
                (buffer-live-p (ai-code-session-buffer session))))
         (ai-code-session-list))
        (lambda (left right)
          (let ((left-rank (ai-code-session-attention--rank left))
                (right-rank (ai-code-session-attention--rank right)))
            (if (/= left-rank right-rank)
                (< left-rank right-rank)
              (< (plist-get (ai-code-session-metadata left) :attention-since)
                 (plist-get (ai-code-session-metadata right) :attention-since)))))))

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

(defun ai-code-session-attention--tab-name-format (name tab _index)
  "Prefix NAME with a marker when TAB shows a session needing attention."
  (if-let* ((session (cl-find-if
                      (lambda (session)
                        (ai-code-session-attention--tab-has-buffer-p
                         tab (ai-code-session-buffer session)))
                      (ai-code-session-attention--pending))))
      (concat (ai-code-session-attention--marker
               (ai-code-session-attention--state session))
              " " name)
    name))

;;; Mode

;;;###autoload
(define-minor-mode ai-code-session-attention-mode
  "Badge tab-bar tabs that show AI sessions needing attention.
A session needs attention when it finished or waits for input while
hidden; showing it clears that."
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
