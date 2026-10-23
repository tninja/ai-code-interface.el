;;; ai-code-session-attention.el --- Show AI sessions that need attention -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Track AI sessions that finished or wait for input while hidden, show a
;; summary in the mode line (and optionally on tab-bar tabs), and jump to
;; them across frames and tabs.  Signals come from the terminal layer, so
;; this works for every AI coding CLI backend.
;;
;; Enable with (ai-code-session-attention-mode 1).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ai-code-session)

(defvar tab-bar-tabs-function)
(defvar tab-bar-tab-name-format-functions)
(declare-function tab-bar-get-buffer-tab "tab-bar"
                  (buffer-or-name &optional all-frames ignore-current-tab all-tabs))
(declare-function tab-bar-select-tab "tab-bar" (&optional tab-number))

(defgroup ai-code-session-attention nil
  "Show AI sessions that need the user's attention."
  :group 'ai-code)

(defcustom ai-code-session-attention-tab-bar-badge t
  "When non-nil, badge tab-bar tabs that show a session needing attention.
This has no effect unless tab-bar tabs are used."
  :type 'boolean
  :group 'ai-code-session-attention)

(defcustom ai-code-session-attention-mode-line-position 'front
  "Where `ai-code-session-attention-mode' puts the mode-line summary.
`front' puts it first in every buffer's mode line, including buffers
with their own `mode-line-format', so no mode-line package can hide it.
`global' adds it to `global-mode-string' instead, which mode-line
packages place themselves; some, like spaceline, show it only in the
selected window and hide it when that window is narrow.
Toggle the mode after changing this."
  :type '(choice (const :tag "First in every mode line" front)
                 (const :tag "In global-mode-string" global))
  :group 'ai-code-session-attention)

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

(defvar ai-code-session-attention--origin nil
  "Plist of the frame, tab index and window selected before the last jump.")

;;; State

(defun ai-code-session-attention--state (session)
  "Return the attention state of SESSION, or nil."
  (plist-get (ai-code-session-metadata session) :attention))

(defun ai-code-session-attention--visible-p (buffer)
  "Return non-nil when BUFFER is shown in a live window."
  (and (get-buffer-window-list buffer nil t) t))

(defun ai-code-session-attention--set (session state)
  "Record attention STATE for SESSION and refresh the mode line."
  (ai-code-session-update-metadata
   (ai-code-session-buffer session)
   (list :attention state :attention-since (and state (float-time))))
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

(defun ai-code-session-attention--describe (session)
  "Return a one-line description of SESSION for menus and tooltips."
  (format "%s %s  (%s)"
          (substring-no-properties
           (ai-code-session-attention--marker
            (ai-code-session-attention--state session)))
          (buffer-name (ai-code-session-buffer session))
          (format-seconds "%Y, %D, %H, %M, %z%S"
                          (- (float-time)
                             (plist-get (ai-code-session-metadata session)
                                        :attention-since)))))

(defvar ai-code-session-attention-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'ai-code-session-attention-next)
    (define-key map [mode-line mouse-3] #'ai-code-session-attention-menu)
    map)
  "Keymap for the attention summary in the mode line.")

(defun ai-code-session-attention-mode-line-string ()
  "Return the attention summary for the mode line, or an empty string."
  (let* ((pending (ai-code-session-attention--pending))
         (counts
          (delq nil
                (mapcar
                 (lambda (marker)
                   (let ((count (cl-count (car marker) pending
                                          :key #'ai-code-session-attention--state)))
                     (when (> count 0)
                       (concat (ai-code-session-attention--marker (car marker))
                               (number-to-string count)))))
                 ai-code-session-attention--markers))))
    (if (null counts)
        ""
      (propertize (concat " AI " (string-join counts " "))
                  'help-echo
                  (concat (mapconcat #'ai-code-session-attention--describe
                                     pending "\n")
                          "\nmouse-1: jump to next, mouse-3: choose")
                  'mouse-face 'mode-line-highlight
                  'local-map ai-code-session-attention-mode-line-map))))

(defconst ai-code-session-attention--mode-line-construct
  '(:eval (ai-code-session-attention-mode-line-string))
  "Mode-line construct added to `global-mode-string'.")

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
  (if-let* ((ai-code-session-attention-tab-bar-badge)
            (session (cl-find-if
                      (lambda (session)
                        (ai-code-session-attention--tab-has-buffer-p
                         tab (ai-code-session-buffer session)))
                      (ai-code-session-attention--pending))))
      (concat (ai-code-session-attention--marker
               (ai-code-session-attention--state session))
              " " name)
    name))

;;; Jumping

(defun ai-code-session-attention--current-tab-index ()
  "Return the index of the selected frame's current tab."
  (cl-position 'current-tab (funcall tab-bar-tabs-function) :key #'car))

(defun ai-code-session-attention--remember-origin ()
  "Remember the selected frame, tab, window and its buffer."
  (setq ai-code-session-attention--origin
        (list :frame (selected-frame)
              :tab (ai-code-session-attention--current-tab-index)
              :window (selected-window)
              :buffer (window-buffer))))

(defun ai-code-session-attention--return ()
  "Return to the frame, tab and window saved before the last jump."
  (let ((frame (plist-get ai-code-session-attention--origin :frame))
        (tab (plist-get ai-code-session-attention--origin :tab))
        (window (plist-get ai-code-session-attention--origin :window))
        (buffer (plist-get ai-code-session-attention--origin :buffer)))
    (unless (frame-live-p frame)
      (user-error "Nothing to return to"))
    (select-frame-set-input-focus frame)
    (when (and tab (< tab (length (funcall tab-bar-tabs-function))))
      (tab-bar-select-tab (1+ tab)))
    (when (window-live-p window)
      (select-window window)
      ;; The jump may have reused this window for a hidden session.
      (when (buffer-live-p buffer)
        (switch-to-buffer buffer nil t)))))

(defun ai-code-session-attention--jump (buffer)
  "Select a window showing BUFFER, switching frame and tab when needed."
  (let ((tab (tab-bar-get-buffer-tab buffer t)))
    (when tab
      (select-frame-set-input-focus (alist-get 'frame tab))
      (unless (eq (car tab) 'current-tab)
        (tab-bar-select-tab (1+ (alist-get 'index tab)))))
    (if-let* ((window (get-buffer-window buffer)))
        (select-window window)
      (pop-to-buffer buffer))
    (ai-code-session-attention-clear buffer)))

;;;###autoload
(defun ai-code-session-attention-next (&optional return)
  "Jump to the most urgent AI session that needs attention.
Sessions waiting for input come first, then finished ones, oldest first.
With prefix argument RETURN, go back to where a run of jumps started."
  (interactive "P")
  (require 'tab-bar)
  (if return
      (ai-code-session-attention--return)
    (if-let* ((session (car (ai-code-session-attention--pending))))
        (progn
          ;; Keep the first origin while jumping through several sessions.
          (unless (eq last-command #'ai-code-session-attention-next)
            (ai-code-session-attention--remember-origin))
          (ai-code-session-attention--jump (ai-code-session-buffer session)))
      (message "No AI session needs attention"))))

(defun ai-code-session-attention-menu (event)
  "Pop up a menu at EVENT to choose an AI session needing attention."
  (interactive "e")
  (require 'tab-bar)
  (if-let* ((pending (ai-code-session-attention--pending)))
      (popup-menu
       (cons "AI sessions"
             (mapcar (lambda (session)
                       (vector (ai-code-session-attention--describe session)
                               (lambda ()
                                 (interactive)
                                 (ai-code-session-attention--remember-origin)
                                 (ai-code-session-attention--jump
                                  (ai-code-session-buffer session)))
                               t))
                     pending))
       event)
    (message "No AI session needs attention")))

;;; Mode

(defun ai-code-session-attention--with-summary (format)
  "Return mode-line FORMAT with the summary first."
  (let ((construct ai-code-session-attention--mode-line-construct))
    (if (and (consp format) (member construct format))
        format
      (cons construct (if (listp format) format (list format))))))

(defun ai-code-session-attention--without-summary (format)
  "Return mode-line FORMAT without the summary."
  (if (consp format)
      (delete ai-code-session-attention--mode-line-construct (copy-sequence format))
    format))

(defun ai-code-session-attention--add-to-local-mode-line ()
  "Put the summary first in the current buffer's own mode line, if any.
Buffers that hide their mode line (a nil format) are left alone."
  (when (and (local-variable-p 'mode-line-format) mode-line-format)
    (setq mode-line-format
          (ai-code-session-attention--with-summary mode-line-format))))

(defun ai-code-session-attention--remove-mode-line ()
  "Remove the mode-line summary from every position it may occupy."
  (setq global-mode-string
        (delete ai-code-session-attention--mode-line-construct global-mode-string))
  (setq-default mode-line-format
                (ai-code-session-attention--without-summary
                 (default-value 'mode-line-format)))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (local-variable-p 'mode-line-format)
        (setq mode-line-format
              (ai-code-session-attention--without-summary mode-line-format)))))
  (remove-hook 'after-change-major-mode-hook
               #'ai-code-session-attention--add-to-local-mode-line))

(defun ai-code-session-attention--add-mode-line ()
  "Add the mode-line summary at `ai-code-session-attention-mode-line-position'."
  (if (eq ai-code-session-attention-mode-line-position 'front)
      (progn
        (setq-default mode-line-format
                      (ai-code-session-attention--with-summary
                       (default-value 'mode-line-format)))
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (ai-code-session-attention--add-to-local-mode-line)))
        ;; Modes such as Info set their own mode line when they start.
        (add-hook 'after-change-major-mode-hook
                  #'ai-code-session-attention--add-to-local-mode-line))
    (add-to-list 'global-mode-string
                 ai-code-session-attention--mode-line-construct t)))

;;;###autoload
(define-minor-mode ai-code-session-attention-mode
  "Show AI sessions that finished or wait for input in the mode line.
Click the summary, or run `ai-code-session-attention-next', to jump to
the session in whichever frame or tab shows it."
  :global t
  :group 'ai-code-session-attention
  (ai-code-session-attention--remove-mode-line)
  (if ai-code-session-attention-mode
      (progn
        (ai-code-session-attention--add-mode-line)
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
