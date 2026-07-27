;;; ai-code-editor-viewport.el --- Edit AI CLI files in Emacs  -*- lexical-binding: t; -*-

;; Author: realazy
;; SPDX-License-Identifier: Apache-2.0

;; Keywords: tools, convenience

;;; Commentary:
;; Route external editor requests from native AI CLI terminal sessions into an
;; Emacs viewport.  The session environment points to a generated helper that
;; emits an authenticated control frame through its PTY.  Terminal adapters
;; either intercept the raw frame before rendering or dispatch it through a
;; buffer-local callback, then open the request in the originating session's
;; window.  The associated-buffer handoff follows the viewport interaction idea
;; from https://github.com/xenodium/agent-shell while preserving each file's
;; normal major mode for editing.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ai-code-editor-viewport-attachments)
(require 'ai-code-editor-viewport-transport)

;;;; Customization and state

(defcustom ai-code-editor-viewport-enabled t
  "Whether native AI CLI editor requests should open in Emacs."
  :type 'boolean
  :group 'ai-code)

(defcustom ai-code-editor-viewport-window-placement 'below
  "Where to display an editor viewport relative to its originating session.
Use `below' to prefer a viewport below the session.  When that is impossible,
try a viewport on the right, then on the left, and temporarily replace the
session window only as a last resort.  Use `replace' to always show the
viewport in the session window."
  :type '(choice (const :tag "Below, right/left, then replace" below)
                 (const :tag "Replace the session window" replace))
  :group 'ai-code)

(defcustom ai-code-editor-viewport-window-height 12
  "Target total height of an editor viewport displayed below its session.
Below placement is attempted only when the originating window is at least
twice this many lines high."
  :type 'integer
  :group 'ai-code)

(defcustom ai-code-editor-viewport-min-height 8
  "Minimum acceptable total height of a viewport displayed below.
If the resulting editor viewport is shorter than this many lines, restore the
window layout and try a side placement instead."
  :type 'integer
  :group 'ai-code)

(defcustom ai-code-editor-viewport-min-width 24
  "Minimum acceptable width of each window in a side-by-side layout.
If either the editor viewport or its originating session is narrower than this
many columns, restore the window layout and try the next placement instead."
  :type 'integer
  :group 'ai-code)

(defcustom ai-code-editor-viewport-submit-delay 0.2
  "Seconds to wait before submitting input after a viewport is saved.
This gives the terminal TUI time to return from its external editor and restore
the input view before AI Code sends the return key."
  :type 'number
  :group 'ai-code)

(defface ai-code-editor-viewport-source-hint-face
  '((t :inherit shadow :slant italic))
  "Face for the originating TUI's disabled-input message."
  :group 'ai-code)

(defface ai-code-editor-viewport-header-key-face
  '((t :inherit bold))
  "Face for keyboard shortcuts in the editor viewport header."
  :group 'ai-code)

(defvar-local ai-code-editor-viewport--outcome nil
  "Outcome of the active editor viewport: `finished' or `canceled'.")

(defvar-local ai-code-editor-viewport--previous-header-line nil
  "Header line shown before editor viewport mode was enabled.")

(defvar-local ai-code-editor-viewport--source-directory nil
  "Working directory of the CLI session associated with this viewport.")

(defvar-local ai-code-editor-viewport--source-buffer nil
  "CLI session buffer associated with this viewport.")

(defvar-local ai-code-editor-viewport--submit-function nil
  "Function that submits restored TUI input for this source buffer.")

(defvar-local ai-code-editor-viewport-source-cursor-function nil
  "Function returning the live terminal cursor position for this source.")

(defvar recentf-exclude)

(defun ai-code-editor-viewport-source-buffer (&optional viewport)
  "Return VIEWPORT's associated live CLI session buffer.
VIEWPORT defaults to the current buffer."
  (let ((buffer (or viewport (current-buffer))))
    (when (buffer-live-p buffer)
      (let ((source
             (buffer-local-value
              'ai-code-editor-viewport--source-buffer buffer)))
        (and (buffer-live-p source) source)))))

(defun ai-code-editor-viewport-source-directory (&optional viewport)
  "Return VIEWPORT's associated CLI session directory.
VIEWPORT defaults to the current buffer."
  (let ((buffer (or viewport (current-buffer))))
    (when (buffer-live-p buffer)
      (buffer-local-value
       'ai-code-editor-viewport--source-directory buffer))))

(defun ai-code-editor-viewport-for-session (session)
  "Return the editor viewport associated with live buffer SESSION."
  (when (buffer-live-p session)
    (seq-find
     (lambda (buffer)
       (and (buffer-local-value 'ai-code-editor-viewport-mode buffer)
            (eq (ai-code-editor-viewport-source-buffer buffer) session)))
     (buffer-list))))

;;;; Viewport mode

(defvar ai-code-editor-viewport-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ai-code-editor-viewport-finish)
    (define-key map (kbd "C-c C-k") #'ai-code-editor-viewport-cancel)
    (define-key map (kbd "C-g") #'ai-code-editor-viewport-cancel)
    (define-key map [remap abort-recursive-edit]
                #'ai-code-editor-viewport-cancel)
    (define-key map [remap exit-recursive-edit]
                #'ai-code-editor-viewport-cancel)
    (define-key map [remap keyboard-escape-quit]
                #'ai-code-editor-viewport-cancel)
    (define-key map [remap yank] #'ai-code-editor-viewport-yank)
    (define-key map [remap clipboard-yank] #'ai-code-editor-viewport-yank)
    map)
  "Keymap for `ai-code-editor-viewport-mode'.")

(defconst ai-code-editor-viewport--header-hints
  '((ai-code-editor-viewport-finish "submit")
    (ai-code-editor-viewport-cancel "cancel" :all-bindings t)
    (ai-code-editor-viewport-yank "paste text, files, or images"))
  "Commands and descriptions shown in an editor viewport header.")

(defun ai-code-editor-viewport--header-command-keys
    (command all-bindings)
  "Return keys for COMMAND in the viewport map.
Join every direct binding when ALL-BINDINGS is non-nil; otherwise return the
first binding, including bindings supplied through command remapping."
  (if all-bindings
      (when-let* ((bindings
                   (seq-remove
                    (lambda (binding)
                      (and (vectorp binding)
                           (> (length binding) 0)
                           (eq (aref binding 0) 'remap)))
                    (where-is-internal
                     command ai-code-editor-viewport-mode-map
                     nil nil t))))
        (mapconcat #'key-description bindings "/"))
    (when-let* ((binding
                 (where-is-internal
                  command ai-code-editor-viewport-mode-map t)))
      (key-description binding))))

(defun ai-code-editor-viewport--source-hint-message ()
  "Return a source-input hint derived from the current viewport bindings."
  (let ((finish-keys
         (ai-code-editor-viewport--header-command-keys
          #'ai-code-editor-viewport-finish nil))
        (cancel-keys
         (ai-code-editor-viewport--header-command-keys
          #'ai-code-editor-viewport-cancel t)))
    (string-join
     (delq nil
           (list (and finish-keys (format "%s: submit" finish-keys))
                 (and cancel-keys (format "%s: cancel" cancel-keys))))
     ", ")))

(defun ai-code-editor-viewport--header-hint (hint)
  "Return a styled header chunk for HINT, or nil when it is unbound."
  (pcase-let ((`(,command ,description . ,properties) hint))
    (when-let* ((keys
                 (ai-code-editor-viewport--header-command-keys
                  command (plist-get properties :all-bindings))))
      (concat
       (propertize keys 'face 'ai-code-editor-viewport-header-key-face)
       ": " description))))

(defun ai-code-editor-viewport--header-line ()
  "Return the styled header line for an editor viewport."
  (concat
   " "
   (mapconcat #'identity
              (delq nil
                    (mapcar #'ai-code-editor-viewport--header-hint
                            ai-code-editor-viewport--header-hints))
              "  ")
   " "))

(define-minor-mode ai-code-editor-viewport-mode
  "Edit a file requested by an AI CLI in a dedicated viewport.

\\{ai-code-editor-viewport-mode-map}"
  :lighter " AI Edit"
  :keymap ai-code-editor-viewport-mode-map
  (if ai-code-editor-viewport-mode
      (progn
        (setq ai-code-editor-viewport--previous-header-line header-line-format)
        (ai-code-editor-viewport-attachments-enable)
        (setq header-line-format
              (ai-code-editor-viewport--header-line)))
    (setq header-line-format ai-code-editor-viewport--previous-header-line)
    (setq ai-code-editor-viewport--previous-header-line nil)
    (ai-code-editor-viewport-attachments-disable)))

(defun ai-code-editor-viewport--source-cursor-position ()
  "Return the live cursor position for the current terminal source buffer."
  (let ((candidate
         (and (functionp ai-code-editor-viewport-source-cursor-function)
              (ignore-errors
                (funcall ai-code-editor-viewport-source-cursor-function)))))
    (if (and (or (integerp candidate)
                 (and (markerp candidate)
                      (eq (marker-buffer candidate) (current-buffer))))
             (<= (point-min) candidate (point-max)))
        candidate
      (point))))

(defun ai-code-editor-viewport--draft-match-column
    (draft cursor-offset source-line source-column)
  "Locate DRAFT's cursor line in SOURCE-LINE.
CURSOR-OFFSET is the offset in DRAFT and SOURCE-COLUMN is its terminal
column.  Return the source column where the draft line begins."
  (when (and (stringp draft)
             (integerp cursor-offset)
             (<= 0 cursor-offset (length draft)))
    (let* ((prefix (substring draft 0 cursor-offset))
           (draft-line-start (or (string-match "[^\n]*\\'" prefix) 0))
           (draft-line-end (or (string-match "\n" draft cursor-offset)
                               (length draft)))
           (draft-line (substring draft draft-line-start draft-line-end))
           (draft-column (- cursor-offset draft-line-start))
           (match-column (- source-column draft-column)))
      (when (and (<= 0 match-column)
                 (<= (+ match-column (length draft-line))
                     (length source-line))
                 (string= draft-line
                          (substring source-line
                                     match-column
                                     (+ match-column
                                        (length draft-line)))))
        match-column))))

(defun ai-code-editor-viewport--disable-source-input
    (source-buffer &optional draft cursor-offset placement)
  "Visually disable the current input line in SOURCE-BUFFER.
When DRAFT and CURSOR-OFFSET are available, use them to find the input
boundary without assuming a particular TUI prompt format.  PLACEMENT is the
actual viewport placement used for this edit.
Return the temporary overlay, or nil when SOURCE-BUFFER is no longer live."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (save-excursion
        (goto-char (ai-code-editor-viewport--source-cursor-position))
        (let* ((line-start (line-beginning-position))
               (line-end (line-end-position))
               (source-position (point))
               (source-line
                (buffer-substring-no-properties line-start line-end))
               (match-column
                (ai-code-editor-viewport--draft-match-column
                 draft cursor-offset source-line
                 (- source-position line-start)))
               (property-start
                (or (previous-single-property-change
                     (min line-end (1+ source-position))
                     'face nil line-start)
                    (previous-single-property-change
                     (min line-end (1+ source-position))
                     'font-lock-face nil line-start)))
               (content-start
                (save-excursion
                  (goto-char
                   (cond
                    (match-column (+ line-start match-column))
                    ((and property-start (> property-start line-start))
                     property-start)
                    (t source-position)))
                  (skip-chars-backward " \t" line-start)
                  (point)))
               (source-window (get-buffer-window source-buffer t))
               (visible-width
                (when (window-live-p source-window)
                  (max
                   0
                   (- (window-body-width source-window)
                      (string-width
                       (buffer-substring-no-properties
                        line-start content-start))))))
               (source-width
                (max
                 (or visible-width 0)
                 (string-width
                  (buffer-substring-no-properties content-start line-end))))
               (source-face
                (seq-some
                 (lambda (position)
                   (or (get-char-property position 'face)
                       (get-char-property position 'font-lock-face)))
                 (delete-dups
                  (delq nil
                        (list (and (> content-start line-start)
                                   (1- content-start))
                              (and (< line-start line-end) line-start)
                              (and (< content-start line-end)
                                   content-start))))))
               (message-face
                (cond
                 ((null source-face)
                  'ai-code-editor-viewport-source-hint-face)
                 ((and (listp source-face)
                       (not (keywordp (car source-face))))
                  (cons 'ai-code-editor-viewport-source-hint-face
                        source-face))
                 (t
                  (list 'ai-code-editor-viewport-source-hint-face
                        source-face))))
               (message-text
                (format
                 (pcase (or placement
                            ai-code-editor-viewport-window-placement)
                   ('replace " Editing in current window — %s")
                   ('side " Editing in viewport beside — %s")
                   (_ " Editing in viewport below — %s"))
                 (ai-code-editor-viewport--source-hint-message)))
               (message
                (propertize
                 (concat message-text
                         (make-string
                          (max 0 (- source-width
                                    (string-width message-text)))
                          ?\s))
                 'face message-face))
               (overlay
                (make-overlay content-start line-end source-buffer)))
          (if (= content-start line-end)
              (overlay-put overlay 'after-string message)
            (overlay-put overlay 'display message))
          (overlay-put overlay 'priority 1000)
          (overlay-put overlay 'help-echo
                       "Input is disabled while the editor viewport is active")
          (overlay-put overlay 'ai-code-editor-viewport-source-input t)
          overlay)))))

;;;; Editing lifecycle

(defun ai-code-editor-viewport-finish ()
  "Finish the current AI CLI editor viewport and save its file."
  (interactive)
  (unless ai-code-editor-viewport-mode
    (user-error "Not in an AI CLI editor viewport"))
  (setq ai-code-editor-viewport--outcome 'finished)
  (exit-recursive-edit))

(defun ai-code-editor-viewport--discard-confirmed-p ()
  "Return non-nil when the current viewport may be discarded."
  (condition-case nil
      (or (save-restriction
            (widen)
            (string-blank-p
             (buffer-substring-no-properties (point-min) (point-max))))
          (y-or-n-p "Discard viewport contents and cancel? "))
    (quit nil)))

(defun ai-code-editor-viewport-cancel ()
  "Cancel the current AI CLI editor viewport without saving changes.
Prompt before discarding content unless the viewport contains only whitespace."
  (interactive)
  (unless ai-code-editor-viewport-mode
    (user-error "Not in an AI CLI editor viewport"))
  (when (ai-code-editor-viewport--discard-confirmed-p)
    (setq ai-code-editor-viewport--outcome 'canceled)
    (exit-recursive-edit)))

(defun ai-code-editor-viewport--replace-window (buffer window)
  "Display BUFFER by replacing WINDOW and return its display state."
  (let ((previous-buffer (window-buffer window)))
    (set-window-buffer window buffer)
    (select-window window)
    (list :window window
          :previous-buffer previous-buffer
          :created-window nil
          :placement 'replace)))

(defun ai-code-editor-viewport--run-display-action
    (buffer anchor-window action &optional alist)
  "Run ACTION for BUFFER from ANCHOR-WINDOW using only ALIST.
Use a fully local `display-buffer' action chain so external actions and alists
cannot alter the placement or use another frame.  Return the resulting window,
or nil when ACTION cannot display BUFFER."
  (with-selected-window anchor-window
    (let ((display-buffer-alist nil)
          (display-buffer-base-action nil)
          (display-buffer-fallback-action nil)
          (display-buffer-overriding-action
           (cons
            (list action #'display-buffer-no-window)
            (append alist
                    '((allow-no-window . t)
                      (inhibit-same-window . t))))))
      (display-buffer buffer nil (window-frame anchor-window)))))

(defun ai-code-editor-viewport--split-normal-window
    (buffer anchor-window size direction)
  "Split ANCHOR-WINDOW by SIZE in DIRECTION and display BUFFER normally."
  (let* ((ignore-window-parameters t)
         (window (split-window anchor-window size direction)))
    (set-window-buffer window buffer)
    window))

(defun ai-code-editor-viewport--display-below-action (buffer anchor-window)
  "Display BUFFER below ANCHOR-WINDOW and return the resulting window.
Split lateral side windows directly so the new viewport is a normal window,
not another same-side sibling that layout transposition can detach."
  (if (memq (window-parameter anchor-window 'window-side) '(left right))
      (ai-code-editor-viewport--split-normal-window
       buffer anchor-window (- ai-code-editor-viewport-window-height) 'below)
    (ai-code-editor-viewport--run-display-action
     buffer anchor-window #'display-buffer-below-selected
     `((window-height . ,ai-code-editor-viewport-window-height)
       (window-min-height . ,ai-code-editor-viewport-min-height)))))

(defun ai-code-editor-viewport--display-side-action
    (direction buffer anchor-window)
  "Display BUFFER from ANCHOR-WINDOW in DIRECTION and return its window.
Use a normal directional window rather than another window with the same
`window-side', since layout transposition can detach same-side siblings from
their required common parent.  Let Emacs split available space evenly instead
of forcing the viewport to its minimum acceptable width."
  (let ((anchor-side (window-parameter anchor-window 'window-side)))
    (if (memq anchor-side '(top bottom left right))
        (ai-code-editor-viewport--split-normal-window
         buffer anchor-window nil direction)
      (ai-code-editor-viewport--run-display-action
       buffer anchor-window #'display-buffer-in-direction
       `((direction . ,direction)
         (window-min-width . ,ai-code-editor-viewport-min-width))))))

(defun ai-code-editor-viewport--window-below-p (window anchor-window)
  "Return non-nil when WINDOW fits below ANCHOR-WINDOW."
  (and (>= (nth 1 (window-edges window))
           (nth 3 (window-edges anchor-window)))
       (>= (window-total-height window)
           ai-code-editor-viewport-min-height)
       (>= (window-total-height anchor-window)
           ai-code-editor-viewport-min-height)))

(defun ai-code-editor-viewport--side-windows-usable-p (window anchor-window)
  "Return non-nil when WINDOW and ANCHOR-WINDOW are both wide enough."
  (and (>= (window-total-width window)
           ai-code-editor-viewport-min-width)
       (>= (window-total-width anchor-window)
           ai-code-editor-viewport-min-width)))

(defun ai-code-editor-viewport--window-right-p (window anchor-window)
  "Return non-nil when WINDOW fits right of a usable ANCHOR-WINDOW."
  (and (>= (nth 0 (window-edges window))
           (nth 2 (window-edges anchor-window)))
       (ai-code-editor-viewport--side-windows-usable-p
        window anchor-window)))

(defun ai-code-editor-viewport--window-left-p (window anchor-window)
  "Return non-nil when WINDOW fits left of a usable ANCHOR-WINDOW."
  (and (<= (nth 2 (window-edges window))
           (nth 0 (window-edges anchor-window)))
       (ai-code-editor-viewport--side-windows-usable-p
        window anchor-window)))

(defun ai-code-editor-viewport--try-display
    (buffer anchor-window action placement geometry-predicate)
  "Try displaying BUFFER from ANCHOR-WINDOW with ACTION.
PLACEMENT identifies the attempted placement.  GEOMETRY-PREDICATE must accept
the resulting window and ANCHOR-WINDOW and confirm the requested geometry.
Return display state on success, or nil after undoing a failed attempt."
  (let* ((window-configuration
          (current-window-configuration (window-frame anchor-window)))
         (windows-before
          (mapcar
           (lambda (window)
             (cons window (window-buffer window)))
           (window-list (window-frame anchor-window) 'nomini)))
         viewport-window)
    (condition-case nil
        (setq viewport-window (funcall action buffer anchor-window))
      (error
       (set-window-configuration window-configuration)))
    (let ((previous (and (window-live-p viewport-window)
                         (assq viewport-window windows-before))))
      (if (and (window-live-p viewport-window)
               (not (eq viewport-window anchor-window))
               (eq (window-frame viewport-window)
                   (window-frame anchor-window))
               (eq (window-buffer viewport-window) buffer)
               (or (not (eq placement 'side))
                   (not previous))
               (or (not (eq placement 'side))
                   (<= (abs (- (window-total-width viewport-window)
                               (window-total-width anchor-window)))
                       1))
               (funcall geometry-predicate viewport-window anchor-window))
          (progn
            (select-window viewport-window)
            (list :window viewport-window
                  :previous-buffer (cdr previous)
                  :created-window (not previous)
                  :placement placement))
        (set-window-configuration window-configuration)
        nil))))

(defun ai-code-editor-viewport--display-with-fallbacks (buffer anchor-window)
  "Display BUFFER from ANCHOR-WINDOW using the default fallback chain."
  (or (and (>= (window-total-height anchor-window)
               (* 2 ai-code-editor-viewport-window-height))
           (ai-code-editor-viewport--try-display
            buffer anchor-window
            #'ai-code-editor-viewport--display-below-action
            'below #'ai-code-editor-viewport--window-below-p))
      (ai-code-editor-viewport--try-display
       buffer anchor-window
       (apply-partially
        #'ai-code-editor-viewport--display-side-action 'right)
       'side #'ai-code-editor-viewport--window-right-p)
      (ai-code-editor-viewport--try-display
       buffer anchor-window
       (apply-partially
        #'ai-code-editor-viewport--display-side-action 'left)
       'side #'ai-code-editor-viewport--window-left-p)
      (ai-code-editor-viewport--replace-window buffer anchor-window)))

(defun ai-code-editor-viewport--display
    (buffer source-buffer &optional origin-frame)
  "Display BUFFER as a viewport associated with SOURCE-BUFFER.
ORIGIN-FRAME is the frame selected when the editor request was dispatched.
Return a plist that records the window and its previous buffer."
  (let* ((origin-frame (if (frame-live-p origin-frame)
                           origin-frame
                         (selected-frame)))
         (source-window (and (buffer-live-p source-buffer)
                             (get-buffer-window source-buffer origin-frame)))
         (anchor-window (if (window-live-p source-window)
                            source-window
                          (frame-selected-window origin-frame))))
    (pcase ai-code-editor-viewport-window-placement
      ('below
       (ai-code-editor-viewport--display-with-fallbacks
        buffer anchor-window))
      ('replace
       (ai-code-editor-viewport--replace-window buffer anchor-window))
      (_
       (user-error "Unknown AI Code viewport placement: %S"
                   ai-code-editor-viewport-window-placement)))))

(defun ai-code-editor-viewport--restore-window (display-state buffer)
  "Restore DISPLAY-STATE after editing BUFFER."
  (let ((window (plist-get display-state :window))
        (previous-buffer (plist-get display-state :previous-buffer))
        (created-window (plist-get display-state :created-window)))
    (when (and (window-live-p window)
               (eq (window-buffer window) buffer))
      (cond
       (created-window
        ;; Delete the exact viewport even after layout transposition moves it
        ;; beside a side window, where `quit-window' may only change its buffer.
        (let ((ignore-window-parameters t))
          (if (eq (window-deletable-p window) t)
              (delete-window window)
            (quit-window nil window))))
       ((buffer-live-p previous-buffer)
        (set-window-buffer window previous-buffer))
       (t
        (quit-window nil window))))))

(defun ai-code-editor-viewport--buffer-text (buffer)
  "Return BUFFER's accessible text without properties."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun ai-code-editor-viewport--source-session-name (source-buffer)
  "Return the user-facing session name for SOURCE-BUFFER."
  (if (buffer-live-p source-buffer)
      (let ((name (buffer-name source-buffer)))
        (if (and (string-prefix-p "*" name)
                 (string-suffix-p "*" name))
            (substring name 1 -1)
          name))
    "CLI"))

(defun ai-code-editor-viewport--source-lines-match-draft-p
    (source-start draft-lines)
  "Return non-nil when DRAFT-LINES appear at SOURCE-START in this buffer."
  (save-excursion
    (goto-char source-start)
    (let ((remaining draft-lines)
          (matched t))
      (while (and matched remaining)
        (let* ((draft-line (pop remaining))
               (source-line
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))))
          (setq matched
                (if (string-empty-p draft-line)
                    (string-blank-p source-line)
                  (and (string-search draft-line source-line) t))))
        (when (and matched remaining)
          (let ((previous-line-start (line-beginning-position)))
            (forward-line 1)
            (when (= (line-beginning-position) previous-line-start)
              (setq matched nil)))))
      matched)))

(defun ai-code-editor-viewport--draft-cursor-offset-at-source
    (draft cursor-position)
  "Map CURSOR-POSITION in this source buffer to an offset within DRAFT."
  (save-excursion
    (goto-char cursor-position)
    (let* ((source-line-start (line-beginning-position))
           (source-line
            (buffer-substring-no-properties
             source-line-start (line-end-position)))
           (cursor-column (- cursor-position source-line-start))
           (draft-lines (split-string draft "\n" nil))
           (draft-offset 0)
           candidates)
      (cl-loop
       for draft-line in draft-lines
       for draft-index from 0
       do
       (when-let* ((draft-start
                    (if (string-empty-p draft-line)
                        (and (string-blank-p source-line)
                             cursor-column)
                      (string-search draft-line source-line))))
         (when (<= draft-start cursor-column
                   (+ draft-start (length draft-line)))
           (let ((source-start
                  (save-excursion
                    (goto-char source-line-start)
                    (when (zerop (forward-line (- draft-index)))
                      (point)))))
             (when (and source-start
                        (ai-code-editor-viewport--source-lines-match-draft-p
                         source-start draft-lines))
               (push (+ draft-offset (- cursor-column draft-start))
                     candidates)))))
       (setq draft-offset (+ draft-offset (length draft-line) 1)))
      (when (= (length candidates) 1)
        (car candidates)))))

(defun ai-code-editor-viewport--source-draft-cursor-offset
    (source-buffer draft)
  "Return SOURCE-BUFFER's cursor offset within DRAFT, when inferable."
  (when (and (buffer-live-p source-buffer)
             (not (string-empty-p draft)))
    (let ((visible-draft (string-remove-suffix "\n" draft)))
      (unless (string-empty-p visible-draft)
        (with-current-buffer source-buffer
          (let ((cursor-position
                 (ai-code-editor-viewport--source-cursor-position)))
            (when (and (integer-or-marker-p cursor-position)
                       (<= (point-min) cursor-position (point-max)))
              (ai-code-editor-viewport--draft-cursor-offset-at-source
               visible-draft cursor-position))))))))

(defmacro ai-code-editor-viewport--without-recentf-file (file &rest body)
  "Run BODY without allowing `recentf' to record FILE."
  (declare (indent 1) (debug (form body)))
  (let ((excluded-file (make-symbol "excluded-file"))
        (candidate (make-symbol "candidate")))
    `(let* ((,excluded-file ,file)
            (recentf-exclude
             (cons
              (lambda (,candidate)
                (file-equal-p ,candidate ,excluded-file))
              (and (boundp 'recentf-exclude) recentf-exclude))))
       ,@body)))

(defun ai-code-editor-viewport--make-buffer
    (file source-buffer &optional staging-file-p)
  "Return per-request editing state for FILE and SOURCE-BUFFER.
When STAGING-FILE-P is non-nil, keep FILE out of `recentf'."
  (let* ((existing-base-buffer (find-buffer-visiting file))
         (base-buffer
          (or existing-base-buffer
              (if staging-file-p
                  (ai-code-editor-viewport--without-recentf-file file
                    (find-file-noselect file))
                (find-file-noselect file))))
         (owned-base-buffer (unless existing-base-buffer base-buffer))
         (snapshot (ai-code-editor-viewport--buffer-text base-buffer))
         (name (generate-new-buffer-name
                (format "Edit: %s"
                        (ai-code-editor-viewport--source-session-name
                         source-buffer))))
         (viewport-buffer
          (with-current-buffer base-buffer
            (let ((buffer-file-name nil)
                  (buffer-file-truename nil)
                  (buffer-auto-save-file-name nil))
              (clone-buffer name nil)))))
    (with-current-buffer viewport-buffer
      (setq ai-code-editor-viewport--source-buffer source-buffer
            ai-code-editor-viewport--source-directory
            (if (buffer-live-p source-buffer)
                (buffer-local-value 'default-directory source-buffer)
              default-directory))
      (set-buffer-modified-p nil))
    (list :base-buffer base-buffer
          :owned-base-buffer owned-base-buffer
          :viewport-buffer viewport-buffer
          :snapshot snapshot)))

(defun ai-code-editor-viewport--commit-buffer
    (base-buffer viewport-buffer snapshot &optional staging-file-p)
  "Save VIEWPORT-BUFFER changes to BASE-BUFFER if it still matches SNAPSHOT.
When STAGING-FILE-P is non-nil, keep the saved file out of `recentf'."
  (unless (buffer-live-p base-buffer)
    (user-error "The file buffer was closed while its viewport was active"))
  (unless (equal snapshot
                 (ai-code-editor-viewport--buffer-text base-buffer))
    (user-error "The file changed while its viewport was active"))
  (let ((contents
         (ai-code-editor-viewport-attachments-serialize-buffer
          viewport-buffer))
        change-group)
    (with-current-buffer base-buffer
      (setq change-group (prepare-change-group))
      (unwind-protect
          (progn
            (activate-change-group change-group)
            (save-restriction
              (widen)
              (erase-buffer)
              (insert contents))
            (if staging-file-p
                (ai-code-editor-viewport--without-recentf-file buffer-file-name
                  (save-buffer))
              (save-buffer))
            (accept-change-group change-group)
            (setq change-group nil))
        (when change-group
          (cancel-change-group change-group))))))

(defun ai-code-editor-viewport--edit-file
    (file source-buffer &optional line column origin-frame staging-file-p)
  "Edit FILE in a viewport associated with SOURCE-BUFFER.
LINE is one-based and COLUMN is zero-based, matching editor arguments.
ORIGIN-FRAME preserves the request's frame across deferred handling.  When
STAGING-FILE-P is non-nil, keep FILE out of `recentf'.  After cancellation,
refocus the original source window when it still displays SOURCE-BUFFER."
  (let* ((state (ai-code-editor-viewport--make-buffer
                 file source-buffer staging-file-p))
         (base-buffer (plist-get state :base-buffer))
         (owned-base-buffer (plist-get state :owned-base-buffer))
         (buffer (plist-get state :viewport-buffer))
         (snapshot (plist-get state :snapshot))
         (source-cursor-offset
          (ai-code-editor-viewport--source-draft-cursor-offset
           source-buffer snapshot))
         (source-window
          (and (buffer-live-p source-buffer)
               (get-buffer-window
                source-buffer
                (if (frame-live-p origin-frame)
                    origin-frame
                  (selected-frame)))))
         (display-state nil)
         (source-input-overlay nil)
         (finished nil))
    (unwind-protect
        (with-current-buffer buffer
          (setq ai-code-editor-viewport--outcome nil)
          (ai-code-editor-viewport-mode 1)
          (cond
           (line
            (goto-char (point-min))
            (forward-line (1- line))
            (when column
              (move-to-column column)))
           (source-cursor-offset
            (goto-char
             (min (point-max)
                  (+ (point-min) source-cursor-offset)))))
          (setq display-state
                (ai-code-editor-viewport--display
                 buffer source-buffer origin-frame))
          (unwind-protect
              (progn
                (setq source-input-overlay
                      (ai-code-editor-viewport--disable-source-input
                       source-buffer snapshot source-cursor-offset
                       (plist-get display-state :placement)))
                (while (not
                        (memq
                         (buffer-local-value
                          'ai-code-editor-viewport--outcome buffer)
                         '(finished canceled)))
                  (condition-case nil
                      (with-current-buffer buffer
                        (recursive-edit))
                    (quit nil))
                  (unless (memq
                           (buffer-local-value
                            'ai-code-editor-viewport--outcome buffer)
                           '(finished canceled))
                    (with-current-buffer buffer
                      (when (ai-code-editor-viewport--discard-confirmed-p)
                        (setq ai-code-editor-viewport--outcome
                              'canceled)))))
                (when (eq (buffer-local-value
                           'ai-code-editor-viewport--outcome buffer)
                          'finished)
                  (ai-code-editor-viewport--commit-buffer
                   base-buffer buffer snapshot staging-file-p)
                  (setq finished t)))
            (when (overlayp source-input-overlay)
              (delete-overlay source-input-overlay))
            (let ((canceled
                   (eq (buffer-local-value
                        'ai-code-editor-viewport--outcome buffer)
                       'canceled)))
              (ai-code-editor-viewport-mode -1)
              (ai-code-editor-viewport--restore-window
               display-state buffer)
              (when (and canceled
                         (window-live-p source-window)
                         (eq (window-buffer source-window) source-buffer))
                (select-window source-window)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (and (buffer-live-p owned-base-buffer)
                 (not (buffer-modified-p owned-base-buffer)))
        (kill-buffer owned-base-buffer)))
    finished))

(defun ai-code-editor-viewport--parse-file-arguments (directory arguments)
  "Parse editor ARGUMENTS relative to DIRECTORY.
Return plists containing :file, :line, and :column entries."
  (let ((accept-options t)
        (next-line nil)
        (next-column nil)
        files)
    (dolist (argument arguments)
      (cond
       ((and accept-options (string= argument "--"))
        (setq accept-options nil))
       ((and accept-options
             (string-match
              "\\`+\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\'"
              argument))
        (setq next-line (string-to-number (match-string 1 argument))
              next-column (and (match-string 2 argument)
                               (string-to-number (match-string 2 argument)))))
       ((and accept-options (string-prefix-p "-" argument)))
       (t
        (push (list :file (expand-file-name argument directory)
                    :line next-line
                    :column next-column)
              files)
        (setq next-line nil
              next-column nil))))
    (nreverse files)))

(defun ai-code-editor-viewport--edit-files
    (source-buffer directory file-arguments
                   &optional origin-frame staging-request-p)
  "Edit FILE-ARGUMENTS for SOURCE-BUFFER relative to DIRECTORY.
ORIGIN-FRAME preserves the request's frame across deferred handling.  When
STAGING-REQUEST-P is non-nil, keep only requested files inside the directory
named by variable `temporary-file-directory' out of `recentf'.
Return non-nil only when every file was saved and finished."
  (when-let* ((files
               (ai-code-editor-viewport--parse-file-arguments
                directory file-arguments)))
    (cl-every
     (lambda (file-request)
       (let* ((file (plist-get file-request :file))
              (staging-file-p
               (and staging-request-p
                    (file-in-directory-p
                     file temporary-file-directory))))
         (ai-code-editor-viewport--edit-file
          file source-buffer
          (plist-get file-request :line)
          (plist-get file-request :column)
          origin-frame staging-file-p)))
     files)))

;;;; Terminal request protocol

(defun ai-code-editor-viewport--decode-request (payload)
  "Decode terminal editor PAYLOAD into a request plist.
The plist records the response file, directory, submit and staging intent,
and editor arguments."
  (let* ((decoded
          (decode-coding-string
           (base64-decode-string payload)
           'utf-8
           t))
         (fields (split-string decoded "\0" nil)))
    (when (and fields (string-empty-p (car (last fields))))
      (setq fields (butlast fields)))
    (unless (>= (length fields) 4)
      (error "Malformed AI Code editor request"))
    (unless (member (nth 2 fields) '("0" "1"))
      (error "Invalid AI Code editor submit intent"))
    (let* ((versioned-request-p
            (equal (nth 3 fields)
                   ai-code-editor-viewport--request-version))
           (request-kind (and versioned-request-p (nth 4 fields))))
      (when (and versioned-request-p
                 (not (member request-kind '("regular" "staging"))))
        (error "Invalid AI Code editor request kind"))
      (list :status-file (nth 0 fields)
            :directory (nth 1 fields)
            :submit-p (string= (nth 2 fields) "1")
            :staging-request-p
            (if versioned-request-p
                (equal request-kind "staging")
              ;; Old helpers only distinguished general editor requests by
              ;; their submit intent; preserve staging hygiene for those.
              (string= (nth 2 fields) "1"))
            ;; Accept payloads from helpers generated before versioned request
            ;; kinds were added.  Newly generated helpers are explicit.
            :arguments (nthcdr (if versioned-request-p 5 3) fields)))))

(defun ai-code-editor-viewport--valid-status-file-p (file)
  "Return non-nil when FILE is a helper-created response file."
  (let ((status-directory
         (or ai-code-editor-viewport--helper-status-directory
             temporary-file-directory)))
    (and (stringp file)
         (file-name-absolute-p file)
         (file-regular-p file)
         (not (file-symlink-p file))
         (string-prefix-p "ai-code-editor-status-"
                          (file-name-nondirectory file))
         (file-in-directory-p
          (file-truename file)
          (file-name-as-directory
           (file-truename status-directory))))))

(defun ai-code-editor-viewport--write-status
    (file status &optional submit-token)
  "Write integer STATUS and optional SUBMIT-TOKEN to helper response FILE."
  (unless (ai-code-editor-viewport--valid-status-file-p file)
    (error "Unsafe AI Code editor response file: %s" file))
  (when (and submit-token
             (or (not (stringp submit-token))
                 (string-empty-p submit-token)
                 (string-match-p "[[:space:]]" submit-token)))
    (error "Invalid AI Code editor submit token"))
  (with-temp-file file
    (insert (number-to-string status))
    (when submit-token
      (insert " 1 " submit-token))
    (insert "\n")))

(defun ai-code-editor-viewport--requested-content-nonblank-p
    (directory arguments)
  "Return non-nil when editor ARGUMENTS name nonblank content in DIRECTORY."
  (seq-some
   (lambda (request)
     (condition-case nil
         (with-temp-buffer
           (insert-file-contents (plist-get request :file))
           (not (string-empty-p (string-trim (buffer-string)))))
       (file-error nil)))
   (ai-code-editor-viewport--parse-file-arguments directory arguments)))

(defun ai-code-editor-viewport--submit-source-buffer (source-buffer)
  "Submit restored TUI input in SOURCE-BUFFER, when it is still live."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (when (functionp ai-code-editor-viewport--submit-function)
        (condition-case err
            (funcall ai-code-editor-viewport--submit-function)
          (error
           (message "AI Code editor submit failed: %s"
                    (error-message-string err))))))))

(defun ai-code-editor-viewport--schedule-submit (source-buffer)
  "Schedule submission of restored TUI input in SOURCE-BUFFER."
  (when (and (buffer-live-p source-buffer)
             (buffer-local-value
              'ai-code-editor-viewport--submit-function source-buffer))
    (run-at-time (max 0 ai-code-editor-viewport-submit-delay)
                 nil
                 #'ai-code-editor-viewport--submit-source-buffer
                 source-buffer)))

(defun ai-code-editor-viewport--open-request
    (source-buffer payload &optional origin-frame)
  "Open terminal editor PAYLOAD for SOURCE-BUFFER in a viewport.
PAYLOAD contains a response file, working directory, submit intent, and
editor arguments.  ORIGIN-FRAME is the frame selected at dispatch time.
Return non-nil after saving every requested file."
  (let (status-file completed succeeded submit-p submit-token)
    (unwind-protect
        (condition-case err
            (let* ((request
                    (ai-code-editor-viewport--decode-request payload))
                   (request-directory (plist-get request :directory))
                   (directory
                    (or (and (not (string-empty-p request-directory))
                             request-directory)
                        default-directory))
                   (arguments (plist-get request :arguments)))
              (setq status-file (plist-get request :status-file))
              (setq succeeded
                    (ai-code-editor-viewport--edit-files
                     source-buffer directory arguments origin-frame
                     (plist-get request :staging-request-p)))
              (setq submit-p
                    (and (plist-get request :submit-p)
                         succeeded
                         (ai-code-editor-viewport--requested-content-nonblank-p
                          directory arguments)))
              (setq completed t))
          ((error quit)
           (message "AI Code editor viewport failed: %s"
                    (error-message-string err))))
      (when status-file
        (condition-case err
            (progn
              (when submit-p
                (setq submit-token
                      (ai-code-editor-viewport--prepare-submit-token
                       source-buffer)))
              (ai-code-editor-viewport--write-status
               status-file (if completed 0 1) submit-token))
          (error
           (when submit-token
             (ai-code-editor-viewport--discard-submit-token
              source-buffer submit-token))
           (message "AI Code editor response failed: %s"
                    (error-message-string err))))))
    succeeded))

(provide 'ai-code-editor-viewport)
;;; ai-code-editor-viewport.el ends here
