;;; ai-code-backends-infra-ghostel.el --- Ghostel support for AI Code terminals  -*- lexical-binding: t; -*-

;; Author: Kang Tu, realazy (cxa)
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Ghostel-specific support for `ai-code-backends-infra'.

;;; Code:

(require 'cl-lib)
(require 'ai-code-editor-viewport)
(require 'ai-code-ghostel-image-preview)
(require 'ai-code-session-link)

;; Prefer `ghostel-exec' for Ghostel backend startup when available, as
;; it simplifies process startup integration.

(defcustom ai-code-backends-infra-ghostel-kitty-graphics-mediums
  '(file temp-file)
  "Extra Kitty graphics image-loading mediums for AI Code Ghostel sessions.

Ghostel always supports direct inline image transmission.  Enabling `file'
and `temp-file' lets trusted local AI CLI tools display image files through
the Kitty graphics protocol as well.  `shared-mem' is intentionally not
enabled by default because it widens the terminal's local resource access."
  :type '(set (const :tag "Local file medium" file)
              (const :tag "Temp-file medium" temp-file)
              (const :tag "Shared-memory medium" shared-mem))
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
(declare-function ai-code-backends-infra--send-string-with-paste
                  "ai-code-backends-infra"
                  (string paste send-function paste-function paste-active-p
                          backend-name))
(declare-function ai-code-backends-infra--sync-terminal-cursor
                  "ai-code-backends-infra" ())
(declare-function ai-code-editor-viewport-filter-output
                  "ai-code-editor-viewport-transport" (process output))
(declare-function ai-code-session-update-metadata
                  "ai-code-session" (id-or-buffer metadata))
(declare-function ghostel-exec "ghostel" (buffer program &optional args))
(declare-function ghostel-send-key "ghostel" (key-name &optional mods))
(declare-function ghostel-send-string "ghostel" (string))
(declare-function ghostel-paste-string "ghostel" (string))
(declare-function ghostel--mode-enabled "ghostel-module" (term mode))
(declare-function ghostel-cursor-point "ghostel" ())
(declare-function ghostel--schedule-link-detection
                  "ghostel" (&optional begin end))
(declare-function ghostel--run-queued-plain-link-detection
                  "ghostel" (buffer))
(declare-function ghostel-ime-mode "ghostel-ime" (&optional arg))
(declare-function ai-code-session-link--recent-output-plain-text
                  "ai-code-session-link" (output))
(declare-function ai-code-session-link--image-preview-enabled-p
                  "ai-code-session-link" ())

(defvar ai-code-backends-infra--session-terminal-backend)
(defvar ai-code-backends-infra--session-directory)
(defvar ai-code-backends-infra--launch-program)
(defvar ai-code-session-link--path-base-regexp)
(defvar ai-code-session-link--url-pattern-regexp)
(defvar ai-code-session-link-inhibit-functions)
(defvar ghostel-eval-cmds)
(defvar ghostel-inhibit-redraw-functions)
(defvar ghostel-kitty-graphics-mediums)
(defvar ghostel-link-map)
(defvar ghostel--term)
(defvar ghostel-use-native-pty)
(eval-when-compile
  (defvar ghostel-command-finish-functions)
  (defvar ghostel-command-start-functions)
  (defvar ghostel-kill-buffer-on-exit)
  (defvar ghostel-progress-function)
  (defvar ghostel-set-title-function)
  (defvar ghostel--copy-mode-active)
  (defvar ghostel--fake-cursor-overlay)
  (defvar ghostel--cursor-char-pos)
  (defvar ghostel--input-mode)
  (defvar ghostel--plain-link-detection-begin)
  (defvar ghostel--plain-link-detection-end))

(defconst ai-code-backends-infra-ghostel--editor-command
  "ai-code-editor-viewport"
  "Ghostel OSC 52;e command used for viewport editor requests.")

(defvar-local ai-code-backends-infra-ghostel--progress-function nil
  "Original Ghostel progress function captured before AI Code wrapping.")

(defconst ai-code-backends-infra-ghostel--linkify-redraw-delay 0.05
  "Seconds to wait before relinkifying recent Ghostel output.")

(defcustom ai-code-backends-infra-ghostel-anti-flicker t
  "Enable short output batching for Ghostel TUI redraws.

Interactive AI CLIs often repaint status rows by clearing and rewriting the
same terminal line in several process-output chunks.  Batching redraw-like
chunks prevents users from seeing those incomplete intermediate frames."
  :type 'boolean
  :group 'ai-code-backends-infra)

(defcustom ai-code-backends-infra-ghostel-render-delay 0.05
  "Maximum seconds to batch queued Ghostel redraw output.

The timer starts with the first redraw chunk, so continuous output cannot
postpone rendering indefinitely.  Chunks arriving within this interval are
rendered together, avoiding flashes of intermediate clear frames behind inline
image previews."
  :type 'number
  :group 'ai-code-backends-infra)

(defcustom ai-code-backends-infra-ghostel-enable-ime-integration t
  "Enable Ghostel IME integration in AI Code Ghostel sessions.

When available, `ghostel-ime-mode' defers Ghostel redraws while an
Emacs Lisp input-method composition is active, preventing active TUI
redraws from clobbering in-progress preedit text."
  :type 'boolean
  :group 'ai-code-backends-infra)

(defcustom ai-code-backends-infra-ghostel-inhibit-redraw-during-native-preedit t
  "Defer Ghostel redraws while a GUI-native preedit overlay is active.

This covers native input-method preedit paths that do not go through
`ghostel-ime-mode', such as GUI framework preedit overlays."
  :type 'boolean
  :group 'ai-code-backends-infra)

(defcustom ai-code-backends-infra-ghostel-inhibit-redraw-after-input nil
  "Defer Ghostel redraws briefly after user input in AI Code sessions.

Some GUI-native preedit underlines are not visible to Lisp as overlays or
text properties.  Deferring redraws during the short window after input keeps
animated status rows from repeatedly repainting the buffer over preedit text.
This is disabled by default because it can make interactive echo feel laggy."
  :type 'boolean
  :group 'ai-code-backends-infra)

(defcustom ai-code-backends-infra-ghostel-input-redraw-inhibit-delay 0.8
  "Seconds to keep Ghostel redraws deferred after recent user input."
  :type 'number
  :group 'ai-code-backends-infra)

(defcustom ai-code-backends-infra-ghostel-native-preedit-overlay-symbols
  '(x-preedit-overlay pgtk-preedit-overlay ns-preedit-overlay
                      mac-preedit-overlay)
  "Overlay variables that may hold an active GUI-native preedit overlay."
  :type '(repeat symbol)
  :group 'ai-code-backends-infra)

(defconst ai-code-backends-infra-ghostel--preedit-properties
  '(preedit preedit-text input-method)
  "Text and overlay properties that identify in-progress preedit text.")

(defconst ai-code-backends-infra-ghostel--redraw-regexp
  "\033\\[[0-9;?]*[A-GJKMH]"
  "Regexp matching ANSI terminal redraw, clear, or cursor movement sequences.")

(defvar-local ai-code-backends-infra-ghostel--render-queue nil
  "Queued Ghostel output waiting for anti-flicker rendering.")

(defvar-local ai-code-backends-infra-ghostel--render-timer nil
  "Timer used to flush `ai-code-backends-infra-ghostel--render-queue'.")

(defvar-local ai-code-backends-infra-ghostel--dim-foreground-active nil
  "Non-nil when AI Code injected an ANSI gray foreground for SGR dim text.")

(defvar-local ai-code-backends-infra-ghostel--foreground-state nil
  "Current foreground state tracked for AI Code Ghostel SGR normalization.
The value is nil for the default foreground, `explicit' for a foreground
set by the subprocess, and `injected' for AI Code's ANSI gray foreground.")

(defvar-local ai-code-backends-infra-ghostel--last-input-activity-time nil
  "Float timestamp of the most recent user input in this Ghostel session.")

(defconst ai-code-backends-infra-ghostel--process-filter-wrapped-property
  'ai-code-backends-infra-ghostel-process-filter-wrapped
  "Process property marking AI Code's Ghostel output filter wrapper.")

(defconst ai-code-backends-infra-ghostel--preserved-link-properties
  '(help-echo mouse-face keymap follow-link font-lock-face face
    ghostel-link-id ai-code-session-link ai-code-session-symbol-link
    ai-code-session-symbol-file ai-code-session-hover-link)
  "Text properties to preserve across Ghostel redraws.")

(defconst ai-code-backends-infra-ghostel--session-link-candidate-regexp
  (concat "\\(?:"
          ai-code-session-link--url-pattern-regexp
          "\\|"
          ai-code-session-link--path-base-regexp
          "\\(?:[#:(][[:alnum:],L-]+\\)?"
          "\\)")
  "Regexp matching URL and path candidates for Ghostel redraw linkify.")

(defvar-local ai-code-backends-infra-ghostel--preserved-link-spans nil
  "Cached clickable link spans restored after Ghostel redraws.")

(defun ai-code-backends-infra-ghostel-ensure-backend ()
  "Ensure the Ghostel backend is available."
  (unless (featurep 'ghostel) (require 'ghostel nil t))
  (unless (featurep 'ghostel)
    (user-error "The package ghostel is not installed")))

(defun ai-code-backends-infra-ghostel-navigation-mode-p ()
  "Return non-nil when the current Ghostel buffer is in copy mode."
  (or (bound-and-true-p ghostel--copy-mode-active)
      (eq ghostel--input-mode 'copy)))

(defun ai-code-backends-infra-ghostel-install-navigation-cursor-sync ()
  "Install cursor synchronization for Ghostel navigation mode."
  (add-hook 'post-command-hook
            #'ai-code-backends-infra--sync-terminal-cursor nil t))

(defun ai-code-backends-infra-ghostel--ai-session-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is an AI Code Ghostel session buffer."
  (when (buffer-live-p (or buffer (current-buffer)))
    (with-current-buffer (or buffer (current-buffer))
      (and (boundp 'ai-code-backends-infra--session-terminal-backend)
           (eq ai-code-backends-infra--session-terminal-backend 'ghostel)))))

(defun ai-code-backends-infra-ghostel--native-editor-transport-p ()
  "Return non-nil when Ghostel supports native editor request callbacks."
  (and (boundp 'ghostel-eval-cmds)
       (fboundp 'ghostel--osc52-eval)))

(defun ai-code-backends-infra-ghostel--editor-frame-prefix ()
  "Return Ghostel's authenticated OSC 52;e editor frame prefix."
  (concat "\e]52;e;"
          ai-code-backends-infra-ghostel--editor-command
          " "
          (ai-code-editor-viewport-frame-token)
          " "))

(defun ai-code-backends-infra-ghostel--handle-editor-request (token payload)
  "Forward Ghostel editor TOKEN and PAYLOAD to the current viewport source."
  (ai-code-editor-viewport-handle-request
   (current-buffer) token payload))

(defun ai-code-backends-infra-ghostel--install-editor-transport ()
  "Whitelist the token-checked viewport command in the current buffer."
  (when (and ai-code-editor-viewport-enabled
             (ai-code-backends-infra-ghostel--native-editor-transport-p))
    (let* ((entry
            (list ai-code-backends-infra-ghostel--editor-command
                  #'ai-code-backends-infra-ghostel--handle-editor-request))
           (commands
            (cons
             entry
             (cl-remove
              ai-code-backends-infra-ghostel--editor-command
              ghostel-eval-cmds
              :key #'car
              :test #'string=))))
      (unless (equal ghostel-eval-cmds commands)
        (setq-local ghostel-eval-cmds commands)))))

(defun ai-code-backends-infra-ghostel--update-session-metadata (buffer metadata)
  "Merge METADATA into the AI Code session associated with BUFFER."
  (when (and (buffer-live-p buffer)
             (fboundp 'ai-code-session-update-metadata)
             (ai-code-backends-infra-ghostel--ai-session-buffer-p buffer))
    (with-current-buffer buffer
      (ai-code-session-update-metadata buffer metadata))))

(defun ai-code-backends-infra-ghostel--command-start (buffer)
  "Record a Ghostel OSC 133 command-start event for BUFFER."
  (ai-code-backends-infra-ghostel--update-session-metadata
   buffer
   (list :ghostel-command-state 'started
         :ghostel-command-started-at (float-time)
         :ghostel-command-finished-at nil
         :ghostel-command-exit-status nil
         :ghostel-status 'running)))

(defun ai-code-backends-infra-ghostel--command-finish (buffer exit-status)
  "Record a Ghostel OSC 133 command-finish event for BUFFER.
EXIT-STATUS is the status reported by Ghostel, or nil when unavailable."
  (ai-code-backends-infra-ghostel--update-session-metadata
   buffer
   (list :ghostel-command-state 'finished
         :ghostel-command-finished-at (float-time)
         :ghostel-command-exit-status exit-status
         :ghostel-status (if (and exit-status (/= exit-status 0))
                              'error
                            'idle))))

(defun ai-code-backends-infra-ghostel--progress-status (state)
  "Return an AI Code status symbol for Ghostel progress STATE."
  (pcase state
    ('remove 'idle)
    ('error 'error)
    ('pause 'paused)
    ((or 'set 'indeterminate) 'running)
    (_ 'running)))

(defun ai-code-backends-infra-ghostel--progress (state progress)
  "Record a Ghostel OSC 9;4 progress report and delegate to Ghostel.
STATE and PROGRESS use the signature of `ghostel-progress-function'."
  (when (ai-code-backends-infra-ghostel--ai-session-buffer-p)
    (ai-code-backends-infra-ghostel--update-session-metadata
     (current-buffer)
     (list :ghostel-progress-state state
           :ghostel-progress-value progress
           :ghostel-progress-updated-at (float-time)
           :ghostel-status
           (ai-code-backends-infra-ghostel--progress-status state))))
  (when (and ai-code-backends-infra-ghostel--progress-function
             (not (eq ai-code-backends-infra-ghostel--progress-function
                      #'ai-code-backends-infra-ghostel--progress)))
    (funcall ai-code-backends-infra-ghostel--progress-function state progress)))

(defun ai-code-backends-infra-ghostel--install-lifecycle-hooks ()
  "Install Ghostel lifecycle hooks for the current AI Code session."
  (when (boundp 'ghostel-command-start-functions)
    (add-hook 'ghostel-command-start-functions
              #'ai-code-backends-infra-ghostel--command-start nil t))
  (when (boundp 'ghostel-command-finish-functions)
    (add-hook 'ghostel-command-finish-functions
              #'ai-code-backends-infra-ghostel--command-finish nil t))
  (when (boundp 'ghostel-progress-function)
    (unless (eq ghostel-progress-function
                #'ai-code-backends-infra-ghostel--progress)
      (setq-local ai-code-backends-infra-ghostel--progress-function
                  ghostel-progress-function))
    (setq-local ghostel-progress-function
                #'ai-code-backends-infra-ghostel--progress)))

(defun ai-code-backends-infra-ghostel--trusted-local-session-p ()
  "Return non-nil when this Ghostel session can safely read local files."
  (not (file-remote-p
        (or ai-code-backends-infra--session-directory
            default-directory))))

(defun ai-code-backends-infra-ghostel--active-preedit-overlay-p
    (overlay buffer)
  "Return non-nil when OVERLAY is an active preedit overlay in BUFFER."
  (and (overlayp overlay)
       (eq (overlay-buffer overlay) buffer)
       (overlay-start overlay)
       (overlay-end overlay)
       (or (< (overlay-start overlay) (overlay-end overlay))
           (overlay-get overlay 'after-string)
           (overlay-get overlay 'before-string)
           (overlay-get overlay 'display))))

(defun ai-code-backends-infra-ghostel--preedit-value-active-p (value)
  "Return non-nil when VALUE represents active preedit state."
  (cond
   ((stringp value) (> (length value) 0))
   ((overlayp value) (overlay-buffer value))
   ((consp value) t)
   (t value)))

(defun ai-code-backends-infra-ghostel--preedit-property-active-p
    (getter object)
  "Return non-nil when GETTER finds a preedit property on OBJECT."
  (cl-some
   (lambda (property)
     (ai-code-backends-infra-ghostel--preedit-value-active-p
      (funcall getter object property)))
   ai-code-backends-infra-ghostel--preedit-properties))

(defun ai-code-backends-infra-ghostel--overlay-near-point-p
    (overlay point)
  "Return non-nil when OVERLAY is attached near POINT."
  (let ((start (overlay-start overlay))
        (end (overlay-end overlay)))
    (and start
         end
         (<= (1- start) point)
         (<= point (1+ end)))))

(defun ai-code-backends-infra-ghostel--margin-display-string-p (string)
  "Return non-nil when STRING is a margin display spec, not preedit text."
  (and (stringp string)
       (> (length string) 0)
       (let ((disp (get-text-property 0 'display string)))
         (and (consp disp)
              (or (eq (car disp) 'margin)
                  (and (consp (car disp))
                       (eq (caar disp) 'margin)))))))

(defun ai-code-backends-infra-ghostel--known-non-preedit-overlay-p
    (overlay)
  "Return non-nil when OVERLAY is known not to represent preedit text."
  (or (overlay-get overlay 'ai-code-session-image-preview)
      (and (boundp 'ghostel--fake-cursor-overlay)
           (eq overlay ghostel--fake-cursor-overlay))
      (ai-code-backends-infra-ghostel--margin-display-string-p
       (overlay-get overlay 'before-string))
      (ai-code-backends-infra-ghostel--margin-display-string-p
       (overlay-get overlay 'after-string))))

(defun ai-code-backends-infra-ghostel--point-preedit-overlay-p
    (overlay buffer point)
  "Return non-nil when OVERLAY looks like preedit text near POINT in BUFFER."
  (and (ai-code-backends-infra-ghostel--active-preedit-overlay-p
        overlay buffer)
       (ai-code-backends-infra-ghostel--overlay-near-point-p
        overlay point)
       (not
        (ai-code-backends-infra-ghostel--known-non-preedit-overlay-p
         overlay))
       (or (ai-code-backends-infra-ghostel--preedit-property-active-p
            #'overlay-get overlay)
           (overlay-get overlay 'after-string)
           (overlay-get overlay 'before-string)
           (overlay-get overlay 'display))))

(defun ai-code-backends-infra-ghostel--point-preedit-overlays-p
    (buffer)
  "Return non-nil when BUFFER has preedit overlays around point."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((point (point))
             (start (max (point-min) (1- point)))
             (end (min (point-max) (1+ point)))
             (overlays (delete-dups
                        (append (overlays-at point)
                                (overlays-in start end)))))
        (cl-some
         (lambda (overlay)
           (ai-code-backends-infra-ghostel--point-preedit-overlay-p
            overlay buffer point))
         overlays)))))

(defun ai-code-backends-infra-ghostel--point-preedit-properties-p
    (buffer)
  "Return non-nil when BUFFER has preedit text properties around point."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((positions (delete-dups
                        (list (point)
                              (max (point-min) (1- (point)))))))
        (cl-some
         (lambda (position)
           (and (<= (point-min) position)
                (< position (point-max))
                (ai-code-backends-infra-ghostel--preedit-property-active-p
                 #'get-text-property position)))
         positions)))))

(defun ai-code-backends-infra-ghostel--preedit-symbol-active-p ()
  "Return non-nil when a native preedit dynamic variable is active."
  (and (boundp 'preedit-text)
       (ai-code-backends-infra-ghostel--preedit-value-active-p
        (symbol-value 'preedit-text))))

(defun ai-code-backends-infra-ghostel--native-preedit-active-p
    (&optional buffer)
  "Return non-nil when BUFFER has an active GUI-native preedit overlay.

BUFFER defaults to the current buffer.  This function is installed in
`ghostel-inhibit-redraw-functions' for AI Code Ghostel sessions."
  (let ((buffer (or buffer (current-buffer))))
    (and ai-code-backends-infra-ghostel-inhibit-redraw-during-native-preedit
         (buffer-live-p buffer)
         (ai-code-backends-infra-ghostel--ai-session-buffer-p buffer)
         (with-current-buffer buffer
           (or
            (ai-code-backends-infra-ghostel--preedit-symbol-active-p)
            (cl-some
             (lambda (symbol)
               (and (boundp symbol)
                    (ai-code-backends-infra-ghostel--active-preedit-overlay-p
                     (symbol-value symbol)
                     buffer)))
             ai-code-backends-infra-ghostel-native-preedit-overlay-symbols)
            (ai-code-backends-infra-ghostel--point-preedit-overlays-p
             buffer)
            (ai-code-backends-infra-ghostel--point-preedit-properties-p
             buffer))))))

(defun ai-code-backends-infra-ghostel--note-input-activity ()
  "Record recent user input for Ghostel redraw protection."
  (when (ai-code-backends-infra-ghostel--ai-session-buffer-p)
    (setq ai-code-backends-infra-ghostel--last-input-activity-time
          (float-time))))

(defun ai-code-backends-infra-ghostel--recent-input-active-p
    (&optional buffer)
  "Return non-nil when BUFFER recently received user input."
  (let ((buffer (or buffer (current-buffer))))
    (and ai-code-backends-infra-ghostel-inhibit-redraw-after-input
         (buffer-live-p buffer)
         (ai-code-backends-infra-ghostel--ai-session-buffer-p buffer)
         (with-current-buffer buffer
           (and ai-code-backends-infra-ghostel--last-input-activity-time
                (< (- (float-time)
                      ai-code-backends-infra-ghostel--last-input-activity-time)
                   ai-code-backends-infra-ghostel-input-redraw-inhibit-delay))))))

(defun ai-code-backends-infra-ghostel--redraw-inhibited-p
    (&optional buffer)
  "Return non-nil when BUFFER should defer redraw or linkification."
  (let ((buffer (or buffer (current-buffer))))
    (or (ai-code-backends-infra-ghostel--native-preedit-active-p buffer)
        (ai-code-backends-infra-ghostel--recent-input-active-p buffer))))

(defun ai-code-backends-infra-ghostel--install-redraw-inhibition ()
  "Install redraw inhibition for the current Ghostel buffer."
  (when (or ai-code-backends-infra-ghostel-inhibit-redraw-during-native-preedit
            ai-code-backends-infra-ghostel-inhibit-redraw-after-input)
    (add-hook 'ghostel-inhibit-redraw-functions
              #'ai-code-backends-infra-ghostel--redraw-inhibited-p nil t)
    (add-hook 'ai-code-session-link-inhibit-functions
              #'ai-code-backends-infra-ghostel--redraw-inhibited-p nil t))
  (when ai-code-backends-infra-ghostel-inhibit-redraw-after-input
    (add-hook 'pre-command-hook
              #'ai-code-backends-infra-ghostel--note-input-activity nil t)
    (add-hook 'post-command-hook
              #'ai-code-backends-infra-ghostel--note-input-activity nil t)))

(defun ai-code-backends-infra-ghostel--link-properties-at (position)
  "Return preserved link properties at POSITION."
  (let (properties)
    (dolist (property ai-code-backends-infra-ghostel--preserved-link-properties)
      (when-let* ((value (get-text-property position property)))
        (setq properties (plist-put properties property value))))
    properties))

(defun ai-code-backends-infra-ghostel--link-properties-p (properties)
  "Return non-nil when PROPERTIES describe a clickable session link."
  (or (plist-get properties 'help-echo)
      (plist-get properties 'ai-code-session-link)
      (plist-get properties 'ai-code-session-hover-link)))

(defun ai-code-backends-infra-ghostel--cache-preserved-link-spans
    (start end)
  "Cache clickable link spans between START and END for redraw restoration."
  (when (and (ai-code-backends-infra-ghostel--ai-session-buffer-p)
             (integer-or-marker-p start)
             (integer-or-marker-p end))
    (setq start (max (point-min) start)
          end (min (point-max) end))
    (when (< start end)
      (let ((position start)
            spans)
        (while (< position end)
          (let* ((next (or (next-property-change position nil end) end))
                 (properties
                  (ai-code-backends-infra-ghostel--link-properties-at
                   position)))
            (when (ai-code-backends-infra-ghostel--link-properties-p
                   properties)
              (push (list :start position
                          :end next
                          :text
                          (buffer-substring-no-properties position next)
                          :properties properties)
                    spans))
            (setq position next)))
        (when spans
          (setq ai-code-backends-infra-ghostel--preserved-link-spans
                (nreverse spans)))))))

(defun ai-code-backends-infra-ghostel--link-span-properties-present-p
    (start end properties)
  "Return non-nil when PROPERTIES are already present from START to END."
  (let ((position start)
        (present t))
    (while (and present (< position end))
      (let ((remaining properties))
        (while (and present remaining)
          (let ((property (pop remaining))
                (value (pop remaining)))
            (unless (equal (get-text-property position property) value)
              (setq present nil)))))
      (setq position (or (next-property-change position nil end) end)))
    present))

(defun ai-code-backends-infra-ghostel--restore-preserved-link-spans
    (&optional start end)
  "Restore cached clickable links after a Ghostel redraw.
START and END optionally bound the redraw region."
  (when (and ai-code-backends-infra-ghostel--preserved-link-spans
             (ai-code-backends-infra-ghostel--ai-session-buffer-p))
    (let ((region-start (and (integer-or-marker-p start) start))
          (region-end (and (integer-or-marker-p end) end))
          (inhibit-read-only t)
          (inhibit-modification-hooks t))
      (dolist (span ai-code-backends-infra-ghostel--preserved-link-spans)
        (let ((span-start (plist-get span :start))
              (span-end (plist-get span :end))
              (text (plist-get span :text))
              (properties (plist-get span :properties)))
          (when (and (integer-or-marker-p span-start)
                     (integer-or-marker-p span-end)
                     (<= (point-min) span-start)
                     (<= span-start span-end)
                     (<= span-end (point-max))
                     (or (not region-start) (>= span-end region-start))
                     (or (not region-end) (<= span-start region-end))
                     (equal (buffer-substring-no-properties
                             span-start span-end)
                            text)
                     (not
                      (ai-code-backends-infra-ghostel--link-span-properties-present-p
                       span-start span-end properties)))
            (add-text-properties span-start span-end properties)))))))

(defun ai-code-backends-infra-ghostel--linkified-candidate-at-p (position)
  "Return non-nil when POSITION already belongs to a clickable link."
  (ai-code-backends-infra-ghostel--link-properties-p
   (ai-code-backends-infra-ghostel--link-properties-at position)))

(defun ai-code-backends-infra-ghostel--region-needs-session-linkify-p
    (start end)
  "Return non-nil when START to END has unlinked session-link candidates."
  (let ((position start)
        needs-linkify)
    (save-excursion
      (goto-char start)
      (while (and (not needs-linkify)
                  (re-search-forward
                   ai-code-backends-infra-ghostel--session-link-candidate-regexp
                   end t))
        (setq position (match-beginning 0))
        (unless (ai-code-backends-infra-ghostel--linkified-candidate-at-p
                 position)
          (setq needs-linkify t))))
    needs-linkify))

(defun ai-code-backends-infra-ghostel--linkify-session-region
    (start end)
  "Linkify START to END without refreshing already preserved links."
  (ai-code-backends-infra-ghostel--restore-preserved-link-spans start end)
  (when (ai-code-backends-infra-ghostel--region-needs-session-linkify-p
         start end)
    (ai-code-session-link--linkify-session-region start end))
  (ai-code-backends-infra-ghostel--cache-preserved-link-spans start end))

(defun ai-code-backends-infra-ghostel--around-schedule-link-detection
    (orig-fn &optional begin end)
  "Call ORIG-FN after restoring links before a redraw scan.
BEGIN and END are the Ghostel link-detection bounds."
  (ai-code-backends-infra-ghostel--restore-preserved-link-spans begin end)
  (funcall orig-fn begin end))

(defun ai-code-backends-infra-ghostel--around-run-queued-link-detection
    (orig-fn buffer)
  "Call ORIG-FN for BUFFER and cache Ghostel link spans afterward."
  (let (begin end)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (ai-code-backends-infra-ghostel--ai-session-buffer-p)
          (setq begin ghostel--plain-link-detection-begin
                end ghostel--plain-link-detection-end))))
    (prog1 (funcall orig-fn buffer)
      (when (and (buffer-live-p buffer)
                 (integer-or-marker-p begin)
                 (integer-or-marker-p end))
        (with-current-buffer buffer
          (ai-code-backends-infra-ghostel--cache-preserved-link-spans
           begin end))))))

(defun ai-code-backends-infra-ghostel--install-link-preservation-advice ()
  "Install Ghostel redraw link preservation advice."
  (when (and (fboundp 'ghostel--schedule-link-detection)
             (not (advice-member-p
                   #'ai-code-backends-infra-ghostel--around-schedule-link-detection
                   'ghostel--schedule-link-detection)))
    (advice-add 'ghostel--schedule-link-detection
                :around
                #'ai-code-backends-infra-ghostel--around-schedule-link-detection))
  (when (and (fboundp 'ghostel--run-queued-plain-link-detection)
             (not
              (advice-member-p
               #'ai-code-backends-infra-ghostel--around-run-queued-link-detection
               'ghostel--run-queued-plain-link-detection)))
    (advice-add
     'ghostel--run-queued-plain-link-detection
     :around
     #'ai-code-backends-infra-ghostel--around-run-queued-link-detection)))

(defun ai-code-backends-infra-ghostel--effective-kitty-graphics-mediums ()
  "Return Kitty graphics mediums for the current AI Code Ghostel session."
  (delete-dups
   (append (and (ai-code-backends-infra-ghostel--trusted-local-session-p)
                ai-code-backends-infra-ghostel-kitty-graphics-mediums)
           (and (boundp 'ghostel-kitty-graphics-mediums)
                ghostel-kitty-graphics-mediums))))

(defun ai-code-backends-infra-ghostel--configure-image-support ()
  "Configure Ghostel image support for the current AI Code session."
  (when (boundp 'ghostel-kitty-graphics-mediums)
    (setq-local ghostel-kitty-graphics-mediums
                (ai-code-backends-infra-ghostel--effective-kitty-graphics-mediums))))

(defun ai-code-backends-infra-ghostel--redraw-output-p (output)
  "Return non-nil when OUTPUT looks like a TUI redraw frame."
  (let* ((output (or output ""))
         (clear-count (1- (length (split-string output "\033\\[K"))))
         (cr-count (cl-count ?\15 output))
         (escape-count (cl-count ?\033 output))
         (output-length (length output))
         (escape-density (if (> output-length 0)
                             (/ (float escape-count) output-length)
                           0)))
    (or (string-match-p ai-code-backends-infra-ghostel--redraw-regexp output)
        (>= cr-count 2)
        (and (> escape-density 0.3) (>= clear-count 2)))))

(defun ai-code-backends-infra-ghostel--animated-status-key (output)
  "Return a plain-text animated status key for OUTPUT, or nil."
  (let ((plain (ai-code-session-link--recent-output-plain-text output)))
    (when (and (not (string= plain ""))
               (string-match-p "\\bWorking\\b" plain))
      plain)))

(defun ai-code-backends-infra-ghostel--animated-status-output-p
    (output)
  "Return non-nil when OUTPUT redraws an animated status row."
  (and (ai-code-backends-infra-ghostel--redraw-output-p output)
       (ai-code-backends-infra-ghostel--animated-status-key output)))

(defun ai-code-backends-infra-ghostel--skip-sgr-color-params (codes)
  "Skip extended SGR color parameters from CODES."
  (pcase (car codes)
    ("5" (nthcdr 2 codes))
    ("2" (nthcdr 4 codes))
    (_ (cdr codes))))

(defun ai-code-backends-infra-ghostel--sgr-code-number (code)
  "Return the SGR number for CODE, treating an empty parameter as reset."
  (string-to-number (if (string= code "") "0" code)))

(defun ai-code-backends-infra-ghostel--sgr-effect (codes)
  "Return the final foreground and intensity effect of SGR CODES."
  (let ((dim nil)
        (normal nil)
        (explicit-foreground nil)
        (foreground-reset nil)
        (foreground-state ai-code-backends-infra-ghostel--foreground-state)
        (remaining codes))
    (while remaining
      (let* ((code (pop remaining))
             (number (ai-code-backends-infra-ghostel--sgr-code-number code)))
        (cond
         ((= number 0)
          (setq dim nil
                normal t
                foreground-reset t
                foreground-state nil))
         ((= number 2)
          (setq dim t
                normal nil))
         ((= number 22)
          (setq dim nil
                normal t))
         ((= number 39)
          (setq foreground-reset t
                foreground-state nil))
         ((or (<= 30 number 37)
              (<= 90 number 97))
          (setq explicit-foreground t
                foreground-state 'explicit))
         ((= number 38)
          (setq explicit-foreground t
                foreground-state 'explicit)
          (setq remaining
                (ai-code-backends-infra-ghostel--skip-sgr-color-params
                 remaining)))
         ((= number 48)
          (setq remaining
                (ai-code-backends-infra-ghostel--skip-sgr-color-params
                 remaining))))))
    (list dim normal explicit-foreground foreground-reset foreground-state)))

(defun ai-code-backends-infra-ghostel--normalize-dim-sgr (output)
  "Render SGR dim text with the standard ANSI gray foreground in OUTPUT."
  (let ((start 0)
        (parts nil)
        (output (or output "")))
    (while (string-match "\e\\[\\([0-9;]*\\)m" output start)
      (let* ((match-beginning (match-beginning 0))
             (match-end (match-end 0))
             (sequence (match-string 0 output))
             (params (match-string 1 output))
             (codes (if (string= params "") '("0") (split-string params ";")))
             (was-injected
              (eq ai-code-backends-infra-ghostel--foreground-state
                  'injected)))
        (cl-destructuring-bind
            (dim normal explicit-foreground foreground-reset foreground-state)
            (ai-code-backends-infra-ghostel--sgr-effect codes)
          (push (substring output start match-beginning) parts)
          (push
           (cond
            ((and dim (null foreground-state))
             (setq ai-code-backends-infra-ghostel--foreground-state 'injected
                   ai-code-backends-infra-ghostel--dim-foreground-active t)
             (format "\e[%sm" (mapconcat #'identity (append codes '("90")) ";")))
            ((and normal
                  was-injected
                  (not foreground-reset)
                  (not explicit-foreground))
             (setq ai-code-backends-infra-ghostel--foreground-state nil
                   ai-code-backends-infra-ghostel--dim-foreground-active nil)
             (format "\e[%sm" (mapconcat #'identity (append codes '("39")) ";")))
            (t
             (setq ai-code-backends-infra-ghostel--foreground-state
                   foreground-state
                   ai-code-backends-infra-ghostel--dim-foreground-active
                   (eq foreground-state 'injected))
             sequence))
           parts)
          (setq start match-end))))
    (push (substring output start) parts)
    (apply #'concat (nreverse parts))))

(defun ai-code-backends-infra-ghostel--render-output
    (buffer orig-filter process output)
  "Render OUTPUT for PROCESS in BUFFER and run AI Code bookkeeping.
ORIG-FILTER is Ghostel's original process filter."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((output (ai-code-backends-infra-ghostel--normalize-dim-sgr
                      output))
             (animated-status-output
              (ai-code-backends-infra-ghostel--animated-status-output-p
               output)))
        ;; Preserve short-lived screenshot bytes before Ghostel redraws or the
        ;; producer has a chance to unlink its temporary file.
        (ai-code-ghostel-image-preview-capture-output output)
        (when orig-filter
          (funcall orig-filter process output))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (ai-code-backends-infra--output-meaningful-p output)
              (ai-code-backends-infra--note-meaningful-output))
            (unless animated-status-output
              (ai-code-session-link--schedule-linkify-recent-output
               buffer
               output
               ai-code-backends-infra-ghostel--linkify-redraw-delay))))))))

(defun ai-code-backends-infra-ghostel--flush-render-queue
    (buffer orig-filter process)
  "Render queued Ghostel output for BUFFER using ORIG-FILTER and PROCESS."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ai-code-backends-infra-ghostel--render-timer nil)
      (when ai-code-backends-infra-ghostel--render-queue
        (let ((output ai-code-backends-infra-ghostel--render-queue))
          (setq ai-code-backends-infra-ghostel--render-queue nil)
          (ai-code-backends-infra-ghostel--render-output
           buffer orig-filter process output))))))

(defun ai-code-backends-infra-ghostel--handle-process-output
    (buffer orig-filter process output)
  "Handle Ghostel PROCESS OUTPUT for BUFFER through ORIG-FILTER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (and ai-code-backends-infra-ghostel-anti-flicker
               (or ai-code-backends-infra-ghostel--render-queue
                   (ai-code-backends-infra-ghostel--redraw-output-p output)))
          (progn
            (setq ai-code-backends-infra-ghostel--render-queue
                  (concat ai-code-backends-infra-ghostel--render-queue output))
            (unless ai-code-backends-infra-ghostel--render-timer
              (setq ai-code-backends-infra-ghostel--render-timer
                    (run-at-time
                     ai-code-backends-infra-ghostel-render-delay nil
                     #'ai-code-backends-infra-ghostel--flush-render-queue
                     buffer
                     orig-filter
                     process))))
        (ai-code-backends-infra-ghostel--render-output
         buffer orig-filter process output)))))

(defun ai-code-backends-infra-ghostel--capture-native-child-exit-status
    (process output)
  "Record a native Ghostel child exit status from PROCESS OUTPUT."
  (when (and (processp process) (stringp output))
    (let* ((fragment
            (or (process-get
                 process 'ai-code-backends-infra-ghostel--event-fragment)
                ""))
           (input (concat fragment output))
           (input-length (length input))
           (offset 0)
           pending)
      (while (< offset input-length)
        (let ((parsed
               (condition-case nil
                   (read-from-string input offset)
                 (end-of-file nil))))
          (if parsed
              (progn
                (when (numberp (car parsed))
                  (process-put
                   process
                   'ai-code-backends-infra--child-exit-status
                   (car parsed)))
                (setq offset (cdr parsed)))
            (setq pending (substring input offset)
                  offset input-length))))
      (process-put
       process 'ai-code-backends-infra-ghostel--event-fragment pending))))

(defun ai-code-backends-infra-ghostel--wrap-process-filter
    (buffer process)
  "Wrap PROCESS output for the AI Code Ghostel session in BUFFER."
  (when (and (buffer-live-p buffer)
             (processp process)
             (not
              (process-get
               process
               ai-code-backends-infra-ghostel--process-filter-wrapped-property)))
    (let ((orig-filter (process-filter process)))
      (process-put
       process
       ai-code-backends-infra-ghostel--process-filter-wrapped-property
       t)
      (set-process-filter
       process
       (lambda (proc output)
         (when (eq orig-filter 'ghostel--events-filter)
           (ai-code-backends-infra-ghostel--capture-native-child-exit-status
            proc output))
         (let ((target (or (ignore-errors (process-buffer proc))
                           buffer)))
           (when (buffer-live-p target)
             (ai-code-backends-infra-ghostel--handle-process-output
              target orig-filter proc
              (ai-code-editor-viewport-filter-output proc output)))))))))

(defun ai-code-backends-infra-ghostel--enable-ime-integration ()
  "Enable Ghostel IME integration for the current AI Code session."
  (when (and ai-code-backends-infra-ghostel-enable-ime-integration
             (require 'ghostel-ime nil t)
             (fboundp 'ghostel-ime-mode))
    (ghostel-ime-mode 1)))

(defun ai-code-backends-infra-ghostel-send-string (string &optional paste)
  "Send STRING to the current Ghostel process.
If PASTE is non-nil, send it as a pasted string."
  (ai-code-backends-infra--send-string-with-paste
   string paste #'ghostel-send-string
   (and (fboundp 'ghostel-paste-string) #'ghostel-paste-string)
   (lambda ()
     (and (fboundp 'ghostel--mode-enabled)
          (bound-and-true-p ghostel--term)
          (ghostel--mode-enabled ghostel--term 2004)))
   "Ghostel"))

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
  "Return the Ghostel resize handler.
Ghostel owns terminal-model resizing through its mode-local window hooks."
  nil)

(defun ai-code-backends-infra--configure-ghostel-buffer ()
  "Configure the current Ghostel buffer for AI Code sessions."
  (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
  (setq-local ai-code-editor-viewport-source-cursor-function
              #'ghostel-cursor-point)
  (setq-local ghostel-set-title-function nil)
  (setq-local ghostel-kill-buffer-on-exit nil)
  (ai-code-backends-infra-ghostel--configure-image-support)
  (ai-code-backends-infra-ghostel--enable-ime-integration)
  (ai-code-backends-infra-ghostel--install-redraw-inhibition)
  (ai-code-backends-infra-ghostel--install-link-preservation-advice)
  (ai-code-backends-infra-ghostel--install-lifecycle-hooks)
  (ai-code-backends-infra-ghostel--install-editor-transport)
  (when (ai-code-session-link--image-preview-enabled-p)
    (ai-code-ghostel-image-preview-enable))
  (ai-code-backends-infra--configure-session-input-shortcuts)
  (ai-code-backends-infra--install-navigation-cursor-sync))

(defun ai-code-backends-infra-ghostel--resolve-program (program)
  "Resolve the active bare AI CLI PROGRAM to an executable path when possible.
Explicit paths and programs outside the current AI CLI launch are preserved.
An unresolved AI CLI name falls back to PROGRAM so Ghostel can report its
normal startup error."
  (let ((launch-program
         (and (boundp 'ai-code-backends-infra--launch-program)
              ai-code-backends-infra--launch-program)))
    (if (and (stringp program)
             (stringp launch-program)
             (string= program launch-program)
             (not (file-name-directory program)))
        (or (executable-find program) program)
      program)))

(defun ai-code-backends-infra--start-ghostel-process (buffer command)
  "Start a Ghostel session in BUFFER for COMMAND."
  (with-current-buffer buffer
    (ai-code-backends-infra--configure-ghostel-buffer)
    (let* ((native-editor-transport-p
            (ai-code-backends-infra-ghostel--native-editor-transport-p))
           (configured-native-pty
            (and (boundp 'ghostel-use-native-pty)
                 (symbol-value 'ghostel-use-native-pty)))
           (effective-native-pty
            (if (and ai-code-editor-viewport-enabled
                     (not native-editor-transport-p))
                nil
              configured-native-pty))
           (argv (split-string-shell-command command))
           (program (car argv))
           (args (cdr argv)))
      (cond
       ((not program) nil)
       ((fboundp 'ghostel-exec)
        (let ((proc
               (let ((ghostel-kitty-graphics-mediums
                      (ai-code-backends-infra-ghostel--effective-kitty-graphics-mediums)))
                 (cl-progv '(ghostel-use-native-pty)
                     (list effective-native-pty)
                   (ghostel-exec
                    buffer
                    (ai-code-backends-infra-ghostel--resolve-program program)
                    args)))))
          ;; `ghostel-exec' enters `ghostel-mode', which resets local state.
          (ai-code-backends-infra--configure-ghostel-buffer)
          proc))
       (t
        (user-error
         "Ghostel backend requires a Ghostel version that provides `ghostel-exec`"))))))

(defun ai-code-backends-infra-ghostel-create-session (buffer-name working-dir command env-vars)
  "Create a Ghostel session named BUFFER-NAME in WORKING-DIR.
COMMAND is the shell command to run and ENV-VARS are extra environment
variables for the terminal process."
  (let* ((working-dir (file-name-as-directory (expand-file-name working-dir)))
         (buffer (get-buffer-create buffer-name))
         (editor-environment
          (if (and (not (file-remote-p working-dir))
                   ai-code-editor-viewport-enabled
                   (ai-code-backends-infra-ghostel--native-editor-transport-p))
              (ai-code-editor-viewport-environment
               env-vars
               (ai-code-backends-infra-ghostel--editor-frame-prefix))
            env-vars))
         (process-environment
          (append editor-environment process-environment)))
    (ai-code-backends-infra--set-session-directory buffer working-dir)
    (with-current-buffer buffer
      (setq-local default-directory working-dir)
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (let ((default-directory working-dir)
            (proc (ai-code-backends-infra--start-ghostel-process buffer command)))
        (setq-local default-directory working-dir)
        (ai-code-backends-infra--set-session-directory buffer working-dir)
        (ai-code-backends-infra--configure-ghostel-buffer)
        (when (processp proc)
          (ignore-errors
            (set-process-query-on-exit-flag proc nil))
          (when-let* ((sentinel (ignore-errors (process-sentinel proc))))
            (ignore-errors
              (process-put proc
                           'ai-code-backends-infra--ghostel-sentinel
                           sentinel)))
          (ai-code-backends-infra-ghostel--wrap-process-filter buffer proc))
        (cons buffer proc)))))

(provide 'ai-code-backends-infra-ghostel)
;;; ai-code-backends-infra-ghostel.el ends here
