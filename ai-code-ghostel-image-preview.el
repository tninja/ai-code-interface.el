;;; ai-code-ghostel-image-preview.el --- Stable Ghostel image previews -*- lexical-binding: t; -*-

;; Author: realazy (cxa)
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Own the complete lifecycle of local image previews in AI Code Ghostel
;; sessions.  Callers enable or disable the module for a buffer; preview row
;; ownership, scrolling, redraw reconciliation, and follow state remain inside
;; this module.

;;; Code:

(require 'cl-lib)
(require 'mwheel)
(require 'pixel-scroll)
(require 'subr-x)
(require 'ai-code-session-link)

(declare-function ai-code-session-link--linkify-inhibited-p
                  "ai-code-session-link" (&optional buffer))
(declare-function ai-code-session-link--recent-output-tail-width
                  "ai-code-session-link" (output))
(declare-function ai-code-session-link--text-may-contain-image-reference-p
                  "ai-code-session-link" (text))
(declare-function ai-code-session-link--strict-image-candidate-bounds
                  "ai-code-session-link" (match-start match-end))
(declare-function ai-code-session-link--strict-image-candidates
                  "ai-code-session-link" (start end link-text))
(declare-function ai-code-session-link--image-preview-existing-local-file
                  "ai-code-session-link" (link-text))
(declare-function ai-code-session-link--image-preview-data
                  "ai-code-session-link" (file))
(declare-function ai-code-session-link--image-preview-file-signature
                  "ai-code-session-link" (file))
(declare-function ai-code-session-link--normalize-file
                  "ai-code-session-link" (filename))
(declare-function ai-code-session-link--linkify-strict-image-preview-region
                  "ai-code-session-link" (start end))
(declare-function ai-code-session-link--delete-image-preview-overlay
                  "ai-code-session-link" (overlay))
(declare-function ghostel--redraw-now "ghostel" (buffer &optional force))
(declare-function ghostel--viewport-start "ghostel" ())
(declare-function ultra-scroll "ultra-scroll" (event &optional arg))
(declare-function ultra-scroll-mac "ultra-scroll" (event &optional arg))

(defvar ai-code-session-link-image-preview-position-function)
(defvar ai-code-session-link-image-preview-source-function)
(defvar ai-code-session-link-image-preview-transaction-function)
(defvar ai-code-session-link--image-extension-regexp)
(defvar ai-code-backends-infra--session-directory)
(defvar ai-code-backends-infra--session-terminal-backend)
(defvar ghostel-inhibit-anchor-functions)
(defvar ghostel--cursor-char-pos)

(defconst ai-code-ghostel-image-preview--captured-source-limit 8
  "Maximum number of temporary image sources retained per session.")

(defconst ai-code-ghostel-image-preview--output-tail-limit 8192
  "Maximum raw process-output characters retained for split image paths.")

(defconst ai-code-ghostel-image-preview--output-probe-limit 4096
  "Maximum recent output characters checked for a new image clue.")

(define-obsolete-variable-alias
  'ai-code-backends-infra-ghostel-visible-image-linkify-delays
  'ai-code-ghostel-image-preview-linkify-delays
  "1.90")

(defcustom ai-code-ghostel-image-preview-linkify-delays '(0.15 0.5)
  "Delays used to scan a newly displayed Ghostel viewport for previews."
  :type '(repeat number)
  :group 'ai-code-backends-infra)

(define-obsolete-variable-alias
  'ai-code-backends-infra-ghostel-visible-image-linkify-max-chars
  'ai-code-ghostel-image-preview-linkify-max-chars
  "1.90")

(defcustom ai-code-ghostel-image-preview-linkify-max-chars 20000
  "Fallback scan size when Ghostel or Emacs cannot report a viewport."
  :type 'integer
  :group 'ai-code-backends-infra)

(define-obsolete-variable-alias
  'ai-code-backends-infra-ghostel-scroll-image-linkify-delay
  'ai-code-ghostel-image-preview-scroll-linkify-delay
  "1.90")

(defcustom ai-code-ghostel-image-preview-scroll-linkify-delay 0.08
  "Debounce delay before scanning a Ghostel viewport after scrolling."
  :type 'number
  :group 'ai-code-backends-infra)

(defcustom ai-code-ghostel-image-preview-prefer-ultra-scroll t
  "Use `ultra-scroll' for wheel events over local image previews.

This is buffer-local in effect: AI Code does not enable
`ultra-scroll-mode' globally.  Native Kitty graphics retain the user's
ordinary pixel-scroll command."
  :type 'boolean
  :group 'ai-code-backends-infra)

(defvar-local ai-code-ghostel-image-preview--user-scrolled-windows nil
  "Windows whose user explicitly entered terminal scrollback.")

(defvar-local ai-code-ghostel-image-preview--linkify-timers nil
  "Alist of windows and their pending visible-preview scan timers.")

(defvar-local ai-code-ghostel-image-preview--transaction-active nil
  "Non-nil while this buffer is reconciling an image-preview transaction.")

(defvar-local ai-code-ghostel-image-preview--captured-sources nil
  "Recent temporary image source plists captured from process output.")

(defvar-local ai-code-ghostel-image-preview--output-tail ""
  "Bounded process-output tail used to reconstruct split image references.")

(defvar ai-code-ghostel-image-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap pixel-scroll-precision]
                #'ai-code-ghostel-image-preview-scroll)
    map)
  "Keymap used while Ghostel image-preview lifecycle management is active.")

(define-minor-mode ai-code-ghostel-image-preview-mode
  "Manage local image previews as one Ghostel render lifecycle."
  :init-value nil
  :lighter nil
  :keymap ai-code-ghostel-image-preview-mode-map)

(defun ai-code-ghostel-image-preview--visible-region (window)
  "Return the bounded visible buffer region for WINDOW."
  (when (and (window-live-p window)
             (buffer-live-p (window-buffer window)))
    (with-current-buffer (window-buffer window)
      (when ai-code-ghostel-image-preview-mode
        (save-excursion
          (save-restriction
            (widen)
            (let* ((start
                    (save-excursion
                      (goto-char (max (point-min) (window-start window)))
                      (forward-line -1)
                      (line-beginning-position)))
                   (window-end-position
                    (ignore-errors (window-end window t)))
                   (end
                    (min (point-max)
                         (or window-end-position
                             (+ start
                                ai-code-ghostel-image-preview-linkify-max-chars)))))
              (goto-char start)
              (setq start (line-beginning-position))
              (goto-char end)
              (setq end (min (point-max) (line-end-position)))
              (when (and (not window-end-position)
                         (> (- end start)
                            ai-code-ghostel-image-preview-linkify-max-chars))
                (setq end
                      (min (point-max)
                           (+ start
                              ai-code-ghostel-image-preview-linkify-max-chars))))
              (and (< start end) (cons start end)))))))))

(defun ai-code-ghostel-image-preview--window-has-preview-p (window)
  "Return non-nil when WINDOW intersects a local image preview."
  (when-let* ((region
               (ai-code-ghostel-image-preview--visible-region window)))
    (with-current-buffer (window-buffer window)
      (cl-some
       (lambda (overlay)
         (overlay-get overlay 'ai-code-session-image-preview))
       (ai-code-ghostel-image-preview--overlays-in-region
        (car region) (cdr region))))))

(defun ai-code-ghostel-image-preview--window-has-kitty-image-p (window)
  "Return non-nil when WINDOW intersects a native Ghostel Kitty image."
  (when-let* ((region
               (ai-code-ghostel-image-preview--visible-region window)))
    (with-current-buffer (window-buffer window)
      (let ((start (car region))
            (end (cdr region)))
        (or (text-property-not-all start end 'ghostel-kitty nil)
            (cl-some
             (lambda (overlay)
               (overlay-get overlay 'ghostel-kitty))
             (delete-dups
              (append (overlays-in start end)
                      (overlays-at start)
                      (overlays-at end)))))))))

(defun ai-code-ghostel-image-preview--window-has-image-p (window)
  "Return non-nil when WINDOW intersects a local or Kitty image."
  (or (ai-code-ghostel-image-preview--window-has-preview-p window)
      (ai-code-ghostel-image-preview--window-has-kitty-image-p window)))

(defun ai-code-ghostel-image-preview--event-window (event)
  "Return the target window of mouse EVENT, or nil."
  (let ((target (ignore-errors (posn-window (event-start event)))))
    (cond
     ((windowp target) target)
     ((framep target) (frame-selected-window target)))))

(defun ai-code-ghostel-image-preview--wheel-event-p (event)
  "Return non-nil when EVENT represents mouse-wheel scrolling."
  (memq (ignore-errors (event-basic-type event))
        '(wheel-up wheel-down mouse-4 mouse-5)))

(defun ai-code-ghostel-image-preview--preview-at-window-start-p (window)
  "Return non-nil when WINDOW starts inside a local preview overlay."
  (when (and (window-live-p window)
             (buffer-live-p (window-buffer window)))
    (with-current-buffer (window-buffer window)
      (cl-some
       (lambda (overlay)
         (overlay-get overlay 'ai-code-session-image-preview))
       (overlays-at (window-start window))))))

(defun ai-code-ghostel-image-preview--clamp-up-event (event window)
  "Clamp upward EVENT to WINDOW's partially hidden preview top."
  (let* ((delta-pair (nth 4 event))
         (delta (and (consp delta-pair) (cdr delta-pair)))
         (vscroll (and (window-live-p window)
                       (window-vscroll window t))))
    (if (and (numberp delta)
             (> delta 0)
             (numberp vscroll)
             (> vscroll 0)
             (> delta vscroll)
             (ai-code-ghostel-image-preview--preview-at-window-start-p
              window))
        (let ((clamped (copy-tree event)))
          (setcdr (nth 4 clamped) vscroll)
          clamped)
      event)))

(defun ai-code-ghostel-image-preview--note-user-scroll (window)
  "Record that WINDOW is scrolling through image content."
  (when (and (window-live-p window)
             (ai-code-ghostel-image-preview--window-has-image-p window))
    (with-current-buffer (window-buffer window)
      (cl-pushnew window ai-code-ghostel-image-preview--user-scrolled-windows
                  :test #'eq))))

(defun ai-code-ghostel-image-preview--clear-user-scroll-at-live-end (window)
  "Clear WINDOW's scrollback record after it visibly reaches live output."
  (when (and (window-live-p window)
             (buffer-live-p (window-buffer window)))
    (with-current-buffer (window-buffer window)
      (when-let* ((end (ignore-errors (window-end window t)))
                  ((>= end (point-max)))
                  ((not
                    (ai-code-ghostel-image-preview--window-has-image-p
                     window))))
        (setq ai-code-ghostel-image-preview--user-scrolled-windows
              (delq window
                    ai-code-ghostel-image-preview--user-scrolled-windows))))))

(defun ai-code-ghostel-image-preview--user-scrolled-window-p (window)
  "Return non-nil when WINDOW entered image scrollback by mouse wheel."
  (when (and (window-live-p window)
             (buffer-live-p (window-buffer window)))
    (with-current-buffer (window-buffer window)
      (setq ai-code-ghostel-image-preview--user-scrolled-windows
            (cl-remove-if-not
             (lambda (candidate)
               (and (window-live-p candidate)
                    (eq (window-buffer candidate) (current-buffer))))
             ai-code-ghostel-image-preview--user-scrolled-windows))
      (memq window ai-code-ghostel-image-preview--user-scrolled-windows))))

(defun ai-code-ghostel-image-preview--input-row-bounds ()
  "Return the bounds of Ghostel's live cursor row, or nil."
  (when (and (boundp 'ghostel--cursor-char-pos)
             (integer-or-marker-p ghostel--cursor-char-pos))
    (save-excursion
      (goto-char (max (point-min)
                      (min (point-max) ghostel--cursor-char-pos)))
      (cons (line-beginning-position) (line-end-position)))))

(defun ai-code-ghostel-image-preview--position-allowed-p (start _end)
  "Return non-nil when an image preview may begin at START."
  (if-let* ((input-bounds
             (ai-code-ghostel-image-preview--input-row-bounds)))
      (not (<= (car input-bounds) start (cdr input-bounds)))
    t))

(defun ai-code-ghostel-image-preview--overlays-in-region (start end)
  "Return image preview overlays touching START through END."
  (cl-remove-if-not
   (lambda (overlay)
     (overlay-get overlay 'ai-code-session-image-preview))
   (delete-dups
    (append (overlays-in start end)
            (overlays-at start)
            (overlays-at end)))))

(defun ai-code-ghostel-image-preview--render-region ()
  "Return the terminal render region affected by a Ghostel redraw."
  (let* ((end (point-max))
         (viewport-start
          (and (fboundp 'ghostel--viewport-start)
               (ignore-errors (ghostel--viewport-start))))
         (start
          (if (integer-or-marker-p viewport-start)
              (max (point-min) (min end viewport-start))
            (max (point-min)
                 (- end ai-code-ghostel-image-preview-linkify-max-chars)))))
    (save-excursion
      (goto-char start)
      (setq start (line-beginning-position))
      (cons start end))))

(defun ai-code-ghostel-image-preview--layout-regions ()
  "Return merged terminal and Emacs-visible image-layout regions."
  (let ((regions
         (cons
          (ai-code-ghostel-image-preview--render-region)
          (delq nil
                (mapcar
                 #'ai-code-ghostel-image-preview--visible-region
                 (get-buffer-window-list (current-buffer) nil t)))))
        merged)
    (dolist (region (sort regions (lambda (left right)
                                    (< (car left) (car right)))))
      (if (and merged (<= (car region) (cdr (car merged))))
          (setcdr (car merged) (max (cdr (car merged)) (cdr region)))
        (push (cons (car region) (cdr region)) merged)))
    (nreverse merged)))

(defun ai-code-ghostel-image-preview--capture-preview-state ()
  "Capture semantic preview anchors before Ghostel rewrites terminal rows."
  (let ((previews
         (delete-dups
          (apply
           #'append
           (mapcar
            (lambda (region)
              (ai-code-ghostel-image-preview--overlays-in-region
               (car region) (cdr region)))
            (ai-code-ghostel-image-preview--layout-regions))))))
    (list
     :signatures
     (sort
      (mapcar
       (lambda (overlay)
         (list (overlay-start overlay)
               (overlay-end overlay)
               (overlay-get overlay 'ai-code-session-image-display-end)
               (overlay-get overlay 'ai-code-session-image-file)
               (overlay-get overlay 'ai-code-session-image-file-signature)
               (overlay-get overlay 'ai-code-session-image-display-text)
               (overlay-get overlay 'before-string)
               (overlay-get overlay 'ai-code-session-image-vertical-margin)
               (overlay-get overlay 'ai-code-session-image-render-context)))
       previews)
      (lambda (left right)
        (< (car left) (car right))))
     :snapshots
     (mapcar
      (lambda (overlay)
        (list :overlay overlay
              :start (overlay-start overlay)
              :end (overlay-end overlay)))
      previews))))

(defun ai-code-ghostel-image-preview--preview-layout-changed-p (state)
  "Return non-nil when final preview layout differs from captured STATE."
  (not
   (equal
    (plist-get state :signatures)
    (plist-get
     (ai-code-ghostel-image-preview--capture-preview-state)
     :signatures))))

(defun ai-code-ghostel-image-preview--preview-changed-p (snapshot)
  "Return non-nil when the overlay in SNAPSHOT changed its anchor."
  (let ((overlay (plist-get snapshot :overlay)))
    (or (not (overlay-buffer overlay))
        (/= (overlay-start overlay) (plist-get snapshot :start))
        (/= (overlay-end overlay) (plist-get snapshot :end)))))

(defun ai-code-ghostel-image-preview--reconcile-preview-state (state)
  "Reconcile previews from pre-render STATE against final terminal text."
  (dolist (snapshot (plist-get state :snapshots))
    (when (ai-code-ghostel-image-preview--preview-changed-p snapshot)
      (ai-code-session-link--delete-image-preview-overlay
       (plist-get snapshot :overlay)))))

(defun ai-code-ghostel-image-preview--linkify-region (start end)
  "Rebuild strict local image previews between START and END."
  (when (and (< start end)
             (not (ai-code-session-link--linkify-inhibited-p)))
    (ai-code-session-link--linkify-strict-image-preview-region start end)))

(defun ai-code-ghostel-image-preview--plain-output (output)
  "Return OUTPUT without terminal controls, preserving hard line breaks."
  (let* ((text (or output ""))
         (text (replace-regexp-in-string
                "\x1b\\][^\x07\x1b]*\\(?:\x07\\|\x1b\\\\\\)" "" text))
         (text (replace-regexp-in-string
                "\x1b\\[[0-9;?]*[ -/]*[@-~]" "" text)))
    (replace-regexp-in-string "[\x00-\x09\x0b-\x1f\x7f]" "" text)))

(defun ai-code-ghostel-image-preview--remember-source
    (file data signature)
  "Remember captured FILE with DATA and SIGNATURE in the current session."
  (let ((source (list :file (expand-file-name file)
                      :data data
                      :signature signature)))
    (setq ai-code-ghostel-image-preview--captured-sources
          (cons
           source
           (cl-remove-if
            (lambda (entry)
              (equal (plist-get entry :file) (plist-get source :file)))
            ai-code-ghostel-image-preview--captured-sources)))
    (when (> (length ai-code-ghostel-image-preview--captured-sources)
             ai-code-ghostel-image-preview--captured-source-limit)
      (setcdr
       (nthcdr
        (1- ai-code-ghostel-image-preview--captured-source-limit)
        ai-code-ghostel-image-preview--captured-sources)
       nil))))

(defun ai-code-ghostel-image-preview--capture-sources-in-tail ()
  "Capture readable local image sources from the current output tail."
  (let ((source-buffer (current-buffer))
        (session-directory ai-code-backends-infra--session-directory)
        (tail ai-code-ghostel-image-preview--output-tail))
    (with-temp-buffer
      (setq default-directory
            (file-name-as-directory
             (expand-file-name (or session-directory default-directory))))
      (setq-local ai-code-backends-infra--session-directory session-directory)
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (insert tail)
      (let ((case-fold-search t)
            (extension-regexp
             (concat "\\.\\(?:"
                     ai-code-session-link--image-extension-regexp
                     "\\)")))
        (goto-char (point-min))
        (while (re-search-forward extension-regexp nil t)
          (when-let* ((bounds
                       (ai-code-session-link--strict-image-candidate-bounds
                        (match-beginning 0) (match-end 0)))
                      (link-text
                       (buffer-substring-no-properties
                        (car bounds) (cdr bounds))))
            (catch 'captured
              (dolist (candidate
                       (ai-code-session-link--strict-image-candidates
                        (car bounds) (cdr bounds) link-text))
                (when-let* ((candidate-text
                             (plist-get candidate :link-text))
                            (file
                             (ai-code-session-link--image-preview-existing-local-file
                              candidate-text))
                            (signature
                             (ai-code-session-link--image-preview-file-signature
                              file)))
                  (let ((existing
                         (with-current-buffer source-buffer
                           (cl-find-if
                            (lambda (source)
                              (equal (plist-get source :file)
                                     (expand-file-name file)))
                            ai-code-ghostel-image-preview--captured-sources))))
                    (if (equal (plist-get existing :signature) signature)
                        (throw 'captured t)
                      (when-let* ((data
                                   (ignore-errors
                                     (ai-code-session-link--image-preview-data
                                      file))))
                        (with-current-buffer source-buffer
                          (ai-code-ghostel-image-preview--remember-source
                           file data signature))
                        (throw 'captured t)))))))))))))

(defun ai-code-ghostel-image-preview--capture-output-sources (output)
  "Capture local image bytes referenced across recent process OUTPUT chunks."
  (let* ((output (or output ""))
         (output-start
          (max 0
               (- (length output)
                  ai-code-ghostel-image-preview--output-tail-limit)))
         (old-tail ai-code-ghostel-image-preview--output-tail)
         (plain-output
          (ai-code-ghostel-image-preview--plain-output
           (substring output output-start)))
         (probe
          (concat
           (substring old-tail (max 0 (- (length old-tail) 64)))
           (substring plain-output
                      (max 0
                           (- (length plain-output)
                              ai-code-ghostel-image-preview--output-probe-limit))))))
    (setq ai-code-ghostel-image-preview--output-tail
          (concat old-tail plain-output))
    (when (> (length ai-code-ghostel-image-preview--output-tail)
             ai-code-ghostel-image-preview--output-tail-limit)
      (setq ai-code-ghostel-image-preview--output-tail
            (substring ai-code-ghostel-image-preview--output-tail
                       (- ai-code-ghostel-image-preview--output-tail-limit))))
    (when (ai-code-session-link--text-may-contain-image-reference-p probe)
      (ai-code-ghostel-image-preview--capture-sources-in-tail)
      t)))

(defun ai-code-ghostel-image-preview--cached-source (link-text)
  "Return captured source bytes matching rendered LINK-TEXT, or nil."
  (when-let* ((normalized
               (ai-code-session-link--normalize-file link-text))
              ((not (file-remote-p normalized)))
              (root
               (file-name-as-directory
                (expand-file-name
                 (or ai-code-backends-infra--session-directory
                     default-directory))))
              (candidate
               (expand-file-name normalized root)))
    (cl-find-if
     (lambda (source)
       (equal (plist-get source :file) candidate))
     ai-code-ghostel-image-preview--captured-sources)))

(defun ai-code-ghostel-image-preview-capture-output (output)
  "Capture temporary images from OUTPUT and schedule post-redraw recovery."
  (when (and ai-code-ghostel-image-preview-mode
             (ai-code-session-link--image-preview-enabled-p))
    (let* ((output (or output ""))
           (tail-width
            (min ai-code-ghostel-image-preview--output-probe-limit
                 (ai-code-session-link--recent-output-tail-width output)))
           (output-tail
            (substring output (max 0 (- (length output) tail-width))))
           (image-output-p
            (or (ai-code-ghostel-image-preview--capture-output-sources output)
                (ai-code-session-link--text-may-contain-image-reference-p
                 output-tail))))
      (when image-output-p
        (save-restriction
          (widen)
          (let* ((end (point-max))
                 (start (max (point-min) (- end tail-width))))
            (save-excursion
              (goto-char start)
              (ai-code-ghostel-image-preview--linkify-region
               (line-beginning-position) end))))
        ;; Ghostel's process filter updates its terminal model immediately but
        ;; materializes that model in the Emacs buffer on a later redraw.  The
        ;; first scan can therefore see either stale text or the live cursor
        ;; row, where previews are deliberately suppressed.  Retry against
        ;; the visible post-redraw buffer so a completed output path is not
        ;; stranded as a text-only link after the cursor moves to the input.
        (ai-code-ghostel-image-preview-schedule-buffer
         (current-buffer))))))

(defun ai-code-ghostel-image-preview--refresh-render-region ()
  "Rebuild image layout in final terminal and Emacs-visible regions."
  (dolist (region (ai-code-ghostel-image-preview--layout-regions))
    (ai-code-ghostel-image-preview--linkify-region
     (car region) (cdr region))))

(defun ai-code-ghostel-image-preview--window-pixel-anchor-state (window)
  "Return WINDOW's pixel follow state as `following' or `preserve'."
  (let* ((body-height (window-body-height window t))
         (line-height (with-selected-window window (default-line-height)))
         (vscroll (max 0 (or (window-vscroll window t) 0)))
         (limit (and (> body-height 0) (> line-height 0)
                     (+ body-height line-height vscroll)))
         (size
          (and limit
               (ignore-errors
                 (window-text-pixel-size
                  window (window-start window) (point-max) nil (1+ limit))))))
    (if (and (consp size)
             (numberp (cdr size))
             (<= (cdr size) limit))
        'following
      'preserve)))

(defun ai-code-ghostel-image-preview--window-at-live-cursor-p (window)
  "Return non-nil when WINDOW point tracks Ghostel's live cursor."
  (when (and (window-live-p window)
             (boundp 'ghostel--cursor-char-pos)
             (integer-or-marker-p ghostel--cursor-char-pos))
    (let ((cursor (if (markerp ghostel--cursor-char-pos)
                      (marker-position ghostel--cursor-char-pos)
                    ghostel--cursor-char-pos)))
      (and cursor
           (= (window-point window)
              (max (point-min) (min (point-max) cursor)))))))

(defun ai-code-ghostel-image-preview--capture-window-state (window)
  "Capture semantic and pixel state for WINDOW."
  (let ((intent
         (cond
          ((memq window ai-code-ghostel-image-preview--user-scrolled-windows)
           'user-scroll)
          ((ai-code-ghostel-image-preview--window-at-live-cursor-p window)
           'following)
          (t
           (ai-code-ghostel-image-preview--window-pixel-anchor-state window)))))
    (list :window window
          :start (copy-marker (window-start window))
          :point (copy-marker (window-point window))
          :vscroll (window-vscroll window t)
          ;; Only following windows use this extent check.  Avoid traversing
          ;; a large scrollback tail for a window the user asked us to freeze.
          :tail-lines (and (eq intent 'following)
                           (count-lines (window-start window) (point-max)))
          :intent intent)))

(defun ai-code-ghostel-image-preview--set-window-vscroll
    (window vscroll)
  "Set WINDOW pixel VSCROLL while preserving it when Emacs supports that."
  (let ((max-args (cdr (subr-arity (symbol-function 'set-window-vscroll)))))
    (if (or (eq max-args 'many)
            (and (integerp max-args) (>= max-args 4)))
        (set-window-vscroll window vscroll t t)
      (set-window-vscroll window vscroll t))))

(defun ai-code-ghostel-image-preview--anchor-window (window)
  "Anchor WINDOW to the final display geometry at live terminal output."
  (let* ((target (point-max))
         (body-height (window-body-height window t))
         (max-args
          (cdr (subr-arity (symbol-function 'window-text-pixel-size))))
         (size
          (and (> body-height 0)
               (integerp max-args)
               (>= max-args 7)
               (ignore-errors
                 (window-text-pixel-size
                  window (cons target (- body-height)) target nil nil))))
         (start (and (consp size) (nth 2 size))))
    (if (integer-or-marker-p start)
        (progn
          (set-window-start window start)
          (ai-code-ghostel-image-preview--set-window-vscroll
           window (max 0 (- (nth 1 size) body-height))))
      (with-selected-window window
        (goto-char target)
        (forward-line (- (floor (window-screen-lines))))
        (set-window-start window (point))
        (ai-code-ghostel-image-preview--set-window-vscroll window 0)))))

(defun ai-code-ghostel-image-preview--restore-exact-window-state (state)
  "Restore the exact viewport captured in window STATE."
  (let ((window (plist-get state :window)))
    (when-let* ((start (marker-position (plist-get state :start))))
      (set-window-start window start))
    (ai-code-ghostel-image-preview--set-window-vscroll
     window (or (plist-get state :vscroll) 0))))

(defun ai-code-ghostel-image-preview--restore-window-state
    (state layout-changed)
  "Restore a window from transaction STATE.
LAYOUT-CHANGED is non-nil when preview geometry changed semantically."
  (let ((window (plist-get state :window)))
    (when (and (window-live-p window)
               (eq (window-buffer window) (current-buffer)))
      (pcase (plist-get state :intent)
        ('following
         (if (or layout-changed
                 (/= (plist-get state :tail-lines)
                     (count-lines
                      (or (marker-position (plist-get state :start))
                          (point-min))
                      (point-max))))
             (ai-code-ghostel-image-preview--anchor-window window)
           ;; In-place terminal animation did not change vertical extent.
           ;; Re-anchoring a jumbo display line here can round to a slightly
           ;; different pixel offset and create the visible 2-3px shimmer.
           (ai-code-ghostel-image-preview--restore-exact-window-state state)))
        (_
         (ai-code-ghostel-image-preview--restore-exact-window-state state)))
      ;; Set the point after geometry restoration because the pixel-anchor
      ;; fallback temporarily moves point while calculating WINDOW's start.
      (when-let* ((point (marker-position (plist-get state :point))))
        (set-window-point window point)))))

(defun ai-code-ghostel-image-preview--capture-final-window-points (states)
  "Retarget window point markers in STATES to their current positions."
  (dolist (state states)
    (let ((window (plist-get state :window)))
      (when (and (window-live-p window)
                 (eq (window-buffer window) (current-buffer)))
        (when-let* ((marker (plist-get state :point))
                    ((markerp marker)))
          (set-marker marker (window-point window)))))))

(defun ai-code-ghostel-image-preview--release-window-state (state)
  "Release marker resources held by window transaction STATE."
  (dolist (key '(:start :point))
    (when-let* ((marker (plist-get state key))
                ((markerp marker)))
      (set-marker marker nil))))

(defun ai-code-ghostel-image-preview--call-atomic
    (buffer function &optional refresh keep-final-window-points)
  "Call FUNCTION as BUFFER's single image-layout transaction.
When REFRESH is non-nil, rebuild the final Ghostel render region before
restoring window geometry and allowing redisplay.  When
KEEP-FINAL-WINDOW-POINTS is non-nil, retain window points established by
FUNCTION instead of restoring their pre-transaction positions."
  (if (not (buffer-live-p buffer))
      (funcall function)
    (with-current-buffer buffer
      (if ai-code-ghostel-image-preview--transaction-active
          (funcall function)
        (let* ((ai-code-ghostel-image-preview--transaction-active t)
               (preview-state
                (ai-code-ghostel-image-preview--capture-preview-state))
               (window-states
                (mapcar #'ai-code-ghostel-image-preview--capture-window-state
                        (get-buffer-window-list buffer nil t)))
               (inhibit-redisplay t)
               completed layout-changed result)
          (unwind-protect
              (progn
                (setq result (funcall function))
                (when keep-final-window-points
                  (ai-code-ghostel-image-preview--capture-final-window-points
                   window-states))
                (setq completed t))
            (when completed
              (with-demoted-errors "AI Code image reconciliation error: %S"
                (when preview-state
                  (ai-code-ghostel-image-preview--reconcile-preview-state
                   preview-state))
                (when refresh
                  (ai-code-ghostel-image-preview--refresh-render-region))
                (setq layout-changed
                      (ai-code-ghostel-image-preview--preview-layout-changed-p
                       preview-state))))
            (dolist (state window-states)
              (unwind-protect
                  (with-demoted-errors "AI Code image window restore error: %S"
                    (ai-code-ghostel-image-preview--restore-window-state
                     state layout-changed))
                (ai-code-ghostel-image-preview--release-window-state state))))
          result)))))

(defun ai-code-ghostel-image-preview--call-transaction
    (_start _end function)
  "Call FUNCTION inside the current preview transaction."
  (ai-code-ghostel-image-preview--call-atomic
   (current-buffer) function))

(defun ai-code-ghostel-image-preview--redraw-now-around
    (original buffer &rest args)
  "Call ORIGINAL for BUFFER inside one atomic image transaction.
ARGS are forwarded unchanged."
  (if (and (buffer-live-p buffer)
           (buffer-local-value 'ai-code-ghostel-image-preview-mode buffer))
      (ai-code-ghostel-image-preview--call-atomic
       buffer (lambda () (apply original buffer args)) t t)
    (apply original buffer args)))

(defun ai-code-ghostel-image-preview--install-redraw-advice ()
  "Install the single Ghostel private redraw adapter."
  (when (and (fboundp 'ghostel--redraw-now)
             (not
              (advice-member-p
               #'ai-code-ghostel-image-preview--redraw-now-around
               'ghostel--redraw-now)))
    (advice-add 'ghostel--redraw-now :around
                #'ai-code-ghostel-image-preview--redraw-now-around)))

(defun ai-code-ghostel-image-preview--inhibit-anchor-p (window force)
  "Return non-nil when Ghostel should not anchor WINDOW.
Never veto a deliberate FORCE anchor."
  (if force
      (progn
        (setq ai-code-ghostel-image-preview--user-scrolled-windows
              (delq window
                    ai-code-ghostel-image-preview--user-scrolled-windows))
        nil)
    (ai-code-ghostel-image-preview--user-scrolled-window-p window)))

(defun ai-code-ghostel-image-preview--cancel-linkify-timers (window)
  "Cancel pending visible-preview scan timers for WINDOW."
  (when-let* ((entry
               (assq window ai-code-ghostel-image-preview--linkify-timers)))
    (dolist (timer (cdr entry))
      (when (timerp timer)
        (cancel-timer timer)))
    (setq ai-code-ghostel-image-preview--linkify-timers
          (assq-delete-all
           window ai-code-ghostel-image-preview--linkify-timers))))

(defun ai-code-ghostel-image-preview--cancel-all-linkify-timers ()
  "Cancel every visible-preview scan timer in the current buffer."
  (dolist (entry ai-code-ghostel-image-preview--linkify-timers)
    (dolist (timer (cdr entry))
      (when (timerp timer)
        (cancel-timer timer))))
  (setq ai-code-ghostel-image-preview--linkify-timers nil))

(defun ai-code-ghostel-image-preview--prune-linkify-timers ()
  "Discard pending scan entries belonging to dead windows."
  (setq ai-code-ghostel-image-preview--linkify-timers
        (cl-remove-if-not
         (lambda (entry)
           (and (consp entry) (window-live-p (car entry))))
         ai-code-ghostel-image-preview--linkify-timers)))

(defun ai-code-ghostel-image-preview--linkify-visible (buffer window)
  "Linkify the visible image-layout region of BUFFER in WINDOW."
  (when (and (buffer-live-p buffer)
             (window-live-p window)
             (eq (window-buffer window) buffer))
    (with-current-buffer buffer
      (ai-code-ghostel-image-preview--prune-linkify-timers)
      (when ai-code-ghostel-image-preview-mode
        (if (ai-code-session-link--linkify-inhibited-p buffer)
            (ai-code-ghostel-image-preview-schedule-visible-linkify
             window
             (list ai-code-session-link--linkify-inhibited-retry-delay))
          (when-let* ((region
                       (ai-code-ghostel-image-preview--visible-region window)))
            (ai-code-ghostel-image-preview--call-atomic
             buffer
             (lambda ()
               (ai-code-ghostel-image-preview--linkify-region
                (car region) (cdr region))))))))))

(defun ai-code-ghostel-image-preview-schedule-visible-linkify
    (window &optional delays)
  "Schedule image-layout linkification for WINDOW.
Optional DELAYS overrides `ai-code-ghostel-image-preview-linkify-delays'."
  (when (and (window-live-p window)
             (buffer-live-p (window-buffer window)))
    (let ((buffer (window-buffer window)))
      (with-current-buffer buffer
        (when ai-code-ghostel-image-preview-mode
          (ai-code-ghostel-image-preview--prune-linkify-timers)
          (ai-code-ghostel-image-preview--cancel-linkify-timers window)
          (let ((timers
                 (mapcar
                  (lambda (delay)
                    (run-at-time
                     delay nil
                     #'ai-code-ghostel-image-preview--linkify-visible
                     buffer window))
                  (or delays
                      ai-code-ghostel-image-preview-linkify-delays))))
            (push (cons window timers)
                  ai-code-ghostel-image-preview--linkify-timers)))))))

(defun ai-code-ghostel-image-preview-schedule-buffer
    (&optional buffer delays)
  "Schedule visible image-layout scans for BUFFER.
Optional DELAYS overrides the default scan delays."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (dolist (window (get-buffer-window-list buffer nil t))
        (ai-code-ghostel-image-preview-schedule-visible-linkify
         window delays)))))

(defun ai-code-ghostel-image-preview--window-scroll (window _display-start)
  "Record wheel intent and schedule image-layout recovery for WINDOW."
  (when (ai-code-ghostel-image-preview--wheel-event-p last-command-event)
    (if (ai-code-ghostel-image-preview--window-has-image-p window)
        (ai-code-ghostel-image-preview--note-user-scroll window)
      (ai-code-ghostel-image-preview--clear-user-scroll-at-live-end window)))
  (ai-code-ghostel-image-preview-schedule-visible-linkify
   window (list ai-code-ghostel-image-preview-scroll-linkify-delay)))

(defun ai-code-ghostel-image-preview-scroll (event)
  "Handle precision scroll EVENT in an enabled Ghostel session."
  (interactive "e")
  (let ((window (or (ai-code-ghostel-image-preview--event-window event)
                    (selected-window))))
    (when (window-live-p window)
      (ai-code-ghostel-image-preview--note-user-scroll window))
    (if (and window
             ai-code-ghostel-image-preview-prefer-ultra-scroll
             (ai-code-ghostel-image-preview--window-has-preview-p window)
             (or (fboundp 'ultra-scroll)
                 (require 'ultra-scroll nil t)))
        (let ((event
               (ai-code-ghostel-image-preview--clamp-up-event event window)))
          (if (and (featurep 'mac-win)
                   (fboundp 'ultra-scroll-mac))
              (ultra-scroll-mac event)
            (ultra-scroll event)))
      (if (fboundp 'pixel-scroll-precision)
          (pixel-scroll-precision event)
        (mwheel-scroll event)))
    (when (window-live-p window)
      (ai-code-ghostel-image-preview-schedule-visible-linkify
       window (list ai-code-ghostel-image-preview-scroll-linkify-delay)))))

(defun ai-code-ghostel-image-preview-enable ()
  "Enable stable Ghostel image previews in the current AI Code session."
  (ai-code-ghostel-image-preview--install-redraw-advice)
  (setq-local ai-code-session-link-image-preview-position-function
              #'ai-code-ghostel-image-preview--position-allowed-p)
  (setq-local ai-code-session-link-image-preview-source-function
              #'ai-code-ghostel-image-preview--cached-source)
  (setq-local ai-code-session-link-image-preview-transaction-function
              #'ai-code-ghostel-image-preview--call-transaction)
  (setq ai-code-ghostel-image-preview--captured-sources nil
        ai-code-ghostel-image-preview--output-tail "")
  (add-hook 'ghostel-inhibit-anchor-functions
            #'ai-code-ghostel-image-preview--inhibit-anchor-p nil t)
  (add-hook 'window-scroll-functions
            #'ai-code-ghostel-image-preview--window-scroll nil t)
  (add-hook 'kill-buffer-hook
            #'ai-code-ghostel-image-preview-disable nil t)
  (ai-code-ghostel-image-preview-mode 1)
  (ai-code-ghostel-image-preview-schedule-buffer
   (current-buffer)))

(defun ai-code-ghostel-image-preview-disable ()
  "Disable stable Ghostel image previews in the current buffer."
  (ai-code-ghostel-image-preview--cancel-all-linkify-timers)
  (remove-hook 'ghostel-inhibit-anchor-functions
               #'ai-code-ghostel-image-preview--inhibit-anchor-p t)
  (remove-hook 'window-scroll-functions
               #'ai-code-ghostel-image-preview--window-scroll t)
  (remove-hook 'kill-buffer-hook
               #'ai-code-ghostel-image-preview-disable t)
  (setq-local ai-code-session-link-image-preview-position-function nil)
  (setq-local ai-code-session-link-image-preview-source-function nil)
  (setq-local ai-code-session-link-image-preview-transaction-function nil)
  (setq ai-code-ghostel-image-preview--user-scrolled-windows nil
        ai-code-ghostel-image-preview--captured-sources nil
        ai-code-ghostel-image-preview--output-tail "")
  (ai-code-ghostel-image-preview-mode -1))

(defun ai-code-ghostel-image-preview-unload-function ()
  "Remove global integration installed by this module."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when ai-code-ghostel-image-preview-mode
        (ai-code-ghostel-image-preview-disable))))
  (when (and (fboundp 'ghostel--redraw-now)
             (advice-member-p
              #'ai-code-ghostel-image-preview--redraw-now-around
              'ghostel--redraw-now))
    (advice-remove 'ghostel--redraw-now
                   #'ai-code-ghostel-image-preview--redraw-now-around))
  t)

(define-obsolete-function-alias
  'ai-code-backends-infra-ghostel-schedule-visible-image-linkify
  #'ai-code-ghostel-image-preview-schedule-visible-linkify
  "1.90")

(define-obsolete-function-alias
  'ai-code-backends-infra-ghostel-schedule-visible-image-linkify-for-buffer
  #'ai-code-ghostel-image-preview-schedule-buffer
  "1.90")

(provide 'ai-code-ghostel-image-preview)
;;; ai-code-ghostel-image-preview.el ends here
