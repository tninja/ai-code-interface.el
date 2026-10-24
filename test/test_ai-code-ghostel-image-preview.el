;;; test_ai-code-ghostel-image-preview.el --- Ghostel image lifecycle tests -*- lexical-binding: t; -*-

;; Author: realazy (cxa)
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the Ghostel image-preview lifecycle module.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ai-code-ghostel-image-preview)

(defvar ai-code-backends-infra--session-directory)
(defvar ai-code-backends-infra--session-terminal-backend)
(defvar ai-code-session-link-image-preview-position-function)
(defvar ai-code-session-link-image-preview-source-function)
(defvar ai-code-session-link-image-preview-transaction-function)
(defvar ai-code-ghostel-image-preview--output-probe-limit)
(defvar ai-code-ghostel-image-preview--output-tail-limit)
(defvar ghostel-inhibit-anchor-functions)
(defvar ghostel--cursor-char-pos)

(declare-function ghostel--redraw-now "ghostel" (buffer &optional force))
(declare-function ai-code-ghostel-image-preview--capture-output-sources
                  "ai-code-ghostel-image-preview" (output))

(ert-deftest test-ai-code-ghostel-image-preview--render-region-keeps-full-viewport ()
  "A known Ghostel viewport must not be truncated by the fallback scan limit."
  (with-temp-buffer
    (let ((ai-code-ghostel-image-preview-linkify-max-chars 10))
      (insert "image.png\n")
      (insert (make-string 100 ?x))
      (cl-letf (((symbol-function 'ghostel--viewport-start)
                 (lambda () (point-min))))
        (should
         (equal (ai-code-ghostel-image-preview--render-region)
                (cons (point-min) (point-max))))))))

(ert-deftest test-ai-code-ghostel-image-preview--input-row-allows-unbound-cursor ()
  "Input-row filtering should tolerate Ghostel not binding its cursor yet."
  (with-temp-buffer
    (let ((was-bound (boundp 'ghostel--cursor-char-pos))
          (saved (and (boundp 'ghostel--cursor-char-pos)
                      ghostel--cursor-char-pos)))
      (unwind-protect
          (progn
            (makunbound 'ghostel--cursor-char-pos)
            (should
             (ai-code-ghostel-image-preview--position-allowed-p
              (point-min) (point-max))))
        (when was-bound
          (set 'ghostel--cursor-char-pos saved))))))

(ert-deftest test-ai-code-ghostel-image-preview--completion-menu-not-allowed ()
  "File names completed below the input row should not be previewed."
  (with-temp-buffer
    (insert "Saved shot.png\n"
            "❯ @sh\n"
            "  shot.png\n"
            "  sheet.png\n")
    (goto-char (point-min))
    (forward-line 1)
    (setq-local ghostel--cursor-char-pos (line-end-position))
    (cl-flet ((allowed-p (text)
                (goto-char (point-min))
                (search-forward text)
                (ai-code-ghostel-image-preview--position-allowed-p
                 (match-beginning 0) (match-end 0))))
      (should (allowed-p "shot.png"))
      (should-not (allowed-p "@sh"))
      (should-not (allowed-p "  shot.png"))
      (should-not (allowed-p "sheet.png")))))

(ert-deftest test-ai-code-ghostel-image-preview--toggle-disables-and-enables-sessions ()
  "Toggling should clear previews in Ghostel sessions, then re-enable them."
  (let ((session (generate-new-buffer " *ai-code-toggle-session*"))
        (other (generate-new-buffer " *ai-code-toggle-other*"))
        (ai-code-session-link-ghostel-image-preview-enabled t)
        enabled disabled)
    (unwind-protect
        (cl-letf (((symbol-function 'display-images-p)
                   (lambda (&optional _display) t))
                  ((symbol-function 'ai-code-ghostel-image-preview-enable)
                   (lambda ()
                     (push (current-buffer) enabled)
                     (setq-local ai-code-ghostel-image-preview-mode t)))
                  ((symbol-function 'ai-code-ghostel-image-preview-disable)
                   (lambda ()
                     (push (current-buffer) disabled)
                     (setq-local ai-code-ghostel-image-preview-mode nil)))
                  ((symbol-function 'message) #'ignore))
          (dolist (buffer (list session other))
            (with-current-buffer buffer
              (insert "Saved shot.png\n")
              (overlay-put (make-overlay (point-min) (point-max))
                           'ai-code-session-image-preview t)))
          (with-current-buffer session
            (setq-local ai-code-backends-infra--session-terminal-backend
                        'ghostel)
            (setq-local ai-code-ghostel-image-preview-mode t))
          (ai-code-toggle-image-preview)
          (should-not ai-code-session-link-ghostel-image-preview-enabled)
          (should (equal disabled (list session)))
          (with-current-buffer session
            (should-not (overlays-in (point-min) (point-max))))
          (with-current-buffer other
            (should (overlays-in (point-min) (point-max))))
          (ai-code-toggle-image-preview)
          (should ai-code-session-link-ghostel-image-preview-enabled)
          (should (equal enabled (list session))))
      (kill-buffer session)
      (kill-buffer other))))

(ert-deftest test-ai-code-ghostel-image-preview--toggle-keeps-captured-sources ()
  "Toggling off and on should keep images captured from deleted files."
  (let ((session (generate-new-buffer " *ai-code-toggle-session*"))
        (ai-code-session-link-ghostel-image-preview-enabled t)
        (sources (list (list :file "/tmp/shot.png"
                             :data "PNG"
                             :signature '(3 . 0)))))
    (unwind-protect
        (cl-letf (((symbol-function 'display-images-p)
                   (lambda (&optional _display) t))
                  ((symbol-function 'ai-code-ghostel-image-preview-enable)
                   (lambda ()
                     (setq ai-code-ghostel-image-preview--captured-sources nil)
                     (setq-local ai-code-ghostel-image-preview-mode t)))
                  ((symbol-function 'ai-code-ghostel-image-preview-disable)
                   (lambda ()
                     (setq ai-code-ghostel-image-preview--captured-sources nil)
                     (setq-local ai-code-ghostel-image-preview-mode nil)))
                  ((symbol-function 'message) #'ignore))
          (with-current-buffer session
            (setq-local ai-code-backends-infra--session-terminal-backend
                        'ghostel)
            (setq-local ai-code-ghostel-image-preview-mode t)
            (setq ai-code-ghostel-image-preview--captured-sources sources))
          (ai-code-toggle-image-preview)
          (with-current-buffer session
            (should-not ai-code-ghostel-image-preview-mode)
            (should (equal ai-code-ghostel-image-preview--captured-sources
                           sources)))
          (ai-code-toggle-image-preview)
          (with-current-buffer session
            (should ai-code-ghostel-image-preview-mode)
            (should (equal ai-code-ghostel-image-preview--captured-sources
                           sources))))
      (kill-buffer session))))

(ert-deftest test-ai-code-ghostel-image-preview--recovery-scans-images-only ()
  "Visible recovery must not run the general session-link regex pipeline."
  (let (generic-called strict-called)
    (cl-letf (((symbol-function
                'ai-code-session-link--linkify-session-region)
               (lambda (_start _end)
                 (setq generic-called t)))
              ((symbol-function
                'ai-code-session-link--linkify-strict-image-preview-region)
               (lambda (_start _end)
                 (setq strict-called t)))
              ((symbol-function
                'ai-code-session-link--linkify-inhibited-p)
               (lambda (&optional _buffer) nil)))
      (ai-code-ghostel-image-preview--linkify-region 1 2))
    (should-not generic-called)
    (should strict-called)))

(ert-deftest test-ai-code-ghostel-image-preview--capture-output-bounds-prefilter ()
  "Synchronous image capture should normalize and inspect bounded tails."
  (let ((output
         (make-string
          (* 2 ai-code-ghostel-image-preview--output-tail-limit)
          ?x))
        normalized-width
        inspected-width)
    (with-temp-buffer
      (setq-local ai-code-ghostel-image-preview-mode t)
      (cl-letf (((symbol-function
                  'ai-code-session-link--image-preview-enabled-p)
                 (lambda () t))
                ((symbol-function
                  'ai-code-ghostel-image-preview--plain-output)
                 (lambda (text)
                   (setq normalized-width (length text))
                   text))
                ((symbol-function
                  'ai-code-session-link--text-may-contain-image-reference-p)
                 (lambda (text)
                   (setq inspected-width (length text))
                   nil)))
        (ai-code-ghostel-image-preview-capture-output output)))
    (should (<= normalized-width
                ai-code-ghostel-image-preview--output-tail-limit))
    (should
     (<= inspected-width
         ai-code-ghostel-image-preview--output-probe-limit))))

(ert-deftest test-ai-code-ghostel-image-preview--capture-output-skips-remote-session ()
  "Remote output must not make image preview read local screenshot paths."
  (let (captured scheduled)
    (with-temp-buffer
      (setq-local ai-code-ghostel-image-preview-mode t)
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (setq-local ai-code-backends-infra--session-directory
                  "/ssh:example.test:/tmp/")
      (cl-letf (((symbol-function 'display-images-p)
                 (lambda (&optional _display) t))
                ((symbol-function
                  'ai-code-ghostel-image-preview--capture-output-sources)
                 (lambda (_output)
                   (setq captured t)))
                ((symbol-function
                  'ai-code-ghostel-image-preview-schedule-buffer)
                 (lambda (_buffer)
                   (setq scheduled t))))
        (ai-code-ghostel-image-preview-capture-output
         "file:///tmp/local-only.png\n")))
    (should-not captured)
    (should-not scheduled)))

(ert-deftest test-ai-code-ghostel-image-preview--plain-output-skips-stale-capture ()
  "Plain output should not rescan an old image reference on every chunk."
  (with-temp-buffer
    (setq-local ai-code-ghostel-image-preview--output-tail
                (concat "file:///tmp/old.png\n" (make-string 100 ?x)))
    (let (scanned)
      (cl-letf (((symbol-function
                  'ai-code-ghostel-image-preview--capture-sources-in-tail)
                 (lambda ()
                   (setq scanned t))))
        (should-not
         (ai-code-ghostel-image-preview--capture-output-sources "Working")))
      (should-not scanned))))

(ert-deftest test-ai-code-ghostel-image-preview--keeps-former-public-names ()
  "Existing Ghostel preview customizations and callers should keep working."
  (dolist (pair
           '((ai-code-backends-infra-ghostel-visible-image-linkify-delays
              . ai-code-ghostel-image-preview-linkify-delays)
             (ai-code-backends-infra-ghostel-visible-image-linkify-max-chars
              . ai-code-ghostel-image-preview-linkify-max-chars)
             (ai-code-backends-infra-ghostel-scroll-image-linkify-delay
              . ai-code-ghostel-image-preview-scroll-linkify-delay)))
    (should (eq (indirect-variable (car pair)) (cdr pair))))
  (dolist (pair
           '((ai-code-backends-infra-ghostel-schedule-visible-image-linkify
              . ai-code-ghostel-image-preview-schedule-visible-linkify)
             (ai-code-backends-infra-ghostel-schedule-visible-image-linkify-for-buffer
              . ai-code-ghostel-image-preview-schedule-buffer)))
    (should (eq (symbol-function (car pair)) (cdr pair)))))

(ert-deftest test-ai-code-ghostel-image-preview--enable-owns-session-hooks ()
  "Enabling the module should install its complete buffer-local interface."
  (should (fboundp 'ai-code-ghostel-image-preview-enable))
  (should (fboundp 'ai-code-ghostel-image-preview-disable))
  (with-temp-buffer
    (setq-local ghostel-inhibit-anchor-functions nil)
    (setq-local window-scroll-functions nil)
    (ai-code-ghostel-image-preview-enable)
    (should (bound-and-true-p ai-code-ghostel-image-preview-mode))
    (should (functionp ai-code-session-link-image-preview-position-function))
    (should (functionp ai-code-session-link-image-preview-source-function))
    (should (functionp ai-code-session-link-image-preview-transaction-function))
    (should (memq #'ai-code-ghostel-image-preview--inhibit-anchor-p
                  ghostel-inhibit-anchor-functions))
    (should (memq #'ai-code-ghostel-image-preview--window-scroll
                  window-scroll-functions))
    (should (eq mwheel-scroll-up-function
                #'ai-code-ghostel-image-preview--wheel-scroll-up))
    (should (eq mwheel-scroll-down-function
                #'ai-code-ghostel-image-preview--wheel-scroll-down))
    (ai-code-ghostel-image-preview-disable)
    (should-not (local-variable-p 'mwheel-scroll-up-function))
    (should-not (local-variable-p 'mwheel-scroll-down-function))
    (should-not (bound-and-true-p ai-code-ghostel-image-preview-mode))
    (should-not ai-code-session-link-image-preview-position-function)
    (should-not ai-code-session-link-image-preview-source-function)
    (should-not ai-code-session-link-image-preview-transaction-function)))

(ert-deftest test-ai-code-ghostel-image-preview--redraw-is-one-atomic-transaction ()
  "Redraw should reconcile previews and restore user scroll before redisplay."
  (should (fboundp 'ai-code-ghostel-image-preview-enable))
  (let ((buffer (generate-new-buffer " *ghostel image transaction test*"))
        preview inhibit-seen relinked original-start original-vscroll)
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (setq-local ghostel-inhibit-anchor-functions nil)
            (insert "/tmp/image.png  suffix\n")
            (dotimes (index 120)
              (insert (format "terminal row %03d\n" index)))
            (setq preview (make-overlay 1 15))
            (overlay-put preview 'ai-code-session-image-preview t)
            (overlay-put preview 'ai-code-session-image-link-text
                         "/tmp/image.png")
            (overlay-put preview 'ai-code-session-image-display-text
                         "/tmp/image.png  suffix")
            (overlay-put preview 'ai-code-session-image-display-end 23))
          (set-window-start (selected-window) 1)
          (set-window-vscroll (selected-window) 7 t)
          (setq original-start (window-start (selected-window))
                original-vscroll (window-vscroll (selected-window) t))
          (cl-letf (((symbol-function 'ghostel--viewport-start)
                     (lambda () (point-min)))
                    ((symbol-function 'ghostel--redraw-now)
                     (lambda (target &optional _force)
                       (setq inhibit-seen inhibit-redisplay)
                       (with-current-buffer target
                         (move-overlay preview (point-min) (point-min)))
                       (set-window-start (selected-window) (point-max))
                       (set-window-vscroll (selected-window) 0 t)))
                    ((symbol-function
                      'ai-code-session-link--linkify-strict-image-preview-region)
                     (lambda (_start _end)
                       (setq relinked t)
                       (let ((replacement (make-overlay 1 15)))
                         (overlay-put replacement
                                      'ai-code-session-image-preview t)
                         (overlay-put replacement
                                      'ai-code-session-image-link-text
                                      "/tmp/image.png")
                         (overlay-put replacement
                                      'ai-code-session-image-display-text
                                      "/tmp/image.png  suffix")
                         (overlay-put replacement
                                      'ai-code-session-image-display-end 23)))))
            (with-current-buffer buffer
              (ai-code-ghostel-image-preview-enable)
              (let ((last-command-event 'wheel-up))
                (run-hook-with-args 'window-scroll-functions
                                    (selected-window) original-start)))
            (ghostel--redraw-now buffer)
            (should inhibit-seen)
            (should relinked)
            (should (= (window-start (selected-window)) original-start))
            (should (= (window-vscroll (selected-window) t)
                       original-vscroll))
            (with-current-buffer buffer
              (should
               (= 1
                  (cl-count-if
                   (lambda (overlay)
                     (and (overlay-get overlay
                                       'ai-code-session-image-preview)
                          (< (overlay-start overlay) (overlay-end overlay))))
                   (overlays-in (point-min) (point-max))))))))
      (when (fboundp 'ai-code-ghostel-image-preview--redraw-now-around)
        (advice-remove 'ghostel--redraw-now
                       #'ai-code-ghostel-image-preview--redraw-now-around))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-ghostel-image-preview--redraw-discovers-new-preview ()
  "A redraw should scan its final viewport even without an old preview."
  (let ((buffer (generate-new-buffer " *ghostel new preview test*"))
        strict-called inhibit-seen)
    (unwind-protect
        (cl-letf (((symbol-function 'ghostel--viewport-start)
                   (lambda () (point-min)))
                  ((symbol-function 'ghostel--redraw-now)
                   (lambda (target &optional _force)
                     (with-current-buffer target
                       (insert "/tmp/new-image.png\n"))))
                  ((symbol-function
                    'ai-code-session-link--linkify-inhibited-p)
                   (lambda (&optional _buffer) nil))
                  ((symbol-function
                    'ai-code-session-link--linkify-strict-image-preview-region)
                   (lambda (_start _end)
                     (setq strict-called t
                           inhibit-seen inhibit-redisplay))))
          (with-current-buffer buffer
            (setq-local ghostel-inhibit-anchor-functions nil)
            (ai-code-ghostel-image-preview-enable))
          (ghostel--redraw-now buffer)
          (should strict-called)
          (should inhibit-seen))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (ai-code-ghostel-image-preview-disable))
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-ghostel-image-preview--layout-signature-includes-geometry ()
  "Preview layout signatures should include display geometry."
  (with-temp-buffer
    (insert "x")
    (let ((preview (make-overlay (point-min) (point-max))))
      (overlay-put preview 'ai-code-session-image-preview t)
      (overlay-put preview 'ai-code-session-image-display-end (point-max))
      (overlay-put preview 'ai-code-session-image-file "/tmp/image.png")
      (overlay-put preview 'ai-code-session-image-file-signature '(1 2 3))
      (overlay-put preview 'ai-code-session-image-display-text "x")
      (overlay-put preview 'ai-code-session-image-render-context
                   '(:backing-scale 1 :dimensions (100)))
      (overlay-put preview 'ai-code-session-image-vertical-margin 0)
      (let ((state
             (ai-code-ghostel-image-preview--capture-preview-state)))
        (overlay-put preview 'before-string "\n")
        (should
         (ai-code-ghostel-image-preview--preview-layout-changed-p state))
        (overlay-put preview 'before-string nil)
        (overlay-put preview 'ai-code-session-image-render-context
                     '(:backing-scale 2 :dimensions (100)))
        (should
         (ai-code-ghostel-image-preview--preview-layout-changed-p state))
        (overlay-put preview 'ai-code-session-image-render-context
                     '(:backing-scale 1 :dimensions (100)))
        (overlay-put preview 'ai-code-session-image-vertical-margin 6)
        (should
         (ai-code-ghostel-image-preview--preview-layout-changed-p state))))))

(ert-deftest test-ai-code-ghostel-image-preview--same-extent-redraw-keeps-pixels ()
  "In-place terminal animation should not re-round a following viewport."
  (let ((buffer (generate-new-buffer " *ghostel same extent test*"))
        original-start original-vscroll anchor-called)
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (insert "Working (00s)\n")
            (dotimes (index 80)
              (insert (format "terminal row %02d\n" index))))
          (set-window-start (selected-window) 1)
          (set-window-vscroll (selected-window) 5 t)
          (setq original-start (window-start (selected-window))
                original-vscroll (window-vscroll (selected-window) t))
          (cl-letf (((symbol-function
                      'ai-code-ghostel-image-preview--window-pixel-anchor-state)
                     (lambda (_window) 'following))
                    ((symbol-function
                      'ai-code-ghostel-image-preview--preview-layout-changed-p)
                     (lambda (_state) nil))
                    ((symbol-function
                      'ai-code-ghostel-image-preview--anchor-window)
                     (lambda (_window)
                       (setq anchor-called t))))
            (ai-code-ghostel-image-preview--call-atomic
             buffer
             (lambda ()
               (with-current-buffer buffer
                 (goto-char (point-min))
                 (search-forward "00")
                 (replace-match "01"))
               (set-window-start (selected-window) (point-max))
               (set-window-vscroll (selected-window) 0 t)))
            (should-not anchor-called)
            (should (= (window-start (selected-window)) original-start))
            (should (= (window-vscroll (selected-window) t)
                       original-vscroll))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-ghostel-image-preview--live-cursor-keeps-following ()
  "A live input cursor should outrank overflowing preview geometry."
  (save-window-excursion
    (with-temp-buffer
      (let ((window (selected-window)))
        (insert "output\ninput\n")
        (set-window-buffer window (current-buffer))
        (set-window-point window (point-max))
        (setq-local ghostel--cursor-char-pos (point-max))
        (cl-letf (((symbol-function
                    'ai-code-ghostel-image-preview--window-pixel-anchor-state)
                   (lambda (_window) 'preserve))
                  ;; Batch Emacs has no display, so visibility is stubbed.
                  ((symbol-function 'pos-visible-in-window-p)
                   (lambda (&rest _args) t)))
          (let ((state
                 (ai-code-ghostel-image-preview--capture-window-state
                  window)))
            (unwind-protect
                (should (eq (plist-get state :intent) 'following))
              (ai-code-ghostel-image-preview--release-window-state
               state)))
          (setq-local ai-code-ghostel-image-preview--user-scrolled-windows
                      (list window))
          (let ((state
                 (ai-code-ghostel-image-preview--capture-window-state
                  window)))
            (unwind-protect
                (should (eq (plist-get state :intent) 'user-scroll))
              (ai-code-ghostel-image-preview--release-window-state
               state))))))))

(ert-deftest test-ai-code-ghostel-image-preview--scrolled-live-cursor-preserves ()
  "A window scrolled back with the input cursor still visible is not following."
  (save-window-excursion
    (with-temp-buffer
      (let ((window (selected-window))
            cursor)
        (insert "output\n")
        (setq cursor (point))
        (insert "input\n")
        (dotimes (index 200)
          (insert (format "status row %03d\n" index)))
        (set-window-buffer window (current-buffer))
        (set-window-start window (point-min))
        (set-window-point window cursor)
        (setq-local ghostel--cursor-char-pos cursor)
        (cl-letf (((symbol-function
                    'ai-code-ghostel-image-preview--window-pixel-anchor-state)
                   (lambda (_window) 'preserve))
                  ;; The input row is on screen, the live end below it is not.
                  ((symbol-function 'pos-visible-in-window-p)
                   (lambda (&optional pos _window _partially)
                     (< pos (point-max)))))
          (let ((state
                 (ai-code-ghostel-image-preview--capture-window-state
                  window)))
            (unwind-protect
                (should (eq (plist-get state :intent) 'preserve))
              (ai-code-ghostel-image-preview--release-window-state
               state))))))))

(ert-deftest test-ai-code-ghostel-image-preview--redraw-keeps-new-terminal-cursor ()
  "Redraw must keep Ghostel's post-render terminal cursor position."
  (let ((buffer (generate-new-buffer " *ghostel cursor redraw test*"))
        working-point input-point)
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (insert "Working\n")
            (setq working-point (point-min))
            (insert "Enter a follow-up prompt\n")
            (setq input-point (line-beginning-position))
            (setq-local ai-code-ghostel-image-preview-mode t))
          (set-window-point (selected-window) working-point)
          (cl-letf (((symbol-function
                      'ai-code-ghostel-image-preview--window-pixel-anchor-state)
                     (lambda (_window) 'following))
                    ((symbol-function
                      'ai-code-ghostel-image-preview--preview-layout-changed-p)
                     (lambda (_state) nil))
                    ((symbol-function
                      'ai-code-ghostel-image-preview--refresh-render-region)
                     #'ignore))
            (ai-code-ghostel-image-preview--redraw-now-around
             (lambda (target &rest _args)
               (with-current-buffer target
                 (setq-local ghostel--cursor-char-pos input-point))
               ;; This is the point update performed by Ghostel after its
               ;; renderer moves the live terminal cursor into the input row.
               (set-window-point (selected-window) input-point))
             buffer)
            (should (= (window-point (selected-window)) input-point))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-ghostel-image-preview--local-wheel-uses-ultra-scroll ()
  "A visible local preview should use ultra-scroll without global advice."
  (let (ultra-event pixel-called scheduled)
    (cl-letf (((symbol-function 'ai-code-ghostel-image-preview--event-window)
               (lambda (_event) (selected-window)))
              ((symbol-function
                'ai-code-ghostel-image-preview--window-has-preview-p)
               (lambda (_window) t))
              ((symbol-function
                'ai-code-ghostel-image-preview--window-has-image-p)
               (lambda (_window) t))
              ((symbol-function
                'ai-code-ghostel-image-preview--clamp-up-event)
               (lambda (_event _window) 'clamped-event))
              ((symbol-function 'ultra-scroll)
               (lambda (event &optional _arg)
                 (setq ultra-event event)))
              ((symbol-function 'pixel-scroll-precision)
               (lambda (_event)
                 (setq pixel-called t)))
              ((symbol-function
                'ai-code-ghostel-image-preview-schedule-visible-linkify)
               (lambda (&rest _args)
                 (setq scheduled t))))
      (let ((ai-code-ghostel-image-preview-prefer-ultra-scroll t))
        (ai-code-ghostel-image-preview-scroll 'wheel-event))
      (should (eq ultra-event 'clamped-event))
      (should-not pixel-called)
      (should scheduled))))

(ert-deftest test-ai-code-ghostel-image-preview--local-wheel-uses-emacs-mac-scroll ()
  "A local preview should preserve emacs-mac wheel event handling."
  (let ((original-featurep (symbol-function 'featurep))
        mac-event
        ultra-called)
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &optional subfeature)
                 (if (eq feature 'mac-win)
                     t
                   (funcall original-featurep feature subfeature))))
              ((symbol-function 'ai-code-ghostel-image-preview--event-window)
               (lambda (_event) (selected-window)))
              ((symbol-function
                'ai-code-ghostel-image-preview--window-has-preview-p)
               (lambda (_window) t))
              ((symbol-function
                'ai-code-ghostel-image-preview--clamp-up-event)
               (lambda (_event _window) 'clamped-event))
              ((symbol-function 'ultra-scroll-mac)
               (lambda (event &optional _arg)
                 (setq mac-event event)))
              ((symbol-function 'ultra-scroll)
               (lambda (&rest _args)
                 (setq ultra-called t)))
              ((symbol-function
                'ai-code-ghostel-image-preview-schedule-visible-linkify)
               #'ignore))
      (let ((ai-code-ghostel-image-preview-prefer-ultra-scroll t))
        (ai-code-ghostel-image-preview-scroll 'wheel-event))
      (should (eq mac-event 'clamped-event))
      (should-not ultra-called))))

(ert-deftest test-ai-code-ghostel-image-preview--kitty-wheel-keeps-pixel-scroll ()
  "Native Kitty graphics should retain the ordinary pixel-scroll path."
  (let (pixel-event ultra-called)
    (cl-letf (((symbol-function 'ai-code-ghostel-image-preview--event-window)
               (lambda (_event) (selected-window)))
              ((symbol-function
                'ai-code-ghostel-image-preview--window-has-preview-p)
               (lambda (_window) nil))
              ((symbol-function
                'ai-code-ghostel-image-preview--window-has-image-p)
               (lambda (_window) t))
              ((symbol-function 'ultra-scroll)
               (lambda (&rest _args)
                 (setq ultra-called t)))
              ((symbol-function 'pixel-scroll-precision)
               (lambda (event)
                 (setq pixel-event event)))
              ((symbol-function
                'ai-code-ghostel-image-preview-schedule-visible-linkify)
               #'ignore))
      (ai-code-ghostel-image-preview-scroll 'kitty-wheel-event)
      (should (eq pixel-event 'kitty-wheel-event))
      (should-not ultra-called))))

(ert-deftest test-ai-code-ghostel-image-preview--mode-map-remaps-line-scroll ()
  "Line-scroll commands, including evil's, should be remapped in sessions."
  (dolist (pair
           '((scroll-up-line
              . ai-code-ghostel-image-preview-scroll-line-down)
             (evil-scroll-line-down
              . ai-code-ghostel-image-preview-scroll-line-down)
             (scroll-down-line
              . ai-code-ghostel-image-preview-scroll-line-up)
             (evil-scroll-line-up
              . ai-code-ghostel-image-preview-scroll-line-up)))
    (should (eq (lookup-key ai-code-ghostel-image-preview-mode-map
                            (vector 'remap (car pair)))
                (cdr pair)))))

(ert-deftest test-ai-code-ghostel-image-preview--line-scroll-uses-pixels-over-preview ()
  "Line scrolling over a visible preview should move by line-height pixels."
  (let (down up noted scheduled original-called)
    (cl-letf (((symbol-function
                'ai-code-ghostel-image-preview--window-has-preview-p)
               (lambda (_window) t))
              ((symbol-function
                'ai-code-ghostel-image-preview--note-user-scroll)
               (lambda (window) (setq noted window)))
              ((symbol-function 'default-line-height) (lambda () 18))
              ((symbol-function 'pixel-scroll-precision-scroll-down)
               (lambda (delta) (push delta down)))
              ((symbol-function 'pixel-scroll-precision-scroll-up)
               (lambda (delta) (push delta up)))
              ((symbol-function 'scroll-up-line)
               (lambda (&rest _args) (setq original-called t)))
              ((symbol-function 'scroll-down-line)
               (lambda (&rest _args) (setq original-called t)))
              ((symbol-function
                'ai-code-ghostel-image-preview-schedule-visible-linkify)
               (lambda (&rest _args) (setq scheduled t))))
      (let ((this-original-command 'scroll-up-line))
        (ai-code-ghostel-image-preview-scroll-line-down 3))
      (let ((this-original-command 'scroll-down-line))
        (ai-code-ghostel-image-preview-scroll-line-up 2))
      (should (equal down '(54)))
      (should (equal up '(36)))
      (should (eq noted (selected-window)))
      (should scheduled)
      (should-not original-called))))

(ert-deftest test-ai-code-ghostel-image-preview--line-scroll-runs-remapped-command ()
  "Without a visible preview, line scrolling should keep the remapped command."
  (let (calls pixel-called)
    (cl-letf (((symbol-function
                'ai-code-ghostel-image-preview--window-has-preview-p)
               (lambda (_window) nil))
              ((symbol-function 'pixel-scroll-precision-scroll-down)
               (lambda (_delta) (setq pixel-called t)))
              ((symbol-function 'pixel-scroll-precision-scroll-up)
               (lambda (_delta) (setq pixel-called t)))
              ((symbol-function 'evil-scroll-line-down)
               (lambda (count) (push (list 'evil-down count) calls)))
              ((symbol-function 'evil-scroll-line-up)
               (lambda (count) (push (list 'evil-up count) calls)))
              ((symbol-function 'scroll-up-line)
               (lambda (&optional count) (push (list 'up-line count) calls)))
              ((symbol-function 'scroll-down-line)
               (lambda (&optional count)
                 (push (list 'down-line count) calls))))
      (let ((this-original-command 'evil-scroll-line-down))
        (ai-code-ghostel-image-preview-scroll-line-down 2))
      (let ((this-original-command 'evil-scroll-line-up))
        (ai-code-ghostel-image-preview-scroll-line-up 4))
      ;; An unrelated outer command must never be re-run as the fallback.
      (let ((this-original-command 'kill-line))
        (ai-code-ghostel-image-preview-scroll-line-down 1)
        (ai-code-ghostel-image-preview-scroll-line-up 1))
      (should (equal (nreverse calls)
                     '((evil-down 2) (evil-up 4)
                       (up-line 1) (down-line 1))))
      (should-not pixel-called))))

(ert-deftest test-ai-code-ghostel-image-preview--line-scroll-clears-scrollback-at-live-end ()
  "Keyboard scrolling should resume following output once it reaches the end."
  (let (handled cleared)
    (cl-letf (((symbol-function
                'ai-code-ghostel-image-preview--scroll-lines)
               (lambda (_lines)
                 (if (eq handled 'error)
                     (signal 'end-of-buffer nil)
                   handled)))
              ((symbol-function
                'ai-code-ghostel-image-preview--clear-user-scroll-at-live-end)
               (lambda (window) (push window cleared)))
              ((symbol-function 'scroll-up-line) #'ignore)
              ((symbol-function 'scroll-down-line) #'ignore))
      (dolist (state '(t nil))
        (setq handled state)
        (ai-code-ghostel-image-preview-scroll-line-down 1)
        (ai-code-ghostel-image-preview-scroll-line-up 1))
      ;; Reaching the end of the buffer is when the window meets live output.
      (setq handled 'error)
      (should-error (ai-code-ghostel-image-preview-scroll-line-down 1)
                    :type 'end-of-buffer)
      (should (equal cleared (make-list 5 (selected-window)))))))

(ert-deftest test-ai-code-ghostel-image-preview--wheel-functions-use-pixels-over-preview ()
  "Plain `mwheel-scroll' should scroll by pixels only over a local preview."
  (let (preview pixels global-calls)
    (cl-letf (((symbol-function
                'ai-code-ghostel-image-preview--window-has-preview-p)
               (lambda (_window) preview))
              ((symbol-function
                'ai-code-ghostel-image-preview--note-user-scroll)
               #'ignore)
              ((symbol-function
                'ai-code-ghostel-image-preview-schedule-visible-linkify)
               #'ignore)
              ((symbol-function
                'ai-code-ghostel-image-preview--scroll-pixels)
               (lambda (delta) (push delta pixels)))
              ((symbol-function 'default-line-height) (lambda () 18)))
      (with-temp-buffer
        (let ((mwheel-scroll-up-function
               (lambda (&optional arg) (push (list 'up arg) global-calls)))
              (mwheel-scroll-down-function
               (lambda (&optional arg) (push (list 'down arg) global-calls))))
          (setq preview t)
          (ai-code-ghostel-image-preview--wheel-scroll-up 3)
          (ai-code-ghostel-image-preview--wheel-scroll-down 2)
          ;; A page scroll has no line count and keeps the global behavior.
          (ai-code-ghostel-image-preview--wheel-scroll-up nil)
          (setq preview nil)
          (ai-code-ghostel-image-preview--wheel-scroll-down 1))))
    (should (equal (nreverse pixels) '(54 -36)))
    (should (equal (nreverse global-calls) '((up nil) (down 1))))))

(ert-deftest test-ai-code-ghostel-image-preview--pixel-scroll-keeps-rows-whole ()
  "Pixel line scrolling should keep text rows whole around image previews."
  (let (start vscroll at-preview scroll-result log)
    (cl-flet ((run (pixels state result)
                (setq start (nth 0 state)
                      vscroll (nth 1 state)
                      at-preview (nth 2 state)
                      scroll-result result
                      log nil)
                (ai-code-ghostel-image-preview--scroll-pixels pixels)
                (nreverse log)))
      (cl-letf (((symbol-function 'window-start)
                 (lambda (&optional _window) start))
                ((symbol-function 'window-vscroll)
                 (lambda (&optional _window _pixels) vscroll))
                ((symbol-function 'default-line-height) (lambda () 18))
                ((symbol-function
                  'ai-code-ghostel-image-preview--preview-at-window-start-p)
                 (lambda (_window) at-preview))
                ((symbol-function
                  'ai-code-ghostel-image-preview--set-window-vscroll)
                 (lambda (_window value) (push (list 'vscroll value) log)))
                ((symbol-function
                  'ai-code-ghostel-image-preview--show-link-row)
                 (lambda (_window)
                   (push '(link-row) log)
                   (setq start 15 vscroll 0 at-preview nil)))
                ((symbol-function 'pixel-scroll-precision-scroll-down)
                 (lambda (delta)
                   (push (list 'down delta) log)
                   (apply (lambda (s v p) (setq start s vscroll v at-preview p))
                          scroll-result)))
                ((symbol-function 'pixel-scroll-precision-scroll-up)
                 (lambda (delta)
                   (push (list 'up delta) log)
                   (apply (lambda (s v p) (setq start s vscroll v at-preview p))
                          scroll-result))))
        ;; Leaving an image for a text row that is 12px into view.
        (should (equal (run 18 '(10 396 t) '(20 12 nil))
                       '((down 18) (vscroll 0))))
        ;; Reaching a preview skips the empty row drawn before its image.
        (should (equal (run 18 '(10 0 nil) '(20 0 t))
                       '((down 18) (vscroll 18))))
        ;; A preview may stay partly scrolled at the window start.
        (should (equal (run 18 '(20 18 t) '(20 36 t))
                       '((down 18))))
        (should (equal (run -18 '(20 100 t) '(20 82 t))
                       '((up 18))))
        ;; From the top of a preview, step onto its link row first.
        (should (equal (run -36 '(20 18 t) '(10 0 nil))
                       '((link-row) (up 18))))
        ;; Scrolling up to a preview's empty leading row shows its link row.
        (should (equal (run -18 '(20 30 t) '(20 12 t))
                       '((up 18) (link-row))))))))

(defmacro test-ai-code-ghostel-image-preview--with-preview-rows (&rest body)
  "Run BODY in a window showing five rows, with a preview after row 1.
BODY sees WINDOW, the preview OVERLAY, and ROW, a function returning the
start of a row.  `window-start' and `window-vscroll' answer from the
START and VSCROLL variables, and the line height is 18 pixels."
  (declare (indent 0))
  `(save-window-excursion
     (with-temp-buffer
       (insert "row 0\nrow 1 image.png\nrow 2\nrow 3\nrow 4\n")
       (let* ((window (selected-window))
              (row (lambda (n)
                     (save-excursion
                       (goto-char (point-min))
                       (forward-line n)
                       (point))))
              (overlay (make-overlay (1- (funcall row 2)) (funcall row 2)))
              (start (funcall row 2))
              (vscroll 0))
         (overlay-put overlay 'ai-code-session-image-preview t)
         (set-window-buffer window (current-buffer))
         (cl-letf (((symbol-function 'window-start)
                    (lambda (&optional _window) start))
                   ((symbol-function 'window-vscroll)
                    (lambda (&optional _window _pixels) vscroll))
                   ((symbol-function 'default-line-height) (lambda () 18)))
           ,@body)))))

(ert-deftest test-ai-code-ghostel-image-preview--preview-above-needs-reach ()
  "An upward scroll should enter a preview only past the preview's lower edge."
  (test-ai-code-ghostel-image-preview--with-preview-rows
    (cl-flet ((above (pixels)
                (ai-code-ghostel-image-preview--preview-above window pixels)))
      ;; Directly below the preview, any upward scroll enters it.
      (should (equal (above 1) (cons overlay 0)))
      (should (equal (above 18) (cons overlay 0)))
      ;; The hidden part of the first row comes into view first.
      (setq vscroll 10)
      (should-not (above 10))
      (should (equal (above 11) (cons overlay 0)))
      ;; One row below, the scroll must exceed that row.
      (setq start (funcall row 3) vscroll 0)
      (should-not (above 18))
      (should (equal (above 19) (cons overlay 1)))
      (setq start (funcall row 4))
      (should-not (above 36))
      (should (equal (above 54) (cons overlay 2)))
      ;; A window already on the preview has nothing to enter.
      (setq start (overlay-start overlay))
      (should-not (above 54))
      ;; Nor has a window above it.
      (setq start (funcall row 1))
      (should-not (above 54)))))

(ert-deftest test-ai-code-ghostel-image-preview--pixel-scroll-enters-preview-from-below ()
  "Scrolling up from below a preview should step onto it without measuring."
  (test-ai-code-ghostel-image-preview--with-preview-rows
    (let (log)
      (cl-letf (((symbol-function
                  'ai-code-ghostel-image-preview--preview-pixel-height)
                 (lambda (_overlay _window) 420))
                ((symbol-function 'set-window-start)
                 (lambda (_window position &optional _noforce)
                   (push (list 'start position) log)
                   (setq start position)))
                ((symbol-function
                  'ai-code-ghostel-image-preview--set-window-vscroll)
                 (lambda (_window value)
                   (push (list 'vscroll value) log)
                   (setq vscroll value)))
                ((symbol-function 'pixel-scroll-precision-scroll-up)
                 (lambda (delta) (push (list 'up delta) log)))
                ((symbol-function
                  'ai-code-ghostel-image-preview--keep-point-visible)
                 #'ignore))
        (cl-flet ((run (pixels from-row from-vscroll)
                    (setq start (funcall row from-row)
                          vscroll from-vscroll
                          log nil)
                    (ai-code-ghostel-image-preview--scroll-pixels pixels)
                    (nreverse log)))
          (let ((link-row (funcall row 1)))
            ;; Directly below, the empty row drawn for the preview's
            ;; trailing newline already counts as one line of it.
            (should (equal (run -18 2 0)
                           `((start ,link-row) (vscroll 384))))
            (should (equal (run -18 2 10)
                           `((start ,link-row) (vscroll 394))))
            ;; From further down, the rows in between come first.
            (should (equal (run -54 4 0)
                           `((start ,link-row) (vscroll 402))))
            ;; Past the whole preview, the rest is an ordinary scroll.
            (should (equal (run -500 2 0)
                           `((start ,link-row) (vscroll 0) (up 98))))
            ;; A scroll that stops below the preview is left to Emacs.
            (should (equal (run -18 3 0) '((up 18))))))))))

(ert-deftest test-ai-code-ghostel-image-preview--alignment-keeps-preview-link-row ()
  "A link row hiding part of its preview should keep its vscroll."
  (test-ai-code-ghostel-image-preview--with-preview-rows
    (let (reset)
      (cl-letf (((symbol-function
                  'ai-code-ghostel-image-preview--set-window-vscroll)
                 (lambda (_window value) (push value reset))))
        (cl-flet ((align (at-row with-vscroll)
                    (setq start (funcall row at-row)
                          vscroll with-vscroll
                          reset nil)
                    (ai-code-ghostel-image-preview--align-window-start
                     window (funcall row 3) nil)
                    reset))
          ;; On the link row, with the image partly hidden.
          (should-not (align 1 384))
          ;; The link row itself is shown whole, like any text row.
          (should (equal (align 1 6) '(0)))
          (should (equal (align 0 100) '(0))))))))

(ert-deftest test-ai-code-ghostel-image-preview--wheel-enters-preview-from-below ()
  "An upward wheel event below a preview should not reach the scroll engines."
  (let (above pixels engine)
    (cl-letf (((symbol-function 'ai-code-ghostel-image-preview--event-window)
               (lambda (_event) (selected-window)))
              ((symbol-function
                'ai-code-ghostel-image-preview--note-user-scroll)
               #'ignore)
              ((symbol-function
                'ai-code-ghostel-image-preview--window-has-preview-p)
               (lambda (_window) t))
              ((symbol-function 'ai-code-ghostel-image-preview--preview-above)
               (lambda (_window reach) (and above (cons 'overlay reach))))
              ((symbol-function 'ai-code-ghostel-image-preview--scroll-pixels)
               (lambda (delta) (push delta pixels)))
              ((symbol-function 'ultra-scroll)
               (lambda (&rest _args) (push 'ultra engine)))
              ((symbol-function 'pixel-scroll-precision)
               (lambda (&rest _args) (push 'precision engine)))
              ((symbol-function
                'ai-code-ghostel-image-preview-schedule-visible-linkify)
               #'ignore))
      (let ((ai-code-ghostel-image-preview-prefer-ultra-scroll t)
            (up '(wheel-up nil 1 1 (0 . 20.4)))
            (down '(wheel-down nil 1 1 (0 . -20.4))))
        (setq above t)
        (ai-code-ghostel-image-preview-scroll up)
        (should (equal pixels '(-20)))
        (should-not engine)
        ;; Scrolling down, or with no preview within reach, is unchanged.
        (ai-code-ghostel-image-preview-scroll down)
        (setq above nil)
        (ai-code-ghostel-image-preview-scroll up)
        (should (equal pixels '(-20)))
        (should (equal engine '(ultra ultra)))))))

(ert-deftest test-ai-code-ghostel-image-preview--wheel-enters-preview-reads-emacs-mac-delta ()
  "An emacs-mac trackpad event should enter a preview from below too."
  (let ((window (selected-window))
        pixels)
    (cl-letf (((symbol-function 'ai-code-ghostel-image-preview--preview-above)
               (lambda (_window reach) (cons 'overlay reach)))
              ((symbol-function 'ai-code-ghostel-image-preview--scroll-pixels)
               (lambda (delta) (push delta pixels))))
      (should (ai-code-ghostel-image-preview--scroll-up-onto-preview
               '(wheel-up nil 1 (:scrolling-delta-y 20.4 :phase changed))
               window))
      (should (equal pixels '(-20)))
      ;; Scrolling down is left to the scroll engines.
      (should-not (ai-code-ghostel-image-preview--scroll-up-onto-preview
                   '(wheel-down nil 1 (:scrolling-delta-y -20.4))
                   window))
      ;; A wheeled mouse reports lines, not pixels.
      (should-not (ai-code-ghostel-image-preview--scroll-up-onto-preview
                   '(wheel-up nil 1 (:delta-y 1.0))
                   window))
      (should-not (ai-code-ghostel-image-preview--scroll-up-onto-preview
                   'wheel-event window))
      (should (equal pixels '(-20))))))

(ert-deftest test-ai-code-ghostel-image-preview--line-scroll-reaches-preview-above ()
  "Line scrolling up should use pixels for a preview it is about to reveal."
  (let (above pixels)
    (cl-letf (((symbol-function
                'ai-code-ghostel-image-preview--window-has-preview-p)
               (lambda (_window) nil))
              ((symbol-function 'ai-code-ghostel-image-preview--preview-above)
               (lambda (_window reach) (and above (cons 'overlay reach))))
              ((symbol-function
                'ai-code-ghostel-image-preview--note-user-scroll)
               #'ignore)
              ((symbol-function
                'ai-code-ghostel-image-preview-schedule-visible-linkify)
               #'ignore)
              ((symbol-function 'ai-code-ghostel-image-preview--scroll-pixels)
               (lambda (delta) (push delta pixels)))
              ((symbol-function 'default-line-height) (lambda () 18)))
      (setq above t)
      (should (ai-code-ghostel-image-preview--scroll-lines -3))
      (should (equal pixels '(-54)))
      ;; A preview above the window does not concern a downward scroll.
      (should-not (ai-code-ghostel-image-preview--scroll-lines 3))
      (setq above nil)
      (should-not (ai-code-ghostel-image-preview--scroll-lines -3))
      (should (equal pixels '(-54))))))

(ert-deftest test-ai-code-ghostel-image-preview--upward-scroll-keeps-point-visible ()
  "After scrolling up, point should sit on a row the window shows whole."
  (test-ai-code-ghostel-image-preview--with-preview-rows
    (let ((last-whole-row (funcall row 3)))
      (setq start (funcall row 0))
      (cl-letf (((symbol-function 'pos-visible-in-window-p)
                 (lambda (&optional position _window _partially)
                   (<= position last-whole-row)))
                ((symbol-function 'window-end)
                 (lambda (&optional _window _update) (point-max))))
        (set-window-point window (funcall row 4))
        (ai-code-ghostel-image-preview--keep-point-visible window)
        (should (= (window-point window) last-whole-row))
        ;; A visible point stays where it is.
        (set-window-point window (funcall row 2))
        (ai-code-ghostel-image-preview--keep-point-visible window)
        (should (= (window-point window) (funcall row 2)))))))

(ert-deftest test-ai-code-ghostel-image-preview--line-scroll-declares-evil-properties ()
  "Remapped evil scrolls should not replace the last change for `.'."
  (let (declared)
    (cl-letf (((symbol-function 'evil-add-command-properties)
               (lambda (command &rest properties)
                 (push (cons command properties) declared))))
      (ai-code-ghostel-image-preview--declare-evil-commands))
    (should
     (equal (sort (mapcar #'car declared) #'string<)
            '(ai-code-ghostel-image-preview-scroll-line-down
              ai-code-ghostel-image-preview-scroll-line-up)))
    (dolist (entry declared)
      (should (equal (cdr entry) '(:repeat nil :keep-visual t))))))

(ert-deftest test-ai-code-ghostel-image-preview--anchor-veto-follows-intent ()
  "Only explicit image scrollback should veto automatic Ghostel anchoring."
  (save-window-excursion
    (with-temp-buffer
      (let ((window (selected-window)))
        (set-window-buffer window (current-buffer))
        (setq-local ai-code-ghostel-image-preview--user-scrolled-windows
                    (list window))
        (should
         (ai-code-ghostel-image-preview--inhibit-anchor-p window nil))
        (should-not
         (ai-code-ghostel-image-preview--inhibit-anchor-p window t))
        (should-not
         (ai-code-ghostel-image-preview--user-scrolled-window-p window))))))

(ert-deftest test-ai-code-ghostel-image-preview--private-advice-is-concentrated ()
  "The module should not restore the old cross-cutting advice fan-out."
  (cl-letf (((symbol-function 'ghostel--redraw-now) #'ignore))
    (unwind-protect
        (progn
          (ai-code-ghostel-image-preview--install-redraw-advice)
          (should
           (advice-member-p
            #'ai-code-ghostel-image-preview--redraw-now-around
            'ghostel--redraw-now))
          (dolist (function
                   '(ai-code-backends-infra-ghostel--pixel-scroll-precision-around
                     ai-code-backends-infra-ghostel--window-anchored-p-around
                     ai-code-backends-infra-ghostel--anchor-window-around
                     ai-code-backends-infra-ghostel--run-queued-link-detection-around
                     ai-code-backends-infra-ghostel--install-image-preview-advice))
            (should-not (fboundp function))))
      (advice-remove 'ghostel--redraw-now
                     #'ai-code-ghostel-image-preview--redraw-now-around))))

(provide 'test-ai-code-ghostel-image-preview)
;;; test_ai-code-ghostel-image-preview.el ends here
