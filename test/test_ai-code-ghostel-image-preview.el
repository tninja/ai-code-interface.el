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
    (ai-code-ghostel-image-preview-disable)
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
                   (lambda (_window) 'preserve)))
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
