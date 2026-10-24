;;; test_ai-code-backends-infra-window.el --- Side window layout tests -*- lexical-binding: t; -*-

;; Author: AI Agent
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Exercise real window resizing without starting terminal subprocesses.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-backends-infra)

(defmacro test-ai-code-backends-infra-window--with-layout (side &rest body)
  "Evaluate BODY with a managed side window on SIDE.
Bind `window', `buffer', `other-buffer', and `frame' for BODY."
  (declare (indent 1) (debug (form body)))
  `(let* ((frame (selected-frame))
          (original-width (window-total-width (frame-root-window frame)))
          (original-height (+ (window-total-height (frame-root-window frame))
                              (window-total-height (minibuffer-window frame))))
          (ai-code-backends-infra-window-side ,side)
          (ai-code-backends-infra-window-width 90)
          (ai-code-backends-infra-window-height 20)
          (ai-code-backends-infra-use-side-window t)
          (ai-code-backends-infra-focus-on-open nil)
          (ai-code-backends-infra-etc-resize-step 5)
          (ai-code-backends-infra--last-accessed-buffer nil)
          (window-size-change-functions nil)
          (delete-frame-functions nil)
          (buffer (generate-new-buffer "*codex[window-layout-A]*"))
          (other-buffer (generate-new-buffer "*codex[window-layout-B]*")))
     (unwind-protect
         (save-window-excursion
           (let ((ignore-window-parameters t))
             (delete-other-windows))
           ;; Drain the initial terminal resize event before setting up the
           ;; synthetic frame geometry used by timer tests in batch Emacs.
           (sleep-for 0.01)
           (set-frame-size frame 160 60)
           (let ((window
                  (ai-code-backends-infra--display-buffer-in-side-window buffer)))
             ,@body))
       (ai-code-backends-infra--cancel-side-window-size-restore frame)
       (kill-buffer buffer)
       (kill-buffer other-buffer)
       (set-frame-size frame original-width original-height))))

(defun test-ai-code-backends-infra-window--assert-preserved (window)
  "Assert that WINDOW preserves its current buffer and controlled dimension."
  (let ((horizontal (memq (window-parameter window 'window-side) '(left right))))
    (should (equal (window-preserved-size window horizontal)
                   (window-body-size window horizontal t)))
    (should (window-size-fixed-p window horizontal))
    (should-not (buffer-local-value 'window-size-fixed (window-buffer window)))))

(ert-deftest test-ai-code-backends-infra-window-reuse-preserves-all-sides ()
  "Reusing each side for another session should retain size protection."
  (dolist (side '(left right top bottom))
    (test-ai-code-backends-infra-window--with-layout side
      (dolist (next-buffer (list other-buffer buffer other-buffer))
        (should (eq window
                    (ai-code-backends-infra--display-buffer-in-side-window
                     next-buffer)))
        (should (eq (window-buffer window) next-buffer))
        (set-frame-size frame 120 45)
        (set-frame-size frame 200 80)
        (should (= (if (memq side '(left right))
                       (window-body-width window)
                     (window-total-height window))
                   (if (memq side '(left right)) 90 20)))
        (test-ai-code-backends-infra-window--assert-preserved window)))))

(ert-deftest test-ai-code-backends-infra-window-body-width-excludes-margins ()
  "Body width should remain the target when the side window has margins."
  (test-ai-code-backends-infra-window--with-layout 'right
    (set-window-margins window 2 3)
    (ai-code-backends-infra--display-buffer-in-side-window buffer)
    (should (= (window-body-width window) 90))
    (should (> (window-total-width window) 90))
    (test-ai-code-backends-infra-window--assert-preserved window)))

(ert-deftest test-ai-code-backends-infra-window-explicit-resize-preserves-all-sides ()
  "Grow and shrink commands should update size protection on all sides."
  (dolist (side '(left right top bottom))
    (test-ai-code-backends-infra-window--with-layout side
      (with-current-buffer buffer
        (ai-code-backends-infra-etc-grow-panel))
      (set-frame-size frame 120 45)
      (set-frame-size frame 200 80)
      (should (= (if (memq side '(left right))
                     (window-body-width window)
                   (window-total-height window))
                 (if (memq side '(left right)) 95 25)))
      (test-ai-code-backends-infra-window--assert-preserved window)
      (with-current-buffer buffer
        (ai-code-backends-infra-etc-shrink-panel))
      (should (= (if (memq side '(left right))
                     (window-body-width window)
                   (window-total-height window))
                 (if (memq side '(left right)) 90 20)))
      (test-ai-code-backends-infra-window--assert-preserved window))))

(ert-deftest test-ai-code-backends-infra-window-visible-switch-repairs-size ()
  "Selecting an already visible session should restore its configured size."
  (test-ai-code-backends-infra-window--with-layout 'right
    (window-resize window -10 t t)
    (should (= (window-body-width window) 80))
    (cl-letf (((symbol-function 'ai-code-backends-infra--resolve-session-buffer)
               (lambda (&rest _args) buffer)))
      (ai-code-backends-infra--switch-to-session-buffer
       (buffer-name buffer) "Missing session"))
    (should (eq (selected-window) window))
    (should (= (window-body-width window) 90))
    (test-ai-code-backends-infra-window--assert-preserved window)))

(ert-deftest test-ai-code-backends-infra-window-space-recovery-retains-target ()
  "Insufficient space should clamp actual size without losing the target."
  (dolist (side '(left right top bottom))
    (test-ai-code-backends-infra-window--with-layout side
      (setq ai-code-backends-infra-window-width 300
            ai-code-backends-infra-window-height 200)
      (let* ((horizontal (memq side '(left right)))
             (target (if horizontal 300 200)))
        (ai-code-backends-infra--restore-side-window-sizes frame)
        (should (< (if horizontal
                       (window-body-width window)
                     (window-total-height window))
                   target))
        (should (= ai-code-backends-infra-window-width 300))
        (should (= ai-code-backends-infra-window-height 200))
        (test-ai-code-backends-infra-window--assert-preserved window)
        (set-frame-size frame 500 300)
        (ai-code-backends-infra--restore-side-window-sizes frame)
        (should (= (if horizontal
                       (window-body-width window)
                     (window-total-height window))
                   target))
        (test-ai-code-backends-infra-window--assert-preserved window)))))

(ert-deftest test-ai-code-backends-infra-window-respects-other-fixed-windows ()
  "Restoration should respect a neighboring buffer's fixed width."
  (test-ai-code-backends-infra-window--with-layout 'right
    (with-current-buffer (window-buffer (window-main-window frame))
      (let ((window-size-fixed 'width))
        (setq ai-code-backends-infra-window-width 110)
        (ai-code-backends-infra--restore-side-window-sizes frame)
        (should (= (window-body-width window) 90))
        (should (= ai-code-backends-infra-window-width 110))))))

(ert-deftest test-ai-code-backends-infra-window-opposing-panels-do-not-oscillate ()
  "Opposing panels should share limited space without repeated resizing."
  (test-ai-code-backends-infra-window--with-layout 'right
    (let* ((ai-code-backends-infra-window-side 'left)
           (left-window
            (ai-code-backends-infra--display-buffer-in-side-window other-buffer))
           (left-width (window-body-width left-window)))
      (should (< left-width 90))
      (dotimes (_ 3)
        (ai-code-backends-infra--restore-side-window-sizes frame)
        (should (= (window-body-width window) 90))
        (should (= (window-body-width left-window) left-width)))
      (set-frame-width frame 300)
      (ai-code-backends-infra--restore-side-window-sizes frame)
      (should (= (window-body-width window) 90))
      (should (= (window-body-width left-window) 90))
      (test-ai-code-backends-infra-window--assert-preserved window)
      (test-ai-code-backends-infra-window--assert-preserved left-window))))

(ert-deftest test-ai-code-backends-infra-window-deferred-restore-coalesces ()
  "Layout events should share a timer and restore without selecting a window."
  (test-ai-code-backends-infra-window--with-layout 'right
    (window-resize window -10 t t)
    (let ((selected (selected-window))
          (resize-function (symbol-function 'window-resize))
          (resize-count 0))
      (run-hook-with-args 'window-size-change-functions frame)
      (let ((timer (frame-parameter frame 'ai-code-backends-infra-resize-timer)))
        (should (timerp timer))
        (run-hook-with-args 'window-size-change-functions frame)
        (should (eq timer (frame-parameter
                           frame 'ai-code-backends-infra-resize-timer))))
      (cl-letf (((symbol-function 'window-resize)
                 (lambda (&rest args)
                   (cl-incf resize-count)
                   (apply resize-function args))))
        (sleep-for 0.15)
        (should (= (window-body-width window) 90))
        (should (= resize-count 1))
        (should-not (frame-parameter frame 'ai-code-backends-infra-resize-timer))
        (ai-code-backends-infra--restore-side-window-sizes frame)
        (should (= resize-count 1)))
      (should (eq (selected-window) selected))
      (test-ai-code-backends-infra-window--assert-preserved window))))

(ert-deftest test-ai-code-backends-infra-window-deferred-restore-skips-replaced-buffer ()
  "A pending timer should leave a non-session replacement buffer alone."
  (test-ai-code-backends-infra-window--with-layout 'right
    (window-resize window -10 t t)
    (ai-code-backends-infra--schedule-side-window-size-restore frame)
    (with-current-buffer other-buffer
      (rename-buffer " *unrelated-side-buffer*" t))
    (set-window-buffer window other-buffer)
    (sleep-for 0.15)
    (should (= (window-body-width window) 80))
    (should-not (frame-parameter frame 'ai-code-backends-infra-resize-timer))))

(ert-deftest test-ai-code-backends-infra-window-deferred-restore-skips-deleted-window ()
  "Deleting a panel before its timer fires should be harmless."
  (test-ai-code-backends-infra-window--with-layout 'right
    (ai-code-backends-infra--schedule-side-window-size-restore frame)
    (delete-window window)
    (sleep-for 0.15)
    (should-not (window-live-p window))
    (should-not (frame-parameter frame 'ai-code-backends-infra-resize-timer))))

(ert-deftest test-ai-code-backends-infra-window-deferred-restore-can-be-canceled ()
  "Frame cleanup should cancel its pending timer without resizing anything."
  (test-ai-code-backends-infra-window--with-layout 'right
    (window-resize window -10 t t)
    (ai-code-backends-infra--schedule-side-window-size-restore frame)
    (run-hook-with-args 'delete-frame-functions frame)
    (sleep-for 0.15)
    (should (= (window-body-width window) 80))
    (should-not (frame-parameter frame 'ai-code-backends-infra-resize-timer))))

(ert-deftest test-ai-code-backends-infra-window-deferred-restore-respects-disabled-option ()
  "Disabling side windows should stop scheduling and pending restoration."
  (test-ai-code-backends-infra-window--with-layout 'right
    (window-resize window -10 t t)
    (ai-code-backends-infra--schedule-side-window-size-restore frame)
    (setq ai-code-backends-infra-use-side-window nil)
    (sleep-for 0.15)
    (ai-code-backends-infra--schedule-side-window-size-restore frame)
    (should (= (window-body-width window) 80))
    (should-not (frame-parameter frame 'ai-code-backends-infra-resize-timer))))

(ert-deftest test-ai-code-backends-infra-window-restore-skips-unmanaged-windows ()
  "Unmanaged side windows should not acquire automatic size enforcement."
  (test-ai-code-backends-infra-window--with-layout 'right
    (set-window-parameter window 'ai-code-backends-infra-side-window nil)
    (window-resize window -10 t t)
    (ai-code-backends-infra--schedule-side-window-size-restore frame)
    (ai-code-backends-infra--restore-side-window-sizes frame)
    (should (= (window-body-width window) 80))
    (should-not (frame-parameter frame 'ai-code-backends-infra-resize-timer))))

(ert-deftest test-ai-code-backends-infra-window-restore-uses-event-frame ()
  "An event for another frame should not resize the selected frame's panel."
  (test-ai-code-backends-infra-window--with-layout 'right
    (window-resize window -10 t t)
    (let ((event-frame 'other-frame)
          inspected-frame)
      ;; Batch Emacs cannot create a second graphical frame.  Model the event
      ;; frame while keeping a real, incorrectly sized selected-frame panel.
      (cl-letf (((symbol-function 'frame-live-p)
                 (lambda (candidate) (eq candidate event-frame)))
                ((symbol-function 'ai-code-backends-infra--cancel-side-window-size-restore)
                 #'ignore)
                ((symbol-function 'window-list)
                 (lambda (&optional candidate &rest _args)
                   (setq inspected-frame candidate)
                   nil)))
        (ai-code-backends-infra--restore-side-window-sizes event-frame))
      (should (eq inspected-frame event-frame))
      (should (= (window-body-width window) 80)))))

(ert-deftest test-ai-code-backends-infra-window-restore-skips-dead-frame ()
  "A callback for a deleted frame should not inspect or resize windows."
  (cl-letf (((symbol-function 'frame-live-p) (lambda (_frame) nil))
            ((symbol-function 'window-list)
             (lambda (&rest _args) (ert-fail "Inspected a dead frame"))))
    (ai-code-backends-infra--restore-side-window-sizes 'deleted-frame)))

(provide 'test_ai-code-backends-infra-window)
;;; test_ai-code-backends-infra-window.el ends here
