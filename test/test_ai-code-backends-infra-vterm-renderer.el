;;; test_ai-code-backends-infra-vterm-renderer.el --- Vterm batching tests -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Regression coverage for output ordering, batching costs, and explicit flushes.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-backends-infra-vterm)

(defmacro ai-code-test-vterm-renderer--with-session (&rest body)
  "Run BODY with a session buffer and deterministic render timers."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let ((buffer (current-buffer))
           (ai-code-backends-infra-vterm-anti-flicker t)
           (ai-code-backends-infra-vterm-render-delay 0.01)
           timers cancelled rendered)
       (cl-letf (((symbol-function 'process-buffer) (lambda (_) buffer))
                 ((symbol-function 'get-buffer-process) (lambda (_) 'process))
                 ((symbol-function 'process-live-p) (lambda (_) t))
                 ((symbol-function 'ai-code-backends-infra--session-buffer-p)
                  (lambda (_) t))
                 ((symbol-function 'run-at-time)
                  (lambda (delay repeat function &rest args)
                    (let ((timer (list delay repeat function args)))
                      (push timer timers)
                      timer)))
                 ((symbol-function 'cancel-timer)
                  (lambda (timer) (push timer cancelled))))
         (let ((render (lambda (_process data) (push data rendered))))
           ,@body)))))

(ert-deftest ai-code-test-vterm-renderer-plain-text-skips-redraw-analysis ()
  "Plain output should render without redraw counting or string splitting."
  (ai-code-test-vterm-renderer--with-session
    (cl-letf (((symbol-function 'cl-count)
               (lambda (&rest _) (ert-fail "Unexpected redraw counting")))
              ((symbol-function 'split-string)
               (lambda (&rest _) (ert-fail "Unexpected redraw splitting"))))
      (ai-code-backends-infra--vterm-smart-renderer
       render 'process "plain streaming output λ\n"))
    (should (equal rendered '("plain streaming output λ\n")))
    (should-not timers)
    (should-not ai-code-backends-infra--vterm-render-queue)))

(ert-deftest ai-code-test-vterm-renderer-active-batch-skips-redraw-analysis ()
  "Output joining a pending batch needs no further redraw classification."
  (ai-code-test-vterm-renderer--with-session
    (ai-code-backends-infra--vterm-smart-renderer render 'process "\e[2Kfirst")
    (cl-letf (((symbol-function 'cl-count)
               (lambda (&rest _) (ert-fail "Unexpected redraw counting")))
              ((symbol-function 'split-string)
               (lambda (&rest _) (ert-fail "Unexpected redraw splitting"))))
      (ai-code-backends-infra--vterm-smart-renderer render 'process "\e[2Knext"))
    (should-not rendered)))

(ert-deftest ai-code-test-vterm-renderer-keeps-existing-redraw-classification ()
  "Preserve existing classification of text and terminal control sequences."
  (dolist (case '(("" . nil)
                  ("hello\r\n" . nil)
                  ("progress\r" . nil)
                  ("\e[31mred\e[0m" . t)
                  ("progress\rnext\r" . t)
                  ("\e[2Kredraw" . t)
                  ("\e[1Aprevious line" . t)))
    (ai-code-test-vterm-renderer--with-session
      (ai-code-backends-infra--vterm-smart-renderer render 'process (car case))
      (if (cdr case)
          (progn
            (should timers)
            (should-not rendered))
        (should-not timers)
        (should (equal rendered (list (car case))))))))

(ert-deftest ai-code-test-vterm-renderer-enqueue-does-not-concatenate ()
  "Appending chunks should not copy the growing pending output string."
  (ai-code-test-vterm-renderer--with-session
    (ai-code-backends-infra--vterm-smart-renderer render 'process "\e[2Kfirst")
    (cl-letf (((symbol-function 'concat)
               (lambda (&rest _) (ert-fail "Copied pending output at enqueue"))))
      (ai-code-backends-infra--vterm-smart-renderer render 'process "second")
      (ai-code-backends-infra--vterm-smart-renderer render 'process "third"))
    (should-not rendered)))

(ert-deftest ai-code-test-vterm-renderer-preserves-debounce-and-output-order ()
  "Restart the existing debounce and flush every chunk in arrival order."
  (ai-code-test-vterm-renderer--with-session
    (dolist (chunk '("\r\r\e[" "2Kfirst λ" "\e[31mthird"))
      (ai-code-backends-infra--vterm-smart-renderer render 'process chunk))
    (should-not rendered)
    (should (= (length timers) 3))
    (should (equal cancelled (cdr timers)))
    (dolist (timer timers)
      (should (= (nth 0 timer) 0.01))
      (should-not (nth 1 timer)))
    (let ((timer (car timers)))
      (apply (nth 2 timer) (nth 3 timer)))
    (should (equal rendered '("\r\r\e[2Kfirst λ\e[31mthird")))
    (should-not ai-code-backends-infra--vterm-render-queue)
    (should-not ai-code-backends-infra--vterm-render-timer)))

(ert-deftest ai-code-test-vterm-renderer-accepts-pending-string-queue ()
  "Output pending before a package reload should survive enqueue and flush."
  (ai-code-test-vterm-renderer--with-session
    (setq ai-code-backends-infra--vterm-render-queue "legacy output")
    (ai-code-backends-infra--vterm-smart-renderer render 'process " then new")
    (ai-code-backends-infra--vterm-render-queued-output render (current-buffer))
    (should (equal rendered '("legacy output then new")))
    (should-not ai-code-backends-infra--vterm-render-queue)))

(ert-deftest ai-code-test-vterm-renderer-explicit-flush-is-immediate-and-once ()
  "Manual flush cancels the timer and must not batch the same redraw again."
  (ai-code-test-vterm-renderer--with-session
    (cl-letf (((symbol-function 'vterm--filter)
               (lambda (process data)
                 (ai-code-backends-infra--vterm-smart-renderer
                  render process data))))
      (ai-code-backends-infra--vterm-smart-renderer render 'process "\e[2Kfirst")
      (ai-code-backends-infra--vterm-smart-renderer render 'process "second")
      (ai-code-backends-infra-vterm-flush-render-queue)
      (should (equal rendered '("\e[2Kfirstsecond")))
      (should (equal cancelled timers))
      (should-not ai-code-backends-infra--vterm-render-queue)
      (should-not ai-code-backends-infra--vterm-render-timer)
      (let ((timer (car timers)))
        (apply (nth 2 timer) (nth 3 timer)))
      (should (equal rendered '("\e[2Kfirstsecond"))))))

(ert-deftest ai-code-test-vterm-renderer-copy-mode-exit-flushes-immediately ()
  "Copy-mode exit cancels the timer and renders redraws without requeueing."
  (ai-code-test-vterm-renderer--with-session
    (setq-local vterm-copy-mode t)
    (cl-letf (((symbol-function 'vterm--filter)
               (lambda (process data)
                 (ai-code-backends-infra--vterm-smart-renderer
                  render process data))))
      (ai-code-backends-infra--vterm-smart-renderer render 'process "\e[2Kfirst")
      (ai-code-backends-infra--vterm-flush-on-copy-mode-exit)
      (should-not rendered)
      (should-not cancelled)
      (setq vterm-copy-mode nil)
      (ai-code-backends-infra--vterm-flush-on-copy-mode-exit)
      (should (equal rendered '("\e[2Kfirst")))
      (should (= (length timers) 1))
      (should (equal cancelled timers))
      (should-not ai-code-backends-infra--vterm-render-queue)
      (should-not ai-code-backends-infra--vterm-render-timer))))

(ert-deftest ai-code-test-vterm-renderer-flush-skips-dead-process ()
  "Queued chunks are cleared without rendering when the process has exited."
  (ai-code-test-vterm-renderer--with-session
    (ai-code-backends-infra--vterm-smart-renderer render 'process "\e[2Kfirst")
    (ai-code-backends-infra--vterm-smart-renderer render 'process "second")
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
              ((symbol-function 'vterm--filter)
               (lambda (&rest _) (ert-fail "Rendered a dead process"))))
      (ai-code-backends-infra-vterm-flush-render-queue))
    (should (equal cancelled timers))
    (should-not ai-code-backends-infra--vterm-render-queue)
    (should-not ai-code-backends-infra--vterm-render-timer)))

(ert-deftest ai-code-test-vterm-renderer-disabled-and-other-buffers-pass-through ()
  "Disabling batching or using a non-session buffer passes output untouched."
  (ai-code-test-vterm-renderer--with-session
    (let ((ai-code-backends-infra-vterm-anti-flicker nil))
      (ai-code-backends-infra--vterm-smart-renderer render 'process "\e[2Kfirst"))
    (cl-letf (((symbol-function 'ai-code-backends-infra--session-buffer-p)
               (lambda (_) nil)))
      (ai-code-backends-infra--vterm-smart-renderer render 'process "\e[2Knext"))
    (should (equal (reverse rendered) '("\e[2Kfirst" "\e[2Knext")))
    (should-not timers)
    (should-not ai-code-backends-infra--vterm-render-queue)))

(provide 'test_ai-code-backends-infra-vterm-renderer)

;;; test_ai-code-backends-infra-vterm-renderer.el ends here
