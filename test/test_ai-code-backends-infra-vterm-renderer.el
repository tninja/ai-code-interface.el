;;; test_ai-code-backends-infra-vterm-renderer.el --- Vterm renderer tests -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Focused tests for the vterm anti-flicker renderer queue.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-backends-infra)

(ert-deftest test-ai-code-backends-infra-vterm-renderer-plain-text-fast-path ()
  "Plain output should render immediately without scheduling a batch timer."
  (with-temp-buffer
    (let ((buffer (current-buffer))
          rendered
          timer-scheduled)
      (cl-letf (((symbol-function 'process-buffer) (lambda (_process) buffer))
                ((symbol-function 'ai-code-backends-infra--session-buffer-p)
                 (lambda (_buffer) t))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                   (setq timer-scheduled t)
                   'timer)))
        (ai-code-backends-infra--vterm-smart-renderer
         (lambda (_process input) (setq rendered input))
         'process
         "plain model output"))
      (should (equal rendered "plain model output"))
      (should-not timer-scheduled)
      (should-not ai-code-backends-infra--vterm-render-queue))))

(ert-deftest test-ai-code-backends-infra-vterm-renderer-queues-chunks-in-o1-order ()
  "Redraw chunks should be pushed onto a reverse list with one timer."
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (timer-count 0))
      (cl-letf (((symbol-function 'process-buffer) (lambda (_process) buffer))
                ((symbol-function 'ai-code-backends-infra--session-buffer-p)
                 (lambda (_buffer) t))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                   (setq timer-count (1+ timer-count))
                   'timer)))
        (ai-code-backends-infra--vterm-smart-renderer #'ignore 'process "\e[2Kfirst")
        (ai-code-backends-infra--vterm-smart-renderer #'ignore 'process "second"))
      (should (= timer-count 1))
      (should (equal ai-code-backends-infra--vterm-render-queue
                     '("second" "\e[2Kfirst"))))))

(ert-deftest test-ai-code-backends-infra-vterm-renderer-flush-preserves-chunk-order ()
  "Flushing a reverse queue should concatenate chunks in arrival order."
  (with-temp-buffer
    (let ((buffer (current-buffer))
          rendered)
      (setq-local ai-code-backends-infra--vterm-render-queue
                  '("third" "second" "first"))
      (setq-local ai-code-backends-infra--vterm-render-timer 'timer)
      (cl-letf (((symbol-function 'get-buffer-process)
                 (lambda (_buffer) 'process))
                ((symbol-function 'process-live-p) (lambda (_process) t)))
        (ai-code-backends-infra--vterm-render-queued-output
         (lambda (_process input) (setq rendered input))
         buffer))
      (should (equal rendered "firstsecondthird"))
      (should-not ai-code-backends-infra--vterm-render-queue)
      (should-not ai-code-backends-infra--vterm-render-timer))))

(provide 'test_ai-code-backends-infra-vterm-renderer)

;;; test_ai-code-backends-infra-vterm-renderer.el ends here
