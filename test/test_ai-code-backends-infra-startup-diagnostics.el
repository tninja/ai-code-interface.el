;;; test_ai-code-backends-infra-startup-diagnostics.el --- Startup integration tests -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Integration tests for terminal startup failure reporting.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-backends-infra)
(require 'ai-code-startup-diagnostics)

(ert-deftest test-ai-code-backends-infra--create-new-session-reports-early-exit ()
  "An early terminal exit should report its process and clean launch state."
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (session-key '("/tmp/project/" . "default"))
          (process-table (make-hash-table :test 'equal))
          reported
          cleanup-count)
      (setq-local default-directory "/tmp/project/")
      (cl-letf (((symbol-function 'ai-code-editor-viewport-environment)
                 (lambda (env-vars) env-vars))
                ((symbol-function
                  'ai-code-backends-infra--create-terminal-session)
                 (lambda (&rest _args)
                   (cons buffer 'fake-process)))
                ((symbol-function 'sleep-for) (lambda (&rest _args)))
                ((symbol-function 'process-live-p) (lambda (_process) nil))
                ((symbol-function 'ai-code-startup-diagnostics-report)
                 (lambda (&rest args)
                   (setq reported args))))
        (ai-code-backends-infra--create-new-session
         "*ai-code-startup-early-exit*"
         "/tmp/project/"
         "codex --flag"
         nil
         session-key
         process-table
         "default"
         "codex"
         nil
         (lambda () (setq cleanup-count (1+ (or cleanup-count 0))))
         nil nil nil nil))
      (should (= cleanup-count 1))
      (should-not (gethash session-key process-table))
      (should (equal reported
                     (list buffer 'fake-process "codex" "codex --flag"))))))

(ert-deftest test-ai-code-backends-infra--create-new-session-reports-synchronous-error ()
  "A synchronous terminal error should report its buffer and clean once."
  (with-temp-buffer
    (rename-buffer " *ai-code-synchronous-startup-failure*" t)
    (let ((buffer (current-buffer))
          (session-key '("/tmp/project/" . "default"))
          (process-table (make-hash-table :test 'equal))
          reported
          cleanup-count)
      (setq-local default-directory "/tmp/project/")
      (cl-letf (((symbol-function 'ai-code-editor-viewport-environment)
                 (lambda (env-vars) env-vars))
                ((symbol-function
                  'ai-code-backends-infra--create-terminal-session)
                 (lambda (&rest _args)
                   (error "Searching for program: missing-cli")))
                ((symbol-function 'ai-code-startup-diagnostics-report)
                 (lambda (&rest args)
                   (setq reported args))))
        (ai-code-backends-infra--create-new-session
         (buffer-name buffer)
         "/tmp/project/"
         "codex"
         nil
         session-key
         process-table
         "default"
         "codex"
         nil
         (lambda () (setq cleanup-count (1+ (or cleanup-count 0))))
         nil nil nil nil))
      (should (= cleanup-count 1))
      (should-not (gethash session-key process-table))
      (should (equal reported (list buffer nil "codex" "codex"))))))

(provide 'test_ai-code-backends-infra-startup-diagnostics)

;;; test_ai-code-backends-infra-startup-diagnostics.el ends here
