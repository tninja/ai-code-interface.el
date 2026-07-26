;;; test_ai-code-backends-infra-startup-diagnostics.el --- Startup diagnostic tests -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Focused tests for terminal startup failure diagnostics.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-backends-infra)

(ert-deftest test-ai-code-backends-infra-startup-failure-details-include-context ()
  "Startup failure details should include command, cwd, exit status, and output."
  (let ((buffer (generate-new-buffer " *ai-code-startup-failure*")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local default-directory "/tmp/example/")
          (setq-local ai-code-backends-infra--session-prefix "codex")
          (insert "initial output\nfatal: missing credential\n")
          (cl-letf (((symbol-function 'processp) (lambda (_process) t))
                    ((symbol-function 'process-command)
                     (lambda (_process) '("/usr/local/bin/codex" "--flag")))
                    ((symbol-function 'process-status) (lambda (_process) 'exit))
                    ((symbol-function 'process-exit-status) (lambda (_process) 127)))
            (let ((details
                   (ai-code-backends-infra-startup--failure-details
                    buffer 'fake-process)))
              (should (equal (plist-get details :backend) "codex"))
              (should (equal (plist-get details :executable) "/usr/local/bin/codex"))
              (should (equal (plist-get details :cwd) "/tmp/example/"))
              (should (equal (plist-get details :status) 'exit))
              (should (= (plist-get details :exit-status) 127))
              (should (string-match-p "missing credential"
                                      (plist-get details :last-output))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-startup-failure-advice-preserves-cleanup ()
  "Diagnostic advice should preserve the original startup-failure cleanup."
  (let ((buffer (generate-new-buffer " *ai-code-startup-failure-advice*"))
        (process-table (make-hash-table :test 'equal))
        original-called
        captured-message)
    (unwind-protect
        (progn
          (puthash '("/tmp/project/" . "default") 'fake-process process-table)
          (with-current-buffer buffer
            (setq-local default-directory "/tmp/project/")
            (setq-local ai-code-backends-infra--session-prefix "claude-code")
            (insert "spawn failed"))
          (cl-letf (((symbol-function 'processp) (lambda (_process) t))
                    ((symbol-function 'process-command)
                     (lambda (_process) '("/opt/bin/claude")))
                    ((symbol-function 'process-status) (lambda (_process) 'exit))
                    ((symbol-function 'process-exit-status) (lambda (_process) 126))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq captured-message
                             (apply #'format format-string args)))))
            (ai-code-backends-infra-startup--diagnose-start-failure
             (lambda (&rest _args) (setq original-called t))
             buffer
             '("/tmp/project/" . "default")
             process-table))
          (should original-called)
          (should (string-match-p "claude-code" captured-message))
          (should (string-match-p "/opt/bin/claude" captured-message))
          (should (string-match-p "/tmp/project/" captured-message))
          (should (string-match-p "126" captured-message))
          (should (string-match-p "spawn failed" captured-message)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'test_ai-code-backends-infra-startup-diagnostics)

;;; test_ai-code-backends-infra-startup-diagnostics.el ends here
