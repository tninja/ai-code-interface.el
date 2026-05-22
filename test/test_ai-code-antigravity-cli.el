;;; test_ai-code-antigravity-cli.el --- Tests for ai-code-antigravity-cli.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-antigravity-cli module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-antigravity-cli)

(ert-deftest ai-code-test-antigravity-cli-start-uses-generic-helper ()
  "Antigravity startup should delegate session setup to the shared helper."
  (let (captured-options
        captured-arg)
    (cl-letf (((symbol-function 'ai-code-backends-infra--start-cli-session)
               (lambda (options arg)
                 (setq captured-options options
                       captured-arg arg)))
              ((symbol-function 'ai-code-backends-infra--session-working-directory)
               (lambda () "/tmp/test"))
              ((symbol-function 'ai-code-backends-infra--resolve-start-command)
               (lambda (&rest _args) '(:command "agy --debug")))
              ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
               (lambda (&rest _args) nil)))
      (let ((ai-code-antigravity-cli-program "agy-test")
            (ai-code-antigravity-cli-program-switches '("--debug")))
        (ai-code-antigravity-cli 'prefix-arg)))
    (should (eq captured-arg 'prefix-arg))
    (should (equal (plist-get captured-options :program) "agy-test"))
    (should (equal (plist-get captured-options :switches) '("--debug")))
    (should (equal (plist-get captured-options :label) "Antigravity"))
    (should (eq (plist-get captured-options :process-table)
                ai-code-antigravity-cli--processes))
    (should (equal (plist-get captured-options :session-prefix) "antigravity"))
    (should (eq (plist-get captured-options :escape-function)
                #'ai-code-antigravity-cli-send-escape))))

(provide 'test_ai-code-antigravity-cli)

;;; test_ai-code-antigravity-cli.el ends here
