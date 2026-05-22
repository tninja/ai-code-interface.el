;;; test_ai-code-grok-cli.el --- Tests for ai-code-grok-cli.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-grok-cli module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-grok-cli)

(ert-deftest ai-code-test-grok-cli-start-uses-generic-helper ()
  "Grok startup should delegate generic session setup to the shared helper."
  (let (captured-options
        captured-arg)
    (cl-letf (((symbol-function 'ai-code-backends-infra--start-cli-session)
               (lambda (options arg)
                 (setq captured-options options
                       captured-arg arg)))
              ((symbol-function 'ai-code-backends-infra--session-working-directory)
               (lambda () "/tmp/test"))
              ((symbol-function 'ai-code-backends-infra--resolve-start-command)
               (lambda (&rest _args) '(:command "grok --debug")))
              ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
               (lambda (&rest _args) nil)))
      (let ((ai-code-grok-cli-program "grok-test")
            (ai-code-grok-cli-program-switches '("--debug")))
        (ai-code-grok-cli 'prefix-arg)))
    (should (eq captured-arg 'prefix-arg))
    (should (equal (plist-get captured-options :program) "grok-test"))
    (should (equal (plist-get captured-options :switches) '("--debug")))
    (should (equal (plist-get captured-options :label) "Grok"))
    (should (eq (plist-get captured-options :process-table)
                ai-code-grok-cli--processes))
    (should (equal (plist-get captured-options :session-prefix) "grok"))
    (should-not (plist-get captured-options :escape-function))))

(provide 'test_ai-code-grok-cli)

;;; test_ai-code-grok-cli.el ends here
