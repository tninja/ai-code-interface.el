;;; test_ai-code-codebuddy-cli.el --- Tests for ai-code-codebuddy-cli.el -*- lexical-binding: t; -*-

;; Author: CodeBuddy
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-codebuddy-cli module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-codebuddy-cli)

(ert-deftest ai-code-test-codebuddy-cli-start-uses-generic-helper ()
  "CodeBuddy startup should delegate session setup to the shared helper."
  (let (captured-options
        captured-arg)
    (cl-letf (((symbol-function 'ai-code-backends-infra--start-cli-session)
               (lambda (options arg)
                 (setq captured-options options
                       captured-arg arg)))
              ((symbol-function 'ai-code-backends-infra--session-working-directory)
               (lambda () "/tmp/test"))
              ((symbol-function 'ai-code-backends-infra--resolve-start-command)
               (lambda (&rest _args) '(:command "codebuddy --debug")))
              ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
               (lambda (&rest _args) nil)))
      (let ((ai-code-codebuddy-cli-program "codebuddy-test")
            (ai-code-codebuddy-cli-program-switches '("--debug")))
        (ai-code-codebuddy-cli 'prefix-arg)))
    (should (eq captured-arg 'prefix-arg))
    (should (equal (plist-get captured-options :program) "codebuddy-test"))
    (should (equal (plist-get captured-options :switches) '("--debug")))
    (should (equal (plist-get captured-options :label) "CodeBuddy"))
    (should (eq (plist-get captured-options :process-table)
                ai-code-codebuddy-cli--processes))
    (should (equal (plist-get captured-options :session-prefix) "codebuddy"))
    (should (eq (plist-get captured-options :escape-function)
                #'ai-code-codebuddy-cli-send-escape))))

(ert-deftest ai-code-test-codebuddy-cli-no-undefined-variable ()
  "Test that ai-code-codebuddy-cli function doesn't reference undefined variables.
This test verifies the fix for the 'force-prompt' undefined variable bug."
  (cl-letf (((symbol-function 'ai-code-backends-infra--session-working-directory)
             (lambda () "/tmp/test"))
            ((symbol-function 'ai-code-backends-infra--resolve-start-command)
             (lambda (program args _arg _label)
               (list :command (concat program " " (mapconcat 'identity args " ")))))
            ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
             (lambda (&rest _args) nil)))
    (should (condition-case nil
                (progn
                  (ai-code-codebuddy-cli)
                  t)
              (void-variable nil)))))

(provide 'test_ai-code-codebuddy-cli)

;;; test_ai-code-codebuddy-cli.el ends here
