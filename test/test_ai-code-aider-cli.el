;;; test_ai-code-aider-cli.el --- Tests for ai-code-aider-cli.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-aider-cli module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-aider-cli)

(ert-deftest ai-code-test-aider-cli-start-uses-generic-helper ()
  "Aider startup should delegate session setup to the shared helper."
  (let (captured-options
        captured-arg)
    (cl-letf (((symbol-function 'ai-code-backends-infra--start-cli-session)
               (lambda (options arg)
                 (setq captured-options options
                       captured-arg arg)))
              ((symbol-function 'ai-code-backends-infra--session-working-directory)
               (lambda () "/tmp/test"))
              ((symbol-function 'ai-code-backends-infra--resolve-start-command)
               (lambda (&rest _args) '(:command "aider --debug")))
              ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
               (lambda (&rest _args) nil)))
      (let ((ai-code-aider-cli-program "aider-test")
            (ai-code-aider-cli-program-switches '("--debug")))
        (ai-code-aider-cli 'prefix-arg)))
    (should (eq captured-arg 'prefix-arg))
    (should (equal (plist-get captured-options :program) "aider-test"))
    (should (equal (plist-get captured-options :switches) '("--debug")))
    (should (equal (plist-get captured-options :label) "Aider"))
    (should (eq (plist-get captured-options :process-table)
                ai-code-aider-cli--processes))
    (should (equal (plist-get captured-options :session-prefix) "aider"))
    (should (eq (plist-get captured-options :escape-function)
                #'ai-code-aider-cli-send-escape))))

(provide 'test_ai-code-aider-cli)

;;; test_ai-code-aider-cli.el ends here
