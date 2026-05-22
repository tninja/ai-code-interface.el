;;; test_ai-code-cursor-cli.el --- Tests for ai-code-cursor-cli.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-cursor-cli module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-cursor-cli)

(ert-deftest ai-code-test-cursor-cli-start-uses-generic-helper ()
  "Cursor startup should delegate generic session setup to the shared helper."
  (let (captured-options
        captured-arg)
    (cl-letf (((symbol-function 'ai-code-backends-infra--start-cli-session)
               (lambda (options arg)
                 (setq captured-options options
                       captured-arg arg)))
              ((symbol-function 'ai-code-backends-infra--session-working-directory)
               (lambda () "/tmp/test"))
              ((symbol-function 'ai-code-backends-infra--resolve-start-command)
               (lambda (&rest _args) '(:command "cursor-agent --debug")))
              ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
               (lambda (&rest _args) nil)))
      (let ((ai-code-cursor-cli-program "cursor-agent-test")
            (ai-code-cursor-cli-program-switches '("--debug")))
        (ai-code-cursor-cli 'prefix-arg)))
    (should (eq captured-arg 'prefix-arg))
    (should (equal (plist-get captured-options :program) "cursor-agent-test"))
    (should (equal (plist-get captured-options :switches) '("--debug")))
    (should (equal (plist-get captured-options :label) "Cursor"))
    (should (eq (plist-get captured-options :process-table)
                ai-code-cursor-cli--processes))
    (should (equal (plist-get captured-options :session-prefix) "cursor"))
    (should (eq (plist-get captured-options :escape-function)
                #'ai-code-cursor-cli-send-escape))))

(provide 'test_ai-code-cursor-cli)

;;; test_ai-code-cursor-cli.el ends here
