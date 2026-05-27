;;; test_ai-code-kilo.el --- Tests for ai-code-kilo.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-kilo module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-kilo)

(ert-deftest ai-code-test-kilo-start-uses-generic-helper ()
  "Kilo startup should delegate session setup to the shared helper."
  (let (captured-options
        captured-arg)
    (cl-letf (((symbol-function 'ai-code-backends-infra--start-cli-session)
               (lambda (options arg)
                 (setq captured-options options
                       captured-arg arg)))
              ((symbol-function 'ai-code-backends-infra--session-working-directory)
               (lambda () "/tmp/test"))
              ((symbol-function 'ai-code-backends-infra--resolve-start-command)
               (lambda (&rest _args) '(:command "kilo --debug")))
              ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
               (lambda (&rest _args) nil)))
      (let ((ai-code-kilo-program "kilo-test")
            (ai-code-kilo-program-switches '("--debug"))
            (ai-code-kilo-extra-env-vars '("OTUI_USE_ALTERNATE_SCREEN=main-screen")))
        (ai-code-kilo 'prefix-arg)))
    (should (eq captured-arg 'prefix-arg))
    (should (equal (plist-get captured-options :program) "kilo-test"))
    (should (equal (plist-get captured-options :switches) '("--debug")))
    (should (equal (plist-get captured-options :label) "Kilo"))
    (should (eq (plist-get captured-options :process-table)
                ai-code-kilo--processes))
    (should (equal (plist-get captured-options :session-prefix) "kilo"))
    (should (equal (plist-get captured-options :env-vars)
                   '("OTUI_USE_ALTERNATE_SCREEN=main-screen")))))

(provide 'test_ai-code-kilo)

;;; test_ai-code-kilo.el ends here
