;;; test_ai-code-opencode.el --- Tests for ai-code-opencode.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-opencode module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-opencode)

(ert-deftest ai-code-test-opencode-start-uses-generic-helper ()
  "Opencode startup should delegate session setup to the shared helper."
  (let (captured-options
        captured-arg)
    (cl-letf (((symbol-function 'ai-code-backends-infra--start-cli-session)
               (lambda (options arg)
                 (setq captured-options options
                       captured-arg arg)))
              ((symbol-function 'ai-code-backends-infra--session-working-directory)
               (lambda () "/tmp/test"))
              ((symbol-function 'ai-code-backends-infra--resolve-start-command)
               (lambda (&rest _args) '(:command "opencode --debug")))
              ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
               (lambda (&rest _args) nil)))
      (let ((ai-code-opencode-program "opencode-test")
            (ai-code-opencode-program-switches '("--debug"))
            (ai-code-opencode-extra-env-vars '("OTUI_USE_ALTERNATE_SCREEN=main-screen")))
        (ai-code-opencode 'prefix-arg)))
    (should (eq captured-arg 'prefix-arg))
    (should (equal (plist-get captured-options :program) "opencode-test"))
    (should (equal (plist-get captured-options :switches) '("--debug")))
    (should (equal (plist-get captured-options :label) "Opencode"))
    (should (eq (plist-get captured-options :process-table)
                ai-code-opencode--processes))
    (should (equal (plist-get captured-options :session-prefix) "opencode"))
    (should (equal (plist-get captured-options :env-vars)
                   '("OTUI_USE_ALTERNATE_SCREEN=main-screen")))))

(provide 'test_ai-code-opencode)

;;; test_ai-code-opencode.el ends here
