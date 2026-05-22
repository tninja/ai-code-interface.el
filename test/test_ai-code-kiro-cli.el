;;; test_ai-code-kiro-cli.el --- Tests for ai-code-kiro-cli.el -*- lexical-binding: t; -*-

;; Author: Jason Jenkins
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-kiro-cli module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-kiro-cli)

(ert-deftest ai-code-test-kiro-cli-no-undefined-variable ()
  "Test that ai-code-kiro-cli function doesn't reference undefined variables.
This test verifies the fix for the 'force-prompt' undefined variable bug."
  ;; Mock the backend infrastructure functions to avoid actually starting a process
  (cl-letf (((symbol-function 'ai-code-backends-infra--session-working-directory)
             (lambda () "/tmp/test"))
            ((symbol-function 'ai-code-backends-infra--resolve-start-command)
             (lambda (program args _arg _label)
               (list :command (concat program " " (mapconcat 'identity args " ")))))
            ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
             (lambda (&rest _args) nil)))
    ;; This should not throw any 'void-variable' error (including 'force-prompt')
    (should (condition-case nil
                (progn
                  (ai-code-kiro-cli)
                  t)
              (void-variable nil)))))

(ert-deftest ai-code-test-kiro-cli-start-uses-generic-helper ()
  "Kiro startup should delegate generic session setup to the shared helper."
  (let (captured-options
        captured-arg)
    (cl-letf (((symbol-function 'ai-code-backends-infra--start-cli-session)
               (lambda (options arg)
                 (setq captured-options options
                       captured-arg arg)))
              ((symbol-function 'ai-code-backends-infra--session-working-directory)
               (lambda () "/tmp/test"))
              ((symbol-function 'ai-code-backends-infra--resolve-start-command)
               (lambda (&rest _args) '(:command "kiro-cli chat --debug")))
              ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
               (lambda (&rest _args) nil)))
      (let ((ai-code-kiro-cli-program "kiro-cli-test")
            (ai-code-kiro-cli-program-switches '("--debug"))
            (ai-code-kiro-cli-trust-all-tools t)
            (ai-code-kiro-cli-agent "builder"))
        (ai-code-kiro-cli 'prefix-arg)))
    (should (eq captured-arg 'prefix-arg))
    (should (equal (plist-get captured-options :program) "kiro-cli-test"))
    (should (equal (plist-get captured-options :switches)
                   '("chat" "--trust-all-tools" "--agent" "builder" "--debug")))
    (should (equal (plist-get captured-options :label) "Kiro"))
    (should (eq (plist-get captured-options :process-table)
                ai-code-kiro-cli--processes))
    (should (equal (plist-get captured-options :session-prefix) "kiro"))
    (should (eq (plist-get captured-options :escape-function)
                #'ai-code-kiro-cli-send-escape))))

(provide 'test_ai-code-kiro-cli)

;;; test_ai-code-kiro-cli.el ends here
