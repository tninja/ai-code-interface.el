;;; test_ai-code-codebuddy-cli.el --- Tests for ai-code-codebuddy-cli.el -*- lexical-binding: t; -*-

;; Author: CodeBuddy
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-codebuddy-cli module.

;;; Code:

(require 'ert)
(require 'ai-code-codebuddy-cli)

(ert-deftest ai-code-test-codebuddy-cli-no-undefined-variable ()
  "Test that ai-code-codebuddy-cli function doesn't reference undefined variables.
This test verifies the fix for the 'force-prompt' undefined variable bug."
  (cl-letf (((symbol-function 'ai-code-backends-infra--session-working-directory)
             (lambda () "/tmp/test"))
            ((symbol-function 'ai-code-backends-infra--resolve-start-command)
             (lambda (program args arg label)
               (list :command (concat program " " (mapconcat 'identity args " ")))))
            ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
             (lambda (&rest args) nil)))
    (should (condition-case nil
                (progn
                  (ai-code-codebuddy-cli)
                  t)
              (void-variable nil)))))

(provide 'test_ai-code-codebuddy-cli)
