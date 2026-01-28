;;; test_ai-code-kiro-cli.el --- Tests for ai-code-kiro-cli.el -*- lexical-binding: t; -*-

;; Author: Jason Jenkins
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-kiro-cli module.

;;; Code:

(require 'ert)
(require 'ai-code-kiro-cli)

(ert-deftest ai-code-test-kiro-cli-no-undefined-variable ()
  "Test that ai-code-kiro-cli function doesn't reference undefined variables.
This test verifies the fix for the 'force-prompt' undefined variable bug."
  ;; Mock the backend infrastructure functions to avoid actually starting a process
  (cl-letf (((symbol-function 'ai-code-backends-infra--session-working-directory)
             (lambda () "/tmp/test"))
            ((symbol-function 'ai-code-backends-infra--resolve-start-command)
             (lambda (program args arg label)
               (list :command (concat program " " (mapconcat 'identity args " ")))))
            ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
             (lambda (&rest args) nil)))
    ;; This should not throw any 'void-variable' error (including 'force-prompt')
    (should (condition-case nil
                (progn
                  (ai-code-kiro-cli)
                  t)
              (void-variable nil)))))

(provide 'test_ai-code-kiro-cli)

;;; test_ai-code-kiro-cli.el ends here
