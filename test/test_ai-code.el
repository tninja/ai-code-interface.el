;;; test_ai-code.el --- Tests for ai-code.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code.el behavior.

;;; Code:

(require 'ert)
(require 'ai-code)

(ert-deftest ai-code-test-set-auto-test-type-tdd-updates-suffix ()
  "Test that setting auto test type to tdd updates the suffix text."
  (let ((ai-code-auto-test-suffix "old")
        (ai-code-auto-test-type nil)
        (ai-code--tdd-test-pattern-instruction nil))
    (ai-code--apply-auto-test-type 'tdd)
    (should (string-match-p "Follow TDD principles" ai-code-auto-test-suffix))))

(provide 'test_ai-code)

;;; test_ai-code.el ends here
