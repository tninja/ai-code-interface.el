;;; test_ai-code.el --- Tests for ai-code.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code.el behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code)

(ert-deftest ai-code-test-set-auto-test-type-tdd-updates-suffix ()
  "Test that setting auto test type to tdd updates the suffix text."
  (let ((ai-code-auto-test-suffix "old")
        (ai-code-auto-test-type nil)
        (ai-code--tdd-test-pattern-instruction nil))
    (ai-code--apply-auto-test-type 'tdd)
    (should (string-match-p "Follow TDD principles" ai-code-auto-test-suffix))))

(ert-deftest ai-code-test-resolve-auto-test-type-for-send ()
  "Test that send-time type resolution is consistent across mode values."
  (let ((ai-code-auto-test-type 'test-after-change))
    (should (eq 'test-after-change (ai-code--resolve-auto-test-type-for-send))))
  (let ((ai-code-auto-test-type 'tdd))
    (should (eq 'tdd (ai-code--resolve-auto-test-type-for-send))))
  (let ((ai-code-auto-test-type nil))
    (should (eq nil (ai-code--resolve-auto-test-type-for-send)))))

(ert-deftest ai-code-test-resolve-auto-test-type-for-send-ask-me ()
  "Test that ask-me mode resolves by interactive per-send selection."
  (let ((ai-code-auto-test-type 'ask-me))
    (cl-letf (((symbol-function 'ai-code--read-auto-test-type-choice)
               (lambda () 'tdd)))
      (should (eq 'tdd (ai-code--resolve-auto-test-type-for-send))))))

(ert-deftest ai-code-test-read-auto-test-type-choice-allow-no-test ()
  "Test that ask choices support selecting no test run."
  (let ((ai-code--auto-test-type-ask-choices
         '(("Run tests after code change" . test-after-change)
           ("Test driven development: Write test first" . tdd)
           ("Do not run test" . nil))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "Do not run test")))
      (should (eq nil (ai-code--read-auto-test-type-choice))))))

(ert-deftest ai-code-test-resolve-auto-test-suffix-for-send-ask-me-no-test ()
  "Test that ask-me can resolve to no test suffix."
  (let ((ai-code-auto-test-type 'ask-me))
    (cl-letf (((symbol-function 'ai-code--read-auto-test-type-choice)
               (lambda () nil)))
      (should (eq nil (ai-code--resolve-auto-test-suffix-for-send))))))

(provide 'test_ai-code)

;;; test_ai-code.el ends here
