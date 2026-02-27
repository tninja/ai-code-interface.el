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

(ert-deftest ai-code-test-resolve-auto-test-type-for-send-ask-me-gptel-non-code-change ()
  "Test that ask-me mode skips selection when GPTel classifies non-code change."
  (let ((ai-code-auto-test-type 'ask-me)
        (ai-code-use-gptel-classify-prompt t))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'non-code-change))
              ((symbol-function 'ai-code--read-auto-test-type-choice)
               (lambda () (ert-fail "Should not ask test type for non-code prompts."))))
      (should (eq nil (ai-code--resolve-auto-test-type-for-send "Explain this function"))))))

(ert-deftest ai-code-test-resolve-auto-test-type-for-send-ask-me-gptel-code-change ()
  "Test that ask-me mode prompts user to select test type when GPTel classifies code change."
  (let ((ai-code-auto-test-type 'ask-me)
        (ai-code-use-gptel-classify-prompt t))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'code-change))
              ((symbol-function 'ai-code--read-auto-test-type-choice)
               (lambda () 'test-after-change)))
      (should (eq 'test-after-change
                  (ai-code--resolve-auto-test-type-for-send "Refactor this function"))))))

(ert-deftest ai-code-test-resolve-auto-test-type-for-send-ask-me-gptel-unknown-fallback ()
  "Test that ask-me mode falls back to interactive selection when GPTel is unknown."
  (let ((ai-code-auto-test-type 'ask-me)
        (ai-code-use-gptel-classify-prompt t))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'unknown))
              ((symbol-function 'ai-code--read-auto-test-type-choice)
               (lambda () 'test-after-change)))
      (should (eq 'test-after-change
                  (ai-code--resolve-auto-test-type-for-send "Please update code"))))))

(ert-deftest ai-code-test-resolve-auto-test-type-for-send-fixed-type-gptel-classification-test-after-change ()
  "Test that test-after-change mode appends suffix only for GPTel code-change classification."
  (let ((ai-code-auto-test-type 'test-after-change)
        (ai-code-use-gptel-classify-prompt t))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'code-change)))
      (should (eq 'test-after-change
                  (ai-code--resolve-auto-test-type-for-send "Refactor this code"))))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'non-code-change)))
      (should (eq nil
                  (ai-code--resolve-auto-test-type-for-send "Explain this design"))))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'unknown)))
      (should (eq nil
                  (ai-code--resolve-auto-test-type-for-send "Do something"))))))

(ert-deftest ai-code-test-resolve-auto-test-type-for-send-fixed-type-gptel-classification-tdd ()
  "Test that tdd mode appends suffix only for GPTel code-change classification."
  (let ((ai-code-auto-test-type 'tdd)
        (ai-code-use-gptel-classify-prompt t))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'code-change)))
      (should (eq 'tdd
                  (ai-code--resolve-auto-test-type-for-send "Implement feature"))))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'non-code-change)))
      (should (eq nil
                  (ai-code--resolve-auto-test-type-for-send "Summarize this file"))))
    (cl-letf (((symbol-function 'ai-code--gptel-classify-prompt-code-change)
               (lambda (_prompt-text) 'unknown)))
      (should (eq nil
                  (ai-code--resolve-auto-test-type-for-send "Review architecture"))))))

(ert-deftest ai-code-test-read-auto-test-type-choice-allow-no-test ()
  "Test that ask choices support selecting no test run."
  (let ((ai-code--auto-test-type-ask-choices
         '(("Run tests after code change" . test-after-change)
           ("Test driven development: Write test first" . tdd)
           ("Do not run test" . no-test))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "Do not run test")))
      (should (eq 'no-test (ai-code--read-auto-test-type-choice))))))

(ert-deftest ai-code-test-resolve-auto-test-suffix-for-send-ask-me-no-test ()
  "Test that ask-me can resolve to explicit no-test suffix."
  (let ((ai-code-auto-test-type 'ask-me))
    (cl-letf (((symbol-function 'ai-code--read-auto-test-type-choice)
               (lambda () 'no-test)))
      (should (equal "Do not run any test."
                     (ai-code--resolve-auto-test-suffix-for-send))))))

(ert-deftest ai-code-test-write-prompt-ask-me-no-test-appends-explicit-no-test-instruction ()
  "Test that ask-me no-test choice appends explicit no-test instruction."
  (let ((sent-command nil)
        (ai-code-auto-test-type 'ask-me)
        (ai-code-use-prompt-suffix t)
        (ai-code-prompt-suffix "BASE SUFFIX")
        (ai-code-auto-test-suffix "SHOULD NOT APPEAR"))
    (cl-letf (((symbol-function 'ai-code--read-auto-test-type-choice)
               (lambda () 'no-test))
              ((symbol-function 'ai-code--get-ai-code-prompt-file-path)
               (lambda () nil))
              ((symbol-function 'ai-code-cli-send-command)
               (lambda (command) (setq sent-command command)))
              ((symbol-function 'ai-code-cli-switch-to-buffer)
               (lambda (&rest _args) nil)))
      (ai-code--write-prompt-to-file-and-send "Implement feature")
      (should (string-match-p "BASE SUFFIX" sent-command))
      (should (string-match-p "Do not run any test\\." sent-command))
      (should-not (string-match-p "SHOULD NOT APPEAR" sent-command)))))

(provide 'test_ai-code)

;;; test_ai-code.el ends here
