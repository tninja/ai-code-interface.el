;;; test_ai-code.el --- Tests for ai-code.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code.el behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)

(defun ai-code-test--maybe-prefer-packaged-transient ()
  "Prefer the newest packaged Transient when one is installed."
  (let* ((pattern (expand-file-name "transient-*" package-user-dir))
         (candidates (sort (file-expand-wildcards pattern) #'version<))
         (latest (car (last candidates))))
    (when latest
      (add-to-list 'load-path latest))))

(ai-code-test--maybe-prefer-packaged-transient)

(require 'transient)

(unless (fboundp 'transient-define-group)
  (error "AI Code tests require transient-define-group; please install transient >= 0.9.0"))

(require 'ai-code)

(defvar ai-code--tdd-run-test-after-each-stage-instruction)

(ert-deftest ai-code-test-set-auto-test-type-tdd-updates-suffix ()
  "Test that setting auto test type to tdd updates the suffix text."
  (let ((ai-code-auto-test-suffix "old")
        (ai-code-auto-test-type nil)
        (ai-code--tdd-test-pattern-instruction nil))
    (ai-code--apply-auto-test-type 'tdd)
    (should (string-match-p "Stage 1 - Red" ai-code-auto-test-suffix))
    (should (string-match-p "Stage 2 - Green" ai-code-auto-test-suffix))))

(ert-deftest ai-code-test-set-auto-test-type-tdd-with-refactoring-updates-suffix ()
  "Test that setting auto test type to tdd-with-refactoring updates suffix text."
  (let ((ai-code-auto-test-suffix "old")
        (ai-code-auto-test-type nil)
        (ai-code--tdd-test-pattern-instruction ""))
    (ai-code--apply-auto-test-type 'tdd-with-refactoring)
    (should (string-match-p
             (regexp-quote ai-code--tdd-with-refactoring-extension-instruction)
             ai-code-auto-test-suffix))
    (should (string-match-p "Stage 3 - Blue" ai-code-auto-test-suffix))
    (should (string-match-p "highest-impact cleanup" ai-code-auto-test-suffix))))

(ert-deftest ai-code-test-resolve-tdd-suffix-includes-strict-stage-contract ()
  "Test that TDD suffix names Red and Green stages and forbids skipping."
  (let ((ai-code--tdd-test-pattern-instruction ""))
    (let ((suffix (ai-code--test-after-code-change--resolve-tdd-suffix)))
      (should (string-match-p "Do not skip stages" suffix))
      (should (string-match-p "Stage 1 - Red" suffix))
      (should (string-match-p "Stage 2 - Green" suffix))
      (should (string-match-p "Do not refactor during Green" suffix)))))

(ert-deftest ai-code-test-resolve-tdd-suffix-reuses-shared-each-stage-instruction ()
  "Test that TDD suffix can reuse shared each-stage instruction when available."
  (let ((ai-code--tdd-test-pattern-instruction "")
        (ai-code--tdd-run-test-after-each-stage-instruction
         " SHARED_EACH_STAGE_TEST_INSTRUCTION"))
    (should (string-match-p "SHARED_EACH_STAGE_TEST_INSTRUCTION"
                            (ai-code--test-after-code-change--resolve-tdd-suffix)))))

(ert-deftest ai-code-test-resolve-auto-test-type-for-send ()
  "Test that send-time type resolution is consistent across mode values."
  (let ((ai-code-auto-test-type 'test-after-change))
    (should (eq 'test-after-change (ai-code--resolve-auto-test-type-for-send))))
  (let ((ai-code-auto-test-type 'tdd))
    (should (eq 'tdd (ai-code--resolve-auto-test-type-for-send))))
  (let ((ai-code-auto-test-type 'tdd-with-refactoring))
    (should (eq 'tdd-with-refactoring (ai-code--resolve-auto-test-type-for-send))))
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
           ("TDD Red + Green (write failing test, then make it pass)" . tdd)
           ("TDD Red + Green + Blue (refactor after Green)" . tdd-with-refactoring)
           ("Do not write or run tests" . no-test))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "Do not write or run tests")))
      (should (eq 'no-test (ai-code--read-auto-test-type-choice))))))

(ert-deftest ai-code-test-read-auto-test-type-choice-allow-tdd-with-refactoring ()
  "Test that ask choices support selecting tdd-with-refactoring."
  (let ((ai-code--auto-test-type-ask-choices
         '(("Run tests after code change" . test-after-change)
           ("TDD Red + Green (write failing test, then make it pass)" . tdd)
           ("TDD Red + Green + Blue (refactor after Green)" . tdd-with-refactoring)
           ("Do not write or run tests" . no-test))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "TDD Red + Green + Blue (refactor after Green)")))
      (should (eq 'tdd-with-refactoring (ai-code--read-auto-test-type-choice))))))

(ert-deftest ai-code-test-auto-test-type-ask-choices-use-explicit-red-green-blue-labels ()
  "Test that default ask choices use explicit staged TDD labels."
  (should (assoc "TDD Red + Green (write failing test, then make it pass)"
                 ai-code--auto-test-type-ask-choices))
  (should (assoc "TDD Red + Green + Blue (refactor after Green)"
                 ai-code--auto-test-type-ask-choices))
  (should-not (assoc "Test driven development: Write test first"
                     ai-code--auto-test-type-ask-choices))
  (should-not (assoc "Test driven development, follow up with refactoring"
                     ai-code--auto-test-type-ask-choices)))

(ert-deftest ai-code-test-resolve-auto-test-suffix-for-send-ask-me-tdd-with-refactoring ()
  "Test that ask-me can resolve to refactoring TDD suffix."
  (let ((ai-code-auto-test-type 'ask-me)
        (ai-code--tdd-test-pattern-instruction ""))
    (cl-letf (((symbol-function 'ai-code--read-auto-test-type-choice)
               (lambda () 'tdd-with-refactoring)))
      (let ((suffix (ai-code--resolve-auto-test-suffix-for-send)))
        (should (string-match-p
                 (regexp-quote ai-code--tdd-with-refactoring-extension-instruction)
                 suffix))
        (should (string-match-p "highest-impact cleanup" suffix))))))

(ert-deftest ai-code-test-resolve-auto-test-suffix-for-send-ask-me-no-test ()
  "Test that ask-me can resolve to explicit no-test suffix."
  (let ((ai-code-auto-test-type 'ask-me))
    (cl-letf (((symbol-function 'ai-code--read-auto-test-type-choice)
               (lambda () 'no-test)))
      (should (equal "Do not write or run any test."
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
      (should (string-match-p "Do not write or run any test\\." sent-command))
      (should-not (string-match-p "SHOULD NOT APPEAR" sent-command)))))

(ert-deftest ai-code-test-menu-prefix-command-default-layout ()
  "Test that the default menu layout uses the original transient."
  (let ((ai-code-menu-layout 'default))
    (should (eq #'ai-code-menu-default
                (ai-code--menu-prefix-command)))))

(ert-deftest ai-code-test-menu-prefix-command-two-columns-layout ()
  "Test that the two-column menu layout uses the narrower transient."
  (let ((ai-code-menu-layout 'two-columns))
    (should (eq #'ai-code-menu-2-columns
                (ai-code--menu-prefix-command)))))

(ert-deftest ai-code-test-menu-prefix-command-fallback-to-default-layout ()
  "Test that unknown menu layout values still fall back to the default transient."
  (let ((ai-code-menu-layout 'unexpected-layout))
    (should (eq #'ai-code-menu-default
                (ai-code--menu-prefix-command)))))

(ert-deftest ai-code-test-menu-dispatches-to-selected-layout ()
  "Test that `ai-code-menu` dispatches to the configured transient command."
  (let ((ai-code-menu-layout 'two-columns)
        called-fn)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (fn &optional _record-flag _keys)
                 (setq called-fn fn))))
      (ai-code-menu)
      (should (eq called-fn #'ai-code-menu-2-columns)))))

(ert-deftest ai-code-test-package-requires-transient-0-9 ()
  "Test that ai-code requires Transient 0.9 or newer."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "ai-code.el" default-directory))
    (should (re-search-forward
             "Package-Requires: ((emacs \"29\\.1\") (transient \"0\\.9\\.0\") (magit \"2\\.1\\.0\"))"
             nil t))))

(ert-deftest ai-code-test-menu-groups-define-four-sections ()
  "Test that menu sections are defined as reusable transient groups."
  (dolist (group '(ai-code--menu-ai-cli-session
                   ai-code--menu-actions-with-context
                   ai-code--menu-agile-development
                   ai-code--menu-other-tools))
    (should (get group 'transient--layout))))

(ert-deftest ai-code-test-menu-prefix-commands-are-interactive ()
  "Test that the main menu and layout-specific menus are defined as commands."
  (dolist (cmd '(ai-code-menu
                 ai-code-menu-default
                 ai-code-menu-2-columns))
    (should (fboundp cmd))
    (should (commandp cmd))))

(ert-deftest ai-code-test-menu-other-tools-includes-speech-to-text-input ()
  "Test that Other Tools menu exposes speech-to-text input."
  (let ((suffix (transient-get-suffix 'ai-code--menu-other-tools ":")))
    (should suffix)
    (should (eq (plist-get (cdr suffix) :command)
                'ai-code-speech-to-text-input))
    (should (equal (plist-get (cdr suffix) :description)
                   "Speech to text input"))))

(provide 'test_ai-code)

;;; test_ai-code.el ends here
