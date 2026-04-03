;;; test_ai-code-agile.el --- Tests for ai-code-agile.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code-agile.el, focusing on TDD workflow helper functions.

;;; Code:

(require 'ert)
(require 'ai-code-agile)

;;; Tests for ai-code--tdd-source-function-context-p

(ert-deftest ai-code-test-tdd-source-function-context-p-source-file ()
  "Return non-nil for a non-test source file in `prog-mode' with a function name."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local buffer-file-name "/project/src/my-module.el")
    (should (ai-code--tdd-source-function-context-p "my-function"))))

(ert-deftest ai-code-test-tdd-source-function-context-p-test-file ()
  "Return nil when variable `buffer-file-name' contains \"test\"."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local buffer-file-name "/project/test/test_my-module.el")
    (should-not (ai-code--tdd-source-function-context-p "my-function"))))

(ert-deftest ai-code-test-tdd-source-function-context-p-no-file ()
  "Return nil when buffer has no associated file."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local buffer-file-name nil)
    (should-not (ai-code--tdd-source-function-context-p "my-function"))))

(ert-deftest ai-code-test-tdd-source-function-context-p-nil-function ()
  "Return nil when function-name is nil."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local buffer-file-name "/project/src/my-module.el")
    (should-not (ai-code--tdd-source-function-context-p nil))))

(ert-deftest ai-code-test-tdd-source-function-context-p-non-prog-mode ()
  "Return nil when buffer is not in a `prog-mode' derived mode."
  (with-temp-buffer
    (text-mode)
    (setq-local buffer-file-name "/project/src/my-module.el")
    (should-not (ai-code--tdd-source-function-context-p "my-function"))))

;;; Tests for ai-code--write-test

(ert-deftest ai-code-test-write-test-prompt-includes-function-name ()
  "Verify prompt passed to ai-code--insert-prompt contains the function name."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local buffer-file-name "/project/src/my-module.el")
    (let (captured-prompt)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_prompt &optional initial _candidates) initial))
                ((symbol-function 'ai-code--get-context-files-string)
                 (lambda () ""))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text) (setq captured-prompt text))))
        (ai-code--write-test "my-function")
        (should (string-match-p "my-function" captured-prompt))))))

(ert-deftest ai-code-test-write-test-prompt-includes-source-file-hint ()
  "Verify prompt includes a hint about the source file when buffer has a file name."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local buffer-file-name "/project/src/my-module.el")
    (let (captured-prompt)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_prompt &optional initial _candidates) initial))
                ((symbol-function 'ai-code--get-context-files-string)
                 (lambda () ""))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text) (setq captured-prompt text))))
        (ai-code--write-test "my-function")
        (should (string-match-p "my-module.el" captured-prompt))))))

(ert-deftest ai-code-test-write-test-prompt-no-file-fallback ()
  "Verify fallback hint when buffer has no file name."
  (with-temp-buffer
    (setq-local buffer-file-name nil)
    (let (captured-prompt)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_prompt &optional initial _candidates) initial))
                ((symbol-function 'ai-code--get-context-files-string)
                 (lambda () ""))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text) (setq captured-prompt text))))
        (ai-code--write-test "my-function")
        (should (string-match-p "corresponding test file" captured-prompt))))))

(ert-deftest ai-code-test-write-test-prompt-includes-tdd-instruction ()
  "Verify TDD instruction appears in the final prompt."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local buffer-file-name "/project/src/my-module.el")
    (let (captured-prompt)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_prompt &optional initial _candidates) initial))
                ((symbol-function 'ai-code--get-context-files-string)
                 (lambda () ""))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text) (setq captured-prompt text))))
        (ai-code--write-test "my-function")
        (should (string-match-p "TDD" captured-prompt))))))

;;; Tests for ai-code-tdd-cycle

(ert-deftest ai-code-test-tdd-cycle-includes-red-green-blue-choice ()
  "Verify TDD cycle stage choices include Red + Green + Blue option."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let (captured-choices)
      (cl-letf (((symbol-function 'which-function) (lambda () "my-function"))
                ((symbol-function 'ai-code--tdd-source-function-context-p) (lambda (_) nil))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq captured-choices collection)
                   "0. Run unit-tests"))
                ((symbol-function 'ai-code-run-test) (lambda () t)))
        (ai-code-tdd-cycle)
        (should (member "5. Red + Green + Blue (One prompt)" captured-choices))))))

(ert-deftest ai-code-test-tdd-cycle-stage-5-dispatches-red-green-blue-handler ()
  "Verify selecting stage 5 dispatches to Red + Green + Blue handler."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((called-function-name nil))
      (cl-letf (((symbol-function 'which-function) (lambda () "my-function"))
                ((symbol-function 'ai-code--tdd-source-function-context-p) (lambda (_) nil))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "5. Red + Green + Blue (One prompt)"))
                ((symbol-function 'ai-code--tdd-red-green-blue-stage)
                 (lambda (function-name) (setq called-function-name function-name))))
        (ai-code-tdd-cycle)
        (should (equal called-function-name "my-function"))))))

(ert-deftest ai-code-test-tdd-red-green-blue-stage-prompt-includes-xp-rules ()
  "Verify Red + Green + Blue prompt includes TDD flow and XP simplicity rules."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let (captured-prompt)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_prompt &optional initial _candidates) initial))
                ((symbol-function 'ai-code--get-context-files-string)
                 (lambda () ""))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text) (setq captured-prompt text))))
        (ai-code--tdd-red-green-blue-stage "my-function")
        (should (string-match-p "Do not skip stages" captured-prompt))
        (should (string-match-p "Stage 1 - Red" captured-prompt))
        (should (string-match-p "Stage 2 - Green" captured-prompt))
        (should (string-match-p "Stage 3 - Blue" captured-prompt))
        (should (string-match-p "Do not refactor during Green" captured-prompt))
        (should (string-match-p "refactor only the files changed in Red/Green" captured-prompt))
        (should (string-match-p "first review the code diff (including tests)" captured-prompt))
        (should (string-match-p "highest-impact cleanup" captured-prompt))))))

(ert-deftest ai-code-test-tdd-red-stage-prompt-includes-test-run-and-change-summary ()
  "Verify Red stage prompt asks to run test and summarize key changes."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let (captured-prompt)
      (cl-letf (((symbol-function 'ai-code--ensure-test-buffer-visible) (lambda () t))
                ((symbol-function 'ai-code-read-string)
                 (lambda (_prompt &optional initial _candidates) initial))
                ((symbol-function 'ai-code--get-context-files-string)
                 (lambda () ""))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text) (setq captured-prompt text))))
        (ai-code--tdd-red-stage "my-function")
        (should (string-match-p "Run test after this stage" captured-prompt))
        (should (string-match-p "summary of test result" captured-prompt))
        (should (string-match-p "List the public API / log key / config key change if there is" captured-prompt))))))

(ert-deftest ai-code-test-tdd-green-stage-prompt-includes-test-run-and-change-summary ()
  "Verify Green stage prompt asks to run test and summarize key changes."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let (captured-prompt)
      (setq-local buffer-file-name "/project/src/my-module.el")
      (cl-letf (((symbol-function 'ai-code--ensure-test-buffer-visible) (lambda () t))
                ((symbol-function 'ai-code-read-string)
                 (lambda (_prompt &optional initial _candidates) initial))
                ((symbol-function 'ai-code--get-context-files-string)
                 (lambda () ""))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text) (setq captured-prompt text))))
        (ai-code--tdd-green-stage "my-function")
        (should (string-match-p "Run test after this stage" captured-prompt))
        (should (string-match-p "summary of test result" captured-prompt))
        (should (string-match-p "List the public API / log key / config key change if there is" captured-prompt))))))

(ert-deftest ai-code-test-tdd-red-green-stage-prompt-includes-stage-test-run-and-change-summary ()
  "Verify Red + Green stage prompt asks to run test after each stage and summarize key changes."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let (captured-prompt)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_prompt &optional initial _candidates) initial))
                ((symbol-function 'ai-code--get-context-files-string)
                 (lambda () ""))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text) (setq captured-prompt text))))
        (ai-code--tdd-red-green-stage "my-function")
        (should (string-match-p "Do not skip stages" captured-prompt))
        (should (string-match-p "Stage 1 - Red" captured-prompt))
        (should (string-match-p "Stage 2 - Green" captured-prompt))
        (should (string-match-p "Do not refactor during Green" captured-prompt))
        (should (string-match-p "Run test after each stage" captured-prompt))
        (should (string-match-p "summary of test result" captured-prompt))
        (should (string-match-p "List the public API / log key / config key change if there is" captured-prompt))))))

(ert-deftest ai-code-test-tdd-red-green-blue-stage-prompt-includes-stage-test-run-and-change-summary ()
  "Verify Red + Green + Blue stage prompt asks to run test after each stage and summarize key changes."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let (captured-prompt)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_prompt &optional initial _candidates) initial))
                ((symbol-function 'ai-code--get-context-files-string)
                 (lambda () ""))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text) (setq captured-prompt text))))
        (ai-code--tdd-red-green-blue-stage "my-function")
        (should (string-match-p "Run test after each stage" captured-prompt))
        (should (string-match-p "summary of test result" captured-prompt))
        (should (string-match-p "List the public API / log key / config key change if there is" captured-prompt))))))

(ert-deftest ai-code-test-refactor-book-method-dired-skips-technique-selection ()
  "Verify Dired refactoring jumps straight to suggestion mode."
  (with-temp-buffer
    (let ((captured-context nil)
          (captured-tdd-mode nil))
      (cl-letf (((symbol-function 'derived-mode-p)
                 (lambda (&rest modes) (memq 'dired-mode modes)))
                ((symbol-function 'dired-get-marked-files)
                 (lambda () '("/repo/src/foo.el" "/repo/test/bar.el")))
                ((symbol-function 'dired-get-filename)
                 (lambda (&optional _localp _no-error) "/repo/src/foo.el"))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (ert-fail "completing-read should not be called for Dired refactoring")))
                ((symbol-function 'ai-code--handle-ask-llm-suggestion)
                 (lambda (context tdd-mode)
                   (setq captured-context context)
                   (setq captured-tdd-mode tdd-mode))))
        (ai-code-refactor-book-method)
        (should (equal (plist-get captured-context :dired-targets)
                       '("/repo/src/foo.el" "/repo/test/bar.el")))
        (should (equal (plist-get captured-context :context-description)
                       "for selected files/directories"))
        (should-not captured-tdd-mode)))))

(ert-deftest ai-code-test-handle-ask-llm-suggestion-dired-includes-selected-files ()
  "Verify Dired refactoring suggestions include selected files."
  (with-temp-buffer
    (let (captured-prompt)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Improve readability and testability"))
                ((symbol-function 'ai-code-read-string)
                 (lambda (_prompt &optional initial-input _candidates)
                   initial-input))
                ((symbol-function 'ai-code--git-root)
                 (lambda (&optional _dir) "/repo/"))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text)
                   (setq captured-prompt text)
                   t)))
        (ai-code--handle-ask-llm-suggestion
         '(:region-active nil
           :current-function nil
           :file-name nil
           :dired-targets ("/repo/src/foo.el" "/repo/test/bar.el")
           :context-description "for selected files/directories")
         nil)
        (should (string-match-p "Analyze the code context below" captured-prompt))
        (should (string-match-p "Context: Selected files/directories" captured-prompt))
        (should (string-match-p "\nFiles:\n@src/foo\\.el\n@test/bar\\.el"
                                (subst-char-in-string ?\\ ?/ captured-prompt)))))))

(ert-deftest ai-code-test-handle-ask-llm-suggestion-offers-common-refactoring-goals ()
  "Verify refactoring suggestion prompt selects a short description first."
  (with-temp-buffer
    (let (captured-short-descriptions captured-selected-description
          captured-read-string-initial-input captured-prompt)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_prompt &optional initial-input _candidates)
                   (setq captured-read-string-initial-input initial-input)
                   "Custom edited prompt"))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq captured-short-descriptions collection)
                   (setq captured-selected-description "Reduce complexity")
                   captured-selected-description))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (text)
                   (setq captured-prompt text)
                   t)))
        (ai-code--handle-ask-llm-suggestion
         '(:region-active nil
           :current-function "my-function"
           :file-name "/repo/src/foo.el"
           :dired-targets nil)
         nil)
        (should (equal captured-short-descriptions
                       '("General refactoring analysis"
                         "Improve readability and testability"
                         "Reduce complexity"
                         "Remove duplication"
                         "Clarify naming and responsibilities")))
        (should (equal captured-selected-description "Reduce complexity"))
        (should (equal captured-read-string-initial-input
                       "Analyze the code context below. Focus on reducing complexity and simplifying control flow. Do not change code logic. Suggest the most impactful refactoring technique and explain why."))
        (should (string-match-p "Custom edited prompt Context: Function 'my-function'"
                                captured-prompt))))))

(ert-deftest ai-code-test-handle-ask-llm-suggestion-default-prompt-mentions-easy-to-understand ()
  "Verify the default refactoring prompt mentions making code easy to understand."
  (with-temp-buffer
    (let (captured-read-string-initial-input)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) ai-code--refactoring-suggestion-default-label))
                ((symbol-function 'ai-code-read-string)
                 (lambda (_prompt &optional initial-input _candidates)
                   (setq captured-read-string-initial-input initial-input)
                   initial-input))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (_text) t)))
        (ai-code--handle-ask-llm-suggestion
         '(:region-active nil
           :current-function "my-function"
           :file-name "/repo/src/foo.el"
           :dired-targets nil)
         nil)
        (should (string-match-p "easy to understand"
                                captured-read-string-initial-input))))))

(provide 'test_ai-code-agile)
;;; test_ai-code-agile.el ends here
