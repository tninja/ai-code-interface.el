;;; test_ai-code-change.el --- Tests for ai-code-change.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-change module, specifically testing
;; the function detection logic for TODO comments.

;;; Code:

(require 'ert)
(require 'ai-code-change)

(ert-deftest ai-code-test-ai-code--get-function-name-for-comment-basic ()
  "Test function name detection when on a comment line before function body.
This simulates the Ruby example from the issue where a TODO comment
is between the function definition and its body."
  (with-temp-buffer
    ;; Simulate Ruby mode comment syntax
    (setq-local comment-start "# ")
    (insert "module Foo\n")
    (insert "  class Bar\n")
    (insert "    def baz\n")
    (insert "    end\n")
    (insert "\n")
    (insert "    # TODO remove this function\n")  ;; Line 6 - cursor will be here
    (insert "    def click_first_available(driver, selectors)\n")
    (insert "      wait = Selenium::WebDriver::Wait.new(timeout: 10)\n")
    (insert "    end\n")
    (insert "  end\n")
    (insert "end\n")
    ;; Move cursor to the TODO comment line (line 6)
    (goto-char (point-min))
    (forward-line 5)  ;; Move 5 lines forward from line 1 to reach line 6
    ;; Mock which-function to simulate the actual behavior
    ;; When on line 6, which-function might return "Bar" (class)
    ;; When on line 7 (def line), it should return "Bar#click_first_available"
    (cl-letf (((symbol-function 'which-function)
               (lambda ()
                 (save-excursion
                   (let ((line-num (line-number-at-pos (point))))
                     (cond
                      ((= line-num 6) "Bar")        ;; On comment, returns class
                      ((= line-num 7) "Bar")        ;; On def, still returns class
                      ((>= line-num 8) "Bar#click_first_available")  ;; Inside method body
                      (t nil)))))))
      ;; Test that on the comment line, we get the correct function name
      (let ((result (ai-code--get-function-name-for-comment)))
        (should (string= result "Bar#click_first_available"))))))

(ert-deftest ai-code-test-ai-code--get-function-name-for-comment-no-function ()
  "Test function name detection when comment is not followed by a function."
  (with-temp-buffer
    (setq-local comment-start "# ")
    (insert "# TODO some task\n")
    (insert "x = 1\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'which-function) (lambda () nil)))
      (let ((result (ai-code--get-function-name-for-comment)))
        (should (null result))))))

(ert-deftest ai-code-test-ai-code--get-function-name-for-comment-multiple-comments ()
  "Test function name detection with multiple comment lines before function."
  (with-temp-buffer
    (setq-local comment-start "# ")
    (insert "    # TODO task 1\n")  ;; Line 1 - cursor here
    (insert "    # TODO task 2\n")  ;; Line 2
    (insert "    def my_function()\n")  ;; Line 3
    (insert "      x = 1\n")
    (insert "    end\n")
    (goto-char (point-min))
    ;; Mock which-function
    (cl-letf (((symbol-function 'which-function)
               (lambda ()
                 (save-excursion
                   (let ((line-num (line-number-at-pos (point))))
                     (cond
                      ((<= line-num 2) nil)  ;; On comments, no function context
                      ((>= line-num 3) "my_function")  ;; On/in function
                      (t nil)))))))
      (let ((result (ai-code--get-function-name-for-comment)))
        (should (string= result "my_function"))))))

(ert-deftest ai-code-test-ai-code--get-function-name-for-comment-same-function ()
  "Test that when comment and next line are in same function, we get that function."
  (with-temp-buffer
    (setq-local comment-start "# ")
    (insert "    def my_function()\n")  ;; Line 1
    (insert "      # TODO implement this\n")  ;; Line 2 - cursor here
    (insert "      x = 1\n")  ;; Line 3
    (insert "      x = 2\n")  ;; Line 4
    (insert "      x = 3\n")  ;; Line 5
    (insert "      x = 4\n")  ;; Line 6
    (insert "      x = 5\n")  ;; Line 7
    (insert "    end\n")
    (goto-char (point-min))
    (forward-line 1)  ;; Move 1 line forward from line 1 to reach line 2 (the comment)
    ;; Mock which-function - both lines return same function
    (cl-letf (((symbol-function 'which-function) (lambda () "my_function")))
      (let ((result (ai-code--get-function-name-for-comment)))
        (should (string= result "my_function"))))))

(ert-deftest ai-code-test-ai-code--is-comment-line ()
  "Test comment line detection."
  ;; Test with hash comment
  (let ((comment-start "# "))
    (should (ai-code--is-comment-line "# This is a comment"))
    (should (ai-code--is-comment-line "  # This is an indented comment"))
    (should (ai-code--is-comment-line "## Multiple hashes"))
    (should-not (ai-code--is-comment-line "This is not a comment"))
    (should-not (ai-code--is-comment-line "  x = 1  # inline comment")))
  ;; Test with semicolon comment (Lisp)
  (let ((comment-start "; "))
    (should (ai-code--is-comment-line "; This is a comment"))
    (should (ai-code--is-comment-line "  ;; This is a comment"))
    (should-not (ai-code--is-comment-line "This is not a comment")))
  ;; Test with double slash comment (C/Java)
  (let ((comment-start "// "))
    (should (ai-code--is-comment-line "// This is a comment"))
    (should (ai-code--is-comment-line "  // This is an indented comment"))
    (should-not (ai-code--is-comment-line "This is not a comment"))))

(ert-deftest ai-code-test-ai-code--is-comment-block ()
  "Test comment block detection using `ai-code--is-comment-block`."
  (with-temp-buffer
    (setq-local comment-start ";")
    (setq-local comment-end "")
    ;; Mock ai-code--is-comment-line for this test
    (cl-letf (((symbol-function 'ai-code--is-comment-line)
               (lambda (line) (string-match-p "^[ \t]*;" line))))

      ;; Test with all commented lines
      (should (ai-code--is-comment-block "; line 1\n; line 2"))

      ;; Test with commented lines and blank lines
      (should (ai-code--is-comment-block "; line 1\n\n; line 3"))

      ;; Test with mixed lines (should fail)
      (should-not (ai-code--is-comment-block "; line 1\ncode line"))

      ;; Test with all code lines (should fail)
      (should-not (ai-code--is-comment-block "code 1\ncode 2"))

      ;; Test with empty string (should pass as technically all (0) lines satisfy condition)
      (should (ai-code--is-comment-block ""))

      ;; Test with only newlines (should pass as blank lines are allowed)
      (should (ai-code--is-comment-block "\n\n")))))

(ert-deftest ai-code-test-ai-code-implement-todo-region-validation ()
  "Test ai-code-implement-todo raises error for non-comment region."
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (setq-local comment-start ";")
    (insert "code line 1\ncode line 2")
    (goto-char (point-min))
    (set-mark (point))
    (goto-char (point-max))

    ;; Mock external functions
    (cl-letf (((symbol-function 'region-active-p) (lambda () t))
              ((symbol-function 'ai-code--get-clipboard-text) (lambda () nil))
              ((symbol-function 'ai-code--get-region-location-info) (lambda (&rest _) "loc info"))
              ((symbol-function 'ai-code--get-context-files-string) (lambda () ""))
              ((symbol-function 'which-function) (lambda () nil))
              ;; Mock ai-code--is-comment-line to fail for our code lines
              ((symbol-function 'ai-code--is-comment-line) (lambda (_) nil)))

      ;; Should signal user-error
      (should-error (ai-code-implement-todo nil) :type 'user-error))))

(ert-deftest ai-code-test-ai-code-implement-todo-blank-line ()
  "Test ai-code-implement-todo on a blank line inserts TODO."
  (with-temp-buffer
    ;; Set up environment
    (setq buffer-file-name "test.el")
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert "Line 1\n\nLine 3")
    (goto-char (point-min))
    (forward-line 1) ;; On the blank line

    ;; Mock interactive functions and external dependencies
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "my task"))
              ((symbol-function 'ai-code--insert-prompt) (lambda (&rest _) t))
              ((symbol-function 'ai-code-read-string) (lambda (&rest _) "my task"))
              ((symbol-function 'ai-code--get-clipboard-text) (lambda () nil))
              ((symbol-function 'ai-code--get-context-files-string) (lambda () ""))
              ((symbol-function 'ai-code--format-repo-context-info) (lambda () ""))
              ((symbol-function 'ai-code--get-function-name-for-comment) (lambda () nil))
              ((symbol-function 'which-function) (lambda () nil))
              ;; Ensure region-active-p returns nil as expected
              ((symbol-function 'region-active-p) (lambda () nil)))

      (ai-code-implement-todo nil)

      ;; Check the line content
      (beginning-of-line)
      ;; It should look like "; TODO: my task" potentially with indentation
      ;; Since we are in temp buffer with no major mode, indentation might be 0.
      (should (looking-at-p "; TODO: my task")))))

(ert-deftest ai-code-test-ai-code-implement-todo-done-line-toggle ()
  "Test ai-code-implement-todo toggles DONE to TODO when toggle action is selected."
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert "Line 1\n;; DONE: completed task\nLine 3")
    (goto-char (point-min))
    (forward-line 1) ;; On the DONE line

    ;; Mock completing-read to return "Toggle to TODO"
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Toggle to TODO"))
              ((symbol-function 'region-active-p) (lambda () nil)))

      (let ((result (ai-code--implement-todo--handle-done-line)))
        ;; Should return t indicating the action was handled
        (should result)
        ;; Check the line was toggled
        (goto-char (point-min))
        (forward-line 1)
        (should (looking-at-p ";; TODO: completed task"))))))

(ert-deftest ai-code-test-ai-code-implement-todo-done-line-delete ()
  "Test ai-code-implement-todo deletes DONE line when delete action is selected."
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert "Line 1\n;; DONE: completed task\nLine 3")
    (goto-char (point-min))
    (forward-line 1) ;; On the DONE line

    ;; Mock completing-read to return "Delete comment line"
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Delete comment line"))
              ((symbol-function 'region-active-p) (lambda () nil)))

      (let ((result (ai-code--implement-todo--handle-done-line)))
        ;; Should return t indicating the action was handled
        (should result)
        ;; Check the DONE line was deleted - buffer should now be "Line 1\nLine 3"
        (goto-char (point-min))
        (should (looking-at-p "Line 1\nLine 3"))))))

(ert-deftest ai-code-test-ai-code-implement-todo-done-line-keep ()
  "Test ai-code-implement-todo keeps DONE line when keep action is selected."
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert "Line 1\n;; DONE: completed task\nLine 3")
    (goto-char (point-min))
    (forward-line 1) ;; On the DONE line

    ;; Mock completing-read to return "Keep as DONE"
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Keep as DONE"))
              ((symbol-function 'region-active-p) (lambda () nil)))

      (let ((result (ai-code--implement-todo--handle-done-line)))
        ;; Should return t indicating the action was handled
        (should result)
        ;; Check the line is unchanged
        (goto-char (point-min))
        (forward-line 1)
        (should (looking-at-p ";; DONE: completed task"))))))

(ert-deftest ai-code-test-ai-code-implement-todo-done-line-not-detected-on-non-done ()
  "Test ai-code--implement-todo--handle-done-line returns nil for non-DONE lines."
  (with-temp-buffer
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert ";; TODO: a task\n")
    (goto-char (point-min))

    (cl-letf (((symbol-function 'region-active-p) (lambda () nil)))
      (let ((result (ai-code--implement-todo--handle-done-line)))
        ;; Should return nil as the line is not a DONE line
        (should-not result)))))

(ert-deftest ai-code-test-ai-code-implement-todo-done-line-not-detected-with-region ()
  "Test ai-code--implement-todo--handle-done-line returns nil when region is active."
  (with-temp-buffer
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert ";; DONE: completed task\n")
    (goto-char (point-min))

    ;; Mock region-active-p to return t
    (cl-letf (((symbol-function 'region-active-p) (lambda () t)))
      (let ((result (ai-code--implement-todo--handle-done-line)))
        ;; Should return nil as region is active
        (should-not result)))))

(ert-deftest ai-code-test-ai-code-implement-todo-done-line-with-different-comment-styles ()
  "Test DONE line detection works with different comment styles."
  ;; Test with hash comment (Python/Ruby style)
  (with-temp-buffer
    (setq-local comment-start "# ")
    (setq-local comment-end "")
    (insert "# DONE: python task\n")
    (goto-char (point-min))

    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Toggle to TODO"))
              ((symbol-function 'region-active-p) (lambda () nil)))
      (let ((result (ai-code--implement-todo--handle-done-line)))
        (should result)
        (goto-char (point-min))
        (should (looking-at-p "# TODO: python task")))))

  ;; Test with double slash comment (C/Java style)
  (with-temp-buffer
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (insert "// DONE: java task\n")
    (goto-char (point-min))

    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Toggle to TODO"))
              ((symbol-function 'region-active-p) (lambda () nil)))
      (let ((result (ai-code--implement-todo--handle-done-line)))
        (should result)
        (goto-char (point-min))
        (should (looking-at-p "// TODO: java task"))))))

(ert-deftest ai-code-test-ai-code-implement-todo-done-line-with-indentation ()
  "Test DONE line detection works with indented comments."
  (with-temp-buffer
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert "    ;; DONE: indented task\n")
    (goto-char (point-min))

    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Toggle to TODO"))
              ((symbol-function 'region-active-p) (lambda () nil)))
      (let ((result (ai-code--implement-todo--handle-done-line)))
        (should result)
        (goto-char (point-min))
        (should (looking-at-p "    ;; TODO: indented task"))))))

(ert-deftest ai-code-test-implement-todo-action-choice-is-presented ()
  "Test that build-and-send-prompt presents action choice via `completing-read'."
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert ";; TODO: implement feature\n")
    (goto-char (point-min))

    (let ((action-asked nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt candidates &rest _args)
                   (when (and (listp candidates)
                              (member "Code change" candidates)
                              (member "Ask question" candidates))
                     (setq action-asked t))
                   "Code change"))
                ((symbol-function 'ai-code-read-string)
                 (lambda (_label input) input))
                ((symbol-function 'ai-code--get-clipboard-text) (lambda () nil))
                ((symbol-function 'ai-code--get-context-files-string) (lambda () ""))
                ((symbol-function 'ai-code--format-repo-context-info) (lambda () ""))
                ((symbol-function 'ai-code--get-function-name-for-comment) (lambda () nil))
                ((symbol-function 'which-function) (lambda () nil))
                ((symbol-function 'region-active-p) (lambda () nil))
                ((symbol-function 'ai-code--insert-prompt) (lambda (_p) nil)))

        (ai-code--implement-todo--build-and-send-prompt nil)

        (should action-asked)))))

(ert-deftest ai-code-test-implement-todo-action-choice-ask-question ()
  "Test that choosing 'Ask question' adds no-code-change suffix to prompt."
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert ";; TODO: implement feature\n")
    (goto-char (point-min))

    (let (captured-prompt)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt candidates &rest _args)
                   (if (and (listp candidates)
                            (member "Ask question" candidates))
                       "Ask question"
                     (if (listp candidates) (car candidates) ""))))
                ((symbol-function 'ai-code-read-string)
                 (lambda (_label input) input))
                ((symbol-function 'ai-code--get-clipboard-text) (lambda () nil))
                ((symbol-function 'ai-code--get-context-files-string) (lambda () ""))
                ((symbol-function 'ai-code--format-repo-context-info) (lambda () ""))
                ((symbol-function 'ai-code--get-function-name-for-comment) (lambda () nil))
                ((symbol-function 'which-function) (lambda () nil))
                ((symbol-function 'region-active-p) (lambda () nil))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (p) (setq captured-prompt p))))

        (ai-code--implement-todo--build-and-send-prompt nil)

        (should (stringp captured-prompt))
        (should (string-match-p "do not make any code change" captured-prompt))))))

(ert-deftest ai-code-test-implement-todo-ask-question-no-implement-wording ()
  "Test that 'Ask question' prompt body does not contain implementation wording."
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert ";; TODO: implement feature\n")
    (goto-char (point-min))

    (let (captured-prompt)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt candidates &rest _args)
                   (if (and (listp candidates)
                            (member "Ask question" candidates))
                       "Ask question"
                     (if (listp candidates) (car candidates) ""))))
                ((symbol-function 'ai-code-read-string)
                 (lambda (_label input) input))
                ((symbol-function 'ai-code--get-clipboard-text) (lambda () nil))
                ((symbol-function 'ai-code--get-context-files-string) (lambda () ""))
                ((symbol-function 'ai-code--format-repo-context-info) (lambda () ""))
                ((symbol-function 'ai-code--get-function-name-for-comment) (lambda () nil))
                ((symbol-function 'which-function) (lambda () nil))
                ((symbol-function 'region-active-p) (lambda () nil))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (p) (setq captured-prompt p))))

        (ai-code--implement-todo--build-and-send-prompt nil)

        (should (stringp captured-prompt))
        ;; Should NOT contain implementation-oriented wording
        (should-not (string-match-p "Please implement code" captured-prompt))
        ;; Should contain question-oriented wording
        (should (string-match-p "TODO comment" captured-prompt))))))

(ert-deftest ai-code-test-implement-todo-code-change-keeps-implement-wording ()
  "Test that 'Code change' prompt body still contains implementation wording."
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert ";; TODO: implement feature\n")
    (goto-char (point-min))

    (let (captured-prompt)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt candidates &rest _args)
                   (if (and (listp candidates)
                            (member "Code change" candidates))
                       "Code change"
                     (if (listp candidates) (car candidates) ""))))
                ((symbol-function 'ai-code-read-string)
                 (lambda (_label input) input))
                ((symbol-function 'ai-code--get-clipboard-text) (lambda () nil))
                ((symbol-function 'ai-code--get-context-files-string) (lambda () ""))
                ((symbol-function 'ai-code--format-repo-context-info) (lambda () ""))
                ((symbol-function 'ai-code--get-function-name-for-comment) (lambda () nil))
                ((symbol-function 'which-function) (lambda () nil))
                ((symbol-function 'region-active-p) (lambda () nil))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (p) (setq captured-prompt p))))

        (ai-code--implement-todo--build-and-send-prompt nil)

        (should (stringp captured-prompt))
        ;; Should contain implementation-oriented wording
        (should (string-match-p "Please implement code" captured-prompt))
        ;; Should NOT contain no-code-change suffix
        (should-not (string-match-p "do not make any code change" captured-prompt))))))

(ert-deftest ai-code-test-implement-todo-ask-question-prompt-label ()
  "Test that 'Ask question' uses a question-oriented prompt label."
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert ";; TODO: implement feature\n")
    (goto-char (point-min))

    (let (captured-label)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt candidates &rest _args)
                   (if (and (listp candidates)
                            (member "Ask question" candidates))
                       "Ask question"
                     (if (listp candidates) (car candidates) ""))))
                ((symbol-function 'ai-code-read-string)
                 (lambda (label input)
                   (setq captured-label label)
                   input))
                ((symbol-function 'ai-code--get-clipboard-text) (lambda () nil))
                ((symbol-function 'ai-code--get-context-files-string) (lambda () ""))
                ((symbol-function 'ai-code--format-repo-context-info) (lambda () ""))
                ((symbol-function 'ai-code--get-function-name-for-comment) (lambda () nil))
                ((symbol-function 'which-function) (lambda () nil))
                ((symbol-function 'region-active-p) (lambda () nil))
                ((symbol-function 'ai-code--insert-prompt) (lambda (_p) nil)))

        (ai-code--implement-todo--build-and-send-prompt nil)

        (should (stringp captured-label))
        ;; Should contain question-oriented label, not implementation
        (should (string-match-p "[Qq]uestion" captured-label))
        (should-not (string-match-p "implementation" captured-label))))))

(ert-deftest ai-code-test-ai-code-implement-todo-org-section-includes-heading-and-content ()
  "Test Org TODO section is used as prompt context without requiring comment syntax."
  (with-temp-buffer
    (require 'org)
    (setq buffer-file-name "todo.org")
    (insert "* TODO Build backend switcher\n")
    (insert "Use Codex for implementation.\n")
    (insert "Keep the UI untouched.\n")
    (org-mode)
    (goto-char (point-min))

    (let (captured-prompt)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Code change"))
                ((symbol-function 'ai-code-read-string)
                 (lambda (_label input) input))
                ((symbol-function 'ai-code--get-clipboard-text) (lambda () nil))
                ((symbol-function 'ai-code--get-context-files-string) (lambda () ""))
                ((symbol-function 'ai-code--format-repo-context-info) (lambda () ""))
                ((symbol-function 'which-function) (lambda () nil))
                ((symbol-function 'region-active-p) (lambda () nil))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq captured-prompt prompt))))

        (ai-code-implement-todo nil)

        (should (stringp captured-prompt))
        (should (string-match-p "Please implement code" captured-prompt))
        (should (string-match-p "\\* TODO Build backend switcher" captured-prompt))
        (should (string-match-p "Use Codex for implementation\\." captured-prompt))
        (should (string-match-p "Keep the UI untouched\\." captured-prompt))))))

(ert-deftest ai-code-test-ai-code-implement-todo-org-body-line-is-not-headline ()
  "Test Org body lines do not count as the TODO entry for implementation."
  (with-temp-buffer
    (require 'org)
    (setq buffer-file-name "todo.org")
    (insert "** TODO my task description\n")
    (insert "Supporting details live here.\n")
    (org-mode)
    (goto-char (point-min))
    (forward-line 1)

    (cl-letf (((symbol-function 'region-active-p) (lambda () nil)))
      (should-error (ai-code-implement-todo nil) :type 'user-error))))

(ert-deftest ai-code-test-ai-code-implement-todo-org-headline-with-colon-prefix ()
  "Test Org headline with `TODO:' prefix is accepted."
  (with-temp-buffer
    (require 'org)
    (setq buffer-file-name "todo.org")
    (insert "** TODO: what is the most important verse in Bible\n")
    (org-mode)
    (goto-char (point-min))

    (let (captured-prompt)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Ask question"))
                ((symbol-function 'ai-code-read-string)
                 (lambda (_label input) input))
                ((symbol-function 'ai-code--get-clipboard-text) (lambda () nil))
                ((symbol-function 'ai-code--get-context-files-string) (lambda () ""))
                ((symbol-function 'ai-code--format-repo-context-info) (lambda () ""))
                ((symbol-function 'which-function) (lambda () nil))
                ((symbol-function 'region-active-p) (lambda () nil))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq captured-prompt prompt))))

        (ai-code-implement-todo nil)

        (should (stringp captured-prompt))
        (should (string-match-p "Regarding this Org TODO headline" captured-prompt))
        (should (string-match-p "\\*\\* TODO: what is the most important verse in Bible"
                                captured-prompt))))))

(ert-deftest ai-code-test-detect-todo-info-org-todo-headline ()
  "Test `ai-code--detect-todo-info' detects Org TODO headlines."
  (with-temp-buffer
    (require 'org)
    (setq buffer-file-name "plan.org")
    (insert "* TODO Build search feature\n")
    (insert "Design the API first.\n")
    (org-mode)
    (goto-char (point-min))

    (cl-letf (((symbol-function 'region-active-p) (lambda () nil)))
      (let ((result (ai-code--detect-todo-info nil)))
        (should result)
        (should (stringp (nth 0 result)))
        (should (string-match-p "TODO Build search feature" (nth 0 result)))
        (should (integerp (nth 1 result)))
        (should (integerp (nth 2 result)))))))

(ert-deftest ai-code-test-detect-todo-info-org-done-headline-returns-nil ()
  "Test `ai-code--detect-todo-info' returns nil for Org DONE headlines."
  (with-temp-buffer
    (require 'org)
    (setq buffer-file-name "plan.org")
    (insert "* DONE Completed task\n")
    (org-mode)
    (goto-char (point-min))

    (cl-letf (((symbol-function 'region-active-p) (lambda () nil)))
      (should-not (ai-code--detect-todo-info nil)))))

(ert-deftest ai-code-test-detect-todo-info-org-non-todo-headline-returns-nil ()
  "Test `ai-code--detect-todo-info' returns nil for non-TODO Org headlines."
  (with-temp-buffer
    (require 'org)
    (setq buffer-file-name "notes.org")
    (insert "* Regular heading\n")
    (org-mode)
    (goto-char (point-min))

    (cl-letf (((symbol-function 'region-active-p) (lambda () nil)))
      (should-not (ai-code--detect-todo-info nil)))))

(ert-deftest ai-code-test-detect-todo-info-org-todo-colon-prefix ()
  "Test `ai-code--detect-todo-info' detects `TODO:' prefixed Org headlines."
  (with-temp-buffer
    (require 'org)
    (setq buffer-file-name "plan.org")
    (insert "** TODO: Implement auth module\n")
    (org-mode)
    (goto-char (point-min))

    (cl-letf (((symbol-function 'region-active-p) (lambda () nil)))
      (let ((result (ai-code--detect-todo-info nil)))
        (should result)
        (should (string-match-p "TODO:" (nth 0 result)))))))

(ert-deftest ai-code-test-implement-todo-default-action-skips-completing-read ()
  "Test that passing default-action skips the completing-read prompt."
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert ";; TODO: implement feature\n")
    (goto-char (point-min))

    (let (captured-prompt (completing-read-called nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _args)
                   (setq completing-read-called t)
                   "Code change"))
                ((symbol-function 'ai-code-read-string)
                 (lambda (_label input) input))
                ((symbol-function 'ai-code--get-clipboard-text) (lambda () nil))
                ((symbol-function 'ai-code--get-context-files-string) (lambda () ""))
                ((symbol-function 'ai-code--format-repo-context-info) (lambda () ""))
                ((symbol-function 'ai-code--get-function-name-for-comment) (lambda () nil))
                ((symbol-function 'which-function) (lambda () nil))
                ((symbol-function 'region-active-p) (lambda () nil))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (p) (setq captured-prompt p))))

        (ai-code--implement-todo--build-and-send-prompt nil "Code change")

        (should (stringp captured-prompt))
        (should-not completing-read-called)
        (should (string-match-p "Please implement code" captured-prompt))))))

(ert-deftest ai-code-test-implement-todo-nil-default-action-prompts-user ()
  "Test that nil default-action still prompts user with completing-read."
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert ";; TODO: implement feature\n")
    (goto-char (point-min))

    (let (captured-prompt (completing-read-called nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt candidates &rest _args)
                   (setq completing-read-called t)
                   (if (and (listp candidates)
                            (member "Code change" candidates))
                       "Code change"
                     (if (listp candidates) (car candidates) ""))))
                ((symbol-function 'ai-code-read-string)
                 (lambda (_label input) input))
                ((symbol-function 'ai-code--get-clipboard-text) (lambda () nil))
                ((symbol-function 'ai-code--get-context-files-string) (lambda () ""))
                ((symbol-function 'ai-code--format-repo-context-info) (lambda () ""))
                ((symbol-function 'ai-code--get-function-name-for-comment) (lambda () nil))
                ((symbol-function 'which-function) (lambda () nil))
                ((symbol-function 'region-active-p) (lambda () nil))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (p) (setq captured-prompt p))))

        (ai-code--implement-todo--build-and-send-prompt nil nil)

        (should (stringp captured-prompt))
        (should completing-read-called)))))

(ert-deftest ai-code-test-code-change-passes-code-change-action ()
  "Test that `ai-code-code-change' passes \"Code change\" as default-action."
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert ";; TODO: implement feature\n")
    (goto-char (point-min))

    (let (captured-default-action)
      (cl-letf (((symbol-function 'ai-code--get-clipboard-text) (lambda () nil))
                ((symbol-function 'ai-code-implement-todo)
                 (lambda (_arg &optional default-action)
                   (setq captured-default-action default-action)))
                ((symbol-function 'region-active-p) (lambda () nil)))

        (ai-code-code-change nil)

        (should (equal captured-default-action "Code change"))))))

(provide 'test_ai-code-change)

;;; test_ai-code-change.el ends here
