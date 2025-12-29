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
              ((symbol-function 'ai-code-read-string) (lambda (&rest _) "some prompt"))
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

(provide 'test_ai-code-change)

;;; test_ai-code-change.el ends here
