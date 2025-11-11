;;; test_ai-code-change.el --- Tests for ai-code-change.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-change module, specifically testing
;; the function detection logic for TODO comments.

;;; Code:

(require 'ert)
(require 'ai-code-change)

(ert-deftest test-ai-code--get-function-name-for-comment-basic ()
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

(ert-deftest test-ai-code--get-function-name-for-comment-no-function ()
  "Test function name detection when comment is not followed by a function."
  (with-temp-buffer
    (setq-local comment-start "# ")
    (insert "# TODO some task\n")
    (insert "x = 1\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'which-function) (lambda () nil)))
      (let ((result (ai-code--get-function-name-for-comment)))
        (should (null result))))))

(ert-deftest test-ai-code--get-function-name-for-comment-multiple-comments ()
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

(ert-deftest test-ai-code--get-function-name-for-comment-same-function ()
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

(ert-deftest test-ai-code--is-comment-line ()
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

(provide 'test_ai-code-change)

;;; test_ai-code-change.el ends here
