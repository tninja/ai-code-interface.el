;;; test_ai-code-prompt-mode.el --- Tests for ai-code-prompt-mode -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code-prompt-mode.

;;; Code:

(require 'ert)
(require 'ai-code-prompt-mode)
(require 'magit)
(require 'cl-lib)

;; Helper macro to set up and tear down the test environment
(defmacro ai-code-with-test-repo (&rest body)
  "Set up a temporary git repository environment for testing.
This macro creates a temporary directory structure, mocks `magit-toplevel`,
and ensures everything is cleaned up afterward."
  `(let* ((git-root (expand-file-name "test-repo/" temporary-file-directory))
          (mock-file-in-repo (expand-file-name "src/main.js" git-root))
          (outside-file (expand-file-name "other-file.txt" temporary-file-directory)))
     (unwind-protect
         (progn
           ;; Setup: Create dummy files and directories
           (make-directory (file-name-directory mock-file-in-repo) t)
           (with-temp-file mock-file-in-repo (insert "content"))
           (with-temp-file outside-file (insert "content"))
           ;; Execute test body with mocks
           (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) git-root)))
             ,@body))
       ;; Teardown: Clean up dummy files and directories
       (when (file-exists-p mock-file-in-repo) (delete-file mock-file-in-repo))
       (when (file-exists-p outside-file) (delete-file outside-file))
       (when (file-directory-p (file-name-directory mock-file-in-repo))
         (delete-directory (file-name-directory mock-file-in-repo)))
       (when (file-directory-p git-root) (delete-directory git-root)))))

(ert-deftest ai-code-test-preprocess-path-in-repo ()
  "Test that a file path inside the git repo is made relative with an @-prefix."
  (ai-code-with-test-repo
   (let ((prompt (format "check file %s" mock-file-in-repo)))
     (should (string= (ai-code--preprocess-prompt-text prompt)
                      "check file @src/main.js")))))

(ert-deftest ai-code-test-preprocess-path-outside-repo ()
  "Test that a file path outside the git repo remains unchanged."
  (ai-code-with-test-repo
   (let ((prompt (format "check file %s" outside-file)))
     (should (string= (ai-code--preprocess-prompt-text prompt)
                      prompt)))))

(ert-deftest ai-code-test-preprocess-non-existent-path ()
  "Test that a non-existent file path remains unchanged."
  (ai-code-with-test-repo
   (let ((prompt "check file /tmp/non-existent-file.txt"))
     (should (string= (ai-code--preprocess-prompt-text prompt)
                      prompt)))))

(ert-deftest ai-code-test-preprocess-prompt-without-path ()
  "Test that a prompt with no file paths remains unchanged."
  (ai-code-with-test-repo
   (let ((prompt "this is a simple prompt"))
     (should (string= (ai-code--preprocess-prompt-text prompt)
                      prompt)))))

(ert-deftest ai-code-test-preprocess-multiple-paths ()
  "Test a prompt with multiple file paths (inside and outside the repo)."
  (ai-code-with-test-repo
   (let ((prompt (format "compare %s and %s" mock-file-in-repo outside-file)))
     (should (string= (ai-code--preprocess-prompt-text prompt)
                      (format "compare @src/main.js and %s" outside-file))))))

(ert-deftest ai-code-test-preprocess-not-in-git-repo ()
  "Test that paths are not modified when not in a git repository."
  (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) nil)))
    (let ((prompt "check file /some/file.txt"))
      (should (string= (ai-code--preprocess-prompt-text prompt)
                       prompt)))))

(provide 'test-ai-code-prompt-mode)
;;; test_ai-code-prompt-mode.el ends here