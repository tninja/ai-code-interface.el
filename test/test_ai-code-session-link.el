;;; test_ai-code-session-link.el --- Tests for ai-code-session-link -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for shared session link helper functions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-session-link)

(ert-deftest ai-code-session-link-test-normalize-file-removes-session-prefixes ()
  "Normalization should trim whitespace and remove session-only prefixes."
  (should (equal (ai-code-session-link--normalize-file " @src/Foo.java ")
                 "src/Foo.java"))
  (should (equal (ai-code-session-link--normalize-file "file:///tmp/project/Foo.java")
                 "/tmp/project/Foo.java"))
  (should-not (ai-code-session-link--normalize-file "   ")))

(ert-deftest ai-code-session-link-test-project-files-expands-relative-project-entries ()
  "Project file enumeration should return absolute paths."
  (let* ((root (make-temp-file "ai-code-session-link-project-files-" t))
         (file (expand-file-name "src/Foo.java" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "class Foo {}\n"))
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&optional _maybe-prompt _dir)
                       'mock-project))
                    ((symbol-function 'project-root)
                     (lambda (_project)
                       root))
                    ((symbol-function 'project-files)
                     (lambda (_project &optional _dirs)
                       '("src/Foo.java"))))
            (should (equal (ai-code-session-link--project-files root)
                           (list file)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-matching-project-files-supports-relative-and-basename ()
  "Matching should support both relative paths and unique basenames."
  (let* ((root (make-temp-file "ai-code-session-link-matching-files-" t))
         (file (expand-file-name "src/Foo.java" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "class Foo {}\n"))
          (should (equal (ai-code-session-link--matching-project-files "./src/Foo.java" root)
                         (list file)))
          (should (equal (ai-code-session-link--matching-project-files "Foo.java" root)
                         (list file))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(provide 'test_ai-code-session-link)

;;; test_ai-code-session-link.el ends here
