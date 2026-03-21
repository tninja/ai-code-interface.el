;;; test_ai-code-session-link.el --- Tests for ai-code-session-link -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for shared session link helper functions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-session-link)

(defvar ai-code-backends-infra--session-directory)

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

(ert-deftest ai-code-session-link-test-linkify-session-region-file-and-url ()
  "Linkify project files, existing local files, and URLs."
  (let* ((root (make-temp-file "ai-code-session-links-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "FileABC.java" src-dir))
         (outside-file (expand-file-name "Elsewhere.java" temporary-file-directory)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class FileABC {}\n"))
          (with-temp-file outside-file
            (insert "class Elsewhere {}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert (format "src/FileABC.java\nsrc/FileABC.java:42\nsrc/FileABC.java:L42-L60\nsrc/FileABC.java#L42-L60\nsrc/FileABC.java:42:7\n%s\nhttps://example.com/path\n"
                            outside-file))
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward-regexp "src/FileABC\\.java")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "src/FileABC.java"))
            (should (eq (get-text-property (match-beginning 0) 'face) 'link))
            (search-forward-regexp "src/FileABC\\.java:42")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "src/FileABC.java:42"))
            (search-forward-regexp "src/FileABC\\.java:L42-L60")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "src/FileABC.java:L42-L60"))
            (search-forward-regexp "src/FileABC\\.java#L42-L60")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "src/FileABC.java#L42-L60"))
            (search-forward-regexp "src/FileABC\\.java:42:7")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "src/FileABC.java:42:7"))
            (search-forward-regexp (regexp-quote outside-file))
            (let ((outside-pos (match-beginning 0)))
              (should-not (ai-code-session-link--in-project-file-p outside-file root))
              (should (equal (get-text-property outside-pos 'ai-code-session-link)
                             outside-file))
              (should (eq (get-text-property outside-pos 'face) 'link)))
            (search-forward-regexp "https://example\\.com/path")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "https://example.com/path"))
            (should (eq (get-text-property (match-beginning 0) 'face) 'link))
            (erase-buffer)
            (insert "Visit https://example.com/docs, please.")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward-regexp "https://example\\.com/docs")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "https://example.com/docs"))
            (goto-char (match-end 0))
            (should-not (get-text-property (point) 'ai-code-session-link))))
      (when (file-exists-p outside-file)
        (delete-file outside-file))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-matches-unique-project-basename ()
  "Linkify basename references when they uniquely match a project file."
  (let* ((root (make-temp-file "ai-code-session-links-basename-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "Foo.java" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class Foo {}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "Foo.java:42\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward-regexp "Foo\\.java:42")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "Foo.java:42"))
            (should (eq (get-text-property (match-beginning 0) 'face) 'link))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-supports-existing-local-file-and-directory ()
  "Linkify existing local file and directory paths, but not missing ones."
  (let* ((root (make-temp-file "ai-code-session-links-local-paths-" t))
         (local-file (expand-file-name "tmp/LocalFile.txt" root))
         (local-dir (expand-file-name "tmp/local-directory" root))
         (missing-file (expand-file-name "tmp/MissingFile.txt" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory local-file) t)
          (make-directory local-dir t)
          (with-temp-file local-file
            (insert "local file\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory
                        (expand-file-name "project" root))
            (insert (format "%s:12\n%s\n%s:9\n"
                            local-file
                            local-dir
                            missing-file))
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward-regexp (concat (regexp-quote local-file) ":12"))
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           (format "%s:12" local-file)))
            (should (eq (get-text-property (match-beginning 0) 'face) 'link))
            (search-forward-regexp (regexp-quote local-dir))
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           local-dir))
            (should (eq (get-text-property (match-beginning 0) 'face) 'link))
            (search-forward-regexp (concat (regexp-quote missing-file) ":9"))
            (should-not (get-text-property (match-beginning 0) 'ai-code-session-link))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-read-only-text ()
  "Linkification should work on read-only terminal output."
  (let* ((root (make-temp-file "ai-code-session-links-read-only-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "FileABC.java" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class FileABC {}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/FileABC.java:42\nhttps://example.com/path\n")
            (add-text-properties (point-min) (point-max) '(read-only t))
            (should
             (condition-case nil
                 (progn
                   (ai-code-session-link--linkify-session-region (point-min) (point-max))
                   t)
               (text-read-only nil)))
            (goto-char (point-min))
            (search-forward-regexp "src/FileABC\\.java:42")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "src/FileABC.java:42"))
            (search-forward-regexp "https://example\\.com/path")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "https://example.com/path"))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-adds-visible-session-link-properties ()
  "Session links should expose visible link styling and unified navigation data."
  (let* ((root (make-temp-file "ai-code-visible-session-links-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "FileABC.java" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class FileABC {}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/FileABC.java:42\nhttps://example.com/path\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward-regexp "src/FileABC\\.java:42")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "src/FileABC.java:42"))
            (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'link))
            (should (eq (lookup-key (get-text-property (match-beginning 0) 'keymap) [mouse-1])
                        'ai-code-session-navigate-link-at-mouse))
            (should (eq (lookup-key (get-text-property (match-beginning 0) 'keymap) [mouse-2])
                        'ai-code-session-navigate-link-at-mouse))
            (search-forward-regexp "https://example\\.com/path")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "https://example.com/path"))
            (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'link))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(provide 'test_ai-code-session-link)

;;; test_ai-code-session-link.el ends here
