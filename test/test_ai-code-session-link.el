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

(ert-deftest ai-code-session-link-test-toggle-defaults-enabled ()
  "Session linkification should be enabled by default."
  (should (boundp 'ai-code-session-link-enabled))
  (should ai-code-session-link-enabled))

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

(ert-deftest ai-code-session-link-test-linkify-session-region-symbol-near-file-link-across-lines ()
  "Linkify a nearby code symbol after a file link across line breaks."
  (let* ((root (make-temp-file "ai-code-session-links-symbol-nearby-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "UserService.java" src-dir))
         (symbol-text "UserService.processRequest()"))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class UserService {\n  void processRequest() {}\n}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "See src/UserService.java:2\n")
            (insert symbol-text)
            (insert "\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward symbol-text)
            (let ((symbol-pos (- (point) (length symbol-text))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             symbol-text))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "src/UserService.java:2"))
              (should (eq (lookup-key (get-text-property symbol-pos 'keymap) [mouse-1])
                          'ai-code-session-link-navigate-symbol-at-mouse))
              (should (eq (lookup-key (get-text-property symbol-pos 'keymap) (kbd "RET"))
                          'ai-code-session-link-navigate-symbol-at-point))
              (should (eq (get-text-property symbol-pos 'face) 'link)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-prefilters-java-camelcase-symbols ()
  "Linkify nearby bare CamelCase symbols for Java files."
  (let* ((root (make-temp-file "ai-code-session-links-java-camelcase-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "UserService.java" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class UserService {}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/UserService.java:1\nUserService\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (forward-line 1)
            (search-forward "UserService")
            (let ((symbol-pos (- (point) (length "UserService"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "UserService"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "src/UserService.java:1")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-rejects-simple-java-capitalized-symbols ()
  "Do not linkify nearby bare symbols with only one uppercase character."
  (let* ((root (make-temp-file "ai-code-session-links-java-capitalized-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "Builder.java" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class Builder {}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/Builder.java:1\nBuilder\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (forward-line 1)
            (search-forward "Builder")
            (let ((symbol-pos (- (point) (length "Builder"))))
              (should-not (get-text-property symbol-pos 'ai-code-session-symbol-link))
              (should-not (get-text-property symbol-pos 'ai-code-session-link)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-rejects-adjacent-uppercase-symbols ()
  "Do not linkify nearby bare symbols whose uppercase letters are only adjacent."
  (let* ((root (make-temp-file "ai-code-session-links-adjacent-uppercase-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "xml_parser.py" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class XMLParser:\n    pass\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/xml_parser.py:1\nXMLParser\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (forward-line 1)
            (search-forward "XMLParser")
            (let ((symbol-pos (- (point) (length "XMLParser"))))
              (should-not (get-text-property symbol-pos 'ai-code-session-symbol-link))
              (should-not (get-text-property symbol-pos 'ai-code-session-link)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-allows-camelcase-symbols-outside-java ()
  "Linkify nearby bare CamelCase symbols without restricting them to Java files."
  (let* ((root (make-temp-file "ai-code-session-links-camelcase-generic-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "builder.py" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class RequestBuilder:\n    pass\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/builder.py:1\nRequestBuilder\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (forward-line 1)
            (search-forward "RequestBuilder")
            (let ((symbol-pos (- (point) (length "RequestBuilder"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "RequestBuilder"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "src/builder.py:1")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-prefilters-python-snake-case-symbols ()
  "Linkify nearby bare snake_case symbols for Python files."
  (let* ((root (make-temp-file "ai-code-session-links-python-snake-case-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "user_service.py" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "def process_request():\n    return None\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/user_service.py:1\nprocess_request\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "process_request")
            (let ((symbol-pos (- (point) (length "process_request"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "process_request"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "src/user_service.py:1")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-allows-snake-case-symbols-outside-python ()
  "Linkify nearby bare snake_case symbols without restricting them to Python files."
  (let* ((root (make-temp-file "ai-code-session-links-snake-case-generic-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "Builder.java" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class Builder {}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/Builder.java:1\nprocess_request\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (forward-line 1)
            (search-forward "process_request")
            (let ((symbol-pos (- (point) (length "process_request"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "process_request"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "src/Builder.java:1")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-prefilters-elisp-hyphen-symbols ()
  "Linkify code-like Elisp symbols while skipping nearby prose hyphen words."
  (let* ((root (make-temp-file "ai-code-session-links-elisp-symbols-" t))
         (lisp-dir (expand-file-name "lisp" root))
         (file (expand-file-name "feature.el" lisp-dir)))
    (unwind-protect
        (progn
          (make-directory lisp-dir t)
          (with-temp-file file
            (insert "(setq-local foo t)\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "lisp/feature.el:1\nsetq-local\nfollow-up\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "setq-local")
            (let ((symbol-pos (- (point) (length "setq-local"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "setq-local"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "lisp/feature.el:1")))
            (search-forward "follow-up")
            (let ((prose-pos (- (point) (length "follow-up"))))
              (should-not (get-text-property prose-pos 'ai-code-session-symbol-link))
              (should-not (get-text-property prose-pos 'ai-code-session-link)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-symbols-cross-blank-lines ()
  "Linkify nearby symbols even when a blank line appears after the file link."
  (let* ((root (make-temp-file "ai-code-session-links-blank-line-symbols-" t))
         (lisp-dir (expand-file-name "lisp" root))
         (file (expand-file-name "feature.el" lisp-dir)))
    (unwind-protect
        (progn
          (make-directory lisp-dir t)
          (with-temp-file file
            (insert "(defvar ai-code-session-link-enabled t)\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "lisp/feature.el:1\n\nai-code-session-link-enabled\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "ai-code-session-link-enabled")
            (let ((symbol-pos (- (point) (length "ai-code-session-link-enabled"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "ai-code-session-link-enabled"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "lisp/feature.el:1")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-uses-fixed-large-symbol-window ()
  "Linkify later nearby symbols within the fixed 512-char and 8-line budget."
  (let* ((root (make-temp-file "ai-code-session-links-extended-symbols-" t))
         (lisp-dir (expand-file-name "lisp" root))
         (file (expand-file-name "feature.el" lisp-dir))
         (later-symbol "ai-code-session-link--linkify-session-region"))
    (unwind-protect
        (progn
          (make-directory lisp-dir t)
          (with-temp-file file
            (insert "(setq-local foo t)\n")
            (insert "(defun ai-code-session-link--linkify-session-region () nil)\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "lisp/feature.el:1\n")
            (dotimes (_ 6)
              (insert "plain prose words only\n"))
            (insert (make-string 260 ?x))
            (insert "\n")
            (insert later-symbol)
            (insert "\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward later-symbol)
            (let ((symbol-pos (- (point) (length later-symbol))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             later-symbol))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "lisp/feature.el:1")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-reuses-file-resolution-for-nearby-symbols ()
  "Resolve each nearby file link once while preserving symbol linkification."
  (let* ((root (make-temp-file "ai-code-session-links-symbol-perf-" t))
         (resolved-paths
          `(("src/user_service.py" . ,(expand-file-name "src/user_service.py" root))
            ("src/next_file.py" . ,(expand-file-name "src/next_file.py" root))))
         (resolve-count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-session-link--resolve-session-file)
                   (lambda (path)
                     (cl-incf resolve-count)
                     (cdr (assoc path resolved-paths)))))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/user_service.py:1\nprocess_request\nsrc/next_file.py:1\nRequestBuilder\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "process_request")
            (let ((symbol-pos (- (point) (length "process_request"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "process_request"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "src/user_service.py:1")))
            (search-forward "RequestBuilder")
            (let ((symbol-pos (- (point) (length "RequestBuilder"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "RequestBuilder"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "src/next_file.py:1")))
            (should (= resolve-count 2))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-reuses-project-files-with-basename-links ()
  "Reuse project file enumeration while linkifying basename file references."
  (let* ((root (make-temp-file "ai-code-session-links-project-cache-" t))
         (project-files-count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current)
                   (lambda (&optional _maybe-prompt _dir)
                     'mock-project))
                  ((symbol-function 'project-root)
                   (lambda (_project)
                     root))
                  ((symbol-function 'project-files)
                   (lambda (_project &optional _dirs)
                     (cl-incf project-files-count)
                     '("src/UserService.java" "src/Builder.java"))))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "UserService.java:1\nUserService\nBuilder.java:1\nRequestBuilder\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (forward-line 1)
            (search-forward "UserService")
            (let ((symbol-pos (- (point) (length "UserService"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "UserService"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "UserService.java:1")))
            (forward-line 2)
            (search-forward "RequestBuilder")
            (let ((symbol-pos (- (point) (length "RequestBuilder"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "RequestBuilder"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "Builder.java:1")))
            (should (= project-files-count 1))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-reuses-project-files-across-passes ()
  "Repeated relinkify passes should reuse project file enumeration."
  (let* ((root (make-temp-file "ai-code-session-links-project-cache-passes-" t))
         (project-files-count 0)
         (ai-code-session-link-enabled t))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current)
                   (lambda (&optional _maybe-prompt _dir)
                     'mock-project))
                  ((symbol-function 'project-root)
                   (lambda (_project)
                     root))
                  ((symbol-function 'project-files)
                   (lambda (_project &optional _dirs)
                     (cl-incf project-files-count)
                     '("src/UserService.java"))))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "UserService.java:1\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (should (= project-files-count 1))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-skips-unchanged-property-churn ()
  "Repeated linkify should not churn properties for unchanged session text."
  (let* ((root (make-temp-file "ai-code-session-links-stable-region-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "FileABC.java" src-dir))
         (ai-code-session-link-enabled t))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class FileABC {}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/FileABC.java:42\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (let ((add-count 0)
                  (remove-count 0)
                  (orig-add (symbol-function 'add-text-properties))
                  (orig-remove (symbol-function 'remove-text-properties)))
              (cl-letf (((symbol-function 'add-text-properties)
                         (lambda (start end props &optional object)
                           (cl-incf add-count)
                           (funcall orig-add start end props object)))
                        ((symbol-function 'remove-text-properties)
                         (lambda (start end props &optional object)
                           (cl-incf remove-count)
                           (funcall orig-remove start end props object))))
                (ai-code-session-link--linkify-session-region (point-min) (point-max)))
              (should (zerop add-count))
              (should (zerop remove-count)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-navigate-symbol-at-point-falls-back-to-associated-file ()
  "Symbol navigation should fall back to the nearby file and move to the symbol."
  (let* ((root (make-temp-file "ai-code-session-links-symbol-nav-" t))
         (lisp-dir (expand-file-name "lisp" root))
         (file (expand-file-name "feature.el" lisp-dir))
         source-buffer navigated-buffer navigated-point)
    (unwind-protect
        (progn
          (make-directory lisp-dir t)
          (with-temp-file file
            (insert "(setq-local foo t)\n"))
          (cl-letf (((symbol-function 'find-file-other-window)
                     (lambda (path)
                       (setq source-buffer (find-file-noselect path))
                       (set-buffer source-buffer)
                       source-buffer))
                    ((symbol-function 'xref-find-definitions)
                     (lambda (_identifier)
                       (error "xref unavailable")))
                    ((symbol-function 'message)
                     (lambda (&rest _args) nil)))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (insert "lisp/feature.el:1\nsetq-local\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (goto-char (point-min))
              (search-forward "setq-local")
              (goto-char (- (point) (length "setq-local")))
              (should (ai-code-session-link-navigate-symbol-at-point))
              (setq navigated-buffer (current-buffer)
                    navigated-point (point))))
          (should (buffer-live-p source-buffer))
          (should (eq navigated-buffer source-buffer))
          (with-current-buffer source-buffer
            (should (equal (buffer-file-name) file))
            (should (= navigated-point (point)))
            (should (looking-at "setq-local"))))
      (when (and source-buffer (buffer-live-p source-buffer))
        (kill-buffer source-buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-navigate-symbol-at-point-falls-back-to-helm-gtags ()
  "Symbol navigation should try helm-gtags after xref fails."
  (let* ((root (make-temp-file "ai-code-session-links-symbol-gtags-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "UserService.java" src-dir))
         source-buffer gtags-symbol)
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class UserService {\n  void processRequest() {}\n}\n"))
          (cl-letf (((symbol-function 'find-file-other-window)
                     (lambda (path)
                       (setq source-buffer (find-file-noselect path))
                       (set-buffer source-buffer)
                       source-buffer))
                    ((symbol-function 'xref-find-definitions)
                     (lambda (_identifier)
                       (error "xref unavailable")))
                    ((symbol-function 'helm-gtags-find-tag)
                     (lambda (identifier)
                       (setq gtags-symbol identifier)
                       t))
                    ((symbol-function 'message)
                     (lambda (&rest _args) nil)))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (insert "src/UserService.java:2\nUserService.processRequest()\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (goto-char (point-min))
              (search-forward "UserService.processRequest()")
              (goto-char (- (point) (length "UserService.processRequest()")))
              (should (ai-code-session-link-navigate-symbol-at-point))))
          (should (buffer-live-p source-buffer))
          (should (equal gtags-symbol "processRequest")))
      (when (and source-buffer (buffer-live-p source-buffer))
        (kill-buffer source-buffer))
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

(ert-deftest ai-code-session-link-test-disabled-toggle-skips-linkify-and-scheduling ()
  "Disabled session linkification should skip properties and timer scheduling."
  (should (boundp 'ai-code-session-link-enabled))
  (let* ((root (make-temp-file "ai-code-session-links-disabled-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "FileABC.java" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class FileABC {}\n"))
          (let ((ai-code-session-link-enabled nil))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (insert "src/FileABC.java:42\nhttps://example.com/path\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (goto-char (point-min))
              (search-forward-regexp "src/FileABC\\.java:42")
              (should-not (get-text-property (match-beginning 0) 'ai-code-session-link))
              (search-forward-regexp "https://example\\.com/path")
              (should-not (get-text-property (match-beginning 0) 'ai-code-session-link)))
            (with-temp-buffer
              (setq ai-code-session-link--pending-tail-width 0
                    ai-code-session-link--linkify-timer nil)
              (ai-code-session-link--schedule-linkify-recent-output
               (current-buffer)
               "src/FileABC.java:42")
              (should-not ai-code-session-link--linkify-timer)
              (should (zerop ai-code-session-link--pending-tail-width)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-schedule-linkify-recent-output-skips-plain-prose ()
  "Plain prose output should not schedule hot-path session relinkification."
  (let ((ai-code-session-link-enabled t))
    (with-temp-buffer
      (setq ai-code-session-link--pending-tail-width 0
            ai-code-session-link--linkify-timer nil)
      (ai-code-session-link--schedule-linkify-recent-output
       (current-buffer)
       "Working on the next step now.\n")
      (should-not ai-code-session-link--linkify-timer)
      (should (zerop ai-code-session-link--pending-tail-width)))))

(provide 'test_ai-code-session-link)

;;; test_ai-code-session-link.el ends here
