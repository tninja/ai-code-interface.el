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
(defvar ai-code-backends-infra--session-terminal-backend)
(defvar ai-code-session-link-image-preview-position-function)
(defvar ai-code-session-link-image-preview-transaction-function)

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
  (should (equal (ai-code-session-link--normalize-file "file://localhost/tmp/project/Foo.java")
                 "/tmp/project/Foo.java"))
  (should (equal (ai-code-session-link--normalize-file "file:/tmp/project/Foo.java")
                 "/tmp/project/Foo.java"))
  (should (equal (ai-code-session-link--normalize-file
                  "file:///tmp/project/image-\nwrapped.png")
                 "/tmp/project/image-wrapped.png"))
  (should (equal (ai-code-session-link--normalize-link-text
                  "/tmp/project/My \n Image.png")
                 "/tmp/project/My Image.png"))
  (should (equal (ai-code-session-link--normalize-url-link-text
                  "https://example.com/app-   \n  page=true")
                 "https://example.com/app-page=true"))
  (should (equal (ai-code-session-link--normalize-file
                  "/tmp/layout-\ncheck/window-\nafter-53660.png")
                 "/tmp/layout-check/window-after-53660.png"))
  (should (equal (ai-code-session-link--normalize-file
                  "<file:///tmp/project/My%20Image.png>")
                 "/tmp/project/My Image.png"))
  (should (equal (ai-code-session-link--normalize-file
                  "./screens/My\\ Image.png")
                 "./screens/My Image.png"))
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

(ert-deftest ai-code-session-link-test-linkify-session-region-supports-wrapped-url ()
  "Linkify all terminal rows of a hard-wrapped URL."
  (with-temp-buffer
    (insert "https://chatgpt.com/codex?app-landing-\npage=true\n")
    (ai-code-session-link--linkify-session-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "https://chatgpt.com/codex?app-landing-")
    (let ((url "https://chatgpt.com/codex?app-landing-page=true")
          (first-row-pos (match-beginning 0))
          (newline-pos (line-end-position)))
      (should (equal (get-text-property first-row-pos 'ai-code-session-link)
                     url))
      (should (eq (get-text-property first-row-pos 'face) 'link))
      (should (eq (get-text-property newline-pos 'mouse-face)
                  'highlight))
      (search-forward "page=true")
      (let ((second-row-pos (match-beginning 0)))
        (should (equal (get-text-property second-row-pos
                                          'ai-code-session-link)
                       url))
        (should (eq (get-text-property second-row-pos 'face) 'link))))))

(ert-deftest ai-code-session-link-test-linkify-session-region-supports-mid-token-url-wrap ()
  "Linkify URLs split in the middle of a path token."
  (with-temp-buffer
    (insert "origin\n")
    (insert "https://example.com/repo/project-int\n")
    (insert "erface.el\n")
    (insert "HEAD\n")
    (ai-code-session-link--linkify-session-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "https://example.com/repo/project-int")
    (let ((url "https://example.com/repo/project-interface.el")
          (first-row-pos (match-beginning 0))
          (newline-pos (line-end-position)))
      (should (equal (get-text-property first-row-pos 'ai-code-session-link)
                     url))
      (should (eq (get-text-property newline-pos 'mouse-face) 'highlight))
      (search-forward "erface.el")
      (let ((second-row-pos (match-beginning 0)))
        (should (equal (get-text-property second-row-pos
                                          'ai-code-session-link)
                       url))
        (should (eq (get-text-property second-row-pos 'face) 'link)))
      (search-forward "HEAD")
      (should-not (get-text-property (match-beginning 0)
                                     'ai-code-session-link)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-supports-url-wrap-after-padding ()
  "Linkify wrapped URLs when terminal rows contain trailing padding."
  (with-temp-buffer
    (insert "origin\n")
    (insert "https://example.com/repo/project-int   \n")
    (insert "erface.el\n")
    (insert "HEAD\n")
    (ai-code-session-link--linkify-session-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "https://example.com/repo/project-int")
    (let ((url "https://example.com/repo/project-interface.el")
          (first-row-pos (match-beginning 0))
          (padding-pos (match-end 0)))
      (should (equal (get-text-property first-row-pos 'ai-code-session-link)
                     url))
      (should-not (get-text-property padding-pos 'ai-code-session-link))
      (should (eq (get-text-property padding-pos 'mouse-face) 'highlight))
      (search-forward "erface.el")
      (should (equal (get-text-property (match-beginning 0)
                                        'ai-code-session-link)
                     url)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-supports-quoted-url-wrap ()
  "Linkify wrapped URLs terminated by a closing quote."
  (with-temp-buffer
    (insert "(use-package ai-code\n")
    (insert "  :vc (:url\n")
    (insert "\"https://example.com/repo/pkg\n")
    (insert "-interface.el\"\n")
    (insert "      :rev \"1.88\"))\n")
    (ai-code-session-link--linkify-session-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "https://example.com/repo/pkg")
    (let ((url "https://example.com/repo/pkg-interface.el")
          (first-row-pos (match-beginning 0))
          (newline-pos (line-end-position)))
      (should (equal (get-text-property first-row-pos
                                        'ai-code-session-link)
                     url))
      (should (eq (get-text-property newline-pos 'mouse-face)
                  'highlight))
      (search-forward "-interface.el")
      (let ((second-row-pos (match-beginning 0))
            (closing-quote-pos (match-end 0)))
        (should (equal (get-text-property second-row-pos
                                          'ai-code-session-link)
                       url))
        (should (eq (get-text-property second-row-pos 'mouse-face)
                    'highlight))
        (should-not (get-text-property closing-quote-pos
                                       'ai-code-session-link))
        (should-not (get-text-property closing-quote-pos 'mouse-face))))))

(ert-deftest ai-code-session-link-test-linkify-session-region-rejects-url-wrap-with-suffix ()
  "Do not wrap URL fragments when a continuation row has trailing prose."
  (with-temp-buffer
    (insert "https://example.com/repo/pkg\n")
    (insert "-interface.el suffix\n")
    (ai-code-session-link--linkify-session-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "https://example.com/repo/pkg")
    (should (equal (get-text-property (match-beginning 0)
                                      'ai-code-session-link)
                   "https://example.com/repo/pkg"))
    (search-forward "-interface.el")
    (should-not (equal (get-text-property (match-beginning 0)
                                          'ai-code-session-link)
                       "https://example.com/repo/pkg-interface.el"))))

(ert-deftest ai-code-session-link-test-linkify-session-region-supports-indented-wrapped-url ()
  "Linkify wrapped URL continuations while leaving indentation unstyled."
  (with-temp-buffer
    (insert "https://example.com/app-\n  page=true.\n")
    (ai-code-session-link--linkify-session-region (point-min) (point-max))
    (goto-char (point-min))
    (let ((url "https://example.com/app-page=true"))
      (search-forward "https://example.com/app-")
      (should (equal (get-text-property (match-beginning 0)
                                        'ai-code-session-link)
                     url))
      (search-forward "  page=true")
      (let ((indent-pos (match-beginning 0))
            (path-pos (+ (match-beginning 0) 2))
            (punctuation-pos (1- (line-end-position))))
        (should-not (get-text-property indent-pos 'ai-code-session-link))
        (should-not (get-text-property indent-pos 'face))
        (should (eq (get-text-property indent-pos 'mouse-face)
                    'highlight))
        (should (equal (get-text-property path-pos 'ai-code-session-link)
                       url))
        (should-not (get-text-property punctuation-pos
                                       'ai-code-session-link))))))

(ert-deftest ai-code-session-link-test-linkify-session-region-does-not-wrap-url-into-prose ()
  "Do not merge an ordinary following prose line into a URL."
  (with-temp-buffer
    (insert "Visit https://example.com/path\nnext line\n")
    (ai-code-session-link--linkify-session-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "https://example.com/path")
    (should (equal (get-text-property (match-beginning 0)
                                      'ai-code-session-link)
                   "https://example.com/path"))
    (forward-line 1)
    (should-not (get-text-property (point) 'ai-code-session-link))
    (should-not (get-text-property (point) 'mouse-face))))

(ert-deftest ai-code-session-link-test-linkify-session-region-does-not-wrap-url-into-word ()
  "Do not merge a single ordinary following word into a URL."
  (with-temp-buffer
    (insert "Visit https://example.com/path\ndone\n")
    (ai-code-session-link--linkify-session-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "https://example.com/path")
    (should (equal (get-text-property (match-beginning 0)
                                      'ai-code-session-link)
                   "https://example.com/path"))
    (forward-line 1)
    (should-not (get-text-property (point) 'ai-code-session-link))
    (should-not (get-text-property (point) 'mouse-face))))

(ert-deftest ai-code-session-link-test-linkify-session-region-does-not-wrap-url-after-slash ()
  "Do not merge prose after an ordinary URL ending in a slash."
  (with-temp-buffer
    (insert "Visit https://example.com/path/\ndone\n")
    (ai-code-session-link--linkify-session-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "https://example.com/path/")
    (should (equal (get-text-property (match-beginning 0)
                                      'ai-code-session-link)
                   "https://example.com/path/"))
    (forward-line 1)
    (should-not (get-text-property (point) 'ai-code-session-link))
    (should-not (get-text-property (point) 'mouse-face))))

(ert-deftest ai-code-session-link-test-linkify-session-region-does-not-wrap-url-after-sentence-punctuation ()
  "Do not merge prose after a URL with trailing sentence punctuation."
  (with-temp-buffer
    (insert "See https://example.com/path.\nDone\n")
    (ai-code-session-link--linkify-session-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "https://example.com/path")
    (should (equal (get-text-property (match-beginning 0)
                                      'ai-code-session-link)
                   "https://example.com/path"))
    (goto-char (match-end 0))
    (should-not (get-text-property (point) 'ai-code-session-link))
    (forward-line 1)
    (should-not (get-text-property (point) 'ai-code-session-link))
    (should-not (get-text-property (point) 'mouse-face))))

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

(ert-deftest ai-code-session-link-test-linkify-session-region-matches-uppercase-project-basename ()
  "Linkify uppercase or mixed-case basename references for project files."
  (let* ((root (make-temp-file "ai-code-session-links-uppercase-basename-" t))
         (src-dir (expand-file-name "src" root))
         (c-file (expand-file-name "Foo.C" src-dir))
         (readme-file (expand-file-name "README.MD" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file c-file
            (insert "int main(void) { return 0; }\n"))
          (with-temp-file readme-file
            (insert "# README\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "Foo.C:12\nREADME.MD\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward-regexp "Foo\\.C:12")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "Foo.C:12"))
            (search-forward-regexp "README\\.MD")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "README.MD"))))
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
  "Linkify later nearby symbols within the fixed 512-char and 3-line budget."
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
            (dotimes (_ 1)
              (insert "plain prose words only\n"))
            (insert (make-string 80 ?x))
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

(ert-deftest ai-code-session-link-test-linkify-session-region-avoids-eager-file-resolution-for-nearby-symbols ()
  "Linkify nearby symbols without resolving file paths during redraw."
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
            (should (zerop resolve-count))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-avoids-project-files-for-basename-links ()
  "Avoid project file enumeration while linkifying basename references."
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
            (should (zerop project-files-count))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-skips-project-files-across-passes ()
  "Repeated relinkify passes should avoid project file enumeration."
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
            (should (zerop project-files-count))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test--open-file-link-resolves-basename-on-demand ()
  "Basename file links should still resolve when the user activates them."
  (let* ((root (make-temp-file "ai-code-session-links-basename-open-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "Foo.java" src-dir))
         source-buffer)
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class Foo {}\nnext line\n"))
          (cl-letf (((symbol-function 'find-file-other-window)
                     (lambda (path)
                       (setq source-buffer (find-file-noselect path))
                       (set-buffer source-buffer)
                       source-buffer)))
            (let ((default-directory root))
              (let ((ai-code-backends-infra--session-directory root))
                (should (ai-code-session-link--open-file-link "Foo.java:2"))
                (should (buffer-live-p source-buffer))
                (with-current-buffer source-buffer
                  (should (equal (buffer-file-name) file))
                  (should (= (line-number-at-pos) 2)))))))
      (when (and source-buffer (buffer-live-p source-buffer))
        (kill-buffer source-buffer))
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
                       (error "Xref unavailable")))
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
                       (error "Xref unavailable")))
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

(ert-deftest ai-code-session-link-test-linkify-session-region-supports-wrapped-local-file ()
  "Linkify all terminal rows of a hard-wrapped local file path."
  (let* ((root (make-temp-file "ai-code-session-links-wrapped-path-" t))
         (dir (expand-file-name "layout-check" root))
         (file (expand-file-name "window-after-53660.png" dir))
         (wrapped-file
          (concat (file-name-as-directory root)
                  "layout-\n  check/\n  window-\n  after-53660.png")))
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-temp-file file
            (insert "fake image bytes\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert wrapped-file)
            (insert "\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward (file-name-as-directory root))
            (let ((first-row-pos (match-beginning 0)))
              (should (equal (get-text-property first-row-pos 'ai-code-session-link)
                             file))
              (should (eq (get-text-property first-row-pos 'face) 'link)))
            (search-forward "  check/")
            (let ((indent-pos (match-beginning 0))
                  (second-row-pos (+ (match-beginning 0) 2)))
              (should-not (get-text-property indent-pos 'ai-code-session-link))
              (should-not (get-text-property indent-pos 'face))
              (should (equal (get-text-property second-row-pos 'ai-code-session-link)
                             file))
              (should (eq (get-text-property second-row-pos 'face) 'link)))
            (search-forward "  window-")
            (let ((indent-pos (match-beginning 0))
                  (third-row-pos (+ (match-beginning 0) 2)))
              (should-not (get-text-property indent-pos 'ai-code-session-link))
              (should-not (get-text-property indent-pos 'face))
              (should (equal (get-text-property third-row-pos 'ai-code-session-link)
                             file))
              (should (eq (get-text-property third-row-pos 'face) 'link)))
            (search-forward "  after-53660.png")
            (let ((indent-pos (match-beginning 0))
                  (last-row-pos (+ (match-beginning 0) 2)))
              (should-not (get-text-property indent-pos 'ai-code-session-link))
              (should-not (get-text-property indent-pos 'face))
              (should (equal (get-text-property last-row-pos 'ai-code-session-link)
                             file))
              (should (eq (get-text-property last-row-pos 'face) 'link)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-supports-wrapped-file-before-argument ()
  "Linkify a hard-wrapped file path followed by another command argument."
  (let* ((root (make-temp-file "ai-code-session-links-wrapped-arg-" t))
         (dir (expand-file-name "tmp/config" root))
         (file (expand-file-name "init.el" dir))
         (prefix (file-name-as-directory root))
         (suffix (file-relative-name file prefix)))
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-temp-file file
            (insert ";;; init.el\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "emacs -Q --batch -l ")
            (insert prefix)
            (insert "\n")
            (insert suffix)
            (insert " --eval\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward prefix)
            (let ((first-row-pos (match-beginning 0))
                  (newline-pos (line-end-position)))
              (should (equal (get-text-property first-row-pos
                                                'ai-code-session-link)
                             file))
              (should (eq (get-text-property newline-pos 'mouse-face)
                          'highlight)))
            (search-forward suffix)
            (let ((second-row-pos (match-beginning 0))
                  (argument-pos (match-end 0)))
              (should (equal (get-text-property second-row-pos
                                                'ai-code-session-link)
                             file))
              (should (eq (get-text-property second-row-pos 'face)
                          'link))
              (should-not (get-text-property argument-pos
                                             'ai-code-session-link))
              (should-not (get-text-property argument-pos 'mouse-face)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-rejects-missing-wrapped-file-before-argument ()
  "Do not merge missing hard-wrapped file paths before arguments."
  (let* ((root (make-temp-file "ai-code-session-links-missing-arg-" t))
         (prefix (file-name-as-directory root))
         (suffix "tmp/config/missing.el"))
    (unwind-protect
        (with-temp-buffer
          (setq-local ai-code-backends-infra--session-directory root)
          (insert "emacs -Q --batch -l ")
          (insert prefix)
          (insert "\n")
          (insert suffix)
          (insert " --eval\n")
          (ai-code-session-link--linkify-session-region (point-min) (point-max))
          (goto-char (point-min))
          (search-forward prefix)
          (should-not (equal (get-text-property (match-beginning 0)
                                                'ai-code-session-link)
                             (concat prefix suffix)))
          (search-forward suffix)
          (should-not (equal (get-text-property (match-beginning 0)
                                                'ai-code-session-link)
                             (concat prefix suffix))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-connects-wrapped-hover ()
  "Hover properties should span wrapped path gaps without underlining them."
  (let* ((root (make-temp-file "ai-code-session-links-wrapped-hover-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "feature.el" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert ";;; feature.el\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/\n  feature.el\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "src/")
            (let ((first-row-pos (match-beginning 0))
                  (newline-pos (line-end-position)))
              (should (eq (get-text-property first-row-pos 'mouse-face)
                          'highlight))
              (should (eq (get-text-property newline-pos 'mouse-face)
                          'highlight)))
            (search-forward "  feature.el")
            (let ((indent-pos (match-beginning 0))
                  (file-pos (+ (match-beginning 0) 2)))
              (should-not (get-text-property indent-pos 'ai-code-session-link))
              (should-not (get-text-property indent-pos 'face))
              (should (eq (get-text-property indent-pos 'mouse-face)
                          'highlight))
              (should (equal (get-text-property file-pos 'ai-code-session-link)
                             "src/feature.el"))
              (should (eq (get-text-property file-pos 'mouse-face)
                          'highlight)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-supports-wrapped-relative-file ()
  "Linkify a relative project file whose first row ends at a directory."
  (let* ((root (make-temp-file "ai-code-session-links-wrapped-relative-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "feature.el" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert ";;; feature.el\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/\n  feature.el\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "src/")
            (let ((first-row-pos (match-beginning 0)))
              (should (equal (get-text-property first-row-pos 'ai-code-session-link)
                             "src/feature.el"))
              (should (eq (get-text-property first-row-pos 'face) 'link)))
            (search-forward "  feature.el")
            (let ((indent-pos (match-beginning 0))
                  (file-pos (+ (match-beginning 0) 2)))
              (should-not (get-text-property indent-pos 'ai-code-session-link))
              (should-not (get-text-property indent-pos 'face))
              (should (equal (get-text-property file-pos 'ai-code-session-link)
                             "src/feature.el"))
              (should (eq (get-text-property file-pos 'face) 'link)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-supports-wrapped-line-suffixes ()
  "Linkify file line suffixes wrapped after their prefix."
  (let* ((root (make-temp-file "ai-code-session-links-wrapped-suffixes-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "foo.el" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert ";;; foo.el\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/foo.el:\n  42\n")
            (insert "src/foo.el#L\n  42\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "src/foo.el:")
            (let ((path-pos (match-beginning 0)))
              (should (equal (get-text-property path-pos 'ai-code-session-link)
                             "src/foo.el:42"))
              (forward-line 1)
              (let ((indent-pos (line-beginning-position))
                    (line-pos (+ (line-beginning-position) 2)))
                (should-not (get-text-property indent-pos 'ai-code-session-link))
                (should-not (get-text-property indent-pos 'face))
                (should (equal (get-text-property line-pos 'ai-code-session-link)
                               "src/foo.el:42"))))
            (search-forward "src/foo.el#L")
            (let ((path-pos (match-beginning 0)))
              (should (equal (get-text-property path-pos 'ai-code-session-link)
                             "src/foo.el#L42"))
              (forward-line 1)
              (let ((indent-pos (line-beginning-position))
                    (line-pos (+ (line-beginning-position) 2)))
                (should-not (get-text-property indent-pos 'ai-code-session-link))
                (should-not (get-text-property indent-pos 'face))
                (should (equal (get-text-property line-pos 'ai-code-session-link)
                               "src/foo.el#L42"))))))
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

(ert-deftest ai-code-session-link-test-ghostel-image-preview-adds-overlay ()
  "Ghostel sessions should preview local image file links."
  (let* ((root (make-temp-file "ai-code-session-image-preview-" t))
         (image-file (expand-file-name "screenshot.png" root))
         (ai-code-session-link-ghostel-image-preview-vertical-margin 'auto)
         (line-height 20)
         (created-image nil))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (setq created-image (cons data args))
                       (list :image data :args args)))
                    ((symbol-function 'default-line-height)
                     (lambda () line-height)))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
              (insert "Saved screenshot.png\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (goto-char (point-min))
              (search-forward "screenshot.png")
              (let* ((link-start (match-beginning 0))
                     (link-end (match-end 0))
                     (line-end (line-end-position))
                     (preview-overlays
                      (cl-remove-if-not
                       (lambda (overlay)
                         (overlay-get overlay 'ai-code-session-image-preview))
                       (overlays-in (point-min) (point-max)))))
                (should (equal (get-text-property (match-beginning 0)
                                                  'ai-code-session-link)
                               "screenshot.png"))
                (should (= (length preview-overlays) 1))
                (should (equal (overlay-get (car preview-overlays)
                                            'ai-code-session-image-file)
                               image-file))
                (should (= (overlay-get (car preview-overlays)
                                        'ai-code-session-image-link-start)
                           link-start))
                (should (= (overlay-get (car preview-overlays)
                                        'ai-code-session-image-link-end)
                           link-end))
                (should (= (overlay-start (car preview-overlays)) line-end))
                (should (= (overlay-end (car preview-overlays))
                           (1+ line-end)))
                (should (equal (overlay-get (car preview-overlays) 'display)
                               (list :image "fake image bytes" :args
                                     (cdr created-image))))
                (should (equal (overlay-get (car preview-overlays)
                                            'after-string)
                               "\n"))
                (should (equal (car created-image) "fake image bytes"))
                (should-not (equal (car created-image) image-file))
                (should (nth 2 created-image))
                (should (equal (cadr (memq :margin created-image))
                               '(0 . 6)))
                (should (member :max-width created-image))
                (should-not (member :max-height created-image))
                (setq line-height 30)
                (ai-code-session-link--linkify-session-region
                 (point-min) (point-max))
                (should (equal (cadr (memq :margin created-image))
                               '(0 . 9)))
                (setq ai-code-session-link-ghostel-image-preview-vertical-margin
                      4)
                (ai-code-session-link--linkify-session-region
                 (point-min) (point-max))
                (should (equal (cadr (memq :margin created-image))
                               '(0 . 4)))
                (setq ai-code-session-link-ghostel-image-preview-vertical-margin
                      0)
                (ai-code-session-link--linkify-session-region
                 (point-min) (point-max))
                (should-not (memq :margin created-image))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-follows-trailing-punctuation ()
  "Image previews should appear after sentence punctuation following a path."
  (let* ((root (make-temp-file "ai-code-session-image-preview-punctuation-" t))
         (image-file (expand-file-name "screenshot.png" root)))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (list :image data :args args))))
            (dolist (suffix (list "." (string ?\u3002)))
              (with-temp-buffer
                (setq-local ai-code-backends-infra--session-directory root)
                (setq-local ai-code-backends-infra--session-terminal-backend
                            'ghostel)
                (insert "Saved " image-file suffix "\n")
                (ai-code-session-link--linkify-session-region
                 (point-min) (point-max))
                (goto-char (point-min))
                (search-forward image-file)
                (let* ((link-start (match-beginning 0))
                       (link-end (match-end 0))
                       (line-end (line-end-position))
                       (preview-overlays
                        (cl-remove-if-not
                         (lambda (overlay)
                           (overlay-get overlay 'ai-code-session-image-preview))
                         (overlays-in (point-min) (point-max))))
                       (overlay (car preview-overlays)))
                  (should (= (length preview-overlays) 1))
                  (should (= (overlay-get overlay
                                          'ai-code-session-image-link-start)
                             link-start))
                  (should (= (overlay-get overlay
                                          'ai-code-session-image-link-end)
                             link-end))
                  (should (= (overlay-start overlay) line-end))
                  (should (= (overlay-end overlay) (1+ line-end)))
                  (should (equal (overlay-get
                                  overlay
                                  'ai-code-session-image-display-text)
                                 (concat image-file suffix)))
                  (should (equal (overlay-get
                                  overlay
                                  'ai-code-session-image-link-text)
                                 image-file))
                  (should-not
                   (ai-code-session-link--image-preview-refresh-needed-p
                    (point-min) (point-max))))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-remote-ghostel-generic-linkify-skips-file-stats ()
  "Remote Ghostel generic linkification should not stat TRAMP candidates."
  (let ((remote-root "/mock-remote/tmp/project/")
        (file-exists-called nil)
        (create-image-called nil))
    (cl-letf (((symbol-function 'display-images-p)
               (lambda (&optional _display) t))
              ((symbol-function 'file-remote-p)
               (lambda (file &rest _args)
                 (and (stringp file)
                      (string-prefix-p "/mock-remote/" file))))
              ((symbol-function 'file-exists-p)
               (lambda (_file)
                 (setq file-exists-called t)
                 nil))
              ((symbol-function 'create-image)
               (lambda (&rest _args)
                 (setq create-image-called t)
                 'image)))
      (with-temp-buffer
        (setq-local ai-code-backends-infra--session-directory
                    remote-root)
        (setq-local ai-code-backends-infra--session-terminal-backend
                    'ghostel)
        (setq-local default-directory remote-root)
        (insert "Saved ./remote.png\n")
        (ai-code-session-link--linkify-session-region
         (point-min) (point-max))
        (goto-char (point-min))
        (search-forward "./remote.png")
        (should (equal (get-text-property (match-beginning 0)
                                          'ai-code-session-link)
                       "./remote.png"))
        (should-not file-exists-called)
        (should-not create-image-called)
        (should-not
         (cl-some
          (lambda (overlay)
            (overlay-get overlay 'ai-code-session-image-preview))
          (overlays-in (point-min) (point-max))))))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-wrapped-bare-path ()
  "A bare local image path hard-wrapped across terminal rows should preview.
Tools such as Claude print bare paths (no `file://' prefix); when the path
is long enough Ghostel wraps it, splitting the token across rows.  The
detection regexp must stitch those rows back together."
  (let* ((root (make-temp-file "ai-code-session-image-preview-wrap-" t))
         (image-file
          (expand-file-name
           "a-very-long-wrapped-screenshot-name-for-terminal-testing.png"
           root))
         (split (/ (length image-file) 2))
         (head (substring image-file 0 split))
         (tail (substring image-file split))
         (created-image nil))
    ;; Guard against an accidental split inside a space run; the temp path
    ;; has none, but keep the invariant explicit for future readers.
    (should-not (string-match-p "[ \t]" image-file))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (setq created-image (cons data args))
                       (list :image data :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
              ;; Mimic a Ghostel hard wrap: the path breaks mid-token and the
              ;; continuation row carries a leading render gutter.
              (insert (format "Wrote %s\n    %s\n" head tail))
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (let ((preview-overlays
                     (cl-remove-if-not
                      (lambda (overlay)
                        (overlay-get overlay 'ai-code-session-image-preview))
                      (overlays-in (point-min) (point-max)))))
                (should (= (length preview-overlays) 1))
                (should (equal (overlay-get (car preview-overlays)
                                            'ai-code-session-image-file)
                               image-file))
                (should (equal (car created-image) "fake image bytes")))
              (dolist (fragment (list head tail))
                (goto-char (point-min))
                (search-forward fragment)
                (let ((fragment-start (match-beginning 0))
                      (fragment-end (match-end 0)))
                  (while (< fragment-start fragment-end)
                    (should-not
                     (get-char-property fragment-start 'display))
                    (should-not
                     (get-char-property fragment-start 'invisible))
                    (setq fragment-start (1+ fragment-start))))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest test-ai-code-session-link--ghostel-image-preview-uses-newline-display-overlay ()
  "Image previews should occupy a dedicated row after their visible path."
  (let* ((root (make-temp-file "ai-code-session-image-preview-indent-" t))
         (image-file (expand-file-name "screenshot.png" root)))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (file &rest args)
                       (list :image file :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
              (insert "    | /tmp/nope.txt\n")
              (insert "    | screenshot.png\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (goto-char (point-min))
              (search-forward "screenshot.png")
              (let ((path-start (match-beginning 0))
                    (preview-overlays
                     (cl-remove-if-not
                      (lambda (overlay)
                        (overlay-get overlay 'ai-code-session-image-preview))
                      (overlays-in (point-min) (point-max)))))
                (should (= (length preview-overlays) 1))
                (should (overlay-get (car preview-overlays) 'display))
                (should-not (get-char-property path-start 'display))
                (should (eq (get-char-property path-start 'face) 'link))
                (should (eq (get-char-property path-start 'mouse-face)
                            'highlight))
                (should (eq (char-after
                             (overlay-start (car preview-overlays)))
                            ?\n))
                (should (equal (overlay-get (car preview-overlays)
                                            'before-string)
                               "\n    "))
                (should (equal (overlay-get (car preview-overlays)
                                            'after-string)
                               "\n"))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest test-ai-code-session-link--ghostel-image-preview-keeps-path-visible ()
  "An image preview must not replace or hide its source path."
  (let* ((root (make-temp-file "ai-code-session-image-preview-visible-path-" t))
         (image-file (expand-file-name "screenshot.png" root)))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (file &rest args)
                       (list :image file :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              (insert "  ╰  screenshot.png\n")
              (ai-code-session-link--linkify-session-region
               (point-min) (point-max))
              (goto-char (point-min))
              (search-forward "screenshot.png")
              (let ((path-start (match-beginning 0))
                    (path-end (match-end 0)))
                (while (< path-start path-end)
                  (should-not (get-char-property path-start 'display))
                  (should-not (get-char-property path-start 'invisible))
                  (setq path-start (1+ path-start)))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest test-ai-code-session-link--ghostel-image-preview-aligns-after-tree-path ()
  "A tree-prefixed preview should align below its still-visible path."
  (let* ((root (make-temp-file "ai-code-session-image-preview-align-" t))
         (image-file (expand-file-name "screenshot.png" root)))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (file &rest args)
                       (list :image file :args args))))
            (dolist (text '("  ╰  screenshot.png\n"
                            "  ╰\n     screenshot.png\n"
                            "  └  screenshot.png\n"
                            "  └\n     screenshot.png\n"))
              (with-temp-buffer
                (setq-local ai-code-backends-infra--session-directory root)
                (setq-local ai-code-backends-infra--session-terminal-backend
                            'ghostel)
                (insert text)
                (ai-code-session-link--linkify-session-region
                 (point-min) (point-max))
                (goto-char (point-min))
                (search-forward "screenshot.png")
                (let ((path-start (match-beginning 0)))
                  (goto-char path-start)
                  (let ((expected-indent (make-string (current-column) ?\s))
                        (preview
                         (cl-find-if
                          (lambda (overlay)
                            (overlay-get overlay
                                         'ai-code-session-image-preview))
                          (overlays-in (point-min) (point-max)))))
                    (should preview)
                      (should-not (get-char-property path-start 'display))
                    (should (equal (overlay-get preview 'before-string)
                                   (concat "\n" expected-indent)))))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest test-ai-code-session-link--ghostel-image-preview-rejects-nonleading-tree-marker ()
  "A non-leading tree marker should use only the row's whitespace indent."
  (let* ((root (make-temp-file "ai-code-session-image-preview-tree-negative-" t))
         (image-file (expand-file-name "screenshot.png" root)))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (file &rest args)
                       (list :image file :args args))))
            (dolist (text '("> ╰  screenshot.png\n"
                            "| ╰\n     screenshot.png\n"))
              (with-temp-buffer
                (setq-local ai-code-backends-infra--session-directory root)
                (setq-local ai-code-backends-infra--session-terminal-backend
                            'ghostel)
                (insert text)
                (ai-code-session-link--linkify-session-region
                 (point-min) (point-max))
                (goto-char (point-min))
                (search-forward "screenshot.png")
                (let ((path-start (match-beginning 0)))
                  (goto-char path-start)
                  (let* ((line-start (line-beginning-position))
                         (indent-end
                          (save-excursion
                            (goto-char line-start)
                            (skip-chars-forward " \t" (line-end-position))
                            (point)))
                         (expected-indent
                          (buffer-substring-no-properties
                           line-start indent-end))
                         (preview
                          (cl-find-if
                           (lambda (overlay)
                             (overlay-get overlay
                                          'ai-code-session-image-preview))
                           (overlays-in (point-min) (point-max)))))
                    (should preview)
                    (should (equal (overlay-get preview 'before-string)
                                   (concat "\n" expected-indent)))))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest test-ai-code-session-link--ghostel-image-preview-refreshes-after-tree-prefix-removal ()
  "Removing a tree prefix should rebuild its preview with row indentation."
  (let* ((root (make-temp-file "ai-code-session-image-preview-tree-refresh-" t))
         (image-file (expand-file-name "screenshot.png" root)))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (file &rest args)
                       (list :image file :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              (insert "  ╰  screenshot.png\n")
              (ai-code-session-link--linkify-session-region
               (point-min) (point-max))
              (let ((old-preview
                     (cl-find-if
                      (lambda (overlay)
                        (overlay-get overlay
                                     'ai-code-session-image-preview))
                      (overlays-in (point-min) (point-max)))))
                (should old-preview)
                (should (equal (overlay-get old-preview 'before-string)
                               "\n     "))
                (goto-char (point-min))
                (search-forward "╰")
                (replace-match ">" t t)
                (ai-code-session-link--linkify-session-region
                 (point-min) (point-max))
                (let ((previews
                       (cl-remove-if-not
                        (lambda (overlay)
                          (overlay-get overlay
                                       'ai-code-session-image-preview))
                        (overlays-in (point-min) (point-max)))))
                  (should (= (length previews) 1))
                  (should (equal (overlay-get (car previews) 'before-string)
                                 "\n  "))
                  (should-not (overlay-buffer old-preview)))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest test-ai-code-session-link--ghostel-image-preview-keeps-line-suffix-visible ()
  "A terminal row's source path and suffix should remain visible together."
  (let* ((root (make-temp-file "ai-code-session-image-preview-suffix-" t))
         (image-file (expand-file-name "screenshot.png" root)))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (file &rest args)
                       (list :image file :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
              (insert "> screenshot.png  describe this image\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (let* ((overlay
                      (cl-find-if
                       (lambda (candidate)
                         (overlay-get candidate 'ai-code-session-image-preview))
                       (overlays-in (point-min) (point-max))))
                     (line-end (save-excursion
                                 (goto-char (point-min))
                                 (line-end-position))))
                (should overlay)
                (should (= (overlay-start overlay) line-end))
                (should (= (overlay-end overlay) (1+ line-end)))
                (should (= (overlay-get overlay
                                        'ai-code-session-image-display-end)
                           line-end))
                (should (equal (overlay-get overlay 'before-string) "\n"))
                (should (equal (overlay-get overlay 'after-string) "\n"))
                (goto-char (point-min))
                (search-forward "screenshot.png  describe this image")
                (let ((row-start (match-beginning 0))
                      (row-end (match-end 0)))
                  (while (< row-start row-end)
                    (should-not (get-char-property row-start 'display))
                    (should-not (get-char-property row-start 'invisible))
                    (setq row-start (1+ row-start))))
                (should-not
                 (overlay-get overlay
                              'ai-code-session-image-preview-suffix-overlay))
                (ai-code-session-link--delete-image-preview-overlays
                 (point-min) line-end)
                (should-not (overlay-buffer overlay))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest test-ai-code-session-link--ghostel-image-preview-does-not-overlap-suffix-image ()
  "A second image reference must not overlap the preview owning its row."
  (let* ((root (make-temp-file "ai-code-session-image-preview-overlap-" t))
         (primary-file (expand-file-name "primary.png" root))
         (suffix-file (expand-file-name "rendered.png" root)))
    (unwind-protect
        (progn
          (dolist (file (list primary-file suffix-file))
            (with-temp-file file
              (insert "fake image bytes")))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (file &rest args)
                       (list :image file :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              (insert primary-file "  rendered.png\n")
              (ai-code-session-link--linkify-strict-image-preview-region
               (point-min) (point-max))
              (let ((previews
                     (cl-remove-if-not
                      (lambda (overlay)
                        (overlay-get overlay 'ai-code-session-image-preview))
                      (overlays-in (point-min) (point-max)))))
                (should (= (length previews) 1))
                (should (equal (overlay-get (car previews)
                                            'ai-code-session-image-file)
                               primary-file))
                (should (equal (overlay-get (car previews) 'after-string)
                               "\n"))
                (goto-char (point-min))
                (search-forward "rendered.png")
                (should-not (get-char-property (match-beginning 0) 'display))
                (should-not
                 (get-char-property (match-beginning 0) 'invisible))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest test-ai-code-session-link--image-preview-uses-lifecycle-seam ()
  "Preview scans should transact once and honor the position owner."
  (let* ((root (make-temp-file "ai-code-session-image-preview-seam-" t))
         (image-file (expand-file-name "screenshot.png" root))
         (transaction-count 0)
         (position-count 0))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (file &rest args)
                       (list :image file :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              (setq-local
               ai-code-session-link-image-preview-transaction-function
               (lambda (_start _end function)
                 (setq transaction-count (1+ transaction-count))
                 (funcall function)))
              (setq-local
               ai-code-session-link-image-preview-position-function
               (lambda (_start _end)
                 (setq position-count (1+ position-count))
                 nil))
              (insert "screenshot.png\n")
              (ai-code-session-link--linkify-strict-image-preview-region
               (point-min) (point-max))
              (should (= transaction-count 1))
              (should (= position-count 1))
              (should-not
               (cl-find-if
                (lambda (overlay)
                  (overlay-get overlay 'ai-code-session-image-preview))
                (overlays-in (point-min) (point-max)))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest test-ai-code-session-link--normal-linkify-transacts-image-layout ()
  "Normal file linkification should use one image lifecycle transaction."
  (let* ((root (make-temp-file "ai-code-session-image-linkify-seam-" t))
         (image-file (expand-file-name "screenshot.png" root))
         (transaction-count 0))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (file &rest args)
                       (list :image file :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              (setq-local
               ai-code-session-link-image-preview-transaction-function
               (lambda (_start _end function)
                 (setq transaction-count (1+ transaction-count))
                 (funcall function)))
              (insert "screenshot.png\n")
              (ai-code-session-link--linkify-session-region
               (point-min) (point-max))
              (should (= transaction-count 1))
              (should
               (cl-find-if
                (lambda (overlay)
                  (overlay-get overlay 'ai-code-session-image-preview))
                (overlays-in (point-min) (point-max)))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest test-ai-code-session-link--ghostel-image-preview-keeps-link-navigation ()
  "A display preview should preserve navigation on the underlying image link."
  (let* ((root (make-temp-file "ai-code-session-image-preview-click-" t))
         (image-file (expand-file-name "screenshot.png" root)))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (file &rest args)
                       (list :image file :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
              (insert "Saved screenshot.png\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (let* ((preview-overlays
                      (cl-remove-if-not
                       (lambda (overlay)
                         (overlay-get overlay 'ai-code-session-image-preview))
                       (overlays-in (point-min) (point-max))))
                     (overlay (car preview-overlays)))
                (should (= (length preview-overlays) 1))
                (goto-char (point-min))
                (search-forward "screenshot.png")
                (should (eq (lookup-key (get-text-property (match-beginning 0)
                                                           'keymap)
                                        [mouse-1])
                            'ai-code-session-navigate-link-at-mouse))
                (should (overlay-get overlay 'display))
                (should (equal (overlay-get overlay 'after-string) "\n"))
                (should-not (overlay-get overlay 'keymap))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-recreates-missing-overlay ()
  "Image previews should be recreated when link text is already linked."
  (let* ((root (make-temp-file "ai-code-session-image-preview-recreate-" t))
         (image-file (expand-file-name "screenshot.png" root)))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (file &rest args)
                       (list :image file :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
              (insert "Saved screenshot.png\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (dolist (overlay (overlays-in (point-min) (point-max)))
                (when (overlay-get overlay 'ai-code-session-image-preview)
                  (delete-overlay overlay)))
              (goto-char (point-min))
              (search-forward "screenshot.png")
              (should (get-text-property (match-beginning 0)
                                         'ai-code-session-link))
              (ai-code-session-link--linkify-file-region
               (point-min) (point-max) t)
              (should
               (= (length
                   (cl-remove-if-not
                    (lambda (overlay)
                      (overlay-get overlay 'ai-code-session-image-preview))
                    (overlays-in (point-min) (point-max))))
                  1)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-refreshes-unchanged-history ()
  "Image previews should refresh for unchanged Ghostel history text."
  (let* ((root (make-temp-file "ai-code-session-image-preview-history-" t))
         (image-file (expand-file-name "history.png" root)))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
            (insert "Saved history.png\n")
            (cl-letf (((symbol-function 'display-images-p)
                       (lambda (&optional _display) nil)))
              (ai-code-session-link--linkify-session-region (point-min) (point-max)))
            (goto-char (point-min))
            (search-forward "history.png")
            (should (get-text-property (match-beginning 0)
                                       'ai-code-session-link))
            (should-not
             (cl-some
              (lambda (overlay)
                (overlay-get overlay 'ai-code-session-image-preview))
              (overlays-in (point-min) (point-max))))
            (cl-letf (((symbol-function 'display-images-p)
                       (lambda (&optional _display) t))
                      ((symbol-function 'create-image)
                       (lambda (file &rest args)
                         (list :image file :args args))))
              (ai-code-session-link--linkify-session-region (point-min) (point-max)))
            (should
             (= (length
                 (cl-remove-if-not
                  (lambda (overlay)
                    (overlay-get overlay 'ai-code-session-image-preview))
                  (overlays-in (point-min) (point-max))))
                1))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-keeps-repeated-history-stable ()
  "Unchanged Ghostel history should not duplicate live image previews."
  (let* ((root (make-temp-file "ai-code-session-image-preview-repeat-" t))
         (image-file (expand-file-name "history.png" root))
         (create-count 0))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (file &rest args)
                       (setq create-count (1+ create-count))
                       (list :image file :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              (insert "Saved history.png\n")
              (ai-code-session-link--linkify-session-region
               (point-min) (point-max))
              ;; One descriptor probes the source dimensions; the other is
              ;; the final backing-pixel-aware preview descriptor.
              (should (= create-count 2))
              (ai-code-session-link--linkify-session-region
               (point-min) (point-max))
              (should (= create-count 2))
              (should
               (= (length
                   (cl-remove-if-not
                    (lambda (overlay)
                      (overlay-get overlay 'ai-code-session-image-preview))
                    (overlays-in (point-min) (point-max))))
                  1)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-strict-visible-scan ()
  "Strict visible image scanning should preview local images without project scans."
  (let* ((root (make-temp-file "ai-code-session-image-preview-visible-" t))
         (image-file (expand-file-name "visible.png" root))
         project-scan-count)
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (list :image data :args args)))
                    ((symbol-function 'ai-code-session-link--project-files)
                     (lambda (&rest _args)
                       (setq project-scan-count (1+ (or project-scan-count 0)))
                       nil)))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              (insert "Saved to: " image-file "\n")
              (setq buffer-read-only t)
              (ai-code-session-link--linkify-strict-image-preview-region
               (point-min) (point-max))
              (should-not project-scan-count)
              (should
               (= (length
                   (cl-remove-if-not
                    (lambda (overlay)
                      (overlay-get overlay 'ai-code-session-image-preview))
                    (overlays-in (point-min) (point-max))))
                  1)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest test-ai-code-session-link--position-owner-removes-existing-preview ()
  "A rejected preview position should not retain an existing overlay."
  (with-temp-buffer
    (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
    (insert "/tmp/input.png")
    (let ((preview (make-overlay (point-min) (point-max)))
          (suffix (make-overlay (point-max) (point-max))))
      (overlay-put preview 'ai-code-session-image-preview t)
      (overlay-put preview 'ai-code-session-image-preview-suffix-overlay
                   suffix)
      (setq-local ai-code-session-link-image-preview-position-function
                  (lambda (_start _end) nil))
      (cl-letf (((symbol-function 'display-images-p)
                 (lambda (&optional _display) t)))
        (ai-code-session-link--apply-image-preview-for-file
         (point-min) (point-max) "/tmp/input.png" "/tmp/input.png"))
      (should-not (overlay-buffer preview))
      (should-not (overlay-buffer suffix)))))

(ert-deftest test-ai-code-session-link--linkify-strict-image-preview-region-refresh ()
  "Repeated scans preserve unchanged previews and refresh changed files."
  (let* ((root (make-temp-file "ai-code-session-image-rescan-" t))
         (image-file (expand-file-name "preview.png" root))
         (create-count 0))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (setq create-count (1+ create-count))
                       (list :image data :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              (insert "Saved " image-file "\n")
              (ai-code-session-link--linkify-strict-image-preview-region
               (point-min) (point-max))
              (let ((preview
                     (cl-find-if
                      (lambda (overlay)
                        (overlay-get overlay 'ai-code-session-image-preview))
                      (overlays-in (point-min) (point-max)))))
                (should preview)
                (ai-code-session-link--linkify-strict-image-preview-region
                 (point-min) (point-max))
                (should (= create-count 2))
                (should (eq (overlay-buffer preview) (current-buffer)))
                (with-temp-file image-file
                  (insert "updated image bytes with a different size"))
                (ai-code-session-link--linkify-strict-image-preview-region
                 (point-min) (point-max))
                (should (= create-count 4))
                (should-not (overlay-buffer preview))
                (should
                 (cl-find-if
                  (lambda (overlay)
                    (overlay-get overlay 'ai-code-session-image-preview))
                  (overlays-in (point-min) (point-max))))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-strict-visible-wrap ()
  "Strict visible image scanning should handle simple wrapped file URLs."
  (let* ((root (make-temp-file "ai-code-session-image-preview-visible-wrap-" t))
         (dir (expand-file-name "generated_images/session" root))
         (image-file (expand-file-name "ig_wrapped_image.png" dir))
         (split-index (- (length image-file)
                         (length (file-name-nondirectory image-file)))))
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (list :image data :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              (insert "file://")
              (insert (substring image-file 0 split-index))
              (insert "\n  ")
              (let ((segment-start (point)))
                (insert (substring image-file split-index))
                (insert "\n")
                (setq buffer-read-only t)
                (ai-code-session-link--linkify-strict-image-preview-region
                 (point-min) (point-max))
                (let ((preview-overlays
                       (cl-remove-if-not
                        (lambda (overlay)
                          (overlay-get overlay 'ai-code-session-image-preview))
                        (overlays-in (point-min) (point-max)))))
                  (should (= (length preview-overlays) 1))
                  (should (equal (get-text-property
                                  segment-start
                                  'ai-code-session-link)
                                 (concat "file://" image-file)))
                  (should (equal (overlay-get (car preview-overlays)
                                              'ai-code-session-image-file)
                                 image-file)))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest test-ai-code-session-link--linkify-strict-image-preview-region-file-url-wrap-before-slash ()
  "Strict scanning should join a file URL wrapped before a path slash."
  (let* ((root (make-temp-file
                "ai-code-session-image-preview-wrap-before-slash-" t))
         (dir (expand-file-name "com.openai.sky.CUAService" root))
         (image-file (expand-file-name
                      "Emacs Screenshot 2026-07-14 at 00.38.35.jpeg"
                      dir))
         (encoded-file
          (replace-regexp-in-string " " "%20" image-file t t))
         (split-index
          (1- (- (length encoded-file)
                 (length (file-name-nondirectory encoded-file)))))
         continuation-start)
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (list :image data :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              (insert "{\"screenshot\": {\"url\": \"file://")
              (insert (substring encoded-file 0 split-index))
              (insert "\n")
              (setq continuation-start (point))
              (insert (substring encoded-file split-index))
              (insert "\"}}\n")
              (setq buffer-read-only t)
              (ai-code-session-link--linkify-strict-image-preview-region
               continuation-start (point-max))
              (let ((preview-overlays
                     (cl-remove-if-not
                      (lambda (overlay)
                        (overlay-get overlay 'ai-code-session-image-preview))
                      (overlays-in (point-min) (point-max)))))
                (should (= (length preview-overlays) 1))
                (should (equal (get-text-property
                                continuation-start
                                'ai-code-session-link)
                               (concat "file://" encoded-file)))
                (should (equal (overlay-get (car preview-overlays)
                                            'ai-code-session-image-file)
                               image-file))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest test-ai-code-session-link--strict-image-candidates-absolute-candidate-precedence ()
  "A standalone absolute image path should precede a joined candidate."
  (with-temp-buffer
    (insert "file:///tmp/wrapped-prefix\n")
    (let ((start (point))
          (link-text "/tmp/standalone.png"))
      (insert link-text)
      (should
       (equal
        (mapcar
         (lambda (candidate)
           (plist-get candidate :link-text))
         (ai-code-session-link--strict-image-candidates
          start (point) link-text))
        '("/tmp/standalone.png"
          "file:///tmp/wrapped-prefix/tmp/standalone.png"))))))

(ert-deftest test-ai-code-session-link--strict-image-candidates-path-after-unicode-punctuation ()
  "A strong image path after Unicode prose punctuation should stand alone."
  (with-temp-buffer
    (let ((path "/tmp/screenshot.png"))
      (insert "结果见 report.org:42，")
      (let ((path-start (point)))
        (insert path "。")
        (goto-char (point-min))
        (re-search-forward "\\.png")
        (let* ((bounds
                (ai-code-session-link--strict-image-candidate-bounds
                 (match-beginning 0) (match-end 0)))
               (link-text
                (buffer-substring-no-properties (car bounds) (cdr bounds)))
               (candidates
                (ai-code-session-link--strict-image-candidates
                 (car bounds) (cdr bounds) link-text)))
          (should (= (plist-get (car candidates) :start) path-start))
          (should (equal (plist-get (car candidates) :link-text) path)))))))

(ert-deftest test-ai-code-session-link--strict-image-candidates-keeps-punctuation-in-path ()
  "A path that starts strongly should retain punctuation in its directories."
  (with-temp-buffer
    (let ((path "/tmp/a，/screenshot.png"))
      (insert path)
      (should-not
       (ai-code-session-link--strict-image-path-suffix-info path)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-file-url-wrap ()
  "Ghostel image previews should handle terminal-wrapped file:// URLs."
  (let* ((root (make-temp-file "ai-code-session-image-preview-url-" t))
         (dir (expand-file-name "generated_images/019f3849-01c6-70d2-9306-f19721bfa0f4" root))
         (image-file (expand-file-name
                      "ig_0316e59b09ff2b45016a4be031a5b8819ab73e1af52a3e7bd2.png"
                      dir))
         (split-index (- (length image-file)
                         (length (file-name-nondirectory image-file))))
         (create-image-data nil))
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (setq create-image-data data)
                       (list :image data :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
              (insert "  file://")
              (insert (substring image-file 0 split-index))
              (insert "\n")
              (insert "    ")
              (insert (substring image-file split-index))
              (insert "\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (let ((preview-overlays
                     (cl-remove-if-not
                      (lambda (overlay)
                        (overlay-get overlay 'ai-code-session-image-preview))
                      (overlays-in (point-min) (point-max)))))
                (should (= (length preview-overlays) 1))
                (should (equal create-image-data "fake image bytes"))
                (should (equal (overlay-get (car preview-overlays)
                                            'ai-code-session-image-file)
                               image-file))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest test-ai-code-session-link--file-url-image-regexp-rejects-incomplete-path-promptly ()
  "An incomplete file URL must not trigger exponential regexp backtracking."
  (should-not
   (string-match-p
    ai-code-session-link--file-url-image-regexp
    (concat "file:" (make-string 64 ?a)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-strict-visible-wrap-bare-path ()
  "Strict visible scanning should rejoin a wrapped BARE absolute path.
Regression: `ai-code-session-link--strict-previous-line-prefix' used a
malformed negated character class (a `]' escaped with a backslash) that
closed the class early, so it only matched lines ending in a literal `]'.
Bare wrapped paths -- as printed by tools such as Claude, which omit the
`file://' prefix -- were therefore never recovered from the previous row."
  (let* ((root (make-temp-file
                "ai-code-session-image-preview-visible-wrap-bare-" t))
         (dir (expand-file-name
               "generated/2d806356-90b2-4e71-82fa-30359ee62190" root))
         (image-file (expand-file-name "ghostel-wrapped-bare-image.png" dir))
         ;; Break right before the file name so the continuation row is a
         ;; relative fragment (no leading slash) -- this is what forces the
         ;; previous-line prefix recovery path (the buggy branch).
         (split-index (- (length image-file)
                         (length (file-name-nondirectory image-file)))))
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (list :image data :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              ;; Bare path (no file:// prefix) hard-wrapped across two rows,
              ;; each carrying a leading render gutter.
              (insert "  ")
              (insert (substring image-file 0 split-index))
              (insert "\n    ")
              (insert (substring image-file split-index))
              (insert "\n")
              (setq buffer-read-only t)
              (ai-code-session-link--linkify-strict-image-preview-region
               (point-min) (point-max))
              (let ((preview-overlays
                     (cl-remove-if-not
                      (lambda (overlay)
                        (overlay-get overlay 'ai-code-session-image-preview))
                      (overlays-in (point-min) (point-max)))))
                (should (= (length preview-overlays) 1))
                (should (equal (overlay-get (car preview-overlays)
                                            'ai-code-session-image-file)
                               image-file))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-strict-visible-wrap-three-rows ()
  "Strict visible scanning should rejoin bare paths wrapped over three rows."
  (let* ((root (make-temp-file
                "ai-code-session-image-preview-visible-wrap-three-" t))
         (dir (expand-file-name "layout-check" root))
         (image-file (expand-file-name "window-after-53660.png" dir))
         (needle "layout-check/window-after-53660.png")
         (needle-start (string-match (regexp-quote needle) image-file))
         (path-prefix (substring image-file 0 needle-start)))
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (list :image data :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              (insert "  ")
              (insert path-prefix)
              (insert "layout-\n    check/window-after-\n    53660.png\n")
              (setq buffer-read-only t)
              (ai-code-session-link--linkify-strict-image-preview-region
               (point-min) (point-max))
              (let ((preview-overlays
                     (cl-remove-if-not
                      (lambda (overlay)
                        (overlay-get overlay 'ai-code-session-image-preview))
                      (overlays-in (point-min) (point-max)))))
                (should (= (length preview-overlays) 1))
                (goto-char (point-min))
                (search-forward "layout-")
                (should (eq (get-text-property (match-beginning 0)
                                               'mouse-face)
                            'highlight))
                (search-forward "    check/")
                (let ((indent-pos (match-beginning 0))
                      (path-pos (+ (match-beginning 0) 4)))
                  (should-not (get-text-property indent-pos
                                                 'ai-code-session-link))
                  (should-not (get-text-property indent-pos 'face))
                  (should (eq (get-text-property indent-pos 'mouse-face)
                              'highlight))
                  (should (equal (get-text-property path-pos
                                                    'ai-code-session-link)
                                 image-file))
                  (should (eq (get-text-property path-pos 'mouse-face)
                              'highlight)))
                (should (equal (overlay-get (car preview-overlays)
                                            'ai-code-session-image-file)
                               image-file))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-image-preview-max-dimensions ()
  "Preview caps apply when no window is available for fitting."
  ;; An explicit integer custom is used verbatim, regardless of any window.
  (let ((ai-code-session-link-ghostel-image-preview-max-width 400)
        (ai-code-session-link-ghostel-image-preview-max-height 300))
    (with-temp-buffer
      (should (equal (ai-code-session-link--image-preview-max-dimensions "")
                     '(400 . 300)))))
  ;; With nil customs and no window showing the buffer, only the fixed width
  ;; fallback applies.  Height remains uncapped because there is no visible
  ;; window to fit.
  (let ((ai-code-session-link-ghostel-image-preview-max-width nil)
        (ai-code-session-link-ghostel-image-preview-max-height nil))
    (with-temp-buffer
      (should (equal (ai-code-session-link--image-preview-max-dimensions "  ")
                     (cons ai-code-session-link--image-preview-fallback-max-width
                           nil))))))

(ert-deftest test-ai-code-session-link--image-preview-uses-frame-backing-scale ()
  "Preview rasterization requests backing pixels before logical scaling."
  (let (captured-properties create-count)
    (cl-letf (((symbol-function 'ai-code-session-link--image-preview-data)
               (lambda (_file) "png data"))
              ((symbol-function 'ai-code-session-link--image-preview-format-hint)
               (lambda (_file) 'image/png))
              ((symbol-function 'image-type)
               (lambda (&rest _args) 'png))
              ((symbol-function 'frame-scale-factor)
               (lambda (&optional _frame) 2.0))
              ((symbol-function 'create-image)
               (lambda (_data _type _data-p &rest properties)
                 (setq create-count (1+ (or create-count 0)))
                 (push properties captured-properties)
                 (if (= create-count 1) 'source-image 'preview-image)))
              ((symbol-function 'image-size)
               (lambda (image &optional _pixels _frame)
                 (should (eq image 'source-image))
                 '(1984 . 436)))
              ((symbol-function 'image-flush) #'ignore))
      (should
       (eq (ai-code-session-link--create-image-preview
            "retina.png" 869 nil 0)
           'preview-image))
      (setq captured-properties (nreverse captured-properties))
      (should (= (length captured-properties) 2))
      (should (= (plist-get (car captured-properties) :scale) 1.0))
      (let ((display-properties (cadr captured-properties)))
        (should (= (plist-get display-properties :width) 1738))
        (should (= (plist-get display-properties :height) 382))
        (should (= (plist-get display-properties :scale) 0.5))
        (should-not (plist-member display-properties :max-width))))))

(ert-deftest test-ai-code-session-link--image-preview-refreshes-for-frame-scale ()
  "A preview created at 1x should be rebuilt after moving to a 2x frame."
  (let* ((root (make-temp-file "ai-code-session-image-frame-scale-" t))
         (image-file (expand-file-name "retina.png" root))
         (frame-scale 1.0)
         (create-count 0))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "png data"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'frame-scale-factor)
                     (lambda (&optional _frame) frame-scale))
                    ((symbol-function 'create-image)
                     (lambda (_data _type _data-p &rest properties)
                       (setq create-count (1+ create-count))
                       (if (cl-oddp create-count)
                           'source-image
                         (cons 'image properties))))
                    ((symbol-function 'image-size)
                     (lambda (image &optional _pixels _frame)
                       (should (eq image 'source-image))
                       '(1984 . 436)))
                    ((symbol-function 'image-flush) #'ignore))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              (insert "Viewed retina.png\n")
              (ai-code-session-link--linkify-session-region
               (point-min) (point-max))
              (let ((preview
                     (cl-find-if
                      (lambda (overlay)
                        (overlay-get overlay 'ai-code-session-image-preview))
                      (overlays-in (point-min) (point-max)))))
                (should preview)
                (should (= create-count 2))
                (should (= (plist-get (cdr (overlay-get preview 'display))
                                      :scale)
                           1.0))
                (setq frame-scale 2.0)
                (ai-code-session-link--linkify-session-region
                 (point-min) (point-max))
                (let ((retina-preview
                       (cl-find-if
                        (lambda (overlay)
                          (overlay-get overlay
                                       'ai-code-session-image-preview))
                        (overlays-in (point-min) (point-max)))))
                  (should retina-preview)
                  (should-not (eq retina-preview preview))
                  (should (= create-count 4))
                  (should (= (plist-get
                              (cdr (overlay-get retina-preview 'display))
                              :scale)
                             0.5)))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest test-ai-code-session-link--image-preview-allows-tall-visible-window ()
  "A Ghostel preview remains tall unless the user supplies a height cap."
  (save-window-excursion
    (with-temp-buffer
      (let ((window (selected-window))
            (ai-code-session-link-ghostel-image-preview-max-height nil))
        (set-window-buffer window (current-buffer))
        (cl-letf (((symbol-function 'window-body-width)
                   (lambda (&rest _args) 800))
                  ((symbol-function 'frame-char-width)
                   (lambda (&rest _args) 10)))
          ;; Tall real display lines are necessary for fractional scrolling.
          (should-not (cdr (ai-code-session-link--image-preview-max-dimensions
                            "")))
          ;; A user cap remains an explicit hard bound.
          (let ((ai-code-session-link-ghostel-image-preview-max-height 1000))
            (should (= (cdr (ai-code-session-link--image-preview-max-dimensions
                             ""))
                       1000)))
          (let ((ai-code-session-link-ghostel-image-preview-max-height 200))
            (should (= (cdr (ai-code-session-link--image-preview-max-dimensions
                             ""))
                       200))))))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-common-reference-forms ()
  "Ghostel image previews should handle common local image reference forms."
  (let* ((root (make-temp-file "ai-code-session-image-preview-forms-" t))
         (image-dir (expand-file-name "generated images" root))
         (spaced-image (expand-file-name "My Image.png" image-dir))
         (relative-dir (expand-file-name "relative output" root))
         (relative-image (expand-file-name "Relative Image.png" relative-dir))
         (plain-image (expand-file-name "plain.png" root)))
    (unwind-protect
        (progn
          (make-directory image-dir t)
          (make-directory relative-dir t)
          (dolist (file (list spaced-image relative-image plain-image))
            (with-temp-file file
              (insert "fake image bytes")))
          (cl-labels
              ((preview-file-for
                (output)
                (let (create-image-called)
                  (cl-letf (((symbol-function 'display-images-p)
                             (lambda (&optional _display) t))
                            ((symbol-function 'create-image)
                             (lambda (data &rest args)
                               (setq create-image-called t)
                               (list :image data :args args))))
                    (with-temp-buffer
                      (setq-local ai-code-backends-infra--session-directory root)
                      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
                      (insert output)
                      (ai-code-session-link--linkify-session-region
                       (point-min) (point-max))
                      (let* ((preview-overlays
                              (cl-remove-if-not
                               (lambda (overlay)
                                 (overlay-get overlay 'ai-code-session-image-preview))
                               (overlays-in (point-min) (point-max))))
                             (preview-count (length preview-overlays))
                             (preview-file
                              (and create-image-called
                                   (overlay-get (car preview-overlays)
                                                'ai-code-session-image-file))))
                        (cons preview-file preview-count)))))))
            (let ((cases
                   (list
                    (list (format "![preview](file://%s)\n"
                                  (replace-regexp-in-string " " "%20" spaced-image))
                          spaced-image)
                    (list (format "[[file:%s][preview]]\n" plain-image)
                          plain-image)
                    (list (format "file://localhost%s\n" plain-image)
                          plain-image)
                    (list (format "Saved to: \"%s\"\n" spaced-image)
                          spaced-image)
                    (list (format "Saved to: %s\n"
                                  (replace-regexp-in-string " " "\\\\ " spaced-image))
                          spaced-image)
                    (list "![preview](relative output/Relative Image.png)\n"
                          relative-image))))
              (dolist (case cases)
                (let ((result (preview-file-for (car case))))
                  (should (equal (car result) (cadr case)))
                  (should (= (cdr result) 1)))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-image-preview-skips-non-ghostel-session ()
  "Image preview overlays should not be added for non-Ghostel terminals."
  (let* ((root (make-temp-file "ai-code-session-image-preview-skip-" t))
         (image-file (expand-file-name "screenshot.png" root))
         (create-image-called nil))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (&rest _args)
                       (setq create-image-called t)
                       'image)))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend 'vterm)
              (insert "Saved screenshot.png\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (should-not create-image-called)
              (should-not
               (cl-some
                (lambda (overlay)
                  (overlay-get overlay 'ai-code-session-image-preview))
                (overlays-in (point-min) (point-max)))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-image-preview-respects-size-limit ()
  "Image previews should skip files over the configured byte limit."
  (let* ((root (make-temp-file "ai-code-session-image-preview-size-" t))
         (image-file (expand-file-name "large.png" root))
         (create-image-called nil)
         (ai-code-session-link-ghostel-image-preview-max-bytes 1))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "too large"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (&rest _args)
                       (setq create-image-called t)
                       'image)))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
              (insert "Saved large.png\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (should-not create-image-called))))
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

(ert-deftest ai-code-session-link-test-scheduled-linkify-defers-while-inhibited ()
  "Delayed linkification should wait while a buffer-local inhibit hook is active."
  (let (called rescheduled)
    (with-temp-buffer
      (insert "src/File.el:12\n")
      (let ((inhibited t)
            (ai-code-session-link--linkify-inhibited-retry-delay 3600))
        (setq-local ai-code-session-link--pending-tail-width 20)
        (setq-local ai-code-session-link-inhibit-functions
                    (list (lambda (_buffer) inhibited)))
        (cl-letf (((symbol-function
                    'ai-code-session-link--linkify-session-region)
                   (lambda (start end)
                     (setq called (cons start end)))))
          (unwind-protect
              (progn
                (ai-code-session-link--flush-scheduled-linkify)
                (setq rescheduled ai-code-session-link--linkify-timer)
                (should rescheduled)
                (should (= ai-code-session-link--pending-tail-width 20))
                (should-not called)
                (cancel-timer rescheduled)
                (setq ai-code-session-link--linkify-timer nil
                      inhibited nil)
                (ai-code-session-link--flush-scheduled-linkify)
                (should called)
                (should (= ai-code-session-link--pending-tail-width 0)))
            (when (timerp ai-code-session-link--linkify-timer)
              (cancel-timer ai-code-session-link--linkify-timer))))))))

(provide 'test_ai-code-session-link)

;;; test_ai-code-session-link.el ends here
