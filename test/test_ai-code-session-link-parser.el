;;; test_ai-code-session-link-parser.el --- Pure session link parser tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Regression tests for the parser without loading terminal rendering.

;;; Code:

(require 'ert)
(require 'ai-code-session-link-parser)

(ert-deftest ai-code-session-link-parser-test-normalize-file ()
  "Normalize file URLs and wrapped terminal paths without filesystem access."
  (should (equal (ai-code-session-link--normalize-file " @src/Foo.java ")
                 "src/Foo.java"))
  (should (equal (ai-code-session-link--normalize-file
                  "<file:///tmp/My%20Image.png>")
                 "/tmp/My Image.png"))
  (should (equal (ai-code-session-link--normalize-file
                  "./screens/My\\ Image.png")
                 "./screens/My Image.png"))
  (should (equal (ai-code-session-link--normalize-file
                  "/tmp/image-\n  wrapped.png")
                 "/tmp/image-wrapped.png"))
  (should-not (ai-code-session-link--normalize-file "   ")))

(ert-deftest ai-code-session-link-parser-test-wrapped-url-and-link-text ()
  "Preserve existing differing whitespace rules for URLs and file links."
  (should (equal (ai-code-session-link--normalize-link-text
                  "/tmp/My \n Image.png")
                 "/tmp/My Image.png"))
  (should (equal (ai-code-session-link--normalize-url-link-text
                  "https://example.com/long-  \n path")
                 "https://example.com/long-path")))

(ert-deftest ai-code-session-link-parser-test-file-line-and-column ()
  "Parse source locations and image URLs with the legacy plist shape."
  (should (equal (ai-code-session-link--parse-file-link-text
                  "src/Foo.java:42:7")
                 '(:file "src/Foo.java" :line-start 42 :column-start 7)))
  (should (equal (ai-code-session-link--parse-file-link-text
                  "src/Foo.java#L42-L60")
                 '(:file "src/Foo.java" :line-start 42 :column-start nil)))
  (should (equal (ai-code-session-link--parse-file-link-text
                  "file:///tmp/photo.png")
                 '(:file "file:///tmp/photo.png")))
  (should (equal (ai-code-session-link--parse-file-link-text "src/photo.png")
                 '(:file "src/photo.png")))
  (should-not (ai-code-session-link--parse-file-link-text "not a path")))

(provide 'test_ai-code-session-link-parser)

;;; test_ai-code-session-link-parser.el ends here
