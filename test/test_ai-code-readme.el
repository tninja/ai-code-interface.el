;;; test_ai-code-readme.el --- Tests for README harness guidance -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for README.org guidance.

;;; Code:

(require 'ert)

(ert-deftest ai-code-test-readme-harness-engineering-docs-mention-diagnostics-first-loop ()
  "Test that README harness guidance documents the diagnostics-first loop."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "README.org" default-directory))
    (should (re-search-forward "\\*\\*\\* Harness Engineering Practice" nil t))
    (should (re-search-forward "get_diagnostics" nil t))
    (should (re-search-forward "baseline" nil t))
    (should (re-search-forward "no new diagnostics" nil t))))

(provide 'test_ai-code-readme)

;;; test_ai-code-readme.el ends here
