;;; test_ai-code-package-hygiene.el --- Package metadata hygiene tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Regression tests for package metadata that CI packaging checks rely on.

;;; Code:

(require 'ert)

(defun ai-code-test--file-prefix (path length)
  "Return the first LENGTH characters from PATH."
  (with-temp-buffer
    (insert-file-contents path nil 0 length)
    (buffer-string)))

(ert-deftest ai-code-test-autoloads-file-has-spdx-header ()
  "Autoloads file should advertise the package license with SPDX."
  (let ((header (ai-code-test--file-prefix "ai-code-autoloads.el" 400)))
    (should (string-match-p "SPDX-License-Identifier: Apache-2\\.0" header))))

(ert-deftest ai-code-test-autoloads-file-has-commentary-section ()
  "Autoloads file should include a Commentary section for package checks."
  (let ((header (ai-code-test--file-prefix "ai-code-autoloads.el" 400)))
    (should (string-match-p "^;;; Commentary:" header))))

(ert-deftest ai-code-test-autoloads-file-keeps-generic-test-after-change-default ()
  "Autoloads file should keep the backend-agnostic test-after-change default."
  (with-temp-buffer
    (insert-file-contents "ai-code-autoloads.el")
    (should (search-forward "run unit-tests and follow up on the test-result" nil t))
    (should-not (search-forward "get_diagnostics MCP tool" nil t))))

(ert-deftest ai-code-test-ai-code-el-does-not-autoload-private-diagnostics-constant ()
  "Private diagnostics helper constants should not be marked for autoload."
  (with-temp-buffer
    (insert-file-contents "ai-code.el")
    (should-not
     (re-search-forward
      "^;;;###autoload\n(defconst ai-code--diagnostics-first-harness-instruction\\_>"
      nil t))))

(ert-deftest ai-code-test-autoloads-file-includes-lint-current-file-command ()
  "Autoloads file should expose `ai-code-lint-current-file'."
  (with-temp-buffer
    (insert-file-contents "ai-code-autoloads.el")
    (should
     (re-search-forward
      "(autoload 'ai-code-lint-current-file "
      nil t))))

(provide 'test_ai-code-package-hygiene)

;;; test_ai-code-package-hygiene.el ends here
