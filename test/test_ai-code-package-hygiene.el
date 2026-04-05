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

(provide 'test_ai-code-package-hygiene)

;;; test_ai-code-package-hygiene.el ends here
