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

(ert-deftest ai-code-test-secondary-files-use-standard-keywords ()
  "Secondary package files should use standard finder keywords."
  (dolist (file '("ai-code-eca.el" "ai-code-backends-infra.el"))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should
       (re-search-forward "^;; Keywords: .*\\_<\\(convenience\\|tools\\)\\_>" nil t)))))

(ert-deftest ai-code-test-package-files-avoid-config-load-forms ()
  "Package files should avoid configuration-only load forms."
  (dolist (file '("ai-code-input.el" "ai-code-eca.el" "ai-code-behaviors.el"))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not
       (re-search-forward "(\\s-*\\(?:with-\\)?eval-after-load\\_>" nil t)))))

(ert-deftest ai-code-test-private-functions-are-not-autoloaded ()
  "Private helper functions should not have autoload cookies."
  (dolist (file '("ai-code-github.el" "ai-code-git.el"))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not
       (re-search-forward "^;;;###autoload\n(defun [[:alnum:]-]+--" nil t)))))

(ert-deftest ai-code-test-generated-autoloads-omit-private-helpers ()
  "Generated autoloads should not expose private helper functions."
  (with-temp-buffer
    (insert-file-contents "ai-code-autoloads.el")
    (should-not
     (re-search-forward "^(autoload 'ai-code--" nil t))))

(ert-deftest ai-code-test-behaviors-avoids-literal-emacs-dotdir ()
  "Behavior docs should not mention the literal Emacs config directory."
  (with-temp-buffer
    (insert-file-contents "ai-code-behaviors.el")
    (should-not (re-search-forward "\\_<\\.emacs\\.d\\_>" nil t))))

(ert-deftest ai-code-test-melpazoid-workflow-build-is-not-hard-disabled ()
  "MELPA packaging workflow should not hard-disable its build job."
  (with-temp-buffer
    (insert-file-contents ".github/workflows/melpazoid.yml")
    (should-not (re-search-forward "^[[:space:]]+if:[[:space:]]+false[[:space:]]*$" nil t))))

(provide 'test_ai-code-package-hygiene)

;;; test_ai-code-package-hygiene.el ends here
