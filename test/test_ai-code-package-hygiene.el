;;; test_ai-code-package-hygiene.el --- Package metadata hygiene tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Regression tests for package metadata that CI packaging checks rely on.

;;; Code:

(require 'ert)
(require 'loaddefs-gen)

(loaddefs-generate
 default-directory
 (expand-file-name "ai-code-autoloads.el" default-directory))

(defun ai-code-test--file-prefix (path length)
  "Return the first LENGTH characters from PATH."
  (with-temp-buffer
    (insert-file-contents path nil 0 length)
    (buffer-string)))

(defun ai-code-test--variable-initializer (file definition variable)
  "Return VARIABLE's initializer from DEFINITION in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward
     (format "^(%s %s\\_>" definition variable))
    (goto-char (match-beginning 0))
    (nth 2 (read (current-buffer)))))

(ert-deftest ai-code-test-autoloads-file-has-standard-generated-header ()
  "Autoloads file should identify itself as generated output."
  (let ((header (ai-code-test--file-prefix "ai-code-autoloads.el" 400)))
    (should (string-match-p "automatically extracted autoloads" header))))

(ert-deftest ai-code-test-autoloads-file-has-code-section ()
  "Autoloads file should include a Code section."
  (let ((header (ai-code-test--file-prefix "ai-code-autoloads.el" 400)))
    (should (string-match-p "^;;; Code:" header))))

(ert-deftest ai-code-test-autoloads-file-omits-harness-test-after-change-custom ()
  "Autoloads file should omit the harness-only test-after-change custom."
  (with-temp-buffer
    (insert-file-contents "ai-code-autoloads.el")
    (should-not
     (search-forward "ai-code-test-after-code-change-suffix" nil t))
    (should-not
     (search-forward "run unit-tests and follow up on the test-result" nil t))
    (should-not (search-forward "get_diagnostics MCP tool" nil t))))

(ert-deftest ai-code-test-bundled-prompts-contain-high-value-guidance ()
  "Bundled prompts and snippets should discourage low-value duplicate tests."
  (dolist (file '("prompt/test-after-change.v1.md"
                  "prompt/test-after-change-diagnostics.v1.md"
                  "prompt/tdd.v1.md"
                  "prompt/tdd-diagnostics.v1.md"
                  "prompt/tdd-with-refactoring.v1.md"
                  "prompt/tdd-with-refactoring-diagnostics.v1.md"
                  "snippets/ai-code-prompt-mode/create-tests"
                  "snippets/ai-code-prompt-mode/unit-tests"))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should (search-forward "small set of high-value" nil t))
      (should (search-forward "duplicate tests" nil t)))))

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

(ert-deftest test-ai-code-package-hygiene--autoloads-includes-native-send-commands ()
  "Autoloads file should expose the native Insert commands."
  (with-temp-buffer
    (insert-file-contents "ai-code-autoloads.el")
    (dolist (command '("ai-code-send-file"
                       "ai-code-send-screenshot"
                       "ai-code-send-clipboard-image"
                       "ai-code-send-region"
                       "ai-code-send-dwim"
                       "ai-code-send-dwim-to"))
      (should (re-search-forward
               (format "(autoload '%s " command)
               nil t)))))

(ert-deftest test-ai-code-package-hygiene--autoload-screenshot-default-matches-source ()
  "Autoloads should preserve the platform-specific screenshot default."
  (should
   (equal
    (ai-code-test--variable-initializer
     "ai-code-autoloads.el" "defvar" 'ai-code-send-screenshot-command)
    (ai-code-test--variable-initializer
     "ai-code-send.el" "defcustom" 'ai-code-send-screenshot-command))))

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
