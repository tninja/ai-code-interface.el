;;; test_ai-code-harness.el --- Tests for ai-code-harness.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for harness generation and prompt suffix helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'ai-code-harness)

(defvar ai-code-mcp-agent-enabled-backends nil)
(defvar ai-code--tdd-run-test-after-each-stage-instruction)

(ert-deftest ai-code-test-resolve-tdd-suffix-includes-strict-stage-contract ()
  "Test that TDD suffix names Red and Green stages and forbids skipping."
  (let ((ai-code--tdd-test-pattern-instruction ""))
    (let ((suffix (ai-code--test-after-code-change--resolve-tdd-suffix)))
      (should (string-match-p "Do not skip stages" suffix))
      (should (string-match-p "Stage 1 - Red" suffix))
      (should (string-match-p "Stage 2 - Green" suffix))
      (should (string-match-p "Do not refactor during Green" suffix)))))

(ert-deftest ai-code-test-resolve-tdd-suffix-reuses-shared-each-stage-instruction ()
  "Test that TDD suffix can reuse shared each-stage instruction when available."
  (let ((ai-code--tdd-test-pattern-instruction "")
        (ai-code--tdd-run-test-after-each-stage-instruction
         " SHARED_EACH_STAGE_TEST_INSTRUCTION"))
    (should (string-match-p "SHARED_EACH_STAGE_TEST_INSTRUCTION"
                            (ai-code--test-after-code-change--resolve-tdd-suffix)))))

(ert-deftest ai-code-test-auto-test-harness-reference-suffix-tells-ai-to-use-local-harness ()
  "Test that harness reference prompt tells AI to read and use the harness."
  (let* ((temp-root (make-temp-file "ai-code-harness-root-" t))
         (ai-files-dir (expand-file-name ".ai.code.files/" temp-root))
         (ai-code-auto-test-harness-cache-directory nil)
         (ai-code-mcp-agent-enabled-backends '(codex))
         (ai-code-selected-backend 'codex)
         (ai-code--tdd-test-pattern-instruction ""))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--ensure-files-directory)
                   (lambda () ai-files-dir))
                  ((symbol-function 'ai-code--git-root)
                   (lambda (&optional _dir) temp-root)))
          (let ((suffix (ai-code--auto-test-harness-reference-suffix 'tdd-with-refactoring)))
            (should (string-match-p "Read the local harness file:" suffix))
            (should (string-match-p "Use its instructions for this work\\." suffix))
            (should (string-match-p
                     (regexp-quote "@.ai.code.files/harness/tdd-with-refactoring-diagnostics.v1.md")
                     suffix))))
      (delete-directory temp-root t))))

(ert-deftest ai-code-test-resolve-tdd-suffix-includes-diagnostics-first-loop ()
  "Test that TDD suffix requires diagnostics checks before completion."
  (let ((ai-code--tdd-test-pattern-instruction "")
        (case-fold-search nil)
        (ai-code-mcp-agent-enabled-backends '(codex))
        (ai-code-selected-backend 'codex))
    (let ((suffix (ai-code--test-after-code-change--resolve-tdd-suffix)))
      (should (string-match-p "get_diagnostics" suffix))
      (should (string-match-p "get_diagnostics MCP tool" suffix))
      (should (string-match-p "baseline" suffix))
      (should (string-match-p "no new diagnostics" suffix)))))

(ert-deftest ai-code-test-resolve-tdd-suffix-omits-diagnostics-for-non-mcp-backend ()
  "Test that TDD suffix omits diagnostics for unsupported backends."
  (let ((ai-code--tdd-test-pattern-instruction "")
        (ai-code-mcp-agent-enabled-backends '(codex))
        (ai-code-selected-backend 'gemini))
    (let ((suffix (ai-code--test-after-code-change--resolve-tdd-suffix)))
      (should-not (string-match-p "get_diagnostics" suffix))
      (should-not (string-match-p "no new diagnostics" suffix)))))

(ert-deftest ai-code-test-maybe-append-diagnostics-harness-instruction-preserves-nil-suffix ()
  "Test that diagnostics harness logic preserves a nil suffix."
  (let ((ai-code-selected-backend 'codex)
        (ai-code-mcp-agent-enabled-backends '(codex)))
    (should-not (ai-code--maybe-append-diagnostics-harness-instruction nil))
    (should-not (ai-code--maybe-append-diagnostics-harness-instruction nil t))))

(ert-deftest ai-code-test-auto-test-harness-directory-defaults-to-ai-code-files-harness ()
  "Test that harness directory defaults to `.ai.code.files/harness/`."
  (let* ((temp-root (make-temp-file "ai-code-harness-root-" t))
         (ai-files-dir (expand-file-name ".ai.code.files/" temp-root))
         (ai-code-auto-test-harness-cache-directory nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--ensure-files-directory)
                   (lambda () ai-files-dir)))
          (should (equal (expand-file-name "harness/" ai-files-dir)
                         (ai-code--auto-test-harness-directory))))
      (delete-directory temp-root t))))

(ert-deftest ai-code-test-ensure-auto-test-harness-cache-directory-tolerates-unbound-custom ()
  "Test that harness directory creation falls back when the custom is unbound."
  (let* ((temp-root (make-temp-file "ai-code-harness-root-" t))
         (ai-files-dir (expand-file-name ".ai.code.files/" temp-root))
         (expected-directory (expand-file-name "harness/" ai-files-dir))
         (was-bound (boundp 'ai-code-auto-test-harness-cache-directory))
         (original-value (when was-bound ai-code-auto-test-harness-cache-directory)))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--ensure-files-directory)
                   (lambda () ai-files-dir)))
          (makunbound 'ai-code-auto-test-harness-cache-directory)
          (should (equal expected-directory
                         (ai-code--ensure-auto-test-harness-cache-directory)))
          (should (file-directory-p expected-directory)))
      (if was-bound
          (setq ai-code-auto-test-harness-cache-directory original-value)
        (makunbound 'ai-code-auto-test-harness-cache-directory))
      (delete-directory temp-root t))))

(ert-deftest ai-code-test-auto-test-harness-prompt-path-uses-repo-relative-at-path ()
  "Test that harness prompt path becomes an `@` repo-relative path."
  (let* ((temp-root (make-temp-file "ai-code-harness-root-" t))
         (harness-file (expand-file-name ".ai.code.files/harness/tdd.v1.md" temp-root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory harness-file) t)
          (with-temp-file harness-file
            (insert "harness"))
          (cl-letf (((symbol-function 'ai-code--git-root)
                     (lambda (&optional _dir) temp-root)))
            (should (equal "@.ai.code.files/harness/tdd.v1.md"
                           (ai-code--auto-test-harness-prompt-path harness-file)))))
      (delete-directory temp-root t))))

(ert-deftest ai-code-test-auto-test-harness-prompt-path-keeps-sibling-path-absolute ()
  "Test that sibling paths with a shared prefix are not treated as repo-local."
  (let* ((temp-root (make-temp-file "ai-code-harness-parent-" t))
         (git-root (directory-file-name (expand-file-name "repo/" temp-root)))
         (external-root (expand-file-name "repo-cache/" temp-root))
         (harness-file (expand-file-name "harness/tdd.v1.md" external-root)))
    (unwind-protect
        (progn
          (make-directory git-root t)
          (make-directory (file-name-directory harness-file) t)
          (with-temp-file harness-file
            (insert "harness"))
          (cl-letf (((symbol-function 'ai-code--git-root)
                     (lambda (&optional _dir) git-root)))
            (should (equal harness-file
                           (ai-code--auto-test-harness-prompt-path harness-file)))))
      (delete-directory temp-root t))))

(ert-deftest ai-code-test-auto-test-harness-cache-directory-docs-cover-non-repo-fallback ()
  "Test that the harness directory custom documents the non-repo fallback."
  (let ((doc (documentation-property 'ai-code-auto-test-harness-cache-directory
                                     'variable-documentation)))
    (should (string-match-p "Outside a Git repository" doc))
    (should (string-match-p "default-directory" doc))
    (should
     (equal
      '(choice
        (const :tag "Use default harness directory (.ai.code.files/harness in a repo, or harness under default-directory otherwise)"
               nil)
        directory)
      (get 'ai-code-auto-test-harness-cache-directory 'custom-type)))))

(provide 'test_ai-code-harness)

;;; test_ai-code-harness.el ends here
