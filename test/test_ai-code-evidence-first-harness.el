;;; test_ai-code-evidence-first-harness.el --- Tests for Evidence-First harness -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the Evidence-First Coding Harness routing and bundled prompts.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-harness)

(defun ai-code-evidence-first-test--prompt-content (diagnostics-p)
  "Return bundled Evidence-First harness text for DIAGNOSTICS-P."
  (let* ((package-dir (file-name-directory (file-truename (locate-library "ai-code"))))
         (file-name (if diagnostics-p
                        "evidence-first-coding-harness-diagnostics.v1.md"
                      "evidence-first-coding-harness.v1.md"))
         (file-path (expand-file-name file-name
                                     (expand-file-name "prompt/" package-dir))))
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string))))

(ert-deftest ai-code-test-evidence-first-harness-is-ask-me-choice ()
  "Evidence-First harness should be available in per-send harness choices."
  (should
   (equal
    (cdr (assoc "Evidence-First coding harness"
                ai-code--auto-test-type-ask-choices))
    'evidence-first-coding-harness)))

(ert-deftest ai-code-test-evidence-first-harness-routes-through-local-prompt ()
  "Evidence-First harness should use the existing local harness routing."
  (cl-letf (((symbol-function 'ai-code--diagnostics-harness-enabled-p)
             (lambda () nil)))
    (let ((suffix
           (ai-code--auto-test-suffix-for-type
            'evidence-first-coding-harness)))
      (should (string-match-p "Read the local harness file:" suffix))
      (should (string-match-p
               "evidence-first-coding-harness\\.v1\\.md"
               suffix)))))

(ert-deftest ai-code-test-evidence-first-harness-keeps-process-flexible ()
  "Evidence-First harness should require evidence without mandatory TDD."
  (let ((content (ai-code-evidence-first-test--prompt-content nil)))
    (should (string-match-p "not a mandatory ceremony" content))
    (should (string-match-p "Do not force a RED → GREEN sequence" content))
    (should (string-match-p "For bug fixes, first reproduce the defect" content))
    (should (string-match-p "Treat false greens as failures" content))
    (should (string-match-p "semantic duplication" content))
    (should (string-match-p "## 6\\. EVIDENCE" content))))

(ert-deftest ai-code-test-evidence-first-diagnostics-variant-adds-sensor-loop ()
  "Diagnostics variant should require baseline-relative final diagnostics."
  (let ((content (ai-code-evidence-first-test--prompt-content t)))
    (should (string-match-p "diagnostics_baseline" content))
    (should (string-match-p "get_diagnostics" content))
    (should (string-match-p "since=\\\"baseline\\\"" content))
    (should (string-match-p "final status is `clean`" content))))

(provide 'test-ai-code-evidence-first-harness)

;;; test_ai-code-evidence-first-harness.el ends here
