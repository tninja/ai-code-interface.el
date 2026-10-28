;;; test_ai-code-verify.el --- On-demand verification tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Regression coverage for state-free evidence-first verification.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'ai-code-verify)

(defmacro ai-code-verify-test--with-org (content &rest body)
  "Evaluate BODY in a saved Org buffer containing CONTENT."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (setq buffer-file-name "/tmp/verify-work.org")
     (insert ,content)
     (org-mode)
     (goto-char (point-min))
     (re-search-forward "^\\*+ " nil t)
     (beginning-of-line)
     (set-buffer-modified-p nil)
     ,@body))

(ert-deftest ai-code-verify-test-todo-needs-no-opt-in-or-state-change ()
  "Any Org task can be verified without a special TODO keyword sequence."
  (ai-code-verify-test--with-org
      "* TODO Update the guide\nCriteria: document the option.\n"
    (let (sent)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (&rest _) ""))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq sent prompt))))
        (ai-code-verify-current-work)
        (should (string-match-p "TODO Update the guide" sent))
        (should (string-match-p "Criteria: document the option" sent))
        (should-not (string-match-p "from VERIFY to DONE" sent))
        (should (equal "TODO" (org-get-todo-state)))
        (should-not (buffer-modified-p))))))

(ert-deftest ai-code-verify-test-done-task-and-nested-section ()
  "DONE headlines remain available, including nested completed sections."
  (ai-code-verify-test--with-org
      "* TODO Parent\n** DONE Child\nAcceptance: preserve behavior.\n"
    (forward-line 1)
    (let (sent)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _) (car choices)))
                ((symbol-function 'ai-code-read-string)
                 (lambda (&rest _) ""))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq sent prompt))))
        (ai-code-verify-current-work)
        (should (string-match-p "DONE Child" sent))
        (should (string-match-p "preserve behavior" sent))
        (should (equal "DONE" (org-get-todo-state)))
        (should-not (buffer-modified-p))))))

(ert-deftest ai-code-verify-test-regular-todo-ignores-done ()
  "Existing implement-TODO behavior still omits DONE sections."
  (ai-code-verify-test--with-org "* DONE Finished\n"
    (should-not (ai-code--org-section-candidates))
    (should (ai-code--org-section-candidates t))))

(ert-deftest ai-code-verify-test-selected-region-wins-over-org-section ()
  "An active selection should not accidentally verify the whole subtree."
  (ai-code-verify-test--with-org
      "* TODO Parent\nImportant line.\nUnrelated details.\n"
    (goto-char (point-min))
    (search-forward "Important line.")
    (set-mark (- (point) (length "Important line.")))
    (let (scope)
      (cl-letf (((symbol-function 'region-active-p) (lambda () t))
                ((symbol-function 'ai-code--scope-context-for-region)
                 (lambda (&rest _) nil))
                ((symbol-function 'ai-code--format-scope-context)
                 (lambda (&rest _) "")))
        (setq scope (ai-code-verify--scope)))
      (should (string-match-p "Important line" scope))
      (should-not (string-match-p "Unrelated details" scope)))))

(ert-deftest ai-code-verify-test-source-function-context ()
  "Verification uses current semantic scope without requiring Org mode."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/example.el")
    (emacs-lisp-mode)
    (insert "(defun example () t)\n")
    (set-buffer-modified-p nil)
    (cl-letf (((symbol-function 'ai-code--current-line-scope-context)
               (lambda () (list :function-name "example")))
              ((symbol-function 'ai-code--get-context-files-string)
               (lambda () "")))
      (let ((scope (ai-code-verify--scope)))
        (should (string-match-p "example.el" scope))
        (should (string-match-p "Function: example" scope))))))

(ert-deftest ai-code-verify-test-git-changes-mode ()
  "Prefix argument selects a Git diff scope and optional user focus."
  (let (sent)
    (cl-letf (((symbol-function 'ai-code--git-root)
               (lambda (&optional _dir) "/tmp/project/"))
              ((symbol-function 'ai-code-read-string)
               (lambda (&rest _) "Backward compatibility"))
              ((symbol-function 'ai-code--format-repo-context-info)
               (lambda () "Repo: project"))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (prompt) (setq sent prompt))))
      (ai-code-verify-current-work '(4))
      (should (string-match-p "Current Git changes" sent))
      (should (string-match-p "staged and unstaged" sent))
      (should (string-match-p "Backward compatibility" sent)))))

(ert-deftest ai-code-verify-test-git-mode-requires-repository ()
  "Git verification rejects buffers outside repositories."
  (cl-letf (((symbol-function 'ai-code--git-root)
             (lambda (&optional _dir) nil)))
    (should-error (ai-code-verify--scope t) :type 'user-error)))

(ert-deftest ai-code-verify-test-unsaved-source-requires-selection ()
  "Do not silently verify stale source content."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/example.el")
    (emacs-lisp-mode)
    (insert "(defun edited () nil)")
    (should-error (ai-code-verify--scope) :type 'user-error)))

(ert-deftest ai-code-verify-test-read-only-suffix-suppression ()
  "Verify turns suppress write-oriented suffixes and Org summary prompts."
  (ai-code-verify-test--with-org "* TODO Check this\n"
    (let ((ai-code-prompt-suffix-functions '(ignore))
          (ai-code--suppress-org-summary-prompt nil)
          effective-suffixes effective-summary)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (&rest _) ""))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (_prompt)
                   (setq effective-suffixes ai-code-prompt-suffix-functions
                         effective-summary ai-code--suppress-org-summary-prompt))))
        (ai-code-verify-current-work)
        (should-not effective-suffixes)
        (should effective-summary)
        (should (equal ai-code-prompt-suffix-functions '(ignore)))
        (should-not ai-code--suppress-org-summary-prompt)
        (should-not (buffer-modified-p))))))

(ert-deftest ai-code-verify-test-prompt-honors-uncertainty ()
  "Verification must use actual evidence and report limitations."
  (let ((prompt (ai-code-verify--prompt "Current file: a.el" "")))
    (dolist (needle '("fresh observations" "counterexamples"
                      "unverified" "without changing task status"
                      "tests" "hypotheses"))
      (should (string-match-p (regexp-quote needle) prompt)))
    (should (string-match-p "do not edit source" prompt))))

(provide 'test_ai-code-verify)

;;; test_ai-code-verify.el ends here
