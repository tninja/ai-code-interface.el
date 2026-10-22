;;; test_ai-code-task-verify.el --- Org task verification workflow tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Regression tests for opt-in TODO -> VERIFY -> DONE task headlines.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'ai-code-change)
(require 'ai-code-task)

(defmacro ai-code-task-verify-test--with-org (content &rest body)
  "Use CONTENT as a saved task Org buffer, then evaluate BODY."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (setq buffer-file-name "/tmp/task-verify.org")
     (insert ,content)
     (org-mode)
     (goto-char (point-min))
     (set-buffer-modified-p nil)
     (cl-letf (((symbol-function 'region-active-p) (lambda () nil))
               ((symbol-function 'ai-code--get-git-relative-paths)
                (lambda (_files) nil))
               ((symbol-function 'ai-code--get-context-files-string)
                (lambda () ""))
               ((symbol-function 'ai-code--format-repo-context-info)
                (lambda () ""))
               ((symbol-function 'which-function) (lambda () nil)))
       ,@body)))

(defconst ai-code-task-verify-test--header
  "#+TODO: TODO(t) VERIFY(v) | DONE(d)\n\n"
  "Opt-in header for task verification tests.")

(ert-deftest ai-code-task-verify-test-new-task-file-enables-workflow ()
  "New task Org files should declare the VERIFY intermediate state."
  (with-temp-buffer
    (cl-letf (((symbol-function 'magit-get-current-branch)
               (lambda () nil))
              ((symbol-function 'ai-code-current-backend-label)
               (lambda () "Claude Code")))
      (ai-code--initialize-task-file-content "Example" "")
      (should (string-match-p
               (regexp-quote "#+TODO: TODO(t) VERIFY(v) | DONE(d)")
               (buffer-string))))))

(ert-deftest ai-code-task-verify-test-file-opt-in-is-explicit ()
  "Plain Org files retain their previous behavior."
  (ai-code-task-verify-test--with-org "* TODO Implement\n"
    (should-not (ai-code--org-task-verification-enabled-p)))
  (ai-code-task-verify-test--with-org
      (concat ai-code-task-verify-test--header "* TODO Implement\n")
    (should (ai-code--org-task-verification-enabled-p))))

(ert-deftest ai-code-task-verify-test-todo-implementation-requests-verify ()
  "Code-changing an opted-in TODO requests VERIFY only after success."
  (ai-code-task-verify-test--with-org
      (concat ai-code-task-verify-test--header
              "* TODO Implement search\nVerification: run regression tests.\n")
    (let (sent)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_label initial) initial))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq sent prompt))))
        (ai-code-implement-todo nil "Code change")
        (should (string-match-p "TODO to VERIFY" sent))
        (should (string-match-p "only after" sent))
        (should (string-match-p "regression tests" sent))
        (should (equal "TODO" (org-get-todo-state)))))))

(ert-deftest ai-code-task-verify-test-todo-question-does-not-progress ()
  "Asking a question on an opt-in TODO must not request a state change."
  (ai-code-task-verify-test--with-org
      (concat ai-code-task-verify-test--header "* TODO Investigate\n")
    (let (sent)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_label initial) initial))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq sent prompt))))
        (ai-code-implement-todo nil "Ask question")
        (should-not (string-match-p "TODO to VERIFY" sent))
        (should (equal "TODO" (org-get-todo-state)))))))

(ert-deftest ai-code-task-verify-test-verify-dispatches-without-prompting ()
  "VERIFY dispatches a read-only validation instead of implementation."
  (ai-code-task-verify-test--with-org
      (concat ai-code-task-verify-test--header
              "* VERIFY Implement search\nVerification: run regression tests.\n")
    (let (sent)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (&rest _) (ert-fail "VERIFY must not ask to implement")))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq sent prompt))))
        (ai-code-implement-todo nil)
        (should (string-match-p "VERIFY to DONE" sent))
        (should (string-match-p "VERIFY to TODO" sent))
        (should (string-match-p "Keep VERIFY" sent))
        (should (string-match-p "Do not modify program code" sent))
        (should (string-match-p "regression tests" sent))
        (should-not (string-match-p "Please implement code" sent))
        (should (equal "VERIFY" (org-get-todo-state)))))))

(ert-deftest ai-code-task-verify-test-code-change-routes-to-verify ()
  "C-c a c and C-c a i converge on VERIFY headline behavior."
  (ai-code-task-verify-test--with-org
      (concat ai-code-task-verify-test--header "* VERIFY Review implementation\n")
    (let (sent)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (&rest _) (ert-fail "No input needed")))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq sent prompt))))
        (ai-code-code-change nil)
        (should (string-match-p "VERIFY to DONE" sent))
        (should (equal "VERIFY" (org-get-todo-state)))))))

(ert-deftest ai-code-task-verify-test-uses-selected-headline-state ()
  "A nested VERIFY step should not inherit its parent's TODO behavior."
  (ai-code-task-verify-test--with-org
      (concat ai-code-task-verify-test--header
              "* TODO Parent\n** VERIFY Child\nEvidence: check fixtures.\n")
    (forward-line 3)
    (let (sent)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _) (car collection)))
                ((symbol-function 'ai-code-read-string)
                 (lambda (&rest _) (ert-fail "No input needed")))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq sent prompt))))
        (ai-code-implement-todo nil)
        (should (string-match-p "VERIFY to DONE" sent))
        (should (string-match-p "Child" sent))
        (should (equal "VERIFY" (org-get-todo-state)))))))

(ert-deftest ai-code-task-verify-test-existing-org-remains-unchanged ()
  "Without an opt-in header, ordinary TODO prompts omit new transitions."
  (ai-code-task-verify-test--with-org "* TODO Plain note\n"
    (let (sent)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_label initial) initial))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq sent prompt))))
        (ai-code-implement-todo nil "Code change")
        (should (string-match-p "Please implement code" sent))
        (should-not (string-match-p "TODO to VERIFY" sent))))))

(provide 'test_ai-code-task-verify)

;;; test_ai-code-task-verify.el ends here
