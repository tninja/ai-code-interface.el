;;; test_ai-code-commit.el --- Tests for ai-code-commit.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the commit-current-changes workflow.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-commit)
(require 'ai-code-github)

(ert-deftest ai-code-test-commit-current-changes-explicit-message ()
  "Stage all changes and commit an explicit message without calling AI."
  (let (git-calls read-count)
    (cl-letf (((symbol-function 'ai-code--git-root)
               (lambda (&optional _dir) "/tmp/repo/"))
              ((symbol-function 'magit-anything-modified-p)
               (lambda () t))
              ((symbol-function 'ai-code-read-string)
               (lambda (&rest _args)
                 (setq read-count (1+ (or read-count 0)))
                 "Add commit workflow"))
              ((symbol-function 'ai-code-call-gptel-sync)
               (lambda (&rest _args)
                 (ert-fail "AI generation should not run for explicit messages")))
              ((symbol-function 'magit-call-git)
               (lambda (&rest args)
                 (push args git-calls)
                 0))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil)))
      (ai-code-git-commit-current-changes)
      (should (= read-count 1))
      (should (equal (nreverse git-calls)
                     '(("add" "-A")
                       ("commit" "-m" "Add commit workflow")))))))

(ert-deftest ai-code-test-commit-current-changes-generates-message-from-staged-diff ()
  "Empty input should generate and edit a message from the staged diff."
  (let ((read-values '("" "Generated commit message"))
        captured-prompt
        git-calls)
    (cl-letf (((symbol-function 'ai-code--git-root)
               (lambda (&optional _dir) "/tmp/repo/"))
              ((symbol-function 'magit-anything-modified-p)
               (lambda () t))
              ((symbol-function 'ai-code-read-string)
               (lambda (&rest _args)
                 (prog1 (car read-values)
                   (setq read-values (cdr read-values)))))
              ((symbol-function 'magit-git-output)
               (lambda (&rest args)
                 (should (equal args '("diff" "--cached")))
                 "diff --git a/a.el b/a.el\n+new line\n"))
              ((symbol-function 'ai-code-call-gptel-sync)
               (lambda (prompt)
                 (setq captured-prompt prompt)
                 "Generated commit message"))
              ((symbol-function 'magit-call-git)
               (lambda (&rest args)
                 (push args git-calls)
                 0))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil)))
      (ai-code-git-commit-current-changes)
      (should (string-match-p "diff --git a/a.el b/a.el" captured-prompt))
      (should (equal (nreverse git-calls)
                     '(("add" "-A")
                       ("commit" "-m" "Generated commit message")))))))

(ert-deftest ai-code-test-commit-current-changes-pushes-with-upstream ()
  "Push normally when the current branch already has an upstream."
  (let (git-calls)
    (cl-letf (((symbol-function 'magit-get-current-branch)
               (lambda () "feature/test"))
              ((symbol-function 'magit-git-string)
               (lambda (&rest args)
                 (when (equal args '("rev-parse" "--abbrev-ref"
                                     "--symbolic-full-name" "@{upstream}"))
                   "origin/feature/test")))
              ((symbol-function 'magit-call-git)
               (lambda (&rest args)
                 (push args git-calls)
                 0)))
      (ai-code-commit--push-current-branch)
      (should (equal git-calls '(("push")))))))

(ert-deftest ai-code-test-github-mode-dispatches-commit-current-changes ()
  "The C-c a v mode dispatcher should invoke the local commit workflow."
  (let (commit-called)
    (cl-letf (((symbol-function 'ai-code--pull-or-review-pr-mode-choice)
               (lambda () 'commit-current-changes))
              ((symbol-function 'call-interactively)
               (lambda (fn &optional _record-flag _keys)
                 (should (eq fn #'ai-code-git-commit-current-changes))
                 (setq commit-called t))))
      (ai-code--pull-or-review-pr-with-source 'github-mcp)
      (should commit-called))))

(ert-deftest ai-code-test-github-action-list-includes-commit-current-changes ()
  "Expose commit current changes in the C-c a v action list."
  (should (eq (cdr (assoc "Commit current changes"
                          ai-code-github--pull-or-review-pr-mode-alist))
              'commit-current-changes)))

(provide 'test_ai-code-commit)

;;; test_ai-code-commit.el ends here
