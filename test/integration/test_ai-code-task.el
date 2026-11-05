;;; test_ai-code-task.el --- Real Git task resumption tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Exercise task snapshots and handoff baselines against real Git and Magit.
;; No agent or external service is invoked.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'ai-code-task)

(defun ai-code-task-test--git (&rest args)
  "Run Git with ARGS in the test repository, failing on errors."
  (with-temp-buffer
    (unless (zerop (apply #'process-file "git" nil t nil args))
      (ert-fail (buffer-string)))
    (string-trim (buffer-string))))

(defmacro ai-code-task-test--with-repository (&rest body)
  "Run BODY in a temporary repository with one initial commit."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory (make-temp-file "ai-code-task-git-" t)))
          (default-directory root))
     (unwind-protect
         (progn
           (ai-code-task-test--git "init" "-q" "--initial-branch=main")
           (ai-code-task-test--git "config" "user.name" "Task Test")
           (ai-code-task-test--git "config" "user.email" "task-test@example.invalid")
           (with-temp-file (expand-file-name "code.el" root) (insert "base\n"))
           (ai-code-task-test--git "add" "code.el")
           (ai-code-task-test--git "commit" "-qm" "Initial task code")
           ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (string-prefix-p root file)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory root t))))

(ert-deftest test-task--git-snapshot-compares-baseline-and-keeps-dirty-work ()
  "Retain multiline state and committed drift without changing tracked work."
  (ai-code-task-test--with-repository
    (let ((baseline (ai-code-task-test--git "rev-parse" "HEAD")))
      (with-temp-file "code.el" (insert "changed since checkpoint\n"))
      (ai-code-task-test--git "add" "code.el")
      (ai-code-task-test--git "commit" "-qm" "Later behavior change")
      (with-temp-file "staged.el" (insert "staged work\n"))
      (ai-code-task-test--git "add" "staged.el")
      (with-temp-file "code.el" (insert "unfinished work\n"))
      (with-temp-file "untracked.log" (insert "log signal\n"))
      (let* ((before (ai-code-task-test--git "status" "--porcelain"))
             (index-before (ai-code-task-test--git "diff" "--cached"))
             (work-before (ai-code-task-test--git "diff"))
             (snapshot (ai-code--task-git-snapshot
                        root (list :head baseline :baseline "Agent Handoff old"))))
        (dolist (text (list baseline "Later behavior change" "staged.el"
                            "code.el" "untracked.log" "Committed diff stat"
                            "excludes the old uncommitted state"))
          (should (string-match-p (regexp-quote text) snapshot)))
        (should (equal before (ai-code-task-test--git "status" "--porcelain")))
        (should (equal index-before (ai-code-task-test--git "diff" "--cached")))
        (should (equal work-before (ai-code-task-test--git "diff")))))))

(ert-deftest test-task--git-snapshot-reports-missing-and-unborn-baselines ()
  "Missing commits and an unborn HEAD remain unknown, never clean evidence."
  (ai-code-task-test--with-repository
    (let ((snapshot (ai-code--task-git-snapshot root (list :head (make-string 40 ?f)))))
      (should (string-match-p "No usable recorded baseline" snapshot)))
    (ai-code-task-test--git "checkout" "--orphan" "new-task")
    (let ((snapshot (ai-code--task-git-snapshot root)))
      (should (string-match-p "unborn HEAD" snapshot))
      (should (string-match-p "unavailable: Git exited" snapshot)))))

(ert-deftest test-task--handoff-properties-round-trip-to-resume ()
  "The generated handoff contains reusable Git metadata and next-action guidance."
  (ai-code-task-test--with-repository
    (let* ((prompt (ai-code--agent-handoff-dump-prompt (expand-file-name "task.org" root)))
           (metadata (ai-code--task-resume-metadata prompt)))
      (should (equal (plist-get metadata :head)
                     (ai-code-task-test--git "rev-parse" "HEAD")))
      (should (file-equal-p (plist-get metadata :repo-root) root))
      (should (string-match-p "One concrete first action" prompt))
      (should (string-match-p "Exact validation commands" prompt))
      (should (string-match-p "Source links, dated meeting/Slack excerpts" prompt)))))

(ert-deftest test-task--creation-baseline-uses-invoking-worktree ()
  "A shared task file records the working checkout, not the storage checkout."
  (ai-code-task-test--with-repository
    (let* ((linked (expand-file-name "linked" root))
           (task-file (expand-file-name ".ai.code.files/task.org" root))
           (worktree-head (ai-code-task-test--git "rev-parse" "HEAD")))
      (ai-code-task-test--git "worktree" "add" "--detach" linked "HEAD")
      (with-temp-file "code.el" (insert "main has moved\n"))
      (ai-code-task-test--git "add" "code.el")
      (ai-code-task-test--git "commit" "-qm" "Advance main")
      (make-directory (file-name-directory task-file) t)
      (with-temp-buffer
        (let ((default-directory (file-name-as-directory linked)))
          (cl-letf (((symbol-function 'find-file-other-window)
                     (lambda (file) (set-buffer (find-file-noselect file))))
                    ((symbol-function 'ai-code-current-backend-label) (lambda () "Test")))
            (ai-code--open-or-create-task-file task-file "task.org" "Task" "")))
        (let ((metadata (ai-code--task-resume-metadata
                         (ai-code--agent-handoff-read-file-or-buffer task-file))))
          (should (equal (plist-get metadata :head) worktree-head))
          (should (file-equal-p (plist-get metadata :repo-root) linked)))))))

(ert-deftest test-task--shared-file-workspace-keeps-consumers-in-one-checkout ()
  "A task stored in main routes to its bound checkout without moving storage."
  (require 'ai-code-backends)
  (require 'ai-code-backends-infra)
  (require 'ai-code-behaviors)
  (ai-code-task-test--with-repository
    (let* ((linked (expand-file-name "linked" root))
           (alias (expand-file-name "linked-alias" root))
           (task-file (expand-file-name ".ai.code.files/task.org" root))
           (main-session (generate-new-buffer "*codex[workspace-main]*"))
           (linked-session (generate-new-buffer "*gemini[workspace-linked]*"))
           (ai-code--repo-backend-alist nil)
           (ai-code--behaviors-session-states (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            (ai-code-task-test--git "worktree" "add" "--detach" linked "HEAD")
            (make-symbolic-link linked alias)
            (make-directory (file-name-directory task-file) t)
            (with-temp-file task-file (insert "* Task source remains in main\n"))
            (dolist (session (list main-session linked-session))
              (with-current-buffer session
                (setq-local ai-code-backends-infra--session-terminal-backend 'vterm)))
            (ai-code-backends-infra--set-session-directory main-session root)
            (ai-code-backends-infra--set-session-directory linked-session linked)
            (with-current-buffer (find-file-noselect task-file)
              (should (file-equal-p (ai-code--git-root) root))
              (ai-code--set-session-project-root alias)
              (let ((ai-code-selected-backend 'gemini))
                (ai-code--remember-current-backend-for-repo))
              (let ((ai-code-selected-backend 'codex))
                (should (eq (ai-code--effective-backend) 'gemini)))
              (should (equal (ai-code--session-project-root)
                             (file-name-as-directory (file-truename linked))))
              (should (equal (ai-code--behaviors-project-root)
                             (ai-code--session-project-root)))
              (ai-code--behaviors-set-state '(:mode "=review"))
              (should-not (ai-code--behaviors-get-state root))
              (should (equal (ai-code--find-project-session-buffers)
                             (list linked-session)))
              ;; Strict task dispatch must not use a visible session in main.
              (let ((ai-code--prompt-project-only t))
                (cl-letf (((symbol-function 'ai-code--find-visible-session-buffer)
                           (lambda () main-session)))
                  (should-not (ai-code--prompt-choose-target-session))))
              (should (file-equal-p (ai-code--get-files-directory)
                                    (file-name-directory task-file)))
              (should (file-equal-p (ai-code--git-root) root)))
            (with-temp-buffer
              (let ((default-directory (file-name-as-directory linked)))
                (should (file-equal-p (ai-code--get-files-directory)
                                      (file-name-directory task-file)))
                (should (eq (ai-code--effective-backend) 'gemini)))))
        (kill-buffer main-session)
        (kill-buffer linked-session)))))

(provide 'test-ai-code-task-integration)
;;; test_ai-code-task.el ends here
