;;; test_ai-code-project-workspace.el --- Workspace identity regressions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Check that project consumers follow the same explicit workspace binding.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code)
(require 'ai-code-behaviors)

(ert-deftest ai-code-test-workspace-binding-controls-backend-and-behaviors ()
  "A shared task bound to B must not use A's backend or behavior state."
  (with-temp-buffer
    (let ((ai-code--repo-backend-alist '(("/tmp/workspace-a/" . codex)
                                       ("/tmp/workspace-b/" . gemini)))
          (ai-code-selected-backend 'claude-code)
          (ai-code--behaviors-session-states (make-hash-table :test 'equal)))
      (ai-code--set-session-project-root "/tmp/workspace-b/")
      (cl-letf (((symbol-function 'ai-code--git-root)
                 (lambda (&rest _) "/tmp/workspace-a/")))
        (should (equal (ai-code--session-project-root) "/tmp/workspace-b/"))
        (should (eq (ai-code--effective-backend) 'gemini))
        (should (equal (ai-code--behaviors-project-root) "/tmp/workspace-b/"))
        (ai-code--behaviors-set-state '(:mode "=review"))
        (should (equal (ai-code--behaviors-get-state "/tmp/workspace-b/")
                       '(:mode "=review")))
        (should-not (ai-code--behaviors-get-state "/tmp/workspace-a/"))))))

(ert-deftest ai-code-test-workspace-binding-records-backend-for-target ()
  "Changing the backend in a bound task updates only the target workspace."
  (with-temp-buffer
    (ai-code--set-session-project-root "/tmp/workspace-b/")
    (let ((ai-code--repo-backend-alist '(("/tmp/workspace-a/" . codex)))
          (ai-code-selected-backend 'gemini))
      (cl-letf (((symbol-function 'ai-code--git-root)
                 (lambda (&rest _) "/tmp/workspace-a/")))
        (ai-code--remember-current-backend-for-repo)
        (should (eq (ai-code--repo-backend-for-root "/tmp/workspace-b/") 'gemini))
        (should (eq (ai-code--repo-backend-for-root "/tmp/workspace-a/") 'codex))))))

(ert-deftest ai-code-test-workspace-behaviors-use-requested-source-buffer ()
  "A background transform resolves the source buffer's binding."
  (with-temp-buffer
    (ai-code--set-session-project-root "/tmp/workspace-source/")
    (let ((source (current-buffer)))
      (with-temp-buffer
        (ai-code--set-session-project-root "/tmp/workspace-other/")
        (should (equal (ai-code--behaviors-project-root source)
                       "/tmp/workspace-source/"))))))

(ert-deftest ai-code-test-workspace-prompt-session-search-uses-binding ()
  "Prompt target discovery uses B even when the task file is stored in A."
  (with-temp-buffer
    (ai-code--set-session-project-root "/tmp/workspace-b/")
    (let ((target (generate-new-buffer "*codex[workspace-b]*")))
      (unwind-protect
          (progn
            (with-current-buffer target
              (setq-local ai-code-backends-infra--session-terminal-backend 'vterm))
            (cl-letf (((symbol-function 'ai-code--git-root)
                       (lambda (&rest _) "/tmp/workspace-a/"))
                      ((symbol-function 'ai-code-backends-infra--session-buffer-matches-directory-p)
                       (lambda (buffer directory)
                         (and (eq buffer target)
                              (equal directory "/tmp/workspace-b/")))))
              (should (equal (ai-code--find-project-session-buffers) (list target)))))
        (kill-buffer target)))))

(ert-deftest ai-code-test-workspace-known-non-git-project-is-shared ()
  "A project.el project has one identity even without Git."
  (with-temp-buffer
    (let ((default-directory "/tmp/")
          (ai-code--repo-backend-alist '(("/tmp/project-el/" . codex))))
      (cl-letf (((symbol-function 'ai-code--git-root) #'ignore)
                ((symbol-function 'project-current) (lambda (&rest _) 'project))
                ((symbol-function 'project-root) (lambda (_) "/tmp/project-el/")))
        (should (equal (ai-code--session-project-root) "/tmp/project-el/"))
        (should (eq (ai-code--effective-backend) 'codex))
        (should (equal (ai-code--behaviors-project-root) "/tmp/project-el/"))))))

(ert-deftest ai-code-test-workspace-no-project-keeps-session-fallback-only ()
  "Ordinary non-project buffers do not acquire backend or behavior affinity."
  (with-temp-buffer
    (let ((default-directory "/tmp/")
          (ai-code-selected-backend 'codex)
          (ai-code--repo-backend-alist nil))
      (cl-letf (((symbol-function 'ai-code--git-root) #'ignore)
                ((symbol-function 'project-current) #'ignore))
        (should (equal (ai-code--session-project-root) "/tmp/"))
        (should-not (ai-code--behaviors-project-root))
        (ai-code--remember-current-backend-for-repo)
        (should-not ai-code--repo-backend-alist)))))

(ert-deftest ai-code-test-workspace-gptel-buffer-keeps-non-project-fallback ()
  "Recognized external agent buffers retain their directory fallback."
  (with-temp-buffer
    (rename-buffer "*gptel-agent:workspace-fallback*" t)
    (let ((default-directory "/tmp/"))
      (cl-letf (((symbol-function 'ai-code--git-root) #'ignore)
                ((symbol-function 'project-current) #'ignore))
        (should (equal (ai-code--behaviors-project-root) "/tmp/"))))))

(ert-deftest ai-code-test-workspace-ordinary-git-affinity-is-unchanged ()
  "Without a binding, Git still wins over project.el for every consumer."
  (with-temp-buffer
    (let ((ai-code--repo-backend-alist '(("/tmp/git-project/" . codex))))
      (cl-letf (((symbol-function 'ai-code--git-root)
                 (lambda (&rest _) "/tmp/git-project/"))
                ((symbol-function 'project-current)
                 (lambda (&rest _) (ert-fail "Git must take precedence"))))
        (should (equal (ai-code--session-project-root) "/tmp/git-project/"))
        (should (eq (ai-code--effective-backend) 'codex))
        (should (equal (ai-code--behaviors-project-root) "/tmp/git-project/"))))))

(provide 'test_ai-code-project-workspace)
;;; test_ai-code-project-workspace.el ends here
