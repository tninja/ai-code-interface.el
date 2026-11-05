;;; test_ai-code-project-workspace.el --- Workspace identity regressions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Check that project consumers follow the same explicit workspace binding.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code)
(require 'ai-code-behaviors)

(defun ai-code-test--workspace (name)
  "Return canonical directory NAME under /tmp, as root resolvers report it.
Resolvers apply `file-truename', and /tmp is a symlink on some systems."
  (file-name-as-directory (file-truename (expand-file-name name "/tmp/"))))

(ert-deftest ai-code-test-workspace-binding-controls-backend-and-behaviors ()
  "A shared task bound to B must not use A's backend or behavior state."
  (with-temp-buffer
    (let* ((root-a (ai-code-test--workspace "workspace-a"))
           (root-b (ai-code-test--workspace "workspace-b"))
           (ai-code--repo-backend-alist (list (cons root-a 'codex)
                                              (cons root-b 'gemini)))
           (ai-code-selected-backend 'claude-code)
           (ai-code--behaviors-session-states (make-hash-table :test 'equal)))
      (ai-code--set-session-project-root root-b)
      (cl-letf (((symbol-function 'ai-code--git-root)
                 (lambda (&rest _) root-a)))
        (should (equal (ai-code--session-project-root) root-b))
        (should (eq (ai-code--effective-backend) 'gemini))
        (should (equal (ai-code--behaviors-project-root) root-b))
        (ai-code--behaviors-set-state '(:mode "=review"))
        (should (equal (ai-code--behaviors-get-state root-b)
                       '(:mode "=review")))
        (should-not (ai-code--behaviors-get-state root-a))))))

(ert-deftest ai-code-test-workspace-binding-records-backend-for-target ()
  "Changing the backend in a bound task updates only the target workspace."
  (with-temp-buffer
    (let* ((root-a (ai-code-test--workspace "workspace-a"))
           (root-b (ai-code-test--workspace "workspace-b"))
           (ai-code--repo-backend-alist (list (cons root-a 'codex)))
           (ai-code-selected-backend 'gemini))
      (ai-code--set-session-project-root root-b)
      (cl-letf (((symbol-function 'ai-code--git-root)
                 (lambda (&rest _) root-a)))
        (ai-code--remember-current-backend-for-repo)
        (should (eq (ai-code--repo-backend-for-root root-b) 'gemini))
        (should (eq (ai-code--repo-backend-for-root root-a) 'codex))))))

(ert-deftest ai-code-test-workspace-behaviors-use-requested-source-buffer ()
  "A background transform resolves the source buffer's binding."
  (with-temp-buffer
    (ai-code--set-session-project-root "/tmp/workspace-source/")
    (let ((source (current-buffer)))
      (with-temp-buffer
        (ai-code--set-session-project-root "/tmp/workspace-other/")
        (should (equal (ai-code--behaviors-project-root source)
                       (ai-code-test--workspace "workspace-source")))))))

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
                       (lambda (&rest _) (ai-code-test--workspace "workspace-a")))
                      ((symbol-function 'ai-code-backends-infra--session-buffer-matches-directory-p)
                       (lambda (buffer directory)
                         (and (eq buffer target)
                              (equal directory
                                     (ai-code-test--workspace "workspace-b"))))))
              (should (equal (ai-code--find-project-session-buffers) (list target)))))
        (kill-buffer target)))))

(ert-deftest ai-code-test-workspace-known-non-git-project-is-shared ()
  "A project.el project has one identity even without Git."
  (with-temp-buffer
    (let* ((root (ai-code-test--workspace "project-el"))
           (default-directory "/tmp/")
           (ai-code--repo-backend-alist (list (cons root 'codex))))
      (cl-letf (((symbol-function 'ai-code--git-root) #'ignore)
                ((symbol-function 'project-current) (lambda (&rest _) 'project))
                ((symbol-function 'project-root) (lambda (_) root)))
        (should (equal (ai-code--session-project-root) root))
        (should (eq (ai-code--effective-backend) 'codex))
        (should (equal (ai-code--behaviors-project-root) root))))))

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
    (let* ((root (ai-code-test--workspace "git-project"))
           (ai-code--repo-backend-alist (list (cons root 'codex))))
      (cl-letf (((symbol-function 'ai-code--git-root)
                 (lambda (&rest _) root))
                ((symbol-function 'project-current)
                 (lambda (&rest _) (ert-fail "Git must take precedence"))))
        (should (equal (ai-code--session-project-root) root))
        (should (eq (ai-code--effective-backend) 'codex))
        (should (equal (ai-code--behaviors-project-root) root))))))

(provide 'test_ai-code-project-workspace)
;;; test_ai-code-project-workspace.el ends here
