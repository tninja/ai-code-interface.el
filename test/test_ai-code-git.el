;;; test_ai-code-git.el --- Tests for ai-code-git.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-git module, especially gitignore and worktree logic.

;;; Code:

(require 'ert)
(require 'ai-code-git)
(require 'ai-code-prompt-mode)

(declare-function difftastic-magit-diff "difftastic" ())
(declare-function magit-worktree-status "magit-worktree" ())

(defun ai-code-test--gitignore-required-entries ()
  "Return the default ignore entries expected from `ai-code-update-git-ignore'."
  (list (concat ai-code-files-dir-name "/")
        ".projectile"
        "GTAGS"
        "GRTAGS"
        "GPATH"
        "__pycache__/"
        "*.elc"
        "flycheck_*"))

(ert-deftest ai-code-test-ai-code-gitignore-regex-pattern ()
  "Test that the regex pattern correctly matches entries in .gitignore."
  (let ((gitignore-content "# Test .gitignore file
.ai.code.prompt.org
.ai.code.notes.org
.projectile
GTAGS
GRTAGS
GPATH
# End of file
"))
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote ".ai.code.prompt.org")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote ".ai.code.notes.org")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote ".projectile")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote "GTAGS")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote "GRTAGS")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote "GPATH")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    (should-not (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                       (regexp-quote "MISSING_ENTRY")
                                       "\\s-*\\(?:\n\\|$\\)")
                                gitignore-content))
    (let ((gitignore-with-whitespace "  .projectile
GTAGS
"))
      (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                     (regexp-quote ".projectile")
                                     "\\s-*\\(?:\n\\|$\\)")
                             gitignore-with-whitespace))
      (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                     (regexp-quote "GTAGS")
                                     "\\s-*\\(?:\n\\|$\\)")
                             gitignore-with-whitespace)))
    (let ((gitignore-start ".ai.code.prompt.org
other-file"))
      (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                     (regexp-quote ".ai.code.prompt.org")
                                     "\\s-*\\(?:\n\\|$\\)")
                             gitignore-start)))
    (let ((gitignore-end "other-file
.ai.code.prompt.org"))
      (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                     (regexp-quote ".ai.code.prompt.org")
                                     "\\s-*\\(?:\n\\|$\\)")
                             gitignore-end)))))

(ert-deftest ai-code-test-ai-code-update-git-ignore-no-duplicates ()
  "Test that ai-code-update-git-ignore does not add duplicate entries."
  (let* ((temp-dir (file-truename (make-temp-file "ai-code-test-" t)))
         (gitignore-path (expand-file-name ".gitignore" temp-dir))
         (required-entries (ai-code-test--gitignore-required-entries)))
    (unwind-protect
        (progn
          (let ((default-directory temp-dir))
            (shell-command "git init"))
          (with-temp-file gitignore-path
            (insert "# Existing entries\n")
            (dolist (entry required-entries)
              (insert entry "\n"))
            (insert "# End of file\n"))
          (let ((original-content (with-temp-buffer
                                    (insert-file-contents gitignore-path)
                                    (buffer-string))))
            (cl-letf (((symbol-function 'ai-code--git-root)
                       (lambda (&optional _dir) temp-dir)))
              (ai-code-update-git-ignore))
            (let ((updated-content (with-temp-buffer
                                     (insert-file-contents gitignore-path)
                                     (buffer-string))))
              (should (string= original-content updated-content))
              (dolist (entry required-entries)
                (let ((count 0))
                  (with-temp-buffer
                    (insert updated-content)
                    (goto-char (point-min))
                    (while (re-search-forward (concat "^\\s-*" (regexp-quote entry) "\\s-*$") nil t)
                      (setq count (1+ count))))
                  (should (= count 1)))))))
      (delete-directory temp-dir t))))

(ert-deftest ai-code-test-ai-code-update-git-ignore-adds-missing ()
  "Test that ai-code-update-git-ignore adds missing entries."
  (let* ((temp-dir (file-truename (make-temp-file "ai-code-test-" t)))
         (gitignore-path (expand-file-name ".gitignore" temp-dir)))
    (unwind-protect
        (progn
          (let ((default-directory temp-dir))
            (shell-command "git init"))
          (with-temp-file gitignore-path
            (insert "# Existing entries\n")
            (insert ".projectile\n")
            (insert "GTAGS\n"))
          (cl-letf (((symbol-function 'ai-code--git-root)
                     (lambda (&optional _dir) temp-dir)))
            (ai-code-update-git-ignore))
          (let ((updated-content (with-temp-buffer
                                   (insert-file-contents gitignore-path)
                                   (buffer-string))))
            (dolist (entry (ai-code-test--gitignore-required-entries))
              (should (string-match-p (regexp-quote entry) updated-content)))))
      (delete-directory temp-dir t))))

(ert-deftest ai-code-test-git-worktree-branch-creates-repo-directory-and-adds-worktree ()
  "Create repo worktree directory and invoke git worktree add with expected path."
  (let* ((temp-worktree-root (make-temp-file "ai-code-worktree-root-" t))
         (ai-code-git-worktree-root temp-worktree-root)
         (git-root "/tmp/sample-repo/")
         (branch "feature/new-branch")
         (start-point "main")
         (repo-dir (expand-file-name "sample-repo" temp-worktree-root))
         (worktree-path (expand-file-name branch repo-dir))
         (worktree-parent-dir (file-name-directory worktree-path))
         captured-git-args
         captured-visited-path)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--validate-git-repository)
                   (lambda () git-root))
                  ((symbol-function 'magit-branch-p)
                   (lambda (_b) nil))
                  ((symbol-function 'magit-run-git)
                   (lambda (&rest _args)
                     (ert-fail "`magit-run-git' should not be used for worktree add status check")))
                  ((symbol-function 'magit-call-git)
                   (lambda (&rest args)
                     (setq captured-git-args args)
                     0))
                  ((symbol-function 'magit-diff-visit-directory)
                   (lambda (path)
                     (setq captured-visited-path path))))
          (should-not (file-directory-p repo-dir))
          (ai-code-git-worktree-branch branch start-point)
          (should (file-directory-p repo-dir))
          (should (file-directory-p worktree-parent-dir))
          (should (equal captured-git-args
                         (list "worktree"
                               "add"
                               "-b"
                               branch
                               (file-truename worktree-path)
                               start-point)))
          (should (equal captured-visited-path worktree-path)))
      (delete-directory temp-worktree-root t))))

(ert-deftest ai-code-test-git-worktree-branch-uses-existing-branch ()
  "When branch already exists, add worktree without -b and notify user."
  (let* ((temp-worktree-root (make-temp-file "ai-code-worktree-root-" t))
         (temp-git-root (make-temp-file "ai-code-git-root-" t))
         (ai-code-git-worktree-root temp-worktree-root)
         (branch "feature/existing-branch")
         (start-point "main")
         (repo-dir (expand-file-name
                    (file-name-nondirectory (directory-file-name temp-git-root))
                    temp-worktree-root))
         (worktree-path (expand-file-name branch repo-dir))
         captured-git-args
         captured-visited-path)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--validate-git-repository)
                   (lambda () temp-git-root))
                  ((symbol-function 'magit-branch-p)
                   (lambda (b) (string= b branch)))
                  ((symbol-function 'magit-call-git)
                   (lambda (&rest args)
                     (setq captured-git-args args)
                     (make-directory worktree-path t)
                     0))
                  ((symbol-function 'magit-diff-visit-directory)
                   (lambda (path)
                     (setq captured-visited-path path))))
          (ai-code-git-worktree-branch branch start-point)
          (should-not (member "-b" captured-git-args))
          (should (equal captured-git-args
                         (list "worktree" "add"
                               (file-truename worktree-path)
                               branch)))
          (should (equal captured-visited-path worktree-path)))
      (delete-directory temp-worktree-root t)
      (delete-directory temp-git-root t))))

(ert-deftest ai-code-test-git-worktree-action-without-prefix-calls-worktree-branch ()
  "Without prefix arg, dispatch to `ai-code-git-worktree-branch'."
  (let (captured-fn)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (fn &optional _record-flag _keys)
                 (setq captured-fn fn))))
      (ai-code-git-worktree-action nil)
      (should (eq captured-fn #'ai-code-git-worktree-branch)))))

(ert-deftest ai-code-test-git-worktree-action-with-prefix-calls-magit-worktree-status ()
  "With prefix arg, dispatch to `magit-worktree-status'."
  (let (captured-fn)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (fn &optional _record-flag _keys)
                 (setq captured-fn fn))))
      (ai-code-git-worktree-action '(4))
      (should (eq captured-fn #'magit-worktree-status)))))

(provide 'test_ai-code-git)

;;; test_ai-code-git.el ends here
