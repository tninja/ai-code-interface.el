;;; test_ai-code-git.el --- Tests for ai-code-git.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-git module, especially gitignore and worktree logic.

;;; Code:

(require 'ert)
(require 'ai-code-git)
(require 'ai-code-codex-cli)
(require 'ai-code-prompt-mode)

(declare-function difftastic-magit-diff "difftastic" ())
(declare-function magit-worktree-status "magit-worktree" ())
(declare-function magit-worktree-delete "magit-worktree" (worktree))

(ert-deftest ai-code-test-ai-code-git-does-not-eagerly-load-github-module ()
  "Loading `ai-code-git' should not eagerly load `ai-code-github'."
  (let ((git-library (locate-library "ai-code-git.el"))
        (github-library (locate-library "ai-code-github")))
    (unwind-protect
        (progn
          ;; Isolate the load behavior under test instead of relying on
          ;; whatever other test files already required globally.
          (when (featurep 'ai-code-git)
            (unload-feature 'ai-code-git t))
          (when (featurep 'ai-code-github)
            (unload-feature 'ai-code-github t))
          (load git-library nil 'nomessage)
          (should (featurep 'ai-code-git))
          (should-not (featurep 'ai-code-github)))
      (unless (featurep 'ai-code-git)
        (load git-library nil 'nomessage))
      (unless (featurep 'ai-code-github)
        (load github-library nil 'nomessage)))))

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

(ert-deftest ai-code-test-git-worktree-branch-interactive-spec-uses-own-reader ()
  "Read the worktree arguments locally instead of via `magit-branch-read-args'."
  (should (equal (cadr (interactive-form 'ai-code-git-worktree-branch))
                 '(ai-code--read-worktree-branch-args))))

(ert-deftest ai-code-test-git-worktree-read-args-accepts-new-branch-name ()
  "Accept a brand new branch name without routing it through completion.
Magit 4.7 reads the name with `magit-completing-read' and passes a
validation function as REQUIRE-MATCH; Helm turns that into a strict
match against an empty candidate list, so the name can never be
confirmed and the command is aborted."
  (let ((read-prompts '())
        (read-defaults '()))
    (cl-letf (((symbol-function 'magit-read-starting-point)
               (lambda (&rest _args) "main"))
              ((symbol-function 'magit-rev-verify)
               (lambda (rev) (string= rev "main")))
              ((symbol-function 'magit-list-remote-branch-names)
               (lambda (&rest _args) '()))
              ((symbol-function 'magit-list-local-branch-names)
               (lambda () '("main")))
              ((symbol-function 'magit-git-success)
               (lambda (&rest _args) t))
              ((symbol-function 'magit-completing-read)
               (lambda (&rest _args)
                 (ert-fail "New branch name must not be read with completion")))
              ((symbol-function 'magit-read-string-ns)
               (lambda (prompt &optional _initial _history default &rest _args)
                 (push prompt read-prompts)
                 (push default read-defaults)
                 "brand-new-branch")))
      (should (equal (ai-code--read-worktree-branch-args)
                     '("brand-new-branch" "main")))
      (should (equal (length read-prompts) 1))
      (should (equal read-defaults '(nil))))))

(ert-deftest ai-code-test-git-worktree-read-args-retries-invalid-branch-name ()
  "Ask again, keeping the rejected input editable, when Git rejects a name."
  (let ((inputs '("bad..name" "good-name"))
        (prompts '()))
    (cl-letf (((symbol-function 'magit-read-starting-point)
               (lambda (&rest _args) "main"))
              ((symbol-function 'magit-rev-verify)
               (lambda (_rev) t))
              ((symbol-function 'magit-list-remote-branch-names)
               (lambda (&rest _args) '()))
              ((symbol-function 'magit-list-local-branch-names)
               (lambda () '()))
              ((symbol-function 'magit-git-success)
               (lambda (&rest args) (not (member "bad..name" args))))
              ((symbol-function 'magit-read-string-ns)
               (lambda (prompt &optional initial &rest _args)
                 (push (cons prompt initial) prompts)
                 (pop inputs))))
      (should (equal (ai-code--read-worktree-branch-args) '("good-name" "main")))
      (should (equal (length prompts) 2))
      (should (equal (cdar prompts) "bad..name"))
      (should (string-prefix-p "Invalid branch name; " (caar prompts))))))

(ert-deftest ai-code-test-git-worktree-read-args-suggests-name-from-remote-branch ()
  "Default the new branch name to the remote branch name without its remote."
  (let (captured-default)
    (cl-letf (((symbol-function 'magit-read-starting-point)
               (lambda (&rest _args) "origin/topic"))
              ((symbol-function 'magit-rev-verify)
               (lambda (_rev) t))
              ((symbol-function 'magit-list-remote-branch-names)
               (lambda (&rest _args) '("origin/topic")))
              ((symbol-function 'magit-list-local-branch-names)
               (lambda () '("main")))
              ((symbol-function 'magit-git-success)
               (lambda (&rest _args) t))
              ((symbol-function 'magit-read-string-ns)
               (lambda (_prompt &optional _initial _history default &rest _args)
                 (setq captured-default default)
                 (or default "unexpected"))))
      (should (equal (ai-code--read-worktree-branch-args) '("topic" "origin/topic")))
      (should (equal captured-default "topic")))))

(ert-deftest ai-code-test-git-worktree-read-args-rejects-invalid-starting-point ()
  "Signal before asking for a name when the starting point does not resolve."
  (cl-letf (((symbol-function 'magit-read-starting-point)
             (lambda (&rest _args) "no-such-ref"))
            ((symbol-function 'magit-rev-verify)
             (lambda (_rev) nil))
            ((symbol-function 'magit-read-string-ns)
             (lambda (&rest _args)
               (ert-fail "Branch name must not be read for an invalid starting point"))))
    (should-error (ai-code--read-worktree-branch-args) :type 'user-error)))

(ert-deftest ai-code-test-git-worktree-branch-signals-when-git-fails ()
  "Report a failing `git worktree add' instead of silently doing nothing."
  (let* ((temp-worktree-root (make-temp-file "ai-code-worktree-root-" t))
         (ai-code-git-worktree-root temp-worktree-root)
         (git-root "/tmp/sample-repo/")
         (dired-called nil)
         (task-prompted nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--validate-git-repository)
                   (lambda () git-root))
                  ((symbol-function 'magit-branch-p)
                   (lambda (_b) nil))
                  ((symbol-function 'magit-call-git)
                   (lambda (&rest _args) 128))
                  ((symbol-function 'dired)
                   (lambda (_dir) (setq dired-called t)))
                  ((symbol-function 'y-or-n-p)
                   (lambda (_prompt) (setq task-prompted t) nil)))
          (should-error (ai-code-git-worktree-branch "feature/git-fails" "main")
                        :type 'user-error)
          (should-not dired-called)
          (should-not task-prompted))
      (delete-directory temp-worktree-root t))))

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
         captured-dired-path)
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
                  ((symbol-function 'dired)
                   (lambda (path)
                     (setq captured-dired-path path)))
                  ((symbol-function 'y-or-n-p)
                   (lambda (_prompt) nil)))
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
          (should (equal captured-dired-path worktree-path)))
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
         captured-dired-path)
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
                  ((symbol-function 'dired)
                   (lambda (path)
                     (setq captured-dired-path path)))
                  ((symbol-function 'y-or-n-p)
                   (lambda (_prompt) nil)))
          (ai-code-git-worktree-branch branch start-point)
          (should-not (member "-b" captured-git-args))
          (should (equal captured-git-args
                         (list "worktree" "add"
                               (file-truename worktree-path)
                               branch)))
          (should (equal captured-dired-path worktree-path)))
      (delete-directory temp-worktree-root t)
      (delete-directory temp-git-root t))))

(defun ai-code-test--run-worktree-action (choice)
  "Run `ai-code-git-worktree-action' selecting CHOICE and return the command.
CHOICE is the completion candidate string to answer with.  The returned
value is the symbol handed to `call-interactively'."
  (let (captured-fn captured-default)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _collection &optional _pred _require-match
                                _initial _hist default &rest _)
                 (setq captured-default default)
                 (or choice default)))
              ((symbol-function 'call-interactively)
               (lambda (fn &optional _record-flag _keys)
                 (setq captured-fn fn))))
      (ai-code-git-worktree-action))
    (cons captured-fn captured-default)))

(ert-deftest ai-code-test-git-worktree-action-defaults-to-worktree-branch ()
  "Accepting the default completion dispatches to `ai-code-git-worktree-branch'."
  (let ((result (ai-code-test--run-worktree-action nil)))
    (should (eq (car result) #'ai-code-git-worktree-branch))
    (should (equal (cdr result) (caar ai-code--worktree-action-choices)))))

(ert-deftest ai-code-test-git-worktree-action-dispatches-open-dir ()
  "Selecting the Dired entry dispatches to `ai-code-git-worktree-open-dir'."
  (should (eq (car (ai-code-test--run-worktree-action
                    "Open worktree directory (Dired)"))
              #'ai-code-git-worktree-open-dir)))

(ert-deftest ai-code-test-git-worktree-action-dispatches-visit ()
  "Selecting the visit entry dispatches to `magit-worktree-status'."
  (should (eq (car (ai-code-test--run-worktree-action "Visit existing worktree"))
              #'magit-worktree-status)))

(ert-deftest ai-code-test-git-worktree-action-dispatches-delete ()
  "Selecting the delete entry dispatches to `magit-worktree-delete'."
  (should (eq (car (ai-code-test--run-worktree-action "Delete worktree"))
              #'magit-worktree-delete)))

(ert-deftest ai-code-test-git-worktree-action-magit-entries-ignore-worktree-root ()
  "Magit-backed entries work even when `ai-code-git-worktree-root' is unset."
  (let ((ai-code-git-worktree-root nil))
    (should (eq (car (ai-code-test--run-worktree-action "Delete worktree"))
                #'magit-worktree-delete))
    (should (eq (car (ai-code-test--run-worktree-action "Visit existing worktree"))
                #'magit-worktree-status))))

(ert-deftest ai-code-test-git-worktree-open-dir-opens-dired ()
  "`ai-code-git-worktree-open-dir' opens Dired on the repo worktree directory."
  (let* ((temp-worktree-root (make-temp-file "wt-root" t))
         (repo-dir (expand-file-name "ai-code-interface.el" temp-worktree-root))
         (ai-code-git-worktree-root temp-worktree-root)
         (dired-called-with nil))
    (make-directory repo-dir t)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--validate-git-repository)
                   (lambda () "/tmp/fake/ai-code-interface.el/"))
                  ((symbol-function 'dired)
                   (lambda (dir) (setq dired-called-with dir))))
          (ai-code-git-worktree-open-dir)
          (should (equal dired-called-with repo-dir)))
      (delete-directory temp-worktree-root t))))

(ert-deftest ai-code-test-git-worktree-open-dir-from-inside-worktree-opens-repo-dired ()
  "`ai-code-git-worktree-open-dir' opens Dired for main repo when in worktree."
  (let* ((temp-worktree-root (make-temp-file "wt-root" t))
         (repo-dir (expand-file-name "main-repo" temp-worktree-root))
         (ai-code-git-worktree-root temp-worktree-root)
         (dired-called-with nil))
    (make-directory repo-dir t)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--validate-git-repository)
                   (lambda () "/tmp/fake/worktrees/main-repo/existing-wt"))
                  ((symbol-function 'ai-code--worktree-main-repo-root)
                   (lambda (&optional _dir) "/tmp/fake/projects/main-repo"))
                  ((symbol-function 'dired)
                   (lambda (dir) (setq dired-called-with dir))))
          (ai-code-git-worktree-open-dir)
          (should (equal dired-called-with repo-dir)))
      (delete-directory temp-worktree-root t))))

(ert-deftest ai-code-test-git-worktree-branch-from-inside-worktree-uses-main-repo ()
  "When called inside a worktree, create new worktree under main repo directory."
  (let* ((temp-worktree-root (make-temp-file "ai-code-worktree-root-" t))
         (ai-code-git-worktree-root temp-worktree-root)
         (branch "feature/from-wt")
         (start-point "main")
         (expected-repo-dir (expand-file-name "main-repo" temp-worktree-root))
         (expected-worktree-path (expand-file-name branch expected-repo-dir))
         captured-task-git-root
         captured-git-args
         captured-dired-path)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--validate-git-repository)
                   (lambda () "/tmp/fake/worktrees/main-repo/existing-wt"))
                  ((symbol-function 'ai-code--worktree-main-repo-root)
                   (lambda (&optional _dir) "/tmp/fake/projects/main-repo"))
                  ((symbol-function 'magit-branch-p)
                   (lambda (_b) nil))
                  ((symbol-function 'magit-call-git)
                   (lambda (&rest args)
                     (setq captured-git-args args)
                     0))
                  ((symbol-function 'dired)
                   (lambda (path)
                     (setq captured-dired-path path)))
                  ((symbol-function 'y-or-n-p)
                   (lambda (_prompt) t))
                  ((symbol-function 'ai-code--worktree-create-and-link-task-file)
                   (lambda (git-root _path _branch)
                     (setq captured-task-git-root git-root))))
          (ai-code-git-worktree-branch branch start-point)
          (should (equal captured-git-args
                         (list "worktree" "add" "-b" branch
                               (file-truename expected-worktree-path)
                               start-point)))
          (should (equal captured-dired-path expected-worktree-path))
          (should (equal captured-task-git-root "/tmp/fake/projects/main-repo")))
      (delete-directory temp-worktree-root t))))

(ert-deftest ai-code-test-git-worktree-open-dir-errors-when-missing ()
  "`ai-code-git-worktree-open-dir' errors when the worktree directory is absent."
  (let ((ai-code-git-worktree-root (expand-file-name "nonexistent-wt" temporary-file-directory)))
    (cl-letf (((symbol-function 'ai-code--validate-git-repository)
               (lambda () "/tmp/fake/some-repo/")))
      (should-error (ai-code-git-worktree-open-dir) :type 'user-error))))

(ert-deftest ai-code-test-git-worktree-open-dir-errors-without-worktree-root ()
  "`ai-code-git-worktree-open-dir' errors when the worktree root is unconfigured."
  (let ((ai-code-git-worktree-root nil))
    (should-error (ai-code-git-worktree-open-dir) :type 'user-error))
  (let ((ai-code-git-worktree-root ""))
    (should-error (ai-code-git-worktree-open-dir) :type 'user-error)))

(ert-deftest ai-code-test-git-worktree-branch-errors-without-worktree-root ()
  "`ai-code-git-worktree-branch' errors when the worktree root is unconfigured.
The directory and Git entry points are stubbed so that a regression fails the
test instead of creating a real worktree next to the repository."
  (let ((ai-code-git-worktree-root nil))
    (cl-letf (((symbol-function 'make-directory)
               (lambda (&rest _) (ert-fail "Must not create a directory")))
              ((symbol-function 'magit-call-git)
               (lambda (&rest _) (ert-fail "Must not invoke git"))))
      (should-error (ai-code-git-worktree-branch "feature/x" "main")
                    :type 'user-error))))

(ert-deftest ai-code-test-git-worktree-branch-opens-dired-after-creation ()
  "After creating worktree, open Dired on the worktree path instead of Magit status."
  (let* ((temp-worktree-root (make-temp-file "ai-code-worktree-root-" t))
         (ai-code-git-worktree-root temp-worktree-root)
         (git-root "/tmp/sample-repo/")
         (branch "feature/dired-test")
         (start-point "main")
         (repo-dir (expand-file-name "sample-repo" temp-worktree-root))
         (worktree-path (expand-file-name branch repo-dir))
         (dired-called-with nil)
         (magit-visit-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--validate-git-repository)
                   (lambda () git-root))
                  ((symbol-function 'magit-branch-p)
                   (lambda (_b) nil))
                  ((symbol-function 'magit-call-git)
                   (lambda (&rest _args)
                     (make-directory worktree-path t)
                     0))
                  ((symbol-function 'dired)
                   (lambda (dir) (setq dired-called-with dir)))
                  ((symbol-function 'magit-diff-visit-directory)
                   (lambda (_path) (setq magit-visit-called t)))
                  ((symbol-function 'y-or-n-p)
                   (lambda (_prompt) nil)))
          (ai-code-git-worktree-branch branch start-point)
          (should dired-called-with)
          (should (equal dired-called-with worktree-path))
          (should-not magit-visit-called))
      (delete-directory temp-worktree-root t))))

(ert-deftest ai-code-test-git-worktree-branch-creates-task-symlink-when-confirmed ()
  "After worktree creation, when user confirms, create task file and symlink it."
  (let* ((temp-worktree-root (make-temp-file "ai-code-worktree-root-" t))
         (temp-git-root (file-truename (make-temp-file "ai-code-git-root-" t)))
         (ai-code-git-worktree-root temp-worktree-root)
         (ai-code-files-dir-name ".ai.code.files")
         (branch "feature/task-link-test")
         (start-point "main")
         (repo-dir (expand-file-name
                    (file-name-nondirectory (directory-file-name temp-git-root))
                    temp-worktree-root))
         (worktree-path (expand-file-name branch repo-dir))
         (files-dir (expand-file-name ai-code-files-dir-name temp-git-root))
         (dired-called nil)
         (find-file-called-with nil)
         (opened-buffer (generate-new-buffer " *ai-code-worktree-task*")))
    (make-directory files-dir t)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--validate-git-repository)
                   (lambda () temp-git-root))
                  ((symbol-function 'ai-code--git-root)
                   (lambda (&optional _dir) temp-git-root))
                  ((symbol-function 'magit-branch-p)
                   (lambda (_b) nil))
                  ((symbol-function 'magit-call-git)
                   (lambda (&rest _args)
                     (make-directory worktree-path t)
                     0))
                  ((symbol-function 'dired)
                   (lambda (_dir) (setq dired-called t)))
                  ((symbol-function 'y-or-n-p)
                   (lambda (_prompt) t))
                  ((symbol-function 'read-string)
                   (lambda (_prompt &optional initial &rest _args)
                     (or initial "task-link-test")))
                  ((symbol-function 'ai-code-read-string)
                   (lambda (_prompt &optional initial &rest _args)
                     (or initial "task-link-test")))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _args)
                     (if (listp choices) (car choices) "")))
                  ((symbol-function 'ai-code-current-backend-label)
                   (lambda () "Codex"))
                  ((symbol-function 'find-file-other-window)
                   (lambda (file)
                     (setq find-file-called-with file)
                     opened-buffer))
                  ((symbol-function 'ai-code--generate-task-filename)
                   (lambda (_name) "task-link-test"))
                  ((symbol-function 'save-buffer)
                   (lambda (&rest _args) nil)))
          (ai-code-git-worktree-branch branch start-point)
          (should dired-called)
          ;; Task file should exist in main repo's files dir
          (let ((task-file (expand-file-name "task-link-test.org" files-dir)))
            (should (file-exists-p task-file))
            ;; Symlink should exist in worktree root
            (let ((symlink-path (expand-file-name "task-link-test.org" worktree-path)))
              (should (file-symlink-p symlink-path))
              (should (string= (file-truename symlink-path)
                               (file-truename task-file)))
              ;; The worktree-specific link should be opened.
              (should (equal find-file-called-with symlink-path)))))
      (when (buffer-live-p opened-buffer)
        (kill-buffer opened-buffer))
      (delete-directory temp-worktree-root t)
      (delete-directory temp-git-root t))))

(ert-deftest ai-code-test-git-worktree-task-starts-session-in-worktree ()
  "Start the public backend in the worktree that owns a shared task link."
  (let* ((git-root (make-temp-file "ai-code-real-git-root-" t))
         (temp-worktree-root (make-temp-file "ai-code-real-worktree-root-" t))
         (ai-code-git-worktree-root temp-worktree-root)
         (branch "feature/session-context")
         (repo-dir (ai-code--git-worktree-repo-dir git-root))
         (worktree-path (expand-file-name branch repo-dir))
         (task-link (expand-file-name "worktree-task.org" worktree-path))
         task-buffer
         captured-working-dir)
    (unwind-protect
        (progn
          (should (zerop (process-file "git" nil nil nil "-C" git-root
                                       "init" "--quiet")))
          (should (zerop (process-file "git" nil nil nil "-C" git-root
                                       "config" "user.email" "test@example.com")))
          (should (zerop (process-file "git" nil nil nil "-C" git-root
                                       "config" "user.name" "AI Code Test")))
          (with-temp-file (expand-file-name "README.md" git-root)
            (insert "# Test repository\n"))
          (should (zerop (process-file "git" nil nil nil "-C" git-root
                                       "add" "README.md")))
          (should (zerop (process-file "git" nil nil nil "-C" git-root
                                       "commit" "--quiet" "-m" "Initial commit")))
          (let ((default-directory git-root)
                (answers '("Worktree task" "worktree-task.org")))
            (cl-letf (((symbol-function 'dired) (lambda (_directory) nil))
                      ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                      ((symbol-function 'ai-code--validate-git-repository)
                       (lambda () git-root))
                      ((symbol-function 'magit-branch-p) (lambda (_branch) nil))
                      ((symbol-function 'magit-call-git)
                       (lambda (&rest args)
                         (apply #'process-file
                                "git" nil nil nil
                                (append (list "-C" git-root) args))))
                      ((symbol-function 'ai-code-read-string)
                       (lambda (&rest _args) (pop answers))))
              (ai-code-git-worktree-branch branch "HEAD")
              (setq task-buffer (find-buffer-visiting task-link))))
          (should (file-symlink-p task-link))
          (should (buffer-live-p task-buffer))
          (cl-letf (((symbol-function 'ai-code-mcp-agent-prepare-launch)
                     (lambda (_backend _working-dir argv) (list :argv argv)))
                    ((symbol-function 'ai-code-backends-infra--toggle-or-create-session)
                     (lambda (working-dir &rest _args)
                       (setq captured-working-dir working-dir))))
            (with-current-buffer task-buffer
              (ai-code-codex-cli)))
          (should (equal (file-name-as-directory (file-truename worktree-path))
                         (file-name-as-directory
                          (file-truename captured-working-dir)))))
      (when (buffer-live-p task-buffer)
        (kill-buffer task-buffer))
      (when (file-directory-p worktree-path)
        (ignore-errors
          (process-file "git" nil nil nil "-C" git-root
                        "worktree" "remove" "--force" worktree-path)))
      (ignore-errors (delete-directory temp-worktree-root t))
      (ignore-errors (delete-directory git-root t)))))

(ert-deftest ai-code-test-git-worktree-branch-skips-task-when-declined ()
  "After worktree creation, when user declines, no task file is created."
  (let* ((temp-worktree-root (make-temp-file "ai-code-worktree-root-" t))
         (temp-git-root (file-truename (make-temp-file "ai-code-git-root-" t)))
         (ai-code-git-worktree-root temp-worktree-root)
         (branch "feature/no-task")
         (start-point "main")
         (repo-dir (expand-file-name
                    (file-name-nondirectory (directory-file-name temp-git-root))
                    temp-worktree-root))
         (worktree-path (expand-file-name branch repo-dir))
         (dired-called nil)
         (find-file-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--validate-git-repository)
                   (lambda () temp-git-root))
                  ((symbol-function 'magit-branch-p)
                   (lambda (_b) nil))
                  ((symbol-function 'magit-call-git)
                   (lambda (&rest _args)
                     (make-directory worktree-path t)
                     0))
                  ((symbol-function 'dired)
                   (lambda (_dir) (setq dired-called t)))
                  ((symbol-function 'y-or-n-p)
                   (lambda (_prompt) nil))
                  ((symbol-function 'find-file-other-window)
                   (lambda (_file) (setq find-file-called t))))
          (ai-code-git-worktree-branch branch start-point)
          (should dired-called)
          (should-not find-file-called))
      (delete-directory temp-worktree-root t)
      (delete-directory temp-git-root t))))

(ert-deftest ai-code-test-git-commit-current-changes-explicit-message ()
  "Stage all changes and commit an explicit message without calling AI."
  (let (git-calls read-count)
    (cl-letf (((symbol-function 'ai-code--git-root)
               (lambda (&optional _dir) "/tmp/repo/"))
              ((symbol-function 'magit-git-output)
               (lambda (&rest args)
                 (if (equal args '("status" "--porcelain"))
                     " M ai-code-git.el"
                   (ert-fail (format "Unexpected git output call: %S" args)))))
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

(ert-deftest ai-code-test-git-commit-current-changes-generates-message-from-staged-diff ()
  "Empty input should generate and edit a message from the staged diff."
  (let ((read-values '("" "Generated commit message"))
        captured-prompt
        git-calls)
    (cl-letf (((symbol-function 'ai-code--git-root)
               (lambda (&optional _dir) "/tmp/repo/"))
              ((symbol-function 'ai-code-read-string)
               (lambda (&rest _args)
                 (prog1 (car read-values)
                   (setq read-values (cdr read-values)))))
              ((symbol-function 'magit-git-output)
               (lambda (&rest args)
                 (cond
                  ((equal args '("status" "--porcelain"))
                   "?? new-file.el")
                  ((equal args '("diff" "--cached"))
                   "diff --git a/a.el b/a.el\n+new line\n")
                  (t
                   (ert-fail (format "Unexpected git output call: %S" args))))))
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

(ert-deftest ai-code-test-git-commit-current-changes-errors-when-clean ()
  "Do not prompt or stage anything when the repository is clean."
  (cl-letf (((symbol-function 'ai-code--git-root)
             (lambda (&optional _dir) "/tmp/repo/"))
            ((symbol-function 'magit-git-output)
             (lambda (&rest args)
               (should (equal args '("status" "--porcelain")))
               ""))
            ((symbol-function 'ai-code-read-string)
             (lambda (&rest _args)
               (ert-fail "Clean repository should not prompt for a message")))
            ((symbol-function 'magit-call-git)
             (lambda (&rest _args)
               (ert-fail "Clean repository should not run git mutation commands"))))
    (should-error (ai-code-git-commit-current-changes) :type 'user-error)))

(defun ai-code-test--commit-with-generated-message
    (generated compose-enabled &optional compose-result)
  "Run the commit flow with GPTel returning GENERATED.
COMPOSE-ENABLED binds `ai-code-use-compose-buffer'.  The compose reader
returns COMPOSE-RESULT; the minibuffer reader accepts its initial input.
Return a plist with :minibuffer-edits, :compose-calls, :git-calls and
:error, where :error is the signaled user-error message, if any."
  (let ((ai-code-use-compose-buffer compose-enabled)
        (first-read t)
        minibuffer-edits
        compose-calls
        git-calls
        error-message)
    (cl-letf (((symbol-function 'ai-code--git-root)
               (lambda (&optional _dir) "/tmp/repo/"))
              ((symbol-function 'ai-code-read-string)
               (lambda (prompt &optional initial-input _candidates)
                 (if first-read
                     (progn (setq first-read nil) "")
                   (push (list prompt initial-input) minibuffer-edits)
                   initial-input)))
              ((symbol-function 'ai-code-compose-read)
               (lambda (prompt &optional initial-input _candidates)
                 (push (list prompt initial-input) compose-calls)
                 compose-result))
              ((symbol-function 'magit-git-output)
               (lambda (&rest args)
                 (if (equal args '("status" "--porcelain"))
                     "?? new-file.el"
                   "diff --git a/a.el b/a.el\n+new line\n")))
              ((symbol-function 'ai-code-call-gptel-sync)
               (lambda (_prompt) generated))
              ((symbol-function 'magit-call-git)
               (lambda (&rest args) (push args git-calls) 0))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil)))
      (condition-case err
          (ai-code-git-commit-current-changes)
        (user-error (setq error-message (error-message-string err)))))
    (list :minibuffer-edits (nreverse minibuffer-edits)
          :compose-calls (nreverse compose-calls)
          :git-calls (nreverse git-calls)
          :error error-message)))

(ert-deftest ai-code-test-git-commit-current-changes-uses-compose-for-long-generated-message ()
  "A multi-line generated message is edited in compose when enabled."
  (let ((result (ai-code-test--commit-with-generated-message
                 "Subject\n\nBody line" t "Edited subject\n\nEdited body\n")))
    (should (equal (plist-get result :compose-calls)
                   '(("Commit message: " "Subject\n\nBody line"))))
    (should-not (plist-get result :minibuffer-edits))
    (should (equal (plist-get result :git-calls)
                   '(("add" "-A")
                     ("commit" "-m" "Edited subject\n\nEdited body"))))))

(ert-deftest ai-code-test-git-commit-current-changes-keeps-minibuffer-for-short-generated-message ()
  "A single-line generated message keeps the minibuffer editor."
  (let ((result (ai-code-test--commit-with-generated-message
                 "Short subject" t "unused")))
    (should-not (plist-get result :compose-calls))
    (should (equal (plist-get result :minibuffer-edits)
                   '(("Commit message: " "Short subject"))))
    (should (equal (plist-get result :git-calls)
                   '(("add" "-A") ("commit" "-m" "Short subject"))))))

(ert-deftest ai-code-test-git-commit-current-changes-keeps-minibuffer-when-compose-disabled ()
  "A multi-line generated message keeps the minibuffer when compose is off."
  (let ((result (ai-code-test--commit-with-generated-message
                 "Subject\n\nBody line" nil "unused")))
    (should-not (plist-get result :compose-calls))
    (should (equal (plist-get result :minibuffer-edits)
                   '(("Commit message: " "Subject\n\nBody line"))))
    (should (equal (plist-get result :git-calls)
                   '(("add" "-A") ("commit" "-m" "Subject\n\nBody line"))))))

(ert-deftest ai-code-test-git-commit-current-changes-compose-cancel-aborts-commit ()
  "Cancelling the compose buffer aborts the commit."
  (let ((result (ai-code-test--commit-with-generated-message
                 "Subject\n\nBody line" t nil)))
    (should (equal (plist-get result :error) "Commit message cannot be empty"))
    (should (= (length (plist-get result :compose-calls)) 1))
    (should-not (assoc "commit" (plist-get result :git-calls)))))

(ert-deftest ai-code-test-git-push-current-branch-with-upstream ()
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
      (ai-code--git-push-current-branch)
      (should (equal git-calls '(("push")))))))

(ert-deftest ai-code-test-git-push-current-branch-sets-origin-upstream ()
  "Set origin as upstream when the branch has no upstream yet."
  (let (git-calls)
    (cl-letf (((symbol-function 'magit-get-current-branch)
               (lambda () "feature/test"))
              ((symbol-function 'magit-git-string)
               (lambda (&rest args)
                 (cond
                  ((equal args '("rev-parse" "--abbrev-ref"
                                 "--symbolic-full-name" "@{upstream}"))
                   nil)
                  ((equal args '("remote" "get-url" "origin"))
                   "git@github.com:acme/demo.git")
                  (t nil))))
              ((symbol-function 'magit-call-git)
               (lambda (&rest args)
                 (push args git-calls)
                 0)))
      (ai-code--git-push-current-branch)
      (should (equal git-calls
                     '(("push" "-u" "origin" "feature/test")))))))

(ert-deftest ai-code-test-git-record-turn-snapshot-uses-stash-create-or-head ()
  "Record turn snapshot using `git stash create' when dirty, or HEAD when clean."
  (let ((repo (make-temp-file "ai-code-turn-snap-" t))
        (ai-code-git--turn-snapshots (make-hash-table :test 'equal))
        (stash-sha "1111111111111111111111111111111111111111")
        (head-sha "2222222222222222222222222222222222222222"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'magit-git-string)
                     (lambda (&rest args)
                       (cond
                        ((equal args '("stash" "create")) stash-sha)
                        ((equal args '("rev-parse" "--verify" "HEAD")) head-sha)
                        (t nil))))
                    ((symbol-function 'magit-git-lines)
                     (lambda (&rest args)
                       (when (equal args '("ls-files" "--others" "--exclude-standard"))
                         '("existing-untracked.txt")))))
            (let ((snap (ai-code-git-record-turn-snapshot "Fix bug" repo)))
              (should (equal (plist-get snap :commit) stash-sha))
              (should (equal (plist-get snap :head) head-sha))
              (should (equal (plist-get snap :untracked) '("existing-untracked.txt")))
              (should (equal (plist-get snap :prompt) "Fix bug"))
              (should (equal (ai-code-git-last-turn-snapshot repo) snap))))
          (cl-letf (((symbol-function 'magit-git-string)
                     (lambda (&rest args)
                       (cond
                        ((equal args '("stash" "create")) "")
                        ((equal args '("rev-parse" "--verify" "HEAD")) head-sha)
                        (t nil))))
                    ((symbol-function 'magit-git-lines)
                     (lambda (&rest _args) nil)))
            (let ((snap (ai-code-git-record-turn-snapshot "Clean prompt" repo)))
              (should (equal (plist-get snap :commit) head-sha)))))
      (delete-directory repo t))))

(ert-deftest ai-code-test-git-review-turn-diff-renders-diff-and-untracked ()
  "Review turn diff should display unified diff and newly created untracked files."
  (let ((repo (make-temp-file "ai-code-turn-diff-" t))
        (ai-code-git--turn-snapshots (make-hash-table :test 'equal))
        (snap-sha "abcdef1234567890abcdef1234567890abcdef12")
        displayed-buffer)
    (unwind-protect
        (progn
          (puthash (file-name-as-directory (file-truename repo))
                   (list :commit snap-sha
                         :head snap-sha
                         :untracked '("old.txt")
                         :prompt "Refactor foo")
                   ai-code-git--turn-snapshots)
          (cl-letf (((symbol-function 'magit-git-output)
                     (lambda (&rest args)
                       (should (equal args (list "diff" snap-sha)))
                       "diff --git a/foo.el b/foo.el\n+added line\n"))
                    ((symbol-function 'magit-git-lines)
                     (lambda (&rest args)
                       (when (equal args '("ls-files" "--others" "--exclude-standard"))
                         '("old.txt" "new-ai-file.el"))))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buf &rest _args)
                       (setq displayed-buffer buf))))
            (let ((buf (ai-code-git-review-turn-diff repo)))
              (unwind-protect
                  (progn
                    (should (eq buf displayed-buffer))
                    (with-current-buffer buf
                      (should (derived-mode-p 'diff-mode))
                      (should (string-match-p "abcdef1" (buffer-string)))
                      (should (string-match-p "new-ai-file\\.el" (buffer-string)))
                      (should (string-match-p "\\+added line" (buffer-string)))))
                (when (buffer-live-p buf)
                  (kill-buffer buf))))))
      (delete-directory repo t))))

(ert-deftest ai-code-test-git-revert-turn-changes-restores-tree-and-removes-new-untracked ()
  "Revert turn changes should checkout snapshot commit and delete only newly added untracked files."
  (let* ((repo (make-temp-file "ai-code-turn-revert-" t))
         (old-untracked (expand-file-name "old.txt" repo))
         (new-untracked (expand-file-name "new-ai.txt" repo))
         (ai-code-git--turn-snapshots (make-hash-table :test 'equal))
         (snap-sha "abcdef1234567890abcdef1234567890abcdef12")
         git-calls)
    (unwind-protect
        (progn
          (with-temp-file old-untracked (insert "keep me\n"))
          (with-temp-file new-untracked (insert "remove me\n"))
          (puthash (file-name-as-directory (file-truename repo))
                   (list :commit snap-sha
                         :head snap-sha
                         :untracked '("old.txt")
                         :prompt "Try experiment")
                   ai-code-git--turn-snapshots)
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_prompt) t))
                    ((symbol-function 'magit-call-git)
                     (lambda (&rest args)
                       (push args git-calls)
                       0))
                    ((symbol-function 'magit-git-lines)
                     (lambda (&rest args)
                       (when (equal args '("ls-files" "--others" "--exclude-standard"))
                         '("old.txt" "new-ai.txt"))))
                    ((symbol-function 'message) #'ignore))
            (should (ai-code-git-revert-turn-changes repo))
            (should (equal git-calls
                           (list (list "checkout" snap-sha "--" "."))))
            (should (file-exists-p old-untracked))
            (should-not (file-exists-p new-untracked))))
      (delete-directory repo t))))

(ert-deftest ai-code-test-send-prompt-records-turn-snapshot ()
  "Sending a prompt should record a Git turn snapshot first."
  (let (recorded-prompt sent-command)
    (cl-letf (((symbol-function 'ai-code-git-record-turn-snapshot)
               (lambda (&optional prompt _root)
                 (setq recorded-prompt prompt)))
              ((symbol-function 'ai-code--prompt-choose-target-session)
               (lambda () nil))
              ((symbol-function 'ai-code-cli-send-command)
               (lambda (cmd)
                 (setq sent-command cmd)
                 t))
              ((symbol-function 'ai-code-cli-switch-to-buffer)
               #'ignore))
      (ai-code--send-prompt "Implement feature X\n")
      (should (equal recorded-prompt "Implement feature X\n"))
      (should (equal sent-command "Implement feature X\n")))))

(ert-deftest ai-code-test-git-worktree-action-dispatches-turn-checkpoint-commands ()
  "Selecting turn diff or revert in `ai-code-git-worktree-action' dispatches to the turn checkpoint commands."
  (should (eq (car (ai-code-test--run-worktree-action "Review last AI turn diff"))
              #'ai-code-git-review-turn-diff))
  (should (eq (car (ai-code-test--run-worktree-action "Revert last AI turn changes"))
              #'ai-code-git-revert-turn-changes)))

(ert-deftest ai-code-test-git-turn-snapshots-maintains-bounded-history-stack ()
  "Recording multiple snapshots keeps newest first up to `ai-code-git-turn-snapshot-max-history'."
  (let ((repo (make-temp-file "ai-code-turn-stack-" t))
        (ai-code-git--turn-snapshots (make-hash-table :test 'equal))
        (ai-code-git-turn-snapshot-max-history 2)
        (commits '("1111111111111111111111111111111111111111"
                   "2222222222222222222222222222222222222222"
                   "3333333333333333333333333333333333333333")))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-git-string)
                   (lambda (&rest args)
                     (cond
                      ((equal args '("stash" "create")) (pop commits))
                      ((equal args '("rev-parse" "--verify" "HEAD")) "head-sha")
                      (t nil))))
                  ((symbol-function 'magit-git-lines)
                   (lambda (&rest _args) nil)))
          (ai-code-git-record-turn-snapshot "Turn 1" repo)
          (ai-code-git-record-turn-snapshot "Turn 2" repo)
          (ai-code-git-record-turn-snapshot "Turn 3" repo)
          (let ((history (ai-code-git-turn-snapshots repo)))
            (should (= (length history) 2))
            (should (equal (mapcar (lambda (s) (plist-get s :prompt)) history)
                           '("Turn 3" "Turn 2")))
            (should (equal (plist-get (ai-code-git-last-turn-snapshot repo) :prompt)
                           "Turn 3"))))
      (delete-directory repo t))))

(ert-deftest ai-code-test-git-review-and-revert-turn-support-selecting-historical-snapshot ()
  "Passing SELECT-FROM-HISTORY prompts for an older snapshot in diff and revert."
  (let ((repo (make-temp-file "ai-code-turn-select-" t))
        (ai-code-git--turn-snapshots (make-hash-table :test 'equal))
        (older-sha "1111111111111111111111111111111111111111")
        (newer-sha "2222222222222222222222222222222222222222")
        diff-args
        git-calls)
    (unwind-protect
        (progn
          (puthash (file-name-as-directory (file-truename repo))
                   (list (list :commit newer-sha
                               :head newer-sha
                               :untracked nil
                               :prompt "Second turn")
                         (list :commit older-sha
                               :head older-sha
                               :untracked nil
                               :prompt "First turn"))
                   ai-code-git--turn-snapshots)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt candidates &rest _args)
                       (car (last candidates))))
                    ((symbol-function 'magit-git-output)
                     (lambda (&rest args)
                       (setq diff-args args)
                       "diff --git a/a.el b/a.el\n+old turn diff\n"))
                    ((symbol-function 'magit-git-lines)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'pop-to-buffer) #'ignore)
                    ((symbol-function 'y-or-n-p) (lambda (_p) t))
                    ((symbol-function 'magit-call-git)
                     (lambda (&rest args)
                       (push args git-calls)
                       0))
                    ((symbol-function 'message) #'ignore))
            (let ((buf (ai-code-git-review-turn-diff repo t)))
              (unwind-protect
                  (should (equal diff-args (list "diff" older-sha)))
                (when (buffer-live-p buf)
                  (kill-buffer buf))))
            (should (ai-code-git-revert-turn-changes repo t))
            (should (equal git-calls
                           (list (list "checkout" older-sha "--" "."))))))
      (delete-directory repo t))))

(ert-deftest ai-code-test-git-turn-diff-buffer-provides-revert-and-navigation-actions ()
  "Turn diff buffer should expose keybindings and buttons to navigate snapshots and revert."
  (let ((repo (make-temp-file "ai-code-turn-diff-actions-" t))
        (ai-code-git--turn-snapshots (make-hash-table :test 'equal))
        (older-sha "1111111111111111111111111111111111111111")
        (newer-sha "2222222222222222222222222222222222222222")
        git-calls)
    (unwind-protect
        (progn
          (puthash (file-name-as-directory (file-truename repo))
                   (list (list :commit newer-sha
                               :head newer-sha
                               :untracked nil
                               :prompt "Newer prompt")
                         (list :commit older-sha
                               :head older-sha
                               :untracked nil
                               :prompt "Older prompt"))
                   ai-code-git--turn-snapshots)
          (cl-letf (((symbol-function 'magit-git-output)
                     (lambda (&rest args)
                       (format "diff for %s\n" (cadr args))))
                    ((symbol-function 'magit-git-lines)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'pop-to-buffer) #'ignore)
                    ((symbol-function 'y-or-n-p) (lambda (_p) t))
                    ((symbol-function 'magit-call-git)
                     (lambda (&rest args)
                       (push args git-calls)
                       0))
                    ((symbol-function 'message) #'ignore))
            (let ((buf (ai-code-git-review-turn-diff repo)))
              (unwind-protect
                  (with-current-buffer buf
                    (should (eq (lookup-key (current-local-map) (kbd "R"))
                                #'ai-code-git-turn-diff-revert))
                    (should (eq (lookup-key (current-local-map) (kbd "p"))
                                #'ai-code-git-turn-diff-previous-snapshot))
                    (should (eq (lookup-key (current-local-map) (kbd "n"))
                                #'ai-code-git-turn-diff-next-snapshot))
                    (should (string-match-p "\\[Revert (R)\\]" (buffer-string)))
                    (should (string-match-p "2222222" (buffer-string)))
                    (ai-code-git-turn-diff-previous-snapshot)
                    (should (string-match-p "1111111" (buffer-string)))
                    (ai-code-git-turn-diff-revert)
                    (should (equal git-calls
                                   (list (list "checkout" older-sha "--" ".")))))
                (when (buffer-live-p buf)
                  (kill-buffer buf))))))
      (delete-directory repo t))))

(provide 'test_ai-code-git)

;;; test_ai-code-git.el ends here