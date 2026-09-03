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

(provide 'test_ai-code-git)

;;; test_ai-code-git.el ends here