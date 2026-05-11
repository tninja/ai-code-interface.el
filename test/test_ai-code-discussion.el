;;; test_ai-code-discussion.el --- Tests for ai-code-discussion.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code-discussion.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dired)
(require 'ai-code-change)
(require 'ai-code-discussion)

(defvar org-roam-directory)

(ert-deftest ai-code-test-explain-dired-uses-marked-files-as-git-relative-context ()
  "Test that marked Dired files are explained using git relative paths."
  (let (captured-initial-prompt captured-final-prompt)
    (cl-letf (((symbol-function 'dired-get-filename)
               (lambda (&rest _) "/tmp/project/a.el"))
              ((symbol-function 'dired-get-marked-files)
               (lambda (&rest _) '("/tmp/project/a.el" "/tmp/project/b.el")))
              ((symbol-function 'ai-code--get-git-relative-paths)
               (lambda (files)
                 (mapcar #'file-name-nondirectory files)))
              ((symbol-function 'ai-code-read-string)
               (lambda (_prompt initial-input &optional _candidate-list)
                 (setq captured-initial-prompt initial-input)
                 initial-input))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (prompt)
                 (setq captured-final-prompt prompt))))
      (ai-code--explain-dired)
      (should (string-match-p (regexp-quote "Please explain the selected files or directories.") captured-initial-prompt))
      (should (string-match-p (regexp-quote "\nFiles:\n@a.el\n@b.el") captured-initial-prompt))
      (should (equal captured-final-prompt captured-initial-prompt)))))

(ert-deftest ai-code-test-explain-with-scope-selection-routes-to-code-change ()
  "Test that scope selection can dispatch to code-change explanation."
  (let (code-change-called)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "code change"))
              ((symbol-function 'ai-code--explain-code-change)
               (lambda ()
                 (setq code-change-called t))))
      (ai-code--explain-with-scope-selection)
      (should code-change-called))))

(ert-deftest ai-code-test-explain-code-change-github-pr-builds-prompt ()
  "Test that GitHub PR change explanation builds the expected prompt."
  (let (captured-initial-prompt captured-final-prompt)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "GitHub PR"))
              ((symbol-function 'ai-code--pull-or-review-source-instruction)
               (lambda (review-source &optional review-mode)
                 (should (eq review-source 'github-mcp))
                 (should (eq review-mode 'explain-code-change))
                 "Use GitHub MCP server to inspect the pull request diff and metadata."))
              ((symbol-function 'ai-code-read-string)
               (lambda (prompt &optional initial-input _candidate-list)
                 (cond
                  ((string-match-p "Pull request URL:" prompt)
                   "https://github.com/acme/demo/pull/123")
                  ((string-match-p "Prompt:" prompt)
                   (setq captured-initial-prompt initial-input)
                   initial-input)
                  (t initial-input))))
              ((symbol-function 'ai-code--format-repo-context-info)
               (lambda ()
                 "\nRepo context: demo"))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (prompt)
                 (setq captured-final-prompt prompt))))
      (ai-code--explain-code-change 'github-mcp)
      (should (string-match-p "https://github.com/acme/demo/pull/123"
                              captured-initial-prompt))
      (should (string-match-p "Use GitHub MCP server"
                              captured-initial-prompt))
      (should (string-match-p "focus on understanding the change"
                              (downcase captured-initial-prompt)))
      (should (string-match-p "Repo context: demo"
                              captured-initial-prompt))
      (should (equal captured-final-prompt captured-initial-prompt)))))

(ert-deftest ai-code-test-explain-code-change-branch-range-builds-prompt ()
  "Test that base..branch change explanation builds the expected prompt."
  (let (captured-initial-prompt captured-final-prompt)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "base..branch"))
              ((symbol-function 'ai-code-read-string)
               (lambda (prompt &optional initial-input _candidate-list)
                 (cond
                  ((string-match-p "Base branch:" prompt) "main")
                  ((string-match-p "Branch to explain:" prompt) "feature/demo")
                  ((string-match-p "Prompt:" prompt)
                   (setq captured-initial-prompt initial-input)
                   initial-input)
                  (t initial-input))))
              ((symbol-function 'ai-code--git-root)
               (lambda (&optional _dir)
                 "/tmp/project/"))
              ((symbol-function 'ai-code--format-repo-context-info)
               (lambda ()
                 "\nRepo context: demo"))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (prompt)
                 (setq captured-final-prompt prompt))))
      (ai-code--explain-code-change)
      (should (string-match-p "main\\.\\.feature/demo" captured-initial-prompt))
      (should (string-match-p "git diff" (downcase captured-initial-prompt)))
      (should (string-match-p "Path: /tmp/project/" captured-initial-prompt))
      (should (string-match-p "Repo context: demo" captured-initial-prompt))
      (should (equal captured-final-prompt captured-initial-prompt)))))

(ert-deftest ai-code-test-explain-code-change-commit-builds-prompt ()
  "Test that single-commit change explanation builds the expected prompt."
  (let (captured-initial-prompt captured-final-prompt)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "commit"))
              ((symbol-function 'ai-code-read-string)
               (lambda (prompt &optional initial-input _candidate-list)
                 (cond
                  ((string-match-p "Commit hash:" prompt) "abc1234")
                  ((string-match-p "Prompt:" prompt)
                   (setq captured-initial-prompt initial-input)
                   initial-input)
                  (t initial-input))))
              ((symbol-function 'ai-code--git-root)
               (lambda (&optional _dir)
                 "/tmp/project/"))
              ((symbol-function 'ai-code--format-repo-context-info)
               (lambda ()
                 "\nRepo context: demo"))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (prompt)
                 (setq captured-final-prompt prompt))))
      (ai-code--explain-code-change)
      (should (string-match-p "abc1234" captured-initial-prompt))
      (should (string-match-p "git show" (downcase captured-initial-prompt)))
      (should (string-match-p "Path: /tmp/project/" captured-initial-prompt))
      (should (string-match-p "Repo context: demo" captured-initial-prompt))
      (should (equal captured-final-prompt captured-initial-prompt)))))

(ert-deftest ai-code-test-ask-question-routes-to-implement-todo-on-todo-comment ()
  "Test `ai-code-ask-question' calls `ai-code-implement-todo' when on a TODO comment."
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert ";; TODO: implement feature\n")
    (goto-char (point-min))

    (let (implement-todo-called)
      (cl-letf (((symbol-function 'ai-code--get-clipboard-text) (lambda () nil))
                ((symbol-function 'ai-code-implement-todo)
                 (lambda (_arg &optional _default-action) (setq implement-todo-called t)))
                ((symbol-function 'ai-code--ask-question-file)
                 (lambda (_ctx) (error "Should not reach ask-question-file")))
                ((symbol-function 'region-active-p) (lambda () nil)))

        (ai-code-ask-question nil)

        (should implement-todo-called)))))

(ert-deftest ai-code-test-ask-question-falls-through-on-non-todo ()
  "Test `ai-code-ask-question' calls `ai-code--ask-question-file' on non-TODO lines."
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert "some code line\n")
    (goto-char (point-min))

    (let (ask-file-called)
      (cl-letf (((symbol-function 'ai-code--get-clipboard-text) (lambda () nil))
                ((symbol-function 'ai-code-implement-todo)
                 (lambda (_arg) (error "Should not reach implement-todo")))
                ((symbol-function 'ai-code--ask-question-file)
                 (lambda (_ctx) (setq ask-file-called t)))
                ((symbol-function 'region-active-p) (lambda () nil)))

        (ai-code-ask-question nil)

        (should ask-file-called)))))

(ert-deftest ai-code-test-ask-question-routes-to-implement-todo-on-org-headline ()
  "Test `ai-code-ask-question' calls `ai-code-implement-todo' on Org TODO headline."
  (with-temp-buffer
    (require 'org)
    (setq buffer-file-name "plan.org")
    (insert "* TODO Build search feature\n")
    (org-mode)
    (goto-char (point-min))

    (let (implement-todo-called)
      (cl-letf (((symbol-function 'ai-code--get-clipboard-text) (lambda () nil))
                ((symbol-function 'ai-code-implement-todo)
                 (lambda (_arg &optional _default-action) (setq implement-todo-called t)))
                ((symbol-function 'ai-code--ask-question-file)
                 (lambda (_ctx) (error "Should not reach ask-question-file")))
                ((symbol-function 'region-active-p) (lambda () nil)))

        (ai-code-ask-question nil)

        (should implement-todo-called)))))

(ert-deftest ai-code-test-ask-question-passes-ask-question-action ()
  "Test that `ai-code-ask-question' passes \"Ask question\" as default-action."
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (insert ";; TODO: implement feature\n")
    (goto-char (point-min))

    (let (captured-default-action)
      (cl-letf (((symbol-function 'ai-code--get-clipboard-text) (lambda () nil))
                ((symbol-function 'ai-code-implement-todo)
                 (lambda (_arg &optional default-action)
                   (setq captured-default-action default-action)))
                ((symbol-function 'region-active-p) (lambda () nil)))

        (ai-code-ask-question nil)

        (should (equal captured-default-action "Ask question"))))))

(ert-deftest ai-code-test-ask-question-routes-to-implement-todo-on-plain-org-headline ()
  "Test `ai-code-ask-question' routes to `ai-code-implement-todo' on plain Org headline."
  (with-temp-buffer
    (require 'org)
    (setq buffer-file-name "notes.org")
    (insert "* Regular heading\n")
    (org-mode)
    (goto-char (point-min))

    (let (implement-todo-called)
      (cl-letf (((symbol-function 'ai-code--get-clipboard-text) (lambda () nil))
                ((symbol-function 'ai-code-implement-todo)
                 (lambda (_arg &optional _default-action)
                   (setq implement-todo-called t)))
                ((symbol-function 'ai-code--ask-question-file)
                 (lambda (_ctx) (error "Should not reach ask-question-file")))
                ((symbol-function 'region-active-p) (lambda () nil)))

        (ai-code-ask-question nil)

        (should implement-todo-called)))))

(ert-deftest ai-code-test-take-notes-org-buffer-inserts-at-point-with-default-request ()
  "Test `ai-code-take-notes' inserts a note at point in current Org buffer."
  (with-temp-buffer
    (require 'org)
    (org-mode)
    (insert "* Existing\n")
    (goto-char (point-max))
    (let ((default-request nil)
          (before-point (point))
          (tmp-dir (make-temp-file "ai-code-notes-org" t)))
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_prompt initial-input &optional _candidate-list)
                   (setq default-request initial-input)
                   initial-input))
                ((symbol-function 'ai-code--get-most-recent-ai-session-output)
                 (lambda ()
                   "Captured decision summary"))
                ((symbol-function 'ai-code--ensure-files-directory)
                 (lambda ()
                   tmp-dir)))
        (unwind-protect
            (progn
              (ai-code-take-notes)
              (should (equal default-request "Add the most recent AI output"))
              (goto-char before-point)
              (should (looking-at-p "\\* Captured decision summary"))
              (should (string-match-p (regexp-quote "Captured decision summary") (buffer-string))))
          (ignore-errors (delete-directory tmp-dir t)))))))

(ert-deftest ai-code-test-take-notes-non-org-can-create-note-under-org-roam-and-sync ()
  "Test `ai-code-take-notes' can create a new note under org-roam and sync DB."
  (let* ((tmp-root (make-temp-file "ai-code-note-roam" t))
         (org-roam-directory (expand-file-name "roam" tmp-root))
         (default-directory tmp-root)
         (created-file nil)
         (db-synced nil))
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'ai-code-read-string)
                     (lambda (prompt initial-input &optional _candidate-list)
                       (cond
                        ((string-match-p "what kind of note" (downcase prompt))
                         (or initial-input "Add the most recent AI output"))
                        (t initial-input))))
                    ((symbol-function 'ai-code--ensure-files-directory)
                     (lambda ()
                       (expand-file-name ".ai.code.files" tmp-root)))
                    ((symbol-function 'ai-code--org-roam-ready-p)
                     (lambda () t))
                    ((symbol-function 'ai-code--get-most-recent-ai-session-output)
                     (lambda ()
                       "Captured in roam"))
                    ((symbol-function 'y-or-n-p)
                     (lambda (_prompt) t))
                    ((symbol-function 'org-roam-db-sync)
                     (lambda (&optional _force)
                       (setq db-synced t)))
                    ((symbol-function 'ai-code--open-note-file-at-end)
                     (lambda (file)
                       (setq created-file file)
                       (find-file-noselect file))))
            (ai-code-take-notes)
            (should created-file)
            (should (string-prefix-p (file-name-as-directory org-roam-directory) created-file))
            (should db-synced)))
      (ignore-errors (delete-directory tmp-root t)))))

(ert-deftest ai-code-test-take-notes-non-org-fallbacks-to-directory-prompt-when-org-roam-unavailable ()
  "Test `ai-code-take-notes' prompts for directory when org-roam is unavailable."
  (let* ((tmp-root (make-temp-file "ai-code-note-dir" t))
         (default-directory tmp-root)
         (asked-directory-default nil))
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'ai-code-read-string)
                     (lambda (_prompt initial-input &optional _candidate-list)
                       (or initial-input "Add the most recent AI output")))
                    ((symbol-function 'ai-code--ensure-files-directory)
                     (lambda ()
                       (expand-file-name ".ai.code.files" tmp-root)))
                    ((symbol-function 'ai-code--get-most-recent-ai-session-output)
                     (lambda ()
                       "General note content"))
                    ((symbol-function 'read-directory-name)
                     (lambda (_prompt _dir default-dir &optional _mustmatch _initial)
                       (setq asked-directory-default default-dir)
                       default-dir))
                    ((symbol-function 'ai-code--open-note-file-at-end)
                     (lambda (file)
                       (find-file-noselect file))))
            (ai-code-take-notes)
            (should asked-directory-default)
            (should (string-match-p (regexp-quote ".ai.code.files/")
                                    (file-name-as-directory asked-directory-default)))))
      (ignore-errors (delete-directory tmp-root t)))))

(ert-deftest ai-code-test-take-notes-does-not-use-gptel ()
  "Test `ai-code-take-notes' does not call GPTel for note generation."
  (with-temp-buffer
    (require 'org)
    (org-mode)
    (let ((tmp-dir (make-temp-file "ai-code-notes-no-gptel" t))
          (gptel-called nil))
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_prompt initial-input &optional _candidate-list)
                   initial-input))
                ((symbol-function 'ai-code--get-most-recent-ai-session-output)
                 (lambda ()
                   "Generated by session"))
                ((symbol-function 'ai-code-call-gptel-sync)
                 (lambda (&rest _args)
                   (setq gptel-called t)
                   (error "Should not call GPTel")))
                ((symbol-function 'ai-code--ensure-files-directory)
                 (lambda ()
                   tmp-dir)))
        (unwind-protect
            (progn
              (ai-code-take-notes)
              (should (not gptel-called)))
          (ignore-errors (delete-directory tmp-dir t)))))))

(provide 'test_ai-code-discussion)

;;; test_ai-code-discussion.el ends here
