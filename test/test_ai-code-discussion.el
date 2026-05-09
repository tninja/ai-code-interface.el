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
  "Test that marked dired files are explained using git relative paths."
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

(ert-deftest ai-code-test-create-notes-uses-org-roam-target-and-syncs ()
  "Test `ai-code-create-notes' uses org-roam target and triggers sync."
  (let* ((roam-dir (make-temp-file "ai-code-roam" t))
         (expected-file (expand-file-name "architecture-notes-2026.org" roam-dir))
         (previous-roam-directory (when (boundp 'org-roam-directory) org-roam-directory))
         captured-prompt
         sync-called)
    (unwind-protect
        (progn
          (setq org-roam-directory roam-dir)
          (cl-letf (((symbol-function 'completing-read)
                     (let ((answers '("all buffers of current window" "new org-roam note")))
                       (lambda (&rest _args)
                         (prog1 (car answers)
                           (setq answers (cdr answers))))))
                    ((symbol-function 'ai-code-read-string)
                     (lambda (&rest _args) "collect architecture notes"))
                    ((symbol-function 'ai-code-call-gptel-sync)
                     (lambda (&rest _args) "Architecture Notes 2026"))
                    ((symbol-function 'ai-code--get-context-files-string)
                     (lambda () "\nFiles:\n@a.el\n@b.el"))
                    ((symbol-function 'org-roam-db-sync)
                     (lambda (&rest _args) (setq sync-called t)))
                    ((symbol-function 'ai-code--insert-prompt)
                     (lambda (prompt) (setq captured-prompt prompt))))
            (ai-code-create-notes)
            (should (file-exists-p expected-file))
            (should sync-called)
            (should-not (string-match-p (regexp-quote "\nScope:") captured-prompt))
            (should (string-match-p (regexp-quote "Do not include a \"Scope\" section in the note.") captured-prompt))
            (should (string-match-p (regexp-quote (format "Target note file: %s" expected-file)) captured-prompt))
            (should (string-match-p (regexp-quote "collect architecture notes") captured-prompt))))
      (setq org-roam-directory previous-roam-directory)
      (delete-directory roam-dir t))))

(ert-deftest ai-code-test-create-notes-uses-custom-directory-and-git-scope ()
  "Test `ai-code-create-notes' uses custom directory target for git repo scope."
  (let* ((target-dir (make-temp-file "ai-code-notes" t))
         (target-dir-with-slash (file-name-as-directory target-dir))
         (expected-file (expand-file-name "error-handling-design.org" target-dir))
         captured-prompt)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'completing-read)
                     (let ((answers '("current git repo" "existing directory")))
                       (lambda (&rest _args)
                         (prog1 (car answers)
                           (setq answers (cdr answers))))))
                    ((symbol-function 'read-directory-name)
                     (lambda (&rest _args) target-dir-with-slash))
                    ((symbol-function 'ai-code-read-string)
                     (lambda (&rest _args) "summarize error handling design"))
                    ((symbol-function 'ai-code-call-gptel-sync)
                     (lambda (&rest _args) "Error Handling Design"))
                    ((symbol-function 'ai-code--git-root)
                     (lambda (&optional _dir) "/tmp/repo"))
                    ((symbol-function 'ai-code--insert-prompt)
                     (lambda (prompt) (setq captured-prompt prompt))))
            (ai-code-create-notes)
            (should (file-exists-p expected-file))
            (should-not (string-match-p (regexp-quote "\nScope:") captured-prompt))
            (should (string-match-p (regexp-quote "Search repository content under /tmp/repo for relevant information.") captured-prompt))
            (should (string-match-p (regexp-quote (format "Target note file: %s" expected-file)) captured-prompt))
            (should (string-match-p (regexp-quote "summarize error handling design") captured-prompt))))
      (delete-directory target-dir t))))

(ert-deftest ai-code-test-create-notes-supports-current-ai-session-scope ()
  "Test `ai-code-create-notes' supports current AI session scope."
  (let* ((target-dir (make-temp-file "ai-code-notes" t))
         (target-dir-with-slash (file-name-as-directory target-dir))
         (expected-file (expand-file-name "session-summary.org" target-dir))
         captured-prompt)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'completing-read)
                     (let ((answers '("current ai session" "existing directory")))
                       (lambda (&rest _args)
                         (prog1 (car answers)
                           (setq answers (cdr answers))))))
                    ((symbol-function 'read-directory-name)
                     (lambda (&rest _args) target-dir-with-slash))
                    ((symbol-function 'ai-code-read-string)
                     (lambda (&rest _args) "summarize implementation decisions"))
                    ((symbol-function 'ai-code-call-gptel-sync)
                     (lambda (&rest _args) "Session Summary"))
                    ((symbol-function 'ai-code--insert-prompt)
                     (lambda (prompt) (setq captured-prompt prompt))))
            (ai-code-create-notes)
            (should (file-exists-p expected-file))
            (should-not (string-match-p (regexp-quote "\nScope:") captured-prompt))
            (should (string-match-p (regexp-quote "Use the content from the current AI session as the primary source.") captured-prompt))
            (should (string-match-p (regexp-quote (format "Target note file: %s" expected-file)) captured-prompt))
            (should (string-match-p (regexp-quote "summarize implementation decisions") captured-prompt))))
      (delete-directory target-dir t))))

(provide 'test_ai-code-discussion)

;;; test_ai-code-discussion.el ends here
