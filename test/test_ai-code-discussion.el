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

(provide 'test_ai-code-discussion)

;;; test_ai-code-discussion.el ends here
