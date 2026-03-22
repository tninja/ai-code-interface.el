;;; test_ai-code-discussion.el --- Tests for ai-code-discussion.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for ai-code-discussion.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dired)
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

(provide 'test_ai-code-discussion)

;;; test_ai-code-discussion.el ends here
