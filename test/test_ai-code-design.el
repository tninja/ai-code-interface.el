;;; test_ai-code-design.el --- Tests for task-aware design discussions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'ai-code)

(ert-deftest ai-code-design-test-org-task-is-context-without-edits ()
  "Design uses the current task and original references without editing Org."
  (with-temp-buffer
    (org-mode)
    (setq buffer-file-name "/tmp/design-task.org")
    (insert "* TODO Keep old configs working\nOriginal issue: https://example.org/issue/42\n")
    (goto-char (point-min))
    (forward-line 1)
    (set-buffer-modified-p nil)
    (let (sent)
      (cl-letf (((symbol-function 'ai-code-compose-read)
                 (lambda (_label prompt) prompt))
                ((symbol-function 'ai-code--format-repo-context-info)
                 (lambda () "Repo: current-worktree"))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq sent prompt))))
        (ai-code-design-solution))
      (should (string-match-p "Keep old configs working" sent))
      (should (string-match-p "https://example.org/issue/42" sent))
      (should (string-match-p "current-worktree" sent))
      (should (string-match-p "DESIGN MODE BOUNDARIES" sent))
      (should (equal (org-get-todo-state) "TODO"))
      (should-not (buffer-modified-p)))))

(ert-deftest ai-code-design-test-selected-region-takes-precedence ()
  "Selected text wins over the enclosing Org section."
  (with-temp-buffer
    (org-mode)
    (setq buffer-file-name "/tmp/design-task.org")
    (insert "* TODO General task\nDesign only this boundary.\nUnrelated notes.\n")
    (goto-char (point-min))
    (search-forward "Design only this boundary.")
    (set-mark (- (point) (length "Design only this boundary.")))
    (let (sent)
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'ai-code-compose-read)
                 (lambda (_label prompt) prompt))
                ((symbol-function 'ai-code--format-repo-context-info)
                 (lambda () nil))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq sent prompt))))
        (ai-code-design-solution))
      (should (string-match-p "Design only this boundary" sent))
      (should-not (string-match-p "Unrelated notes" sent)))))

(ert-deftest ai-code-design-test-outside-org-asks-for-goal ()
  "The action also supports a standalone design question."
  (with-temp-buffer
    (fundamental-mode)
    (let (sent)
      (cl-letf (((symbol-function 'ai-code-read-string)
                 (lambda (_label &rest _) "Design a retry policy"))
                ((symbol-function 'ai-code-compose-read)
                 (lambda (_label prompt) prompt))
                ((symbol-function 'ai-code--format-repo-context-info)
                 (lambda () nil))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq sent prompt))))
        (ai-code-design-solution))
      (should (string-match-p "Design a retry policy" sent))))
  (with-temp-buffer
    (fundamental-mode)
    (cl-letf (((symbol-function 'ai-code-read-string)
               (lambda (&rest _) "  "))
              ((symbol-function 'ai-code-compose-read)
               (lambda (&rest _) (ert-fail "No goal must not open Compose"))))
      (should-error (ai-code-design-solution) :type 'user-error))))

(ert-deftest ai-code-design-test-compose-cancel-is-side-effect-free ()
  "Cancellation does not send anything or ask to append notes."
  (with-temp-buffer
    (org-mode)
    (setq buffer-file-name "/tmp/design-task.org")
    (insert "* TODO Refactor retry policy\nOriginal requirement: no regressions.\n")
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (cl-letf (((symbol-function 'ai-code-compose-read)
               (lambda (&rest _) nil))
              ((symbol-function 'ai-code--format-repo-context-info)
               (lambda () nil))
              ((symbol-function 'y-or-n-p)
               (lambda (&rest _) (ert-fail "Cancelled Compose must not prompt")))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (&rest _) (ert-fail "Cancelled Compose must not send"))))
      (ai-code-design-solution))
    (should-not (buffer-modified-p))))

(ert-deftest ai-code-design-test-safety-rule-survives-compose-edits ()
  "Editable prompt cannot remove the required no-code boundary."
  (with-temp-buffer
    (fundamental-mode)
    (let (sent policies)
      (let ((ai-code-prompt-suffix-functions '(ignore)))
        (cl-letf (((symbol-function 'ai-code-read-string)
                   (lambda (&rest _) "Implement retry"))
                  ((symbol-function 'ai-code-compose-read)
                   (lambda (_label _prompt) "Please implement everything"))
                  ((symbol-function 'ai-code--insert-prompt)
                   (lambda (prompt)
                     (setq sent prompt
                           policies (list ai-code-prompt-suffix-functions
                                          ai-code--suppress-org-summary-prompt)))))
          (ai-code-design-solution)))
      (should (string-prefix-p "Please implement everything" sent))
      (should (string-match-p "Never implement the design" sent))
      (should (string-match-p "ONLY after" sent))
      (should-not (car policies))
      (should-not (cadr policies)))))

(ert-deftest ai-code-design-test-summary-is-opt-in-and-delayed ()
  "Existing Org note flow is optional and requires approval to write."
  (dolist (approve '(t nil))
    (with-temp-buffer
      (org-mode)
      (setq buffer-file-name "/tmp/design-task.org")
      (insert "* TODO Change cache policy\nHuman decision: preserve API.\n")
      (goto-char (point-min))
      (let ((ai-code-prompt-preprocess-filepaths nil)
            sent asked)
        (cl-letf (((symbol-function 'ai-code-compose-read)
                   (lambda (_label prompt) prompt))
                  ((symbol-function 'ai-code--format-repo-context-info)
                   (lambda () nil))
                  ((symbol-function 'y-or-n-p)
                   (lambda (_prompt) (setq asked t) approve))
                  ((symbol-function 'ai-code--write-prompt-to-file-and-send)
                   (lambda (prompt) (setq sent prompt))))
          (ai-code-design-solution))
        (should asked)
        (should (string-match-p "Change cache policy" sent))
        (should (string-match-p "only after" (downcase sent)))
        (should (string-match-p "DESIGN MODE BOUNDARIES" sent))
        (if approve
            (should (string-match-p "After completing, append a concise result summary" sent))
          (should-not (string-match-p "After completing, append a concise result summary" sent)))))))

(provide 'test-ai-code-design)

;;; test_ai-code-design.el ends here
