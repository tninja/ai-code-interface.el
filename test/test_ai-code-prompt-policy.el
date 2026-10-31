;;; test_ai-code-prompt-policy.el --- Workflow prompt boundaries -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Exercise the complete prompt pipeline with conflicting global extras enabled.

;;; Code:

(require 'ert)
(require 'ai-code)

(defun ai-code-policy-test--send (action &optional accept-summary)
  "Run ACTION in a task and return its final prompt and questions.
ACCEPT-SUMMARY controls the answer to the generic Org summary question."
  (let* ((directory (file-name-as-directory
                     (file-truename (make-temp-file "ai-code-policy-" t))))
         (file (expand-file-name "task.org" directory))
         (ai-code-use-prompt-suffix t)
         (ai-code-prompt-suffix "CUSTOM-EXECUTION-EXTRA")
         (ai-code-auto-test-type 'tdd)
         (ai-code-auto-commit-enabled t)
         (ai-code-discussion-auto-follow-up-enabled 'always)
         (ai-code-grill-me-enabled t)
         (ai-code-prompt-suffix-functions
          (append ai-code-prompt-suffix-functions
                  (list (lambda (_context) "EXTENSION-EXECUTION-EXTRA"))))
         (ai-code-use-compose-buffer t)
         (ai-code--suppress-org-summary-prompt nil)
         buffer prompt questions)
    (unwind-protect
        (progn
          (with-temp-file file (insert "* TODO Task\nOriginal requirements.\n"))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (org-mode)
            (goto-char (point-min))
            (cl-letf (((symbol-function 'ai-code--git-root)
                       (lambda (&optional _dir) directory))
                      ((symbol-function 'magit-get-current-branch) (lambda () "feature"))
                      ((symbol-function 'magit-git-string) (lambda (&rest _) nil))
                      ((symbol-function 'ai-code--task-resume-root) (lambda (_) directory))
                      ((symbol-function 'ai-code--task-git-snapshot)
                       (lambda (&rest _) "Observed Git facts"))
                      ((symbol-function 'ai-code--get-context-files-string) (lambda () ""))
                      ((symbol-function 'ai-code--format-repo-context-info) (lambda () nil))
                      ((symbol-function 'ai-code--get-ai-code-prompt-file-path) (lambda () nil))
                      ((symbol-function 'ai-code--send-prompt)
                       (lambda (text) (setq prompt text)))
                      ((symbol-function 'ai-code-compose-read)
                       (lambda (_label text &rest _) text))
                      ((symbol-function 'read-string)
                       (lambda (_label &optional initial &rest _) (or initial "")))
                      ((symbol-function 'ai-code-read-string)
                       (lambda (label &optional initial &rest _)
                         (if (string= label "Repository topic: ")
                             "Cache complexity"
                           (or initial ""))))
                      ((symbol-function 'completing-read)
                       (lambda (label &rest _)
                         (cond
                          ((string= label "Analyze: ") "Repository Topic")
                          ((string-prefix-p "Select refactoring technique" label)
                           "Value-driven Simplification")
                          (t "Current branch"))))
                      ((symbol-function 'y-or-n-p)
                       (lambda (question)
                         (push question questions)
                         accept-summary)))
              (funcall action))
            (should ai-code-use-prompt-suffix)
            (should ai-code-prompt-suffix-functions)
            (should-not ai-code--suppress-org-summary-prompt)
            (should-not (buffer-modified-p)))
          (should prompt)
          (list prompt (nreverse questions)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory directory t))))

(defun ai-code-policy-test--no-extras (prompt)
  "Reject enabled execution extras in PROMPT."
  (dolist (text '("CUSTOM-EXECUTION-EXTRA" "EXTENSION-EXECUTION-EXTRA"
                  "prompt/tdd" "commit just your own changes"
                  "discussion-follow-up.v1.md" "grilling.v1.md"))
    (should-not (string-match-p (regexp-quote text) prompt))))

(ert-deftest ai-code-policy-test-grow-waits-for-discussion-before-writeback ()
  "Grow keeps its own harness and writeback choice without automatic extras."
  (pcase-let ((`(,prompt ,questions)
               (ai-code-policy-test--send #'ai-code-grow-next-step t)))
    (ai-code-policy-test--no-extras prompt)
    (should-not questions)
    (should (string-match-p "growing-design.v1.md" prompt))
    (should (string-match-p "do not edit any file yet" prompt))
    (should (string-match-p "write it back as a direct child TODO" prompt))))

(ert-deftest ai-code-policy-test-value-simplification-stays-a-discussion ()
  "Value simplification does not acquire TDD, commit, or Org-write requests."
  (pcase-let ((`(,prompt ,questions)
               (ai-code-policy-test--send
                (lambda ()
                  (let ((this-command 'ai-code-refactor-book-method))
                    (ai-code-refactor-book-method)))
                t)))
    (ai-code-policy-test--no-extras prompt)
    (should-not questions)
    (should (string-match-p "Ask ONE high-value question at a time" prompt))
    (should-not (string-match-p "After completing, append" prompt))))

(ert-deftest ai-code-policy-test-context-workflows-keep-their-own-writes ()
  "Resume, both handoff loads, handoff dump, and checkpoint keep their contracts."
  (dolist (case (list (cons #'ai-code-task-resume "Append the briefing ONLY")
                     (cons (lambda () (ai-code-agent-handoff t))
                           "Load this whole task file")
                     (cons (lambda () (ai-code-agent-handoff))
                           "Load this agent handoff context")
                     (cons (lambda ()
                             (cl-letf (((symbol-function 'ai-code--agent-handoff-load-start)
                                        (lambda () nil)))
                               (ai-code-agent-handoff)))
                           "Modify only the task file")
                     (cons #'ai-code-session-checkpoint "Please stop and output a CHECKPOINT")))
    (pcase-let ((`(,prompt ,questions) (ai-code-policy-test--send (car case) t)))
      (ai-code-policy-test--no-extras prompt)
      (should-not questions)
      (should (string-match-p (regexp-quote (cdr case)) prompt))
      (should-not (string-match-p "After completing, append" prompt)))))

(ert-deftest ai-code-policy-test-verify-preserves-opt-in-notes ()
  "Verify keeps its mandatory rules and lets the user choose Org notes."
  (dolist (accept '(nil t))
    (pcase-let ((`(,prompt ,questions)
                 (ai-code-policy-test--send #'ai-code-verify-current-work accept)))
      (ai-code-policy-test--no-extras prompt)
      (should (equal questions '("Append result summary to current section? ")))
      (should (string-match-p "VERIFY MODE SAFETY RULE" prompt))
      (should (string-match-p "VERIFY SOURCE-OF-TRUTH RULE" prompt))
      (should (eq (and (string-match-p "After completing, append" prompt) t)
                  accept)))))

(ert-deftest ai-code-policy-test-normal-prompts-still-get-enabled-extras ()
  "Isolating specialized workflows does not disable ordinary prompt providers."
  (pcase-let ((`(,prompt ,_questions)
               (ai-code-policy-test--send
                (lambda () (ai-code--insert-prompt "Change the code.")))))
    (dolist (text '("CUSTOM-EXECUTION-EXTRA" "EXTENSION-EXECUTION-EXTRA"
                    "prompt/tdd" "commit just your own changes"))
      (should (string-match-p (regexp-quote text) prompt)))))

(ert-deftest ai-code-policy-test-restores-settings-after-error ()
  "A failed workflow cannot disable extras or summary questions globally."
  (let* ((providers (list #'ignore))
         (ai-code-prompt-suffix-functions providers)
         (ai-code-use-prompt-suffix t)
         (ai-code--suppress-org-summary-prompt nil))
    (should-error
     (ai-code-with-prompt-policy ()
       (should-not ai-code-prompt-suffix-functions)
       (should-not ai-code-use-prompt-suffix)
       (should ai-code--suppress-org-summary-prompt)
       (user-error "Workflow failed"))
     :type 'user-error)
    (should (eq ai-code-prompt-suffix-functions providers))
    (should ai-code-use-prompt-suffix)
    (should-not ai-code--suppress-org-summary-prompt)))

(ert-deftest ai-code-policy-test-nested-policy-keeps-summary-suppression ()
  "An inner optional-summary policy does not override its enclosing policy."
  (let ((ai-code--suppress-org-summary-prompt nil))
    (ai-code-with-prompt-policy ()
      (ai-code-with-prompt-policy (:org-summary t)
        (should ai-code--suppress-org-summary-prompt)))
    (should-not ai-code--suppress-org-summary-prompt)))

(provide 'test_ai-code-prompt-policy)

;;; test_ai-code-prompt-policy.el ends here
