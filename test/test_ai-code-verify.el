;;; test_ai-code-verify.el --- On-demand verification tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Regression coverage for state-free evidence-first verification.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'ai-code-verify)

(defmacro ai-code-verify-test--with-org (content &rest body)
  "Evaluate BODY in a saved Org buffer containing CONTENT."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (setq buffer-file-name "/tmp/verify-work.org")
     (insert ,content)
     (org-mode)
     (goto-char (point-min))
     (re-search-forward "^\\*+ " nil t)
     (beginning-of-line)
     (set-buffer-modified-p nil)
     ,@body))


(ert-deftest ai-code-verify-test-pr-url-validation ()
  "Accept standard PR URLs without accepting unrelated GitHub pages."
  (should (ai-code-verify--pr-url-p "https://github.com/tninja/x.el/pull/541"))
  (should (ai-code-verify--pr-url-p "https://github.com/tninja/x.el/pull/541/"))
  (should (ai-code-verify--pr-url-p "https://github.com/tninja/x.el/pull/541#issuecomment-1"))
  (should-not (ai-code-verify--pr-url-p "https://github.com/tninja/x.el/issues/541"))
  (should-not (ai-code-verify--pr-url-p "https://example.org/tninja/x.el/pull/541"))
  (should-not (ai-code-verify--pr-url-p "https://github.com/tninja/x.el/pull/no")))

(ert-deftest ai-code-verify-test-branch-scope-requires-repo-and-branch ()
  "Branch verification must identify an actual local branch and base."
  (cl-letf (((symbol-function 'ai-code--git-root) (lambda (&optional _) "/tmp/repo/"))
            ((symbol-function 'magit-get-current-branch) (lambda () "feature-x")))
    (let ((scope (ai-code-verify--target-scope 'branch)))
      (should (string-match-p "feature-x" scope))
      (should (string-match-p "merge-base...HEAD" scope))
      (should (string-match-p "staged/unstaged changes separately" scope))))
  (cl-letf (((symbol-function 'ai-code--git-root) (lambda (&optional _) nil))
            ((symbol-function 'magit-get-current-branch) (lambda () "feature-x")))
    (should-error (ai-code-verify--target-scope 'branch) :type 'user-error))
  (cl-letf (((symbol-function 'ai-code--git-root) (lambda (&optional _) "/tmp/repo/"))
            ((symbol-function 'magit-get-current-branch) (lambda () nil)))
    (should-error (ai-code-verify--target-scope 'branch) :type 'user-error)))

(ert-deftest ai-code-verify-test-pr-scope-no-local-repo ()
  "PR verification targets remote changes even without local Git checkout."
  (let ((ai-code-default-review-source 'gh-cli))
    (cl-letf (((symbol-function 'ai-code--git-root)
               (lambda (&optional _) (ert-fail "Must not inspect local Git"))))
      (let ((scope (ai-code-verify--target-scope
                    'pr "https://github.com/owner/repo/pull/23")))
        (should (string-match-p "GitHub pull request" scope))
        (should (string-match-p "base and head SHAs" scope))
        (should (string-match-p "gh CLI" scope)))))
  (should-error (ai-code-verify--target-scope 'pr "not-a-pr") :type 'user-error))

(ert-deftest ai-code-verify-test-org-context-can-reference-done-headline ()
  "A completed Org task can supply requirements without status updates."
  (ai-code-verify-test--with-org
      "* TODO Parent\n** DONE Child\nAcceptance: support old configs.\n"
    (forward-line 1)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _) (car choices))))
      (let ((context (ai-code-verify--requirement-context)))
        (should (string-match-p "DONE Child" context))
        (should (string-match-p "support old configs" context))))
    (should (equal (org-get-todo-state) "DONE"))
    (should-not (buffer-modified-p))))

(ert-deftest ai-code-verify-test-selected-requirements-win-over-org ()
  "An explicit selection is authoritative over enclosing Org headlines."
  (ai-code-verify-test--with-org
      "* TODO Parent\nOnly verify this requirement.\nUnrelated context.\n"
    (goto-char (point-min))
    (search-forward "Only verify this requirement.")
    (set-mark (- (point) (length "Only verify this requirement.")))
    (cl-letf (((symbol-function 'use-region-p) (lambda () t)))
      (let ((context (ai-code-verify--requirement-context)))
        (should (string-match-p "Only verify" context))
        (should-not (string-match-p "Unrelated context" context))))))

(ert-deftest ai-code-verify-test-prompt-prioritizes-discussion ()
  "Investigate requirement fit and ask one useful question, not binary PASS."
  (cl-letf (((symbol-function 'ai-code--format-repo-context-info)
             (lambda () "Repository context")))
    (let ((prompt (ai-code-verify--prompt
                   "Current branch: feature-x"
                   "Fix retry when connection drops"
                   "Expected: exactly one retry")))
      (dolist (needle '("Fix retry when connection drops"
                        "Expected: exactly one retry"
                        "requirements-to-evidence"
                        "counterexamples"
                        "not a one-shot PASS/FAIL"
                        "ask ONE"
                        "wait for the user's answer"
                        "In subsequent turns"
                        "The user decides"
                        "Do not commit, push, merge"))
        (should (string-match-p (regexp-quote needle) prompt))))))

(ert-deftest ai-code-verify-test-pr-without-explicit-requirements ()
  "When the PR itself supplies the problem, treat intent as provisional."
  (cl-letf (((symbol-function 'ai-code--format-repo-context-info)
             (lambda () nil)))
    (let ((prompt (ai-code-verify--prompt
                   "GitHub pull request: https://github.com/a/b/pull/2"
                   "" nil)))
      (should (string-match-p "linked issue" prompt))
      (should (string-match-p "ask the user to correct" prompt))
      (should (string-match-p "inferred intent" prompt)))))

(ert-deftest ai-code-verify-test-command-branch-requires-problem ()
  "Avoid an aimless branch review without any requirements context."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) "Current branch"))
            ((symbol-function 'ai-code--git-root)
             (lambda (&optional _) "/tmp/project/"))
            ((symbol-function 'magit-get-current-branch)
             (lambda () "feature-x"))
            ((symbol-function 'ai-code-read-string)
             (lambda (&rest _) ""))
            ((symbol-function 'ai-code-compose-read)
             (lambda (_label prompt) prompt))
            ((symbol-function 'ai-code--insert-prompt)
             (lambda (&rest _) (ert-fail "Must not send without criteria"))))
    (with-temp-buffer
      (emacs-lisp-mode)
      (should-error (ai-code-verify-current-work) :type 'user-error))))

(ert-deftest ai-code-verify-test-command-branch-with-problem ()
  "Current branch and user problem drive the discussion."
  (let (sent)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Current branch"))
              ((symbol-function 'ai-code--git-root)
               (lambda (&optional _) "/tmp/project/"))
              ((symbol-function 'magit-get-current-branch)
               (lambda () "feature-x"))
              ((symbol-function 'ai-code-read-string)
               (lambda (&rest _) "Fix previous regression"))
              ((symbol-function 'ai-code--format-repo-context-info)
               (lambda () nil))
              ((symbol-function 'ai-code-compose-read)
               (lambda (_label prompt) prompt))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (p) (setq sent p))))
      (with-temp-buffer
        (emacs-lisp-mode)
        (ai-code-verify-current-work)
        (should (string-match-p "Fix previous regression" sent))
        (should (string-match-p "feature-x" sent))
        (should (string-match-p "ONE" sent))))))

(ert-deftest ai-code-verify-test-command-pr-without-local-repo ()
  "PR verification asks for URL and can derive intent from linked issue."
  (let (sent)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "GitHub PR"))
              ((symbol-function 'ai-code-read-string)
               (lambda (label &rest _)
                 (if (string-match-p "URL" label)
                     "https://github.com/owner/repo/pull/9" "")))
              ((symbol-function 'ai-code--format-repo-context-info)
               (lambda () nil))
              ((symbol-function 'ai-code--git-root)
               (lambda (&optional _) (ert-fail "No repo required")))
              ((symbol-function 'ai-code-compose-read)
               (lambda (_label prompt) prompt))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (p) (setq sent p))))
      (with-temp-buffer
        (fundamental-mode)
        (ai-code-verify-current-work)
        (should (string-match-p "owner/repo/pull/9" sent))
        (should (string-match-p "linked issue" sent))))))

(ert-deftest ai-code-verify-test-read-only-suffix-suppression ()
  "Discussion cannot inherit auto-commit or append an Org summary."
  (ai-code-verify-test--with-org "* TODO Fix regression\n"
    (let ((ai-code-prompt-suffix-functions '(ignore))
          (ai-code--suppress-org-summary-prompt nil)
          applied-suffixes applied-summary)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Current branch"))
                ((symbol-function 'ai-code--git-root)
                 (lambda (&optional _) "/tmp/project/"))
                ((symbol-function 'magit-get-current-branch)
                 (lambda () "feature-x"))
                ((symbol-function 'ai-code-read-string)
                 (lambda (&rest _) ""))
                ((symbol-function 'ai-code--format-repo-context-info)
                 (lambda () nil))
                ((symbol-function 'ai-code-compose-read)
                 (lambda (_label prompt) prompt))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (_p)
                   (setq applied-suffixes ai-code-prompt-suffix-functions
                         applied-summary ai-code--suppress-org-summary-prompt))))
        (ai-code-verify-current-work)
        (should-not applied-suffixes)
        (should applied-summary)
        (should (equal ai-code-prompt-suffix-functions '(ignore)))
        (should-not ai-code--suppress-org-summary-prompt)
        (should-not (buffer-modified-p))
        (should (equal (org-get-todo-state) "TODO"))))))

(ert-deftest ai-code-verify-test-original-todo-handler-still-omits-done ()
  "Existing implementation action continues to omit completed tasks."
  (ai-code-verify-test--with-org "* DONE Finished\n"
    (should-not (ai-code--org-section-candidates))
    (should (ai-code--org-section-candidates t))))


(ert-deftest ai-code-verify-test-org-section-is-primary-requirement-no-reprompt ()
  "The current task section alone is enough to verify a branch."
  (ai-code-verify-test--with-org
      "* TODO Fix cache invalidation\nAcceptance: clear stale entries.\n"
    (let (sent)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Current branch"))
                ((symbol-function 'ai-code--git-root)
                 (lambda (&optional _) "/tmp/project/"))
                ((symbol-function 'magit-get-current-branch)
                 (lambda () "fix-cache"))
                ((symbol-function 'ai-code-read-string)
                 (lambda (&rest _)
                   (ert-fail "Do not ask for requirements already in task section")))
                ((symbol-function 'ai-code--format-repo-context-info)
                 (lambda () nil))
                ((symbol-function 'ai-code-compose-read)
                 (lambda (_label prompt) prompt))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq sent prompt))))
        (ai-code-verify-current-work)
        (should (string-match-p "Selected task/requirements context" sent))
        (should (string-match-p "Fix cache invalidation" sent))
        (should (string-match-p "clear stale entries" sent))
        (should (string-match-p "STARTING POINT" sent))
        (should-not (string-match-p "No explicit problem statement was supplied" sent))
        (should (equal (org-get-todo-state) "TODO"))
        (should-not (buffer-modified-p))))))

(ert-deftest ai-code-verify-test-pr-selected-text-only-asks-url ()
  "Selected requirements drive PR verification without another text prompt."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/issue-notes.org")
    (insert "Must preserve old behavior.\nUnrelated follow-up.\n")
    (goto-char (point-min))
    (set-mark (+ (point) (length "Must preserve old behavior.")))
    (let (sent)
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "GitHub PR"))
                ((symbol-function 'ai-code-read-string)
                 (lambda (label &rest _)
                   (if (string-match-p "GitHub PR URL" label)
                       "https://github.com/tninja/ai-code-interface.el/pull/541"
                     (ert-fail "Selected requirements should skip re-entry"))))
                ((symbol-function 'ai-code--format-repo-context-info)
                 (lambda () nil))
                ((symbol-function 'ai-code-compose-read)
                 (lambda (_label prompt) prompt))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq sent prompt))))
        (ai-code-verify-current-work)
        (should (string-match-p "Selected requirements in" sent))
        (should (string-match-p "Must preserve old behavior" sent))
        (should-not (string-match-p "Unrelated follow-up" sent))
        (should (string-match-p "pull/541" sent))))))

(ert-deftest ai-code-verify-test-blank-selection-falls-back-to-org-section ()
  "Whitespace-only selections do not hide the useful task section."
  (ai-code-verify-test--with-org
      "* TODO Verify retry change\n   \nAcceptance: old API still works.\n"
    (forward-line 1)
    (set-mark (line-end-position))
    (cl-letf (((symbol-function 'use-region-p) (lambda () t)))
      (let ((context (ai-code-verify--requirement-context)))
        (should (string-match-p "Verify retry change" context))
        (should (string-match-p "old API still works" context))
        (should-not (string-match-p "Selected requirements in" context))))))


(ert-deftest ai-code-verify-test-strict-read-only-boundaries ()
  "Verification may discuss fixes but must prohibit implementation edits."
  (cl-letf (((symbol-function 'ai-code--format-repo-context-info)
             (lambda () nil)))
    (let ((prompt (ai-code-verify--prompt
                   "Current branch: fix-bug"
                   "Ensure backwards compatibility"
                   nil)))
      (dolist (needle '("STRICT READ-ONLY INVESTIGATION"
                        "Never modify, create, delete"
                        "source code, test code"
                        "snapshots, fixtures"
                        "documentation, Org tasks"
                        "Do not commit, push, merge"
                        "stage, checkout, install dependencies"
                        "Never automatically implement"
                        "non-destructive"
                        "does not modify the repository"
                        "ask the user to perform it separately"
                        "DO NOT APPLY"
                        "separate actions"))
        (should (string-match-p (regexp-quote needle) prompt)))
      (should (string-match-p "If the user wants changes" prompt)))))

(ert-deftest ai-code-verify-test-pr-also-strict-read-only ()
  "Read-only boundaries apply equally to PR investigation."
  (cl-letf (((symbol-function 'ai-code--format-repo-context-info)
             (lambda () nil)))
    (let ((prompt (ai-code-verify--prompt
                   "GitHub pull request: https://github.com/owner/repo/pull/5"
                   "" "A task section with acceptance criteria")))
      (should (string-match-p "STRICT READ-ONLY INVESTIGATION" prompt))
      (should (string-match-p "DO NOT APPLY" prompt))
      (should (string-match-p "PR comments" prompt)))))


(ert-deftest ai-code-verify-test-compose-edits-full-prompt ()
  "Users can inspect and change the complete Verify prompt before sending."
  (let ((ai-code-use-compose-buffer nil)
        draft label sent)
    (cl-letf (((symbol-function 'ai-code-compose-read)
               (lambda (prompt-label initial)
                 (setq label prompt-label
                       draft initial)
                 "Review only backwards compatibility and failure cases."))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (prompt) (setq sent prompt))))
      (ai-code-verify--confirm-and-send "Original full verification prompt")
      (should (string-match-p "Edit Verify prompt" label))
      (should (equal draft "Original full verification prompt"))
      (should (string-prefix-p
               "Review only backwards compatibility and failure cases." sent))
      (should-not (string-match-p "Original full verification prompt" sent))
      (should (string-suffix-p
               ai-code-verify--mandatory-source-rule sent))
      (should (string-match-p "VERIFY MODE SAFETY RULE" sent)))))

(ert-deftest ai-code-verify-test-compose-cancel-does-not-send ()
  "Cancelling the compose buffer does not send a prompt."
  (cl-letf (((symbol-function 'ai-code-compose-read)
             (lambda (_label _initial) nil))
            ((symbol-function 'ai-code--insert-prompt)
             (lambda (_prompt) (ert-fail "Cancelled Verify must not send"))))
    (should-not (ai-code-verify--confirm-and-send "Review branch changes"))))

(ert-deftest ai-code-verify-test-compose-read-only-rule-survives-edit ()
  "Removing the original boundaries cannot omit the final safety rule."
  (let (sent)
    (cl-letf (((symbol-function 'ai-code-compose-read)
               (lambda (_label _initial)
                 "Only discuss the changed behavior."))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (prompt) (setq sent prompt))))
      (ai-code-verify--confirm-and-send
       (ai-code-verify--prompt "Current branch: feature" "Fix bug" nil))
      (should (equal
               sent
               (concat "Only discuss the changed behavior."
                       ai-code-verify--mandatory-read-only-suffix
                       ai-code-verify--mandatory-source-rule)))
      (should (string-match-p "Do not create, edit, delete" sent))
      (should (string-match-p "Discuss proposed fixes without implementing" sent)))))

(ert-deftest ai-code-verify-test-command-org-context-shown-in-compose ()
  "The compose buffer receives task context before any prompt is sent."
  (ai-code-verify-test--with-org
      "* TODO Preserve cache behavior\nAcceptance: old cache keys still work.\n"
    (let (draft sent)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Current branch"))
                ((symbol-function 'ai-code--git-root)
                 (lambda (&optional _) "/tmp/project/"))
                ((symbol-function 'magit-get-current-branch)
                 (lambda () "fix-cache"))
                ((symbol-function 'ai-code-read-string)
                 (lambda (&rest _)
                   (ert-fail "Do not ask to retype the task section")))
                ((symbol-function 'ai-code--format-repo-context-info)
                 (lambda () nil))
                ((symbol-function 'ai-code-compose-read)
                 (lambda (_label prompt)
                   (setq draft prompt)
                   (concat prompt "\nCheck legacy cache keys explicitly.")))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq sent prompt))))
        (ai-code-verify-current-work)
        (should (string-match-p "Preserve cache behavior" draft))
        (should (string-match-p "old cache keys still work" draft))
        (should (string-match-p "Check legacy cache keys explicitly" sent))
        (should (string-suffix-p ai-code-verify--mandatory-source-rule sent))
        (should (string-match-p "VERIFY MODE SAFETY RULE" sent))
        (should (equal (org-get-todo-state) "TODO"))
        (should-not (buffer-modified-p))))))


(ert-deftest ai-code-verify-test-original-human-sources-over-ai-summary ()
  "Branch review must trace human references rather than AI-derived context."
  (cl-letf (((symbol-function 'ai-code--format-repo-context-info)
             (lambda () nil)))
    (let ((prompt (ai-code-verify--prompt
                   "Current branch: fix-cache" ""
                   (concat "Task notes: AI summary says cache must be rewritten.\n"
                           "Original references: Slack thread, Radar 1234, "
                           "Splunk log, and meeting notes."))))
      (dolist (needle '("STARTING POINT"
                        "SOURCE OF TRUTH"
                        "Slack threads"
                        "Radar tickets"
                        "GitHub issues and PR discussions"
                        "meeting notes"
                        "Splunk logs are raw"
                        "resume summaries"
                        "NOT acceptance"
                        "never silently"
                        "original-source"
                        "source-to-requirement-"
                        "URLs or precise source locations"
                        "Tests and CI show implementation"))
        (should (string-match-p (regexp-quote needle) prompt)))
      (should (string-match-p "AI summary says cache must be rewritten" prompt)))))

(ert-deftest ai-code-verify-test-conflicting-or-unavailable-originals ()
  "Verify should disclose unavailable sources and ask about conflicting intent."
  (cl-letf (((symbol-function 'ai-code--format-repo-context-info)
             (lambda () nil)))
    (let ((prompt (ai-code-verify--prompt
                   "GitHub pull request: https://github.com/owner/repo/pull/2"
                   "" nil)))
      (dolist (needle '("PR description may"
                        "AI-generated"
                        "check its provenance"
                        "later human"
                        "conflicting sources"
                        "ask for clarification"
                        "original references cannot"
                        "what remains unverified"))
        (should (string-match-p (regexp-quote needle) prompt))))))

(ert-deftest ai-code-verify-test-compose-cannot-omit-original-source-rule ()
  "Edited prompts still require comparison with original human references."
  (let (sent)
    (cl-letf (((symbol-function 'ai-code-compose-read)
               (lambda (_label _initial) "Only check generated design notes."))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (prompt) (setq sent prompt))))
      (ai-code-verify--confirm-and-send "Initial source-of-truth instructions")
      (should (string-suffix-p ai-code-verify--mandatory-source-rule sent))
      (should (string-match-p "original human-provided requirements" sent))
      (should (string-match-p "not intermediate AI-generated summaries" sent))
      (should (string-match-p "VERIFY MODE SAFETY RULE" sent)))))

(provide 'test_ai-code-verify)

;;; test_ai-code-verify.el ends here
