;;; ai-code-verify.el --- Discuss requirement fit of Git changes -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Evidence-first, interactive investigation of a branch or GitHub PR
;; against an issue, design goal, or user-provided requirements context.

;;; Code:

(require 'org)
(require 'subr-x)
(require 'magit)
(require 'ai-code-change)
(require 'ai-code-prompt-mode)
(require 'ai-code-utils)

(declare-function ai-code-read-string "ai-code-input")
(declare-function ai-code--format-repo-context-info "ai-code-utils")

(defvar ai-code--suppress-org-summary-prompt)
(defvar ai-code-prompt-suffix-functions)
(defvar ai-code-default-review-source)

(defun ai-code-verify--requirement-context ()
  "Return selected text or an enclosing Org section as requirement context.
This is background for discussion; no task states are changed."
  (cond
   ((use-region-p)
    (string-trim (buffer-substring-no-properties
                  (region-beginning) (region-end))))
   ((and (derived-mode-p 'org-mode) buffer-file-name)
    (when-let ((info (ai-code--implement-todo--get-org-todo-section-info t)))
      (concat
       (format "Org section from %s at line %d:\n%s"
               (expand-file-name buffer-file-name)
               (plist-get info :line-number)
               (ai-code--implement-todo--format-org-section-block info))
       (unless (plist-get info :inlined)
         (format "\nRead the full section in %s at lines %d-%d."
                 (expand-file-name buffer-file-name)
                 (plist-get info :line-number)
                 (plist-get info :end-line-number))))))
   (t nil)))

(defun ai-code-verify--pr-url-p (url)
  "Return non-nil if URL is a GitHub pull request URL."
  (and (stringp url)
       (string-match-p
        "^https://github\\.com/[^/[:space:]]+/[^/[:space:]]+/pull/[0-9]+/?\\(?:[?#][^[:space:]]*\\)?$"
        url)))

(defun ai-code-verify--target-scope (target &optional pr-url)
  "Describe TARGET as current branch or GitHub PR at PR-URL."
  (pcase target
    ('branch
     (let ((root (ai-code--git-root))
           (branch (magit-get-current-branch)))
       (unless root
         (user-error "Current branch verification requires a Git repository"))
       (unless branch
         (user-error "Cannot verify a detached HEAD as a named branch"))
       (format
        (concat "Current branch: %s\nRepository: %s\n"
                "Find the actual comparison base from branch/PR metadata; "
                "ask if ambiguous. Inspect the full committed branch delta "
                "(merge-base...HEAD), plus staged/unstaged changes separately. "
                "Do not silently include unrelated untracked work.")
        branch root)))
    ('pr
     (unless (ai-code-verify--pr-url-p pr-url)
       (user-error "Enter a GitHub PR URL such as https://github.com/owner/repo/pull/123"))
     (concat
      "GitHub pull request: " pr-url "\n"
      "Retrieve PR metadata, linked issue(s), base and head SHAs, "
      "actual diff, relevant surrounding code, review discussion, "
      "and available CI. Prefer the configured review source "
      (pcase ai-code-default-review-source
        ('github-mcp "(GitHub MCP); ")
        ('gh-cli "(gh CLI); ")
        (_ "(GitHub MCP or gh CLI); "))
      "if access is missing, say so rather than guessing. "
      "Review the precise PR head, not an unrelated local branch."))
    (_ (error "Unknown verification target: %S" target))))

(defun ai-code-verify--prompt (target-scope requirements context)
  "Build a discussion-first verification brief.
TARGET-SCOPE identifies the change; REQUIREMENTS are the user's problem
description, and CONTEXT is selected editor text or an Org subtree."
  (ai-code--compose-question-brief
   :goal
   (concat
    "Investigate together whether this branch or PR actually solves the "
    "specified problem and satisfies its requirements. This is an "
    "interactive engineering discussion, not a one-shot PASS/FAIL gate.")
   :scope target-scope
   :context
   (concat
    (or (ai-code--format-repo-context-info) "")
    (when (and context (not (string-blank-p context)))
      (concat "\n\nAdditional requirement context:\n" context)))
   :boundaries
   (concat
    "Investigation only. Do not edit code, tests, Org tasks, PR comments, "
    "or metadata; do not commit, push, or merge. Run only safe, authorized "
    "checks. Production or sensitive data access needs permission.")
   :instruction
   (concat
    (if (string-blank-p (or requirements ""))
        (concat "No explicit problem statement was entered. Start by "
                "reconstructing the intended behavior from the PR description, "
                "linked issue or supplied context. Identify assumptions and "
                "ask the user to correct or complete them rather than "
                "treating inferred intent as a fact.\n")
      (concat "User's problem/requirements:\n" requirements "\n"))
    "First, inspect the actual changed behavior against the intended "
    "behavior, not just code style. Build a small requirements-to-evidence "
    "map: which requirements are addressed, which may be missing, and "
    "which claims lack validation. Inspect relevant implementation paths, "
    "tests, and surrounding code as needed. Distinguish observed facts, "
    "inferences, and assumptions; cite concrete files, locations, "
    "test commands/results or CI evidence. Passing tests are not proof "
    "unless they exercise the required behavior. Check for regressions, "
    "unwanted scope, unnecessary complexity, and realistic counterexamples. "
    "For reported bugs, compare failing and working behavior when useful "
    "and possible; a similar symptom is not necessarily the same cause.\n"
    "DISCUSSION STYLE: Give an initial concise set of the most important "
    "evidence-backed findings, open questions, and possible next checks. "
    "Do not end with a mandatory binary verdict, a superficial checklist, "
    "or an automatic recommendation to merge/close. When human domain "
    "knowledge, a requirement ambiguity, or a tradeoff matters, ask ONE "
    "specific, high-value question and wait for the user's answer. "
    "In subsequent turns, incorporate their feedback, challenge earlier "
    "hypotheses, investigate the requested path, and refine conclusions. "
    "The user decides when evidence is sufficient or when to stop. "
    "If requested, provide a final assessment that distinguishes "
    "demonstrated behavior from uncertainty.")))

;;;###autoload
(defun ai-code-verify-current-work ()
  "Start a discussion verifying current branch or GitHub PR against a problem.
Selected text or the enclosing Org section supplies optional requirements
context. The agent investigates evidence, then discusses unresolved questions
in the ongoing session without changing source, PR, or Org task state."
  (interactive)
  (let* ((context (ai-code-verify--requirement-context))
         (target-choice
          (completing-read "Verify changes in: "
                           '("Current branch" "GitHub PR")
                           nil t nil nil "Current branch"))
         (target (if (equal target-choice "GitHub PR") 'pr 'branch))
         (pr-url (when (eq target 'pr)
                   (ai-code-read-string "GitHub PR URL: " "")))
         (target-scope (ai-code-verify--target-scope target pr-url))
         (requirements
          (ai-code-read-string
           (if context
               "Problem / requirements (optional; selected context included): "
             "Problem / requirements (PR may supply linked issue): ")
           "")))
    (when (and (eq target 'branch)
               (string-blank-p (or context ""))
               (string-blank-p (or requirements "")))
      (user-error "Provide the problem/requirements or select an Org section/region"))
    (when requirements
      ;; Do not inherit code-change, commit, or unrelated discussion suffixes
      ;; or add Org summaries during a read-only, open-ended discussion.
      (let ((ai-code-prompt-suffix-functions nil)
            (ai-code--suppress-org-summary-prompt t))
        (ai-code--insert-prompt
         (ai-code-verify--prompt target-scope requirements context))))))

(provide 'ai-code-verify)

;;; ai-code-verify.el ends here
