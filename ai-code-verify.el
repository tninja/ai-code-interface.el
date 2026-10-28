;;; ai-code-verify.el --- On-demand evidence-first verification -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Verify current work on demand, without introducing an Org TODO state.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'ai-code-change)
(require 'ai-code-prompt-mode)
(require 'ai-code-utils)

(declare-function ai-code-read-string "ai-code-input")
(declare-function ai-code--get-context-files-string "ai-code-discussion")
(declare-function ai-code--format-repo-context-info "ai-code-utils")

(defvar ai-code--suppress-org-summary-prompt)
(defvar ai-code-prompt-suffix-functions)

(defun ai-code-verify--scope (&optional git-changes)
  "Return a scope description for current work.
With GIT-CHANGES, inspect the current repository changes instead."
  (cond
   (git-changes
    (let ((root (ai-code--git-root)))
      (unless root
        (user-error "Cannot verify Git changes outside a Git repository"))
      (format (concat "Current Git changes in %s. Inspect the status and "
                      "diff against an appropriate base (ask if ambiguous), "
                      "including staged and unstaged changes. Do not assume "
                      "that unrelated or untracked files belong to this task.")
              root)))
   ((region-active-p)
    (unless buffer-file-name
      (user-error "Select a region in a file to verify"))
    (let* ((beg (region-beginning))
           (end (region-end))
           (semantic (ai-code--format-scope-context
                      (ai-code--scope-context-for-region beg end))))
      (format "Selected region in %s (lines %d-%d):\n%s%s"
              (expand-file-name buffer-file-name)
              (line-number-at-pos beg)
              (line-number-at-pos end)
              (buffer-substring-no-properties beg end)
              (if (string-empty-p semantic)
                  ""
                (concat "\n" semantic)))))
   ((derived-mode-p 'org-mode)
    (unless buffer-file-name
      (user-error "Save this Org file before verifying its contents"))
    (if-let ((info (ai-code--implement-todo--get-org-todo-section-info t)))
        (concat
         (format "Org task/section in %s, starting at line %d:\n%s"
                 (expand-file-name buffer-file-name)
                 (plist-get info :line-number)
                 (ai-code--implement-todo--format-org-section-block info))
         (unless (plist-get info :inlined)
           (format "\nRead the full section at %s, lines %d-%d."
                   (expand-file-name buffer-file-name)
                   (plist-get info :line-number)
                   (plist-get info :end-line-number))))
      (if (buffer-modified-p)
          (user-error "Save the Org file or select a region to verify")
        (format "Current Org file: %s"
                (expand-file-name buffer-file-name)))))
   (t
    (unless buffer-file-name
      (user-error "Open a file or select a region to verify"))
    (when (buffer-modified-p)
      (user-error "Save the buffer or select a region to verify unsaved content"))
    (let ((semantic
           (ai-code--format-scope-context
            (ai-code--current-line-scope-context))))
      (concat (format "Current file: %s" (expand-file-name buffer-file-name))
              (unless (string-empty-p semantic)
                (concat "\n" semantic))
              (ai-code--get-context-files-string))))))

(defun ai-code-verify--prompt (scope focus)
  "Build an evidence-first verification brief for SCOPE and optional FOCUS."
  (ai-code--compose-question-brief
   :goal (concat
          "Verify whether the current work satisfies its intended behavior "
          "and relevant constraints, using independent, current evidence."
          (when (and focus (not (string-blank-p focus)))
            (concat "\nVerification focus: " focus)))
   :scope scope
   :context (ai-code--format-repo-context-info)
   :boundaries
   (concat
    "Verification only: do not edit source, tests, configuration, or Org "
    "headlines/states; do not commit or push. Run only safe, authorized "
    "checks. Do not access production or sensitive data without permission.")
   :instruction
   (concat
    "Choose checks proportionate to the task and its risk. Identify stated "
    "acceptance criteria and important invariants; flag unclear requirements "
    "rather than inventing them. Gather fresh observations from applicable "
    "tests, outputs, artifacts, logs, or reproductions. Separate observed "
    "facts from hypotheses. For a regression, compare with a working "
    "control/baseline if useful and available, and confirm the reproduction "
    "matches the actual failure signals. Seek plausible counterexamples; "
    "a previous agent's claim or passing unrelated tests is not proof. "
    "Report what was checked, evidence and results, what remains unverified, "
    "and the most useful next check if necessary. Give a concise assessment "
    "(supported / failed / uncertain) without changing task status.")))

;;;###autoload
(defun ai-code-verify-current-work (&optional git-changes)
  "Verify the current Org section, region, function, or file on demand.
A completed Org headline is valid scope; verification never changes its
TODO state. With prefix argument GIT-CHANGES, verify the current Git
changes instead. Optional focus can narrow the verification."
  (interactive "P")
  (let* ((scope (ai-code-verify--scope git-changes))
         (focus (ai-code-read-string "Verification focus (optional): " "")))
    (when focus
      ;; A read-only verification must not inherit implementation, commit,
      ;; or follow-up suffixes, or prompt an Org result-summary file edit.
      (let ((ai-code-prompt-suffix-functions nil)
            (ai-code--suppress-org-summary-prompt t))
        (ai-code--insert-prompt (ai-code-verify--prompt scope focus))))))

(provide 'ai-code-verify)

;;; ai-code-verify.el ends here
