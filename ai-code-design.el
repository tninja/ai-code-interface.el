;;; ai-code-design.el --- Discussion-first design for an AI task -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Explore a task-aware design without implementing it.  Reuse Verify's
;; region/Org context collection and the existing Compose and Org summary flow.

;;; Code:

(require 'subr-x)
(require 'ai-code-verify)
(require 'ai-code-input)
(require 'ai-code-prompt-mode)
(require 'ai-code-utils)

(defconst ai-code-design--safety-rule
  (concat
   "\n\nDESIGN MODE BOUNDARIES (mandatory): This is a design discussion, "
   "not an implementation request. Do not change source code, tests, "
   "configuration, Git state, PR metadata, TODO states, or existing human "
   "notes. Do not run commands that mutate the repository. Inspect code "
   "and tests read-only as needed. Do not write any file during the "
   "initial discussion. You may save design documentation ONLY after "
   "the user explicitly approves the final design and its destination. "
   "An opt-in Org result-summary instruction permits an append-only design "
   "note at that point, not an automatic first-turn write. If no destination "
   "was approved, ask first. A separate design document and any link back "
   "to the task require explicit approval. Never implement the design "
   "in this workflow; use a separate Code Change action.")
  "Non-implementation boundary appended after Compose edits.
This is an agent instruction, not an enforced filesystem permission.")

(defun ai-code-design--prompt (goal)
  "Build a discussion-first design prompt for GOAL and repository context."
  (let ((repo-context (ai-code--format-repo-context-info)))
    (concat
     "Discuss a software design with me before any implementation.\n\n"
     "Task / selected context:\n" (string-trim goal) "\n\n"
     (when (and repo-context (not (string-blank-p repo-context)))
       (concat "Repository context:\n" repo-context "\n\n"))
     "Read the relevant existing code, tests, and original human-provided "
     "requirements, including referenced issues and discussions when "
     "accessible. Distinguish those sources from AI-generated summaries "
     "and assumptions; identify missing or conflicting evidence.\n\n"
     "Start with a concise understanding of the problem and a recommended "
     "minimal design. Explain trade-offs or alternatives only where they "
     "matter; favor reuse, compatibility, and small independently "
     "verifiable changes. Ask one important clarifying question at a "
     "time when a human decision is needed. Invite discussion; do not "
     "generate a lengthy design document or implementation roadmap "
     "without being asked.\n\n"
     "After we agree, propose a concise Org design note covering only "
     "useful fields: Problem, Constraints, Decision, Alternatives, "
     "Verification, Open Questions, and original Sources. Preserve "
     "raw reference links. Ask whether to save the agreed result in "
     "the current task section or, if warranted, a separate Org design "
     "document linked from the task. Reuse an optional Org summary "
     "writeback instruction if present, but do not write before "
     "explicit approval of the design. Implementation and verification "
     "remain separate actions.")))

;;;###autoload
(defun ai-code-design-solution ()
  "Discuss a design for the selected region or enclosing Org task.
Without either, ask for a design goal.  Always offer editable Compose;
do not change code.  The existing optional Org summary flow can save
an agreed design after explicit approval during the conversation."
  (interactive)
  (let* ((ai-code--org-summary-anchor nil)
         (context (ai-code-verify--requirement-context t))
         (goal (or context (ai-code-read-string "Design goal: " ""))))
    (when (string-blank-p (or goal ""))
      (user-error "Select a task/region or provide a design goal"))
    (ai-code-with-prompt-policy (:org-summary t)
      (ai-code--confirm-and-send
       "Edit design discussion prompt (safety rule appended)"
       (ai-code-design--prompt goal)
       :force-compose t
       :after-edit (lambda (edited)
                     (concat edited ai-code-design--safety-rule))))))

(provide 'ai-code-design)

;;; ai-code-design.el ends here
