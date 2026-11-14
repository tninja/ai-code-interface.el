;;; ai-code-verify.el --- Discuss requirement fit of Git changes -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Evidence-first, interactive investigation of a branch or GitHub PR
;; against an issue, design goal, or user-provided requirements context.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'magit)
(require 'ai-code-change)
(require 'ai-code-prompt-mode)
(require 'ai-code-compose)
(require 'ai-code-utils)

(declare-function ai-code-read-string "ai-code-input")
(declare-function ai-code--confirm-and-send "ai-code-input")
(declare-function ai-code--format-repo-context-info "ai-code-utils")

(defvar ai-code-default-review-source)
(defvar ai-code-use-prompt-suffix)

(defconst ai-code-verify--mandatory-read-only-suffix
  (concat
   "\n\nVERIFY MODE SAFETY RULE (mandatory): This is a read-only review. "
   "Do not create, edit, delete, or apply changes to code, tests, "
   "repository files, Git state, or PR metadata. Do not commit or push. "
   "The ONLY permitted write is one append-only verification note in "
   "the selected Org task section, if the user explicitly opted in via "
   "the appended summary instruction and later concludes the discussion. "
   "Never overwrite original requirements or change TODO states. "
   "Discuss proposed code fixes without implementing them.")
  "Read-only rule appended after editing the Verify prompt.
This rule is not a filesystem or backend permission boundary.")

(defconst ai-code-verify--mandatory-source-rule
  (concat
   "\n\nVERIFY SOURCE-OF-TRUTH RULE (mandatory): Judge the actual changes "
   "against original human-provided requirements and referenced discussions, "
   "not intermediate AI-generated summaries or implementation plans. "
   "If original references are unavailable or contradictory, say so and "
   "ask for clarification; do not claim verified compliance. "
   "CODE STATE: Record the actual examined checkout and HEAD, comparison "
   "base/head SHAs and merge-base, and staged/unstaged scope. Treat an "
   "editor observation as a starting point, not evidence that a review or "
   "test ran. Before final conclusions or the optional Org result summary, "
   "recheck the examined source state; for a PR re-fetch its base/head metadata. "
   "If it changed, label earlier evidence historical and investigate the "
   "new delta before extending conclusions. Record the actual reviewed "
   "identifiers and evidence scope in the optional Org result summary. If identity or "
   "freshness cannot be established, label it unverified. Never substitute "
   "local HEAD for a remote PR head.")
  "Original-source requirement appended after editing the Verify prompt.")

(defvar-local ai-code-verify--last-code-state nil
  "Local code observation for the last Verify sent from this buffer.
This is an in-memory observation, not a verification result.")

(defconst ai-code-verify--diff-arguments
  '("--binary" "--full-index" "--no-ext-diff" "--no-textconv" "--no-color"
    "--no-renames" "--ignore-submodules=none" "--src-prefix=a/" "--dst-prefix=b/")
  "Read-only diff options used for tracked-content fingerprints.
Binary patches retain content differences; external diff tools are disabled.")

(defun ai-code-verify--git-output (&rest arguments)
  "Return literal Git output for ARGUMENTS, signaling errors on failure."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let* ((coding-system-for-read 'no-conversion)
           (coding-system-for-write 'no-conversion)
           (status (apply #'magit-git-insert "--no-optional-locks"
                          "-c" "core.fsmonitor=false" arguments)))
      (unless (eq status 0)
        (error "Git %s failed (%s)" (car arguments) status))
      (buffer-string))))

(defun ai-code-verify--capture-code-state (root)
  "Observe tracked Git state in ROOT without saving buffers or writing Git.
Hash full staged and unstaged patches, not diff statistics.  Untracked
contents, ignored files and submodule contents are outside this identity.
Unsaved buffer ticks are recorded separately.  This is not atomic; agents
must recheck their actual source state before concluding a review."
  (let ((state (list :observed-at (format-time-string "%FT%T%z"))))
    (condition-case err
        (let* ((default-directory (file-name-as-directory root))
               (checkout (string-trim
                          (ai-code-verify--git-output "rev-parse" "--show-toplevel")))
               (head (string-trim
                      (ai-code-verify--git-output "rev-parse" "--verify" "HEAD")))
               (staged (apply #'ai-code-verify--git-output "diff" "--cached"
                              (append ai-code-verify--diff-arguments '("--"))))
               (unstaged (apply #'ai-code-verify--git-output "diff"
                                (append ai-code-verify--diff-arguments '("--"))))
               (status (ai-code-verify--git-output
                        "status" "--porcelain=v1" "--untracked-files=normal"
                        "--ignore-submodules=none"))
               unsaved)
          (unless (equal head (string-trim
                               (ai-code-verify--git-output "rev-parse" "--verify" "HEAD")))
            (error "HEAD changed during observation"))
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (when (and buffer-file-name (buffer-modified-p)
                         (file-in-directory-p buffer-file-name checkout))
                (push (cons buffer-file-name (buffer-chars-modified-tick)) unsaved))))
          (append state
                  (list :root (file-name-as-directory (file-truename checkout))
                        :head head
                        :staged (secure-hash 'sha256 staged)
                        :unstaged (secure-hash 'sha256 unstaged)
                        :status status
                        :unsaved (sort unsaved (lambda (a b) (string< (car a) (car b)))))))
      (error (append state (list :root root :error (error-message-string err)))))))

(defun ai-code-verify--same-code-state-p (before after)
  "Return non-nil if usable observations BEFORE and AFTER have the same scope."
  (and before after
       (not (plist-get before :error)) (not (plist-get after :error))
       (plist-get before :head) (plist-get before :staged) (plist-get before :unstaged)
       (cl-every (lambda (key) (equal (plist-get before key) (plist-get after key)))
                 '(:root :head :staged :unstaged :status :unsaved))))

(defun ai-code-verify--code-state-text (state)
  "Format code observation STATE with its explicit coverage limits."
  (concat
   (format "Observed at: %s\nCheckout: %s\n"
           (plist-get state :observed-at) (plist-get state :root))
   (if-let ((failure (plist-get state :error)))
       (format "Code identity unavailable: %s. Do not claim current-state verification.\n"
               failure)
     (format
      (concat "HEAD: %s\nStaged diff SHA-256: %s\nUnstaged diff SHA-256: %s\n"
              "Git status SHA-256: %s\nGit status (at most 3000 characters):\n%s\n"
              "Unsaved buffers (excluded from disk identity): %S\n")
      (plist-get state :head) (plist-get state :staged) (plist-get state :unstaged)
      (secure-hash 'sha256 (plist-get state :status))
      (truncate-string-to-width (plist-get state :status) 3000 nil nil "...")
      (plist-get state :unsaved)))
   "Scope: Git-visible tracked patches only; excludes untracked contents, "
   "ignored files, unsaved buffer contents and submodule contents. "
   "Untracked paths are discovery context, not reviewed content. "
   "This is not an atomic snapshot or proof that tests ran. "
   "Reproduce diff fingerprints with SHA-256 of raw stdout in this checkout: "
   (format "git --no-optional-locks -c core.fsmonitor=false diff [--cached] %s -- "
           (mapconcat #'identity ai-code-verify--diff-arguments " "))
   "(include --cached for staged; omit it for unstaged). "
   "Use M-x ai-code-verify-check-code-state in the invoking buffer to "
   "compare with this observation; preserve these identifiers and scope "
   "alongside the actual comparison base/head in the optional Org result summary."))

;;;###autoload
(defun ai-code-verify-check-code-state ()
  "Compare this buffer's last local Verify observation with current code.
Report changed, unchanged within the recorded scope, or unavailable.
Do not run tests, contact a remote PR, write files or change task states.
The observation is kept only until this buffer or Emacs is closed."
  (interactive)
  (unless ai-code-verify--last-code-state
    (user-error "No local Verify observation in this buffer"))
  (let* ((current (ai-code-verify--capture-code-state (ai-code--session-project-root)))
         (previous ai-code-verify--last-code-state)
         (status (cond ((or (plist-get previous :error) (plist-get current :error))
                        'unavailable)
                       ((ai-code-verify--same-code-state-p previous current) 'unchanged)
                       (t 'changed))))
    (message "%s"
             (pcase status
               ('unchanged "Verify observation unchanged within recorded scope; this is not a test/review result")
               ('changed "Code changed since Verify observation; earlier evidence may be stale.  Verify the new state")
               (_ "Code identity unavailable; verification freshness is unknown")))
    status))

(defun ai-code-verify--requirement-context (&optional anchor-summary)
  "Return the selected text or enclosing Org section as problem context.
A nonblank region takes precedence; Org section contents are used otherwise.
When ANCHOR-SUMMARY is non-nil, remember the selected Org section for an
optional, user-approved result summary.  No task states are changed."
  (let ((selected (when (use-region-p)
                    (string-trim
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))))))
    (cond
     ((and selected (not (string-blank-p selected)))
      (format "Selected requirements in %s (lines %d-%d):\n%s"
              (or buffer-file-name (buffer-name))
              (line-number-at-pos (region-beginning))
              (line-number-at-pos (region-end))
              selected))
     ((and (derived-mode-p 'org-mode) buffer-file-name)
    (when-let ((info (ai-code--implement-todo--get-org-todo-section-info t t)))
      (when anchor-summary
        (setq ai-code--org-summary-anchor
              (list :name (replace-regexp-in-string
                           "\\`\\*+[[:space:]]*" ""
                           (plist-get info :heading-line))
                    :end-line (plist-get info :end-line-number))))
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
     (t nil))))

(defun ai-code-verify--target-scope (target &optional pr-url)
  "Describe TARGET as current branch or a GitHub PR or compare view at PR-URL."
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
     (let ((url (string-trim (or pr-url ""))))
       (when (string-empty-p url)
         (user-error "Enter a GitHub PR or compare URL"))
       (concat
        "GitHub PR or compare URL: " url "\n"
        "For a pull request, retrieve PR metadata, linked issue(s), base and "
        "head SHAs, actual diff, relevant surrounding code, review discussion, "
        "and available CI. For a compare view or other change URL, review the "
        "exact base...head range it names. Prefer the configured review source "
        (pcase ai-code-default-review-source
          ('github-mcp "(GitHub MCP); ")
          ('gh-cli "(gh CLI); ")
          (_ "(GitHub MCP or gh CLI); "))
        "if access is missing, say so rather than guessing. "
        "Review the precise head, not an unrelated local branch.")))
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
      (concat "\n\nSelected task/requirements context (may include original references and AI-generated notes):\n" context)))
   :boundaries
   (concat
    "STRICT READ-ONLY INVESTIGATION. Never modify, create, delete, "
    "format, regenerate, or apply patches to source code, test code, "
    "snapshots, fixtures, configurations, documentation, or original "
    "Org task content. Do not change TODO states, Git state, PR comments, "
    "or metadata. Do not commit, push, merge, "
    "stage, checkout, install dependencies, or run auto-fix commands. "
    "Never automatically implement a suggested fix, even when a defect "
    "is obvious. Use read-only inspection and existing CI/test artifacts "
    "as evidence. Run a fresh check only when it is known to be "
    "non-destructive and does not modify the repository; otherwise "
    "describe the check and ask the user to perform it separately. "
    "Do not access production or sensitive data without permission. "
    "If the user wants code changes, propose a separate implementation "
    "action/session. The ONLY exception to read-only verification is a "
    "user-approved append-only verification note in the selected Org "
    "task section, and only when the discussion is explicitly finished.")
   :instruction
   (concat
    (cond
     ((and context (not (string-blank-p context)))
      (concat "The supplied selection or Org task section is the STARTING POINT "
              "for finding original requirements, not necessarily the "
              "authoritative requirement itself. Use any direct human "
              "instructions it contains, and follow its links or IDs to "
              "original human-authored discussions and decisions. Do not "
              "ask the user to repeat available context. Clarify only "
              "material ambiguities that affect the investigation.\n"
              (when (and requirements (not (string-blank-p requirements)))
                (concat "Additional user clarification:\n"
                        requirements "\n"))))
     ((string-blank-p (or requirements ""))
      (concat "No explicit problem statement was supplied. Trace original "
              "human-authored requirements in the linked issue, PR "
              "discussion, or other references. A PR description may "
              "be AI-generated: check its provenance. Identify assumptions "
              "and ask the user to correct them rather than treating "
              "inferred intent as fact.\n"))
     (t (concat "User's problem/requirements:\n" requirements "\n")))
    "SOURCE OF TRUTH: Before judging the changes, trace the task's or "
    "selection's original references when available: human-authored "
    "Slack threads, Radar tickets, GitHub issues and PR discussions, "
    "meeting notes, and direct user instructions. Splunk logs are raw "
    "observations, not automatically human requirements. Treat AI-generated "
    "resume summaries, derived context, design drafts, implementation plans, "
    "PR summaries, and agent conclusions as navigation aids, NOT acceptance "
    "criteria. Read the original sources if accessible; never silently "
    "substitute a generated summary. Identify which requirements came "
    "directly from humans and which are inferences. Check for later human "
    "decisions or conflicting sources; ask for clarification instead of "
    "choosing a convenient interpretation. If original references cannot "
    "be accessed, name the missing sources and state what remains unverified.\n"
    "Then compare the actual changed behavior to the original-source "
    "requirements, not just code style. Build a small source-to-requirement-"
    "to-change-to-evidence map, with URLs or precise source locations: "
    "which human requirements are addressed, which may be missing, and "
    "which claims lack validation. Tests and CI show implementation "
    "behavior, not what humans originally asked for. Inspect relevant "
    "implementation paths, "
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
    "OPTIONAL ORG WRITEBACK: If an appended result-summary instruction "
    "appears after this prompt, the user opted in to saving the result. "
    "Do not write any note during initial findings or while discussion "
    "is ongoing. Only after the user explicitly finishes the discussion, "
    "append ONE result-summary sub-heading in the indicated Org task "
    "section following the appended Org result-summary guidance (state the "
    "specific verification verdict rather than a generic title). "
    "Record the branch/PR, original human reference "
    "links, evidence-backed findings, unmet requirements, and open "
    "questions; clearly label AI interpretation and uncertainty. "
    "Never replace or rewrite original human-authored requirements, "
    "change task states, or turn the AI notes into acceptance criteria. "
    "If there is no appended summary instruction, do not write anything. "
    "Suggestions may name locations and show proposed changes as text, "
    "but DO NOT APPLY or write them during Verify; keep discussion and "
    "implementation as separate actions. If requested, provide a final "
    "assessment that distinguishes demonstrated behavior from uncertainty.")))

(defun ai-code-verify--confirm-and-send (prompt &optional code-state)
  "Let the user edit the complete Verify PROMPT, then send if accepted.
Always append mandatory read-only and original-source rules after editing.
Cancellation does not send anything or update task or repository state.
When CODE-STATE is supplied, reobserve that checkout after Compose and
remember it in the invoking buffer only after the prompt is actually sent."
  (let* ((origin (current-buffer))
         current
         (sent
          (ai-code--confirm-and-send
           "Edit Verify prompt (read-only rule appended)"
           (concat prompt
                   (when code-state
                     (concat "\n\nInitial code observation before Compose:\n"
                             (ai-code-verify--code-state-text code-state))))
           :force-compose t
           :after-edit
           (lambda (edited)
             (when code-state
               (setq current (ai-code-verify--capture-code-state (plist-get code-state :root))))
             (concat edited
                     (when current
                       (concat "\n\nCurrent code observation after Compose (supersedes the initial observation):\n"
                               (unless (ai-code-verify--same-code-state-p code-state current)
                                 "Code changed while Compose was open, or its identity is unavailable.\n")
                               (ai-code-verify--code-state-text current)))
                     ai-code-verify--mandatory-read-only-suffix
                     ai-code-verify--mandatory-source-rule)))))
    ;; Sending selects the session window, so store in the invoking buffer.
    ;; A nil result means missing-session recovery was declined.
    (when (and sent current (buffer-live-p origin))
      (with-current-buffer origin
        (setq-local ai-code-verify--last-code-state current)))
    sent))


;;;###autoload
(defun ai-code-verify-current-work ()
  "Start a discussion verifying current branch or GitHub PR against a problem.
Selected text takes precedence over the enclosing Org task section and
supplies the primary problem statement automatically.  Request a description
only when neither is available.  Edit the full prompt in a compose buffer
before sending; cancel without sending.  Continue discussing evidence
without changing code, PR metadata, or Org task state."
  (interactive)
  (let* ((ai-code--org-summary-anchor nil)
         (context (ai-code-verify--requirement-context t))
         (target-choice
          (completing-read "Verify changes in: "
                           '("Current branch" "GitHub PR")
                           nil t nil nil "Current branch"))
         (target (if (equal target-choice "GitHub PR") 'pr 'branch))
         (default-directory (if (eq target 'branch)
                                (ai-code--session-project-root)
                              default-directory))
         (pr-url (when (eq target 'pr)
                   (ai-code-read-string "GitHub PR or compare URL: " "")))
         (target-scope (ai-code-verify--target-scope target pr-url))
         (requirements
          (unless (and context (not (string-blank-p context)))
            (ai-code-read-string
             "Problem / requirements (PR may supply linked issue): " ""))))
    (when (and (eq target 'branch)
               (string-blank-p (or context ""))
               (string-blank-p (or requirements "")))
      (user-error "Provide the problem/requirements or select an Org section/region"))
    ;; Keep implementation/auto-commit suffixes disabled, but reuse the
    ;; existing opt-in Org result-summary question.  A summary is the only
    ;; permitted write, and only after the user concludes the discussion.
    (ai-code-with-prompt-policy (:org-summary t)
      (ai-code-verify--confirm-and-send
       (ai-code-verify--prompt target-scope requirements context)
       (when (eq target 'branch)
         (ai-code-verify--capture-code-state (ai-code--git-root)))))))

(defun ai-code-verify--document-prompt (file environment context documents)
  "Build a prompt asking the AI to write and run a deployment verification doc.
FILE is the target path.  ENVIRONMENT, CONTEXT and DOCUMENTS are optional:
the target environment or hosts, task text, and a user-provided list of
source documents."
  (concat
   "Write a deployment verification document in Org mode to: " file "\n\n"
   "Purpose: show that the change works where it is deployed.  Cover what "
   "unit tests and CI cannot: real endpoints and hosts, environment "
   "configuration and routing, front layers such as proxies or gateways, "
   "real clients such as browsers or apps, backward compatibility for "
   "existing callers, and whether every instance runs the new version "
   "during a rollout.  Do not repeat unit test cases; say in one line "
   "which behavior unit tests already cover.\n"
   (when environment
     (concat "Target environment(s): " environment "\n"))
   "\nFind the deployment verification cases in these sources, as available:\n"
   "- This AI coding session: requirements, decisions, the change, and "
   "checks already discussed.\n"
   (when context
     (concat "- Task context:\n" context "\n"))
   (when documents
     (concat "- User-provided documents: " documents "\n"))
   "\nUse this Org structure:\n"
   "* Summary: what is verified, the overall result with date and "
   "environment, known failures, and links to the ticket, PR, and "
   "requirement sources.\n"
   "* Test case list: a table with id, case, source link, expected result, "
   "and result with date.\n"
   "* Before testing: an environment table (name, host, deployed version or "
   "status), things to know, and the shell variables every command uses.\n"
   "* One section per verification method (for example curl, browser or "
   "client, scripts), with one heading per case: an Org link to its source, "
   "a runnable command in a src block, the expected result, the observed "
   "output in an example block with date and environment, and a status of "
   "PASS, FAIL, NOT RUN, or TO CONFIRM with the reason.\n"
   "* Known gaps: failures, cases not run, and risks such as a partial "
   "rollout.\n\n"
   "Run each read-only, safe case against the target non-production "
   "environment and record what you observed; never mark a case PASS "
   "without evidence.  Ask before running anything against production, "
   "anything that changes data or state, or any load test.  Do not modify "
   "source code, tests, or Git state; the only file to write is this "
   "document."))

(defun ai-code-verify--read-optional (prompt)
  "Read a string with PROMPT; return nil when the answer is blank."
  (let ((answer (string-trim (ai-code-read-string prompt ""))))
    (unless (string-empty-p answer) answer)))

;;;###autoload
(defun ai-code-verify-generate-deployment-document ()
  "Ask the AI to write and run an Org deployment verification document.
The document covers what unit tests cannot, such as real hosts, routing,
clients, and rollout state.  Prompt for the output directory (the file is
named verify.org) and, optionally, the target environment.  Cases come
from the AI coding session, the selected region or current Org task
section when available, and an optional list of documents (RET to skip)."
  (interactive)
  (let* ((context (ai-code-verify--requirement-context))
         (dir (read-directory-name
               "Directory for verify.org: "
               (if buffer-file-name
                   (file-name-directory buffer-file-name)
                 default-directory)))
         (file (expand-file-name "verify.org" dir))
         (environment (ai-code-verify--read-optional
                       "Target environment / hosts (optional, RET to skip): "))
         (documents (ai-code-verify--read-optional
                     "Source documents for test cases (optional, RET to skip): ")))
    (ai-code-with-prompt-policy ()
      (ai-code--insert-prompt
       (ai-code-verify--document-prompt file environment context documents)))))

(provide 'ai-code-verify)

;;; ai-code-verify.el ends here
