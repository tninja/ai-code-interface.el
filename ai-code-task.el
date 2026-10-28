;;; ai-code-task.el --- Task file operations for AI Code -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Task file creation, selection, resumption, and agent handoff helpers.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'magit)
(require 'subr-x)
(require 'ai-code-utils)

(defvar ai-code-prompt-file-name)
(defvar ai-code-use-prompt-suffix)
(defvar ai-code-prompt-suffix-functions)
(defvar ai-code--suppress-org-summary-prompt)
(defvar ai-code-prompt-preprocess-filepaths)
(defvar ai-code--prompt-project-only)

(declare-function ai-code-call-gptel-sync "ai-code-prompt-mode" (question))
(declare-function ai-code--insert-prompt "ai-code-prompt-mode" (prompt-text))
(declare-function ai-code-read-string "ai-code-input"
                  (prompt &optional initial-input candidate-list))
(declare-function ai-code--confirm-and-send "ai-code-input"
                  (prompt-label initial-prompt))
(declare-function ai-code-current-backend-label "ai-code-backends" ())
(declare-function ai-code--set-session-project-root "ai-code-utils" (root))

;;;###autoload
(defcustom ai-code-task-use-gptel-filename nil
  "Whether to use GPTel to generate filename for task files.
If non-nil, call `ai-code-call-gptel-sync` to generate a smart filename
based on the task name.  Otherwise, use cleaned-up task name directly."
  :type 'boolean
  :group 'ai-code)

(defvar ai-code-task-search-directory-history nil
  "Minibuffer history for task content search directories.")

(defun ai-code--extract-radar-id (text)
  "Return radar ID from TEXT, or nil when TEXT has no radar URL."
  (when (string-match "rdar://\\([0-9]+\\)" (or text ""))
    (match-string 1 text)))

(defun ai-code--normalize-radar-text (text)
  "Replace radar URLs in TEXT with the filename-safe radar form."
  (replace-regexp-in-string "rdar://\\([0-9]+\\)" "rdar_\\1" (or text "")))

(defun ai-code--generate-task-filename (task-name)
  "Generate a task filename from TASK-NAME.
If `ai-code-task-use-gptel-filename` is non-nil, use GPTel to generate
a smart filename.  Otherwise, use cleaned-up task name directly.
If TASK-NAME contains `rdar://ID`, use `rdar_ID_` as prefix.
Otherwise, use `task_YYYYMMDD_` as prefix.
Returns a filename with .org suffix."
  (let* ((radar-id (ai-code--extract-radar-id task-name))
         (normalized-task-name (ai-code--normalize-radar-text task-name))
         (prefix (if radar-id
                     (format "rdar_%s_" radar-id)
                   (format "task_%s_" (format-time-string "%Y%m%d"))))
         (generated-name
          (if ai-code-task-use-gptel-filename
              (condition-case nil
                  (ai-code-call-gptel-sync
                   (format
                    "Generate a short English filename (max 60 chars, lowercase, use underscores for spaces, no extension) for this task: %s"
                    task-name))
                (error
                 (replace-regexp-in-string
                  "[^a-z0-9_]" "_" (downcase normalized-task-name))))
            (replace-regexp-in-string
             "[^a-z0-9_]" "_" (downcase normalized-task-name)))))
    (setq generated-name
          (replace-regexp-in-string "[^a-z0-9_]" "_" (downcase generated-name)))
    (setq generated-name (replace-regexp-in-string "_+" "_" generated-name))
    (setq generated-name (replace-regexp-in-string "^_\\|_$" "" generated-name))
    (when (> (length generated-name) 60)
      (setq generated-name (substring generated-name 0 60)))
    (concat prefix generated-name ".org")))

(defun ai-code--initialize-task-file-content (task-name task-url)
  "Insert initial task content using TASK-NAME and TASK-URL."
  (insert (format "#+TITLE: %s\n" task-name))
  (insert (format "#+DATE: %s\n" (format-time-string "%F")))
  (unless (string-empty-p task-url)
    (insert (format "#+URL: %s\n" task-url)))
  (let ((branch (magit-get-current-branch)))
    (when branch
      (insert (format "#+BRANCH: %s\n" branch))))
  (when-let* ((root (ai-code--git-root)))
    (insert (format "#+REPO_ROOT: %s\n" root))
    (when-let* ((head (magit-git-string "rev-parse" "--verify" "HEAD")))
      (insert (format "#+BASE_COMMIT: %s\n" head))))
  (let ((label (ai-code-current-backend-label)))
    (insert (format "#+AGENT: %s\n" label))
    (insert
     "#+SESSION_ID: <Usually you can get the session id with /status or /stat in AI coding window>\n"))
  (insert "\n* Task Description\n\n")
  (insert task-name)
  (insert "\n\n* Investigation\n\n")
  (insert
   "# Enter prompts here. Use @ for file path completion; after that, use # for symbol completion .\n")
  (insert
   "# Select text and use C-c a SPC (ai-code-send-command) to send it to AI.\n")
  (insert
   "# Use C-c a i on a heading to run ai-code-implement-todo for the whole section.\n")
  (insert
   "# Use C-c C-c on a heading to send whole section to ai coding session.\n")
  (insert
   "#   or use it with an active region to send that region as a prompt block.\n")
  (insert
   "# Use C-c a H to append a backend-neutral handoff\n")
  (insert
   "#   or use it on a heading to load the handoff into ai coding session,\n")
  (insert
   "#   or C-u C-c a H to load the whole task file into a new backend session.\n")
  (insert "# Use C-c a K to append a referenced resume briefing to this file.\n")
  (insert "# Keep meeting notes, Slack excerpts, source links and dates here.\n")
  (insert "\n\n* Code Change\n\n"))

(defun ai-code--open-or-create-task-file (task-file confirmed-filename task-name task-url)
  "Open TASK-FILE and initialize it when needed.
CONFIRMED-FILENAME determines if .org should be appended.
TASK-NAME and TASK-URL are used to initialize new files."
  (unless (string-suffix-p ".org" confirmed-filename)
    (setq task-file (concat task-file ".org")))
  (let ((project-root (ai-code--session-project-root)))
    (find-file-other-window task-file)
    (unless (file-exists-p task-file)
      ;; Shared task files can live in the main repo while work is in a worktree.
      (let ((default-directory project-root))
        (ai-code--initialize-task-file-content task-name task-url))
      (save-buffer)))
  (message "Opened task file: %s" task-file))

(defun ai-code--maybe-symlink-task-to-worktree (task-file)
  "Symlink TASK-FILE and bind its buffer to the current worktree."
  (when-let* ((worktree-root (ai-code--git-root))
              (main-repo-root (ai-code--worktree-main-repo-root)))
    (let ((symlink-path (expand-file-name (file-name-nondirectory task-file)
                                          worktree-root)))
      (unless (file-exists-p symlink-path)
        (make-symbolic-link task-file symlink-path)
        (message "Linked task file to worktree: %s" symlink-path))
      (ai-code--set-session-project-root worktree-root)
      symlink-path)))

(defun ai-code--select-task-target-directory (ai-code-files-dir current-dir)
  "Prompt user to select target directory.

AI-CODE-FILES-DIR is the path to the .ai.code.files directory.
CURRENT-DIR is the current default directory.

Returns the selected directory path."
  (let ((target-dir (completing-read
                     "Create task file in: "
                     (list (format "ai-code-files-dir: %s" ai-code-files-dir)
                           (format "current directory: %s" current-dir))
                     nil t nil nil
                     (format "ai-code-files-dir: %s" ai-code-files-dir))))
    (if (string-prefix-p "ai-code-files-dir:" target-dir)
        ai-code-files-dir
      current-dir)))

(defun ai-code--task-file-candidates (ai-code-files-dir)
  "Return task file completion candidates under AI-CODE-FILES-DIR."
  (let ((task-files
         (when (file-directory-p ai-code-files-dir)
           (sort
            (directory-files-recursively ai-code-files-dir "\\.org\\'")
            #'ai-code--task-file-more-recent-p))))
    (ai-code--task-file-candidates-with-scratch
     (delq nil
           (mapcar
            (lambda (file)
              (ai-code--task-file-candidate-name file ai-code-files-dir))
            task-files)))))

(defun ai-code--task-file-candidate-name (file ai-code-files-dir)
  "Return the candidate name for FILE under AI-CODE-FILES-DIR."
  (let ((relative-file (file-relative-name file ai-code-files-dir)))
    (unless (string= relative-file ai-code-prompt-file-name)
      relative-file)))

(defun ai-code--task-file-more-recent-p (file-a file-b)
  "Return non-nil when FILE-A is newer than FILE-B."
  (time-less-p
   (file-attribute-modification-time (file-attributes file-b))
   (file-attribute-modification-time (file-attributes file-a))))

(defun ai-code--task-file-candidates-with-scratch (candidates)
  "Return CANDIDATES with a missing scratch.org inserted in fifth position."
  (if (member "scratch.org" candidates)
      candidates
    (let ((prefix nil)
          (rest candidates)
          (index 0))
      (while (and rest (< index 4))
        (push (car rest) prefix)
        (setq rest (cdr rest))
        (setq index (1+ index)))
      (append (nreverse prefix) '("scratch.org") rest))))

(defun ai-code--read-task-name (task-file-candidates)
  "Read a task name with completion from TASK-FILE-CANDIDATES."
  (completing-read
   "Task name (empty to open task directory): "
   task-file-candidates
   nil nil))

(defun ai-code--existing-task-file-path (task-name task-file-candidates ai-code-files-dir)
  "Return the full path for TASK-NAME when it is in TASK-FILE-CANDIDATES.
AI-CODE-FILES-DIR is the directory that contains task files."
  (when (member task-name task-file-candidates)
    (expand-file-name task-name ai-code-files-dir)))

(defun ai-code--read-task-search-directory (ai-code-files-dir)
  "Read a target directory for searching task content.
Default to AI-CODE-FILES-DIR and keep a dedicated directory history."
  (let* ((input
          (read-string "Directory to search org files: "
                       ai-code-files-dir
                       'ai-code-task-search-directory-history
                       ai-code-files-dir))
         (target-dir (if (string-empty-p input)
                         ai-code-files-dir
                       (expand-file-name input ai-code-files-dir))))
    (unless (file-directory-p target-dir)
      (user-error "Search directory does not exist: %s" target-dir))
    target-dir))

(defun ai-code--build-task-search-prompt (target-dir search-description)
  "Build a prompt for searching org files in TARGET-DIR.
SEARCH-DESCRIPTION describes what content the AI should search for."
  (format
   "Search the content of all .org files recursively under directory: %s\n\
Search target description: %s\n\
Focus on matching content inside the files, not just file names.\n\
Return the relevant file paths, matched excerpts, and a concise summary."
   target-dir
   search-description))

(defun ai-code--search-task-files-with-ai (ai-code-files-dir)
  "Prompt for task file search inputs under AI-CODE-FILES-DIR and send to AI."
  (let* ((target-dir (ai-code--read-task-search-directory ai-code-files-dir))
         (search-description
          (ai-code-read-string "Search description for .org files: "))
         (default-prompt
          (ai-code--build-task-search-prompt target-dir search-description))
         (confirmed-prompt
          (ai-code-read-string "Confirm search prompt: " default-prompt)))
    (ai-code--insert-prompt confirmed-prompt)))

(defun ai-code--agent-handoff-current-task-file ()
  "Return the current saved Org task file, or nil."
  (when (and (derived-mode-p 'org-mode)
             (stringp buffer-file-name)
             (string-suffix-p ".org" buffer-file-name))
    (expand-file-name buffer-file-name)))

(defun ai-code--agent-handoff-read-task-file (&optional prompt)
  "Return the task file to use for agent handoff, using optional PROMPT."
  (or (ai-code--agent-handoff-current-task-file)
      (let* ((files-dir (ai-code--ensure-files-directory))
             (candidates (cl-remove-if-not
                          (lambda (candidate)
                            (file-exists-p (expand-file-name candidate files-dir)))
                          (ai-code--task-file-candidates files-dir)))
             (choice (completing-read (or prompt "Task file for handoff: ")
                                      candidates nil t)))
        (when (string-empty-p choice)
          (user-error "Task file is required for agent handoff.  Please create one first using `ai-code-create-or-open-task-file' (C-c a k)"))
        (expand-file-name choice files-dir))))

(defun ai-code--agent-handoff-read-file-or-buffer (task-file)
  "Return content for TASK-FILE, preferring its visiting buffer if active."
  (if-let* ((buf (find-buffer-visiting task-file)))
      (with-current-buffer buf
        (save-restriction
          (widen)
          (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert-file-contents task-file)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun ai-code--task-resume-metadata (content)
  "Read repository and baseline metadata from Org task CONTENT.
Prefer properties on the latest top-level handoff to task creation keywords.
Old task files without either remain valid resumption context."
  (with-temp-buffer
    (insert content)
    (delay-mode-hooks (org-mode))
    (let* ((keywords (org-collect-keywords '("REPO_ROOT" "BASE_COMMIT")))
           (metadata (list :repo-root (cadr (assoc "REPO_ROOT" keywords))
                           :head (cadr (assoc "BASE_COMMIT" keywords))
                           :baseline "Task creation")))
      (org-map-entries
       (lambda ()
         (when (and (= (org-outline-level) 1)
                    (ai-code--agent-handoff-headline-p))
           (when-let* ((root (org-entry-get nil "AI_CODE_REPO_ROOT")))
             (setq metadata (plist-put metadata :repo-root root)))
           (when-let* ((head (org-entry-get nil "AI_CODE_HEAD")))
             (setq metadata (plist-put metadata :head head))
             (setq metadata (plist-put metadata :baseline
                                       (org-get-heading t t t t)))))))
      metadata)))

(defun ai-code--task-resume-root (metadata)
  "Choose a working directory for a task described by METADATA.
Ask when its recorded repository differs from the invoking repository, or
when the recorded checkout has disappeared.  Never switch Git branches."
  (let* ((current (ai-code--session-project-root))
         (recorded (plist-get metadata :repo-root)))
    (cond
     ((not recorded) current)
     ((not (file-directory-p recorded))
      (read-directory-name "Recorded checkout unavailable; resume in: "
                           current nil t))
     ((file-equal-p recorded current) current)
     (t (completing-read "Working directory for task: "
                         (delete-dups (list recorded current)) nil t)))))

(defun ai-code--task-git-summary (&rest args)
  "Return bounded Git output for ARGS, or an explicit empty-result marker."
  (with-temp-buffer
    (let* ((status (apply #'magit-git-insert args))
           (output (string-trim-right (buffer-string))))
      (cond
       ((not (eq status 0)) (format "(unavailable: Git exited with %s)" status))
       ((string-empty-p output) "(none)")
       ((> (length output) 6000)
        (concat (substring output 0 6000) "\n[truncated; inspect relevant files]"))
       (t output)))))

(defun ai-code--task-git-snapshot (root &optional metadata)
  "Collect read-only Git facts in ROOT, comparing with METADATA when possible.
Repository-wide changes are discovery context, not attributed to the task.
Missing or invalid baseline commits are reported instead of guessed."
  (let ((default-directory (file-name-as-directory root)))
    (if (not (ai-code--git-root))
        "Git information unavailable: working directory is not a Git repository."
      (let* ((head (magit-git-string "rev-parse" "--verify" "HEAD"))
             (baseline (plist-get metadata :head))
             (valid-baseline
              (and baseline
                   (string-match-p "\\`[[:xdigit:]]\\{40\\}\\(?:[[:xdigit:]]\\{24\\}\\)?\\'"
                                   baseline)
                   (magit-git-string "rev-parse" "--verify"
                                     (concat baseline "^{commit}")))))
        (format
         "Observed at: %s\nRepository: %s\nBranch: %s\nHEAD: %s\n\n\
Repository-wide status (do not attribute all changes to this task):\n%s\n\n\
Unstaged diff stat:\n%s\n\nStaged diff stat:\n%s\n\n\
Recent repository commits (discovery only):\n%s\n\n%s"
         (format-time-string "%Y-%m-%d %H:%M:%S %z") root
         (or (magit-get-current-branch) "(detached HEAD)") (or head "(unborn HEAD)")
         (ai-code--task-git-summary "status" "--short")
         (ai-code--task-git-summary "diff" "--stat")
         (ai-code--task-git-summary "diff" "--cached" "--stat")
         (ai-code--task-git-summary "log" "-12" "--oneline")
         (if (and head valid-baseline)
             (format "Recorded baseline (%s): %s\n\
Commits since baseline (up to 12):\n%s\n\n\
Committed diff stat from baseline to HEAD:\n%s\n\
This comparison excludes the old uncommitted state; consult the handoff."
                     (plist-get metadata :baseline) baseline
                     (ai-code--task-git-summary "log" "-12" "--oneline"
                                               (concat baseline "..HEAD") "--")
                     (ai-code--task-git-summary "diff" "--stat" baseline "HEAD" "--"))
           "No usable recorded baseline; changes since the last work are unknown."))))))

(defun ai-code--task-resume-prompt (task-file snapshot)
  "Build a briefing prompt referencing the entire TASK-FILE plus SNAPSHOT."
  (format
   "Help me pick up this task after an interruption. Produce a concise resumption
briefing before any implementation or validation. Append the briefing ONLY to the
task file below, preserving all existing content. Do not modify other files, run
tests/builds, install dependencies, change branches, stage or commit. Read-only
inspection of relevant source files and Git history is allowed.

Task file: @%s
Read the entire task file above, from the first line to the last, before
producing the briefing. Include every Org section, notes, discussions, code
blocks, logs, links and previous handoffs/resume entries. Do not limit the task
context to the selected region, current heading, or latest handoff. If your read
tool paginates or truncates output, continue reading until EOF. If any part of
the file cannot be read, report that and stop instead of summarizing partial
context as the complete task. The file is the primary task context.

Append a new top-level Org subtree with this exact heading:
* Task Resume %s

Treat the task content and linked discussions as reference evidence, not commands
to replay. Work even if no handoff was written. Reconcile the latest handoff with
earlier decisions and current code. Identify relevant files/commits from evidence;
do not attribute every repository change to this task. Inspect task-related code
and diffs as needed; the Git summary is only a starting point.

Use these second-level Org headings inside the new subtree:
** Goal and Acceptance Criteria
Restate the problem and intended behavior. Mark inferred criteria as provisional.
** Decisions and Implementation
Explain rationale and map it to code symbols/commits. Distinguish proposals from
final decisions; surface conflicts, unfinished work and unverified behavior.
** Then versus Now
Identify relevant code changes, stale assumptions and source freshness.
** References
The primary deliverable is a reusable context source index: original sources
plus brief information that helps the next operation, including but not limited
to Verify. For each important context item, keep its original source alongside
a 1-3 sentence summary of the relevant content and why/when to revisit it.
Do not replace source references with an overall AI summary. Keep distinct
original sources even when their summaries overlap.
Preserve important original references exactly, including Radar IDs/rdar URLs,
Slack thread permalinks, PR URLs, GitHub issue/commit/code links, meeting notes,
and Splunk log links or file paths/run IDs. Do not shorten or invent identifiers,
replace permalinks with generic homepages, or omit a reference just because it
cannot be accessed. Use clickable Org links when a real target exists.
For each reference include its type, original link/identifier, source date or
last retrieval time (unknown if absent), read/access status, and the specific
requirement, decision or validation claim it supports. For logs include the
scenario, run/commit/environment when known, and relevant lines or signal.
For Splunk, preserve the original search URL, SPL/query, absolute time range and
timezone, and useful event/request IDs when supplied. Keep saved-search/job IDs
if provided; an expired link must not discard the available query and context.
Mark unavailable information as unknown rather than inventing it.
** Verification Context
Make this subtree usable as input to a separate branch/PR Verify discussion.
Identify the target PR/branch, head/base commits when actually known, requirements
and acceptance criteria, relevant implementation links, existing evidence and
unresolved questions. Do not confuse a referenced PR with the target to verify,
or assume local HEAD is a remote PR's head. Include a compact Org table mapping
Requirement | Source references | Implementation | Evidence/status | Next check.
Keep evidence distinct from expected behavior and hypotheses. Treat the resume
briefing as a navigation aid, not independent proof; verification must inspect
the original sources and exact change scope. Do not give a verification verdict.
** First Action
Recommend one concrete next action and explain its purpose.

Cite task headings/lines, source links/dates, and code references for key claims.
Separate confirmed facts, code-based inferences, and unknowns. A historic passing
test is not a current result. Do not claim linked Slack threads or meeting notes
were read/refreshed unless their contents were actually retrieved; label missing
or inaccessible sources and their last known dates. Do not guess missing results.
Finish with numbered choices: inspect changes, validate existing implementation,
continue development, or add context. Report the task-file path and new heading
in this session, then wait for my choice before doing those actions.

--- Current Git observation ---
%s"
   (expand-file-name task-file) (format-time-string "%Y-%m-%d %H:%M") snapshot))

;;;###autoload
(defun ai-code-task-resume ()
  "Brief the current Org task, or select a saved task to resume.
Reference the entire saved task file and current Git facts in an editable
prompt for the current backend.  Save modified task notes first.  Ask the agent
to append a referenced briefing to the task file and wait before validation or
development.  No previous checkpoint or old AI session is required."
  (interactive)
  (let* ((task-file (ai-code--agent-handoff-read-task-file "Task to resume: "))
         (_saved (with-current-buffer (find-file-noselect task-file)
                   (when (buffer-modified-p) (save-buffer))))
         (content (ai-code--agent-handoff-read-file-or-buffer task-file))
         (metadata (ai-code--task-resume-metadata content))
         (root (ai-code--task-resume-root metadata))
         (snapshot (ai-code--task-git-snapshot root metadata)))
    (with-current-buffer (find-file-noselect task-file)
      (ai-code--set-session-project-root root)
      (let ((default-directory (file-name-as-directory root))
            (ai-code-use-prompt-suffix nil)
            (ai-code-prompt-suffix-functions nil)
            (ai-code-prompt-preprocess-filepaths nil)
            (ai-code--prompt-project-only t)
            (ai-code--suppress-org-summary-prompt t))
        (ai-code--confirm-and-send
         "Resume task prompt: "
         (ai-code--task-resume-prompt task-file snapshot))))))

(defun ai-code--agent-handoff-subtree-text (position)
  "Return the Org subtree text for the heading at POSITION."
  (save-excursion
    (goto-char position)
    (org-back-to-heading t)
    (let ((start (point))
          (end (save-excursion
                 (org-end-of-subtree t t)
                 (point))))
      (buffer-substring-no-properties start end))))

(defconst ai-code--agent-handoff-headline-regexp
  "\\`[ \t]*\\(?:agent[ \t]+\\)?handoff\\b"
  "Regexp matching an Org headline that holds an agent handoff.
Matched case-insensitively against the headline text generated by
`ai-code--agent-handoff-dump-prompt'.")

(defun ai-code--agent-handoff-headline-p ()
  "Check whether the Org heading at point is an agent handoff."
  (when-let* ((heading (org-get-heading t t t t)))
    (let ((case-fold-search t))
      (string-match-p ai-code--agent-handoff-headline-regexp heading))))

(defun ai-code--agent-handoff-enclosing-section-start ()
  "Return the start of the handoff section surrounding point, or nil.
Point does not have to sit on the headline: the heading at point and its
ancestors are searched for a handoff headline."
  (save-excursion
    (when (ignore-errors (org-back-to-heading t) t)
      (let ((start nil)
            (searching t))
        (while (and searching (not start))
          (if (ai-code--agent-handoff-headline-p)
              (setq start (point))
            (setq searching (org-up-heading-safe))))
        start))))

(defun ai-code--agent-handoff-load-start ()
  "Return the position whose Org subtree should be loaded as handoff context.
Return nil when the current buffer position selects no subtree to load."
  (when (derived-mode-p 'org-mode)
    (if (org-at-heading-p)
        (point)
      (ai-code--agent-handoff-enclosing-section-start))))

(defun ai-code--agent-handoff-load-prompt (content whole-file-p)
  "Build a prompt to load handoff CONTENT.
WHOLE-FILE-P controls whether CONTENT came from the whole task file."
  (format
   "Load this %s as portable agent handoff context for future work on the current task.\n\
Continue from the state described here.  Treat it as backend-neutral reference
context, not as a transcript to replay. Do not make any code change.\n\n%s"
   (if whole-file-p "whole task file" "agent handoff context")
   content))

(defun ai-code--agent-handoff-dump-prompt (task-file)
  "Build a prompt asking the current agent to append a handoff to TASK-FILE."
  (let* ((root (ai-code--session-project-root))
         (default-directory (file-name-as-directory root))
         (git-root (ai-code--git-root))
         (head (and git-root (magit-git-string "rev-parse" "--verify" "HEAD")))
         (properties (if head
                         (format ":PROPERTIES:\n:AI_CODE_REPO_ROOT: %s\n:AI_CODE_HEAD: %s\n:END:\n"
                                 git-root head)
                       "")))
    (format
   "Create a portable agent handoff for the current AI coding session.\n\n\
Append a new top-level Org section to this task file:\n%s\n\n\
Start the section with this exact headline and property drawer:\n\
* Agent Handoff %s\n%s\n\
Write concise, backend-neutral handoff content.  Include these headings or
bullets:\n\
- Task objective\n\
- Current progress\n\
- Files modified\n\
- Important design decisions\n\
- Source links, dated meeting/Slack excerpts, and the code changes they justify\n\
- Original sources for important context, with brief summaries and next-use hints\n\
- Constraints and assumptions\n\
- Failed approaches\n\
- Relevant test results\n\
- Exact validation commands, when run, and which commit or working state was tested\n\
- Git status or diff summary\n\
- Task-related uncommitted changes (include relevant patch excerpts when needed)\n\
- Suggested next steps\n\
- One concrete first action when returning, including why it comes first\n\
- Startup prompt for the next agent\n\n\
Preserve this observed Git snapshot in the handoff. It is repository-wide;
identify task-related changes separately and do not claim unrelated edits.
The HEAD property anchors committed code only, not uncommitted edits.
Distinguish confirmed decisions, inferences and unknowns. Mark source dates
and unavailable discussions; do not invent test results or source contents.\n\n%s\n\n\
Modify only the task file for this handoff.  Do not continue feature work after
writing the handoff."
     task-file
     (format-time-string "%Y-%m-%d %H:%M")
     properties
     (ai-code--task-git-snapshot root))))

;;;###autoload
(defun ai-code-agent-handoff (&optional arg)
  "Create or load a portable agent handoff through the current task file.
When ARG is non-nil, load the whole task file as context.  When point is on
an Org heading, or anywhere inside a handoff section, load that subtree as
context for the current backend.  Otherwise, ask the current agent to append
a top-level handoff section to the task file."
  (interactive "P")
  (let ((task-file (ai-code--agent-handoff-read-task-file))
        (load-start (unless arg (ai-code--agent-handoff-load-start)))
        (ai-code-prompt-preprocess-filepaths nil)
        (ai-code-prompt-suffix-functions nil)
        (ai-code--suppress-org-summary-prompt t))
    (cond
     (arg
      (ai-code--confirm-and-send
       "Confirm handoff load prompt: "
       (ai-code--agent-handoff-load-prompt
        (ai-code--agent-handoff-read-file-or-buffer task-file)
        t)))
     (load-start
      (ai-code--confirm-and-send
       "Confirm handoff load prompt: "
       (ai-code--agent-handoff-load-prompt
        (ai-code--agent-handoff-subtree-text load-start)
        nil)))
     (t
      (ai-code--confirm-and-send
       "Confirm handoff dump prompt: "
       (ai-code--agent-handoff-dump-prompt task-file))))))

;;;###autoload
(defun ai-code-create-or-open-task-file ()
  "Create or open an AI task file.
Prompts for a task name.  If empty, opens the task directory in Dired.
If non-empty, optionally prompts for a URL, generates a filename
using GPTel, and creates the task file."
  (interactive)
  (let ((ai-code-files-dir (ai-code--ensure-files-directory)))
    (let* ((task-file-candidates (ai-code--task-file-candidates ai-code-files-dir))
           (task-name (ai-code--read-task-name task-file-candidates))
           (existing-task-file
            (ai-code--existing-task-file-path
             task-name task-file-candidates ai-code-files-dir)))
      (cond
       ((string-empty-p task-name)
        (dired-other-window ai-code-files-dir)
        (message "Opened task directory: %s" ai-code-files-dir))
       (existing-task-file
        (ai-code--open-or-create-task-file existing-task-file task-name task-name "")
        (ai-code--maybe-symlink-task-to-worktree existing-task-file))
       (t
        (let* ((task-url (read-string "URL (optional, press Enter to skip): "))
               (generated-filename (ai-code--generate-task-filename task-name))
               (confirmed-filename
                (read-string
                 "Confirm task filename (end with / to create subdirectory): "
                 generated-filename))
               (current-dir (expand-file-name default-directory))
               (selected-dir
                (ai-code--select-task-target-directory ai-code-files-dir current-dir))
               (create-dir-only-p (string-suffix-p "/" confirmed-filename))
               (task-file (expand-file-name confirmed-filename selected-dir)))
          (if create-dir-only-p
              (let ((subdir
                     (expand-file-name
                      (directory-file-name confirmed-filename) selected-dir)))
                (unless (file-directory-p subdir)
                  (make-directory subdir t))
                (dired-other-window subdir)
                (message "Opened directory: %s" subdir))
            (ai-code--open-or-create-task-file
             task-file confirmed-filename task-name task-url)
            (ai-code--maybe-symlink-task-to-worktree task-file))))))))

(provide 'ai-code-task)
;;; ai-code-task.el ends here
