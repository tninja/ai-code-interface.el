;;; ai-code-task-artifacts.el --- Task-linked code artifacts -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Link Org tasks to Git revisions, PRs and verification documents.
;; Git operations here are read-only; dirty fingerprints are not snapshots.

;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)
(require 'ai-code-utils)

(defun ai-code-task-artifacts--task-heading ()
  "Return a task heading or signal a user-facing error."
  (unless (derived-mode-p 'org-mode)
    (user-error "Task artifacts require an Org task heading"))
  (save-excursion
    (when (org-before-first-heading-p)
      (user-error "Move point to an Org task heading"))
    (org-back-to-heading t)
    (when (string= (org-get-heading t t t t) "AI Artifacts")
      (unless (org-up-heading-safe)
        (user-error "AI Artifacts has no parent task")))
    (point)))

(defun ai-code-task-artifacts--find-section ()
  "Find the direct AI Artifacts child of the current task, if present."
  (let ((start (ai-code-task-artifacts--task-heading)))
    (save-excursion
      (goto-char start)
      (let ((level (org-outline-level))
            (limit (save-excursion (org-end-of-subtree t t) (point))))
        (catch 'found
          (while (and (outline-next-heading) (< (point) limit))
            (when (and (= (org-outline-level) (1+ level))
                       (string= (org-get-heading t t t t) "AI Artifacts"))
              (throw 'found (point))))
          nil)))))

(defun ai-code-task-artifacts--ensure-section ()
  "Return or create the AI Artifacts child of this task."
  (or (ai-code-task-artifacts--find-section)
      (save-excursion
        (goto-char (ai-code-task-artifacts--task-heading))
        (let ((stars (make-string (1+ (org-outline-level)) ?*)))
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (let ((start (point)))
            (insert stars " AI Artifacts\n")
            start)))))

(defun ai-code-task-artifacts--record (properties entry)
  "Update the generated artifact child with PROPERTIES and log ENTRY."
  (let ((section (ai-code-task-artifacts--ensure-section)))
    (save-excursion
      (goto-char section)
      (dolist (property properties)
        (org-entry-put nil (car property) (cdr property)))
      (org-end-of-subtree t t)
      (unless (bolp) (insert "\n"))
      (insert "- " entry "\n"))))

(defun ai-code-task-artifacts-latest ()
  "Read latest recorded artifacts from the current Org task."
  (when (derived-mode-p 'org-mode)
    (when-let ((section (ai-code-task-artifacts--find-section)))
      (save-excursion
        (goto-char section)
        (list :head (org-entry-get nil "AI_CODE_HEAD")
              :branch (org-entry-get nil "AI_CODE_BRANCH")
              :root (org-entry-get nil "AI_CODE_ROOT")
              :change-hash (org-entry-get nil "AI_CODE_CHANGE_HASH")
              :tracked (equal (org-entry-get nil "AI_CODE_TRACKED") "yes")
              :untracked (equal (org-entry-get nil "AI_CODE_UNTRACKED") "yes")
              :pr (org-entry-get nil "AI_CODE_PR")
              :verification (org-entry-get nil "AI_CODE_VERIFICATION"))))))

(defun ai-code-task-artifacts--current-root ()
  "Use the task execution worktree instead of shared task storage."
  (or (ai-code--session-project-root)
      (ai-code--git-root)
      (user-error "No active Git worktree for this task")))

(defun ai-code-task-artifacts--git-output (root &rest args)
  "Run read-only Git ARGS in ROOT, returning exact command output."
  (let ((default-directory (file-name-as-directory root)))
    (with-temp-buffer
      (let ((exit (apply #'process-file "git" nil (current-buffer) nil args)))
        (unless (and (integerp exit) (zerop exit))
          (user-error "Git %s failed in %s: %s"
                      (car args) root (string-trim (buffer-string))))
        (buffer-string)))))

(defun ai-code-task-artifacts--git-state (root)
  "Return current HEAD and a status/tracked-diff fingerprint for ROOT.
Untracked content is never hashed, and a digest is not a restorable state."
  (let* ((head (string-trim
                (ai-code-task-artifacts--git-output root "rev-parse" "--verify" "HEAD")))
         (branch (string-trim
                  (ai-code-task-artifacts--git-output root "rev-parse" "--abbrev-ref" "HEAD")))
         (status (ai-code-task-artifacts--git-output
                  root "status" "--porcelain=v1" "--untracked-files=all"))
         (diff (ai-code-task-artifacts--git-output root "diff" "--binary" "HEAD" "--"))
         (untracked (seq-some (lambda (line) (string-prefix-p "?? " line))
                              (split-string status "\n" t))))
    (unless (string-match-p "^[[:xdigit:]]\\{40,64\\}$" head)
      (user-error "Git returned an invalid HEAD"))
    (list :head head :branch branch
          :change-hash (secure-hash 'sha256 (concat status "\0" diff))
          :tracked (not (string-empty-p diff))
          :untracked (and untracked t))))

;;;###autoload
(defun ai-code-task-artifacts-record-snapshot ()
  "Record read-only Git HEAD, branch and working-change identity."
  (interactive)
  (ai-code-task-artifacts--task-heading)
  (let* ((root (ai-code-task-artifacts--current-root))
         (state (ai-code-task-artifacts--git-state root))
         (head (plist-get state :head))
         (branch (plist-get state :branch))
         (hash (plist-get state :change-hash))
         (tracked (if (plist-get state :tracked) "yes" "no"))
         (untracked (if (plist-get state :untracked) "yes" "no")))
    (ai-code-task-artifacts--record
     (list (cons "AI_CODE_ROOT" (expand-file-name root))
           (cons "AI_CODE_HEAD" head)
           (cons "AI_CODE_BRANCH" branch)
           (cons "AI_CODE_CHANGE_HASH" hash)
           (cons "AI_CODE_TRACKED" tracked)
           (cons "AI_CODE_UNTRACKED" untracked))
     (format "Snapshot %s: branch =%s= HEAD =%s= tracked %s untracked %s fingerprint =%s= (untracked contents not captured)"
             (format-time-string "[%Y-%m-%d %H:%M]")
             branch head tracked untracked hash))
    (message "Recorded Git snapshot; no Git state was changed")))

;;;###autoload
(defun ai-code-task-artifacts-record-pr (url)
  "Attach an explicitly provided GitHub or Enterprise PR URL."
  (interactive (list (read-string "PR URL: ")))
  (ai-code-task-artifacts--task-heading)
  (unless (and (stringp url)
               (string-match-p
                "^https://[^/[:space:]]+/[^/[:space:]]+/[^/[:space:]]+/pull/[0-9]+/?\\(?:[?#][^[:space:]]*\\)?$"
                url))
    (user-error "Provide an HTTPS GitHub pull request URL"))
  (ai-code-task-artifacts--record
   (list (cons "AI_CODE_PR" url))
   (format "PR %s: [[%s][%s]] (verify its current head separately)"
           (format-time-string "[%Y-%m-%d %H:%M]") url url))
  (message "Recorded PR link; no remote operations were run"))

;;;###autoload
(defun ai-code-task-artifacts-record-verification (path)
  "Record verification document PATH relative to the current task file.
A reference is not a claim that tests passed."
  (interactive (list (read-file-name "Verification document: " nil nil t)))
  (ai-code-task-artifacts--task-heading)
  (unless (and (stringp path) (not (string-blank-p path)) buffer-file-name)
    (user-error "A saved Org task and verification document path are required"))
  (let ((relative (file-relative-name
                   (expand-file-name path)
                   (file-name-directory (expand-file-name buffer-file-name)))))
    (ai-code-task-artifacts--record
     (list (cons "AI_CODE_VERIFICATION" relative))
     (format "Verification %s: [[file:%s][%s]] (link only, not a passing result)"
             (format-time-string "[%Y-%m-%d %H:%M]")
             (org-link-escape relative) relative))
    (message "Recorded verification link; no tests were run")))

;;;###autoload
(defun ai-code-task-artifacts-record ()
  "Choose an artifact type to associate with the current Org task."
  (interactive)
  (pcase (completing-read "Record task artifact: "
                         '("Git snapshot" "Pull request" "Verification document")
                         nil t nil nil "Git snapshot")
    ("Git snapshot" (ai-code-task-artifacts-record-snapshot))
    ("Pull request" (call-interactively #'ai-code-task-artifacts-record-pr))
    ("Verification document"
     (call-interactively #'ai-code-task-artifacts-record-verification))))

(defun ai-code-task-artifacts-pr-scope ()
  "Return a strict review target for the task's recorded PR."
  (let ((url (plist-get (ai-code-task-artifacts-latest) :pr)))
    (unless url (user-error "This task has no recorded PR"))
    (concat "Recorded task PR: " url "\n"
            "Read the actual PR head and base, full diff, linked issues, "
            "discussion and CI. Verify the actual PR head, not a local "
            "branch or an old AI summary. Record the observed head SHA.")))

(defun ai-code-task-artifacts-snapshot-scope ()
  "Return a review target for the recorded commit, with drift warnings."
  (let* ((saved (ai-code-task-artifacts-latest))
         (head (plist-get saved :head))
         (root (plist-get saved :root))
         (branch (plist-get saved :branch))
         (hash (plist-get saved :change-hash))
         (dirty (or (plist-get saved :tracked) (plist-get saved :untracked)))
         (current-root (ignore-errors (ai-code-task-artifacts--current-root)))
         (same-root (and root current-root
                         (string= (expand-file-name root)
                                  (expand-file-name current-root))))
         (now (when same-root
                (condition-case nil
                    (ai-code-task-artifacts--git-state current-root)
                  (error nil)))))
    (unless (and head root)
      (user-error "This task has no recorded Git snapshot"))
    (concat
     (format "Recorded task Git snapshot: branch %s, HEAD %s, worktree %s.\n"
             branch head root)
     "Use the recorded HEAD as the immutable committed-code target. "
     "Never silently substitute the current HEAD or another worktree.\n"
     (when dirty
       (concat "WARNING: dirty work is not a reproducible snapshot. "
               "The fingerprint covers status paths and tracked diff, "
               "not untracked file contents; do not claim uncommitted changes "
               "were verified without obtaining the actual content.\n"))
     (cond
      ((not same-root) "Recorded worktree and current worktree differ; freshness unavailable.\n")
      ((not now) "Could not inspect current Git state; freshness unknown.\n")
      ((or (not (equal head (plist-get now :head)))
           (not (equal hash (plist-get now :change-hash))))
       "DRIFT: current HEAD or tracked changes differ from the recorded snapshot.\n")
      (t "HEAD and tracked-diff/status fingerprints match; untracked content is not proven.\n"))
     "Compare actual code and original human requirements, and report evidence gaps.")))

(provide 'ai-code-task-artifacts)
;;; ai-code-task-artifacts.el ends here
