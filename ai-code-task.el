;;; ai-code-task.el --- Task file operations for AI Code -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Task file creation, selection, and agent handoff helpers.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'magit)
(require 'subr-x)
(require 'ai-code-utils)

(defvar ai-code-prompt-file-name)

(declare-function ai-code-call-gptel-sync "ai-code-prompt-mode" (question))
(declare-function ai-code--insert-prompt "ai-code-prompt-mode" (prompt-text))
(declare-function ai-code-read-string "ai-code-input"
                  (prompt &optional initial-input candidate-list))
(declare-function ai-code--confirm-and-send "ai-code-input"
                  (prompt-label initial-prompt))
(declare-function ai-code-current-backend-label "ai-code-backends" ())

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
  (let ((label (ai-code-current-backend-label)))
    (insert (format "#+AGENT: %s\n" label))
    (insert
     "#+SESSION_ID: <Usually you can get the session id with /status or /stat in AI coding window>\n"))
  (insert "\n* Task Description\n\n")
  (insert task-name)
  (insert "\n\n* Investigation\n\n")
  (insert
   "# Enter your prompts here. After that,\n# Select them and use C-c a SPC (ai-code-send-command) to send to AI\n")
  (insert
   "#   Or You use C-c C-c (ai-code-prompt-send-block) to send the whole prompt block to AI\n\n")
  (insert "# Use C-c a n (ai-code-take-notes) to copy notes back from AI session\n\n")
  (insert "\n\n* Code Change\n\n"))

(defun ai-code--open-or-create-task-file (task-file confirmed-filename task-name task-url)
  "Open TASK-FILE and initialize it when needed.
CONFIRMED-FILENAME determines if .org should be appended.
TASK-NAME and TASK-URL are used to initialize new files."
  (unless (string-suffix-p ".org" confirmed-filename)
    (setq task-file (concat task-file ".org")))
  (find-file-other-window task-file)
  (unless (file-exists-p task-file)
    (ai-code--initialize-task-file-content task-name task-url)
    (save-buffer))
  (message "Opened task file: %s" task-file))

(defun ai-code--maybe-symlink-task-to-worktree (task-file)
  "Symlink TASK-FILE into the worktree root when inside a git worktree."
  (when-let* ((worktree-root (ai-code--git-root))
              (main-repo-root (ai-code--worktree-main-repo-root)))
    (let ((symlink-path (expand-file-name (file-name-nondirectory task-file)
                                          worktree-root)))
      (unless (file-exists-p symlink-path)
        (make-symbolic-link task-file symlink-path)
        (message "Linked task file to worktree: %s" symlink-path)))))

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

(defun ai-code--agent-handoff-read-task-file ()
  "Return the task file to use for agent handoff."
  (or (ai-code--agent-handoff-current-task-file)
      (let* ((files-dir (ai-code--ensure-files-directory))
             (candidates (cl-remove-if-not
                          (lambda (candidate)
                            (file-exists-p (expand-file-name candidate files-dir)))
                          (ai-code--task-file-candidates files-dir)))
             (choice (completing-read "Task file for handoff: "
                                      candidates nil t)))
        (when (string-empty-p choice)
          (user-error "Task file is required for agent handoff.  Please create one first using `ai-code-create-or-open-task-file' (C-c a K)"))
        (expand-file-name choice files-dir))))

(defun ai-code--agent-handoff-read-file-or-buffer (task-file)
  "Return content for TASK-FILE, preferring its visiting buffer if active."
  (if-let* ((buf (find-buffer-visiting task-file)))
      (with-current-buffer buf
        (buffer-substring-no-properties (point-min) (point-max)))
    (with-temp-buffer
      (insert-file-contents task-file)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun ai-code--agent-handoff-current-subtree-text ()
  "Return the current Org heading subtree text."
  (save-excursion
    (org-back-to-heading t)
    (let ((start (point))
          (end (save-excursion
                 (org-end-of-subtree t t)
                 (point))))
      (buffer-substring-no-properties start end))))

(defun ai-code--agent-handoff-load-prompt (content whole-file-p)
  "Build a prompt to load handoff CONTENT.
WHOLE-FILE-P controls whether CONTENT came from the whole task file."
  (format
   "Use this %s as portable agent handoff context for the current task.\n\
Continue from the state described here.  Treat it as backend-neutral context,
not as a transcript to replay.  Before making changes, restate the next action
you plan to take.\n\n%s"
   (if whole-file-p "whole task file" "agent handoff context")
   content))

(defun ai-code--agent-handoff-dump-prompt (task-file)
  "Build a prompt asking the current agent to append a handoff to TASK-FILE."
  (format
   "Create a portable agent handoff for the current AI coding session.\n\n\
Append a new top-level Org section to this task file:\n%s\n\n\
The section headline must be:\n* Agent Handoff %s\n\n\
Write concise, backend-neutral handoff content.  Include these headings or
bullets:\n\
- Task objective\n\
- Current progress\n\
- Files modified\n\
- Important design decisions\n\
- Constraints and assumptions\n\
- Failed approaches\n\
- Relevant test results\n\
- Git status or diff summary\n\
- Suggested next steps\n\
- Startup prompt for the next agent\n\n\
Modify only the task file for this handoff.  Do not continue feature work after
writing the handoff."
   task-file
   (format-time-string "%Y-%m-%d %H:%M")))

;;;###autoload
(defun ai-code-agent-handoff (&optional arg)
  "Create or load a portable agent handoff through the current task file.
When point is on an Org heading, load that subtree as context for the current
backend.  When ARG is non-nil, load the whole task file as context.  Otherwise,
ask the current agent to append a top-level handoff section to the task file."
  (interactive "P")
  (let ((task-file (ai-code--agent-handoff-read-task-file)))
    (cond
     (arg
      (ai-code--confirm-and-send
       "Confirm handoff load prompt: "
       (ai-code--agent-handoff-load-prompt
        (ai-code--agent-handoff-read-file-or-buffer task-file)
        t)))
     ((and (derived-mode-p 'org-mode)
           (org-at-heading-p))
      (ai-code--confirm-and-send
       "Confirm handoff load prompt: "
       (ai-code--agent-handoff-load-prompt
        (ai-code--agent-handoff-current-subtree-text)
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
