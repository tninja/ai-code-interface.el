;;; ai-code-session.el --- AI session registry and dashboard -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This library tracks active AI coding sessions and provides a small query/update
;; API plus a simple dashboard for returning to active work.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(declare-function magit-get-current-branch "magit-git" ())
(declare-function magit-git-lines "magit-git" (&rest args))
(declare-function magit-status-setup-buffer "magit-status" (directory))

(cl-defstruct ai-code-session
  id
  buffer
  backend
  repo-root
  task-file
  created-at
  updated-at
  metadata)

(defvar ai-code-session--sessions (make-hash-table :test 'equal)
  "Hash table mapping AI session ids to `ai-code-session' objects.")

(defvar ai-code-session--next-id 0
  "Counter used to generate lightweight AI session ids.")

(defun ai-code-session--generate-id ()
  "Return a new AI session id."
  (format "S%s" (cl-incf ai-code-session--next-id)))

(defun ai-code-session--normalize-backend (backend)
  "Return BACKEND normalized for storage."
  (cond
   ((symbolp backend) (symbol-name backend))
   ((stringp backend) backend)
   (t nil)))

(defun ai-code-session--normalize-directory (directory)
  "Return DIRECTORY as a canonical absolute directory path."
  (when (and (stringp directory)
             (not (string-empty-p directory)))
    (file-name-as-directory
     (file-truename (expand-file-name directory)))))

(defun ai-code-session--normalize-file (file)
  "Return FILE normalized as an absolute file path."
  (when (and (stringp file)
             (not (string-empty-p file)))
    (expand-file-name file)))

(defun ai-code-session--find-by-buffer (buffer)
  "Return the session object associated with BUFFER, or nil."
  (when (buffer-live-p buffer)
    (cl-find-if (lambda (session)
                  (eq (ai-code-session-buffer session) buffer))
                (hash-table-values ai-code-session--sessions))))

(defun ai-code-session--merge-plists (base plist)
  "Return BASE with the plist values from PLIST applied."
  (let ((result (copy-sequence (or base '()))))
    (while plist
      (setq result (plist-put result (pop plist) (pop plist))))
    result))

(cl-defun ai-code-session-register (&key buffer backend repo-root task-file metadata id)
  "Register or update an AI session and return it.
BUFFER is the session buffer.  BACKEND, REPO-ROOT, TASK-FILE, and METADATA
describe the session.  ID is optional and mainly useful when restoring state."
  (unless (buffer-live-p buffer)
    (user-error "Cannot register session without a live buffer"))
  (let* ((now (current-time))
         (session (or (and id (gethash id ai-code-session--sessions))
                      (ai-code-session--find-by-buffer buffer))))
    (if session
        (progn
          (setf (ai-code-session-buffer session) buffer
                (ai-code-session-updated-at session) now)
          (when backend
            (setf (ai-code-session-backend session)
                  (ai-code-session--normalize-backend backend)))
          (when repo-root
            (setf (ai-code-session-repo-root session)
                  (ai-code-session--normalize-directory repo-root)))
          (when task-file
            (setf (ai-code-session-task-file session)
                  (ai-code-session--normalize-file task-file)))
          (when metadata
            (setf (ai-code-session-metadata session)
                  (ai-code-session--merge-plists
                   (ai-code-session-metadata session)
                   metadata))))
      (setq session
            (make-ai-code-session
             :id (or id (ai-code-session--generate-id))
             :buffer buffer
             :backend (ai-code-session--normalize-backend backend)
             :repo-root (ai-code-session--normalize-directory repo-root)
             :task-file (ai-code-session--normalize-file task-file)
             :created-at now
             :updated-at now
             :metadata (copy-sequence metadata)))
      (puthash (ai-code-session-id session) session ai-code-session--sessions))
    session))

(defun ai-code-session-unregister (id-or-buffer)
  "Remove the session identified by ID-OR-BUFFER from the registry."
  (when-let ((session (ai-code-session-get id-or-buffer)))
    (remhash (ai-code-session-id session) ai-code-session--sessions)))

(defun ai-code-session-get (id-or-buffer)
  "Return the registered session identified by ID-OR-BUFFER."
  (cond
   ((bufferp id-or-buffer)
    (ai-code-session--find-by-buffer id-or-buffer))
   ((and (stringp id-or-buffer)
         (not (string-empty-p id-or-buffer)))
    (gethash id-or-buffer ai-code-session--sessions))
   (t nil)))

(defun ai-code-session-list ()
  "Return the current registered AI sessions ordered by recent activity."
  (sort (copy-sequence (hash-table-values ai-code-session--sessions))
        (lambda (left right)
          (time-less-p (ai-code-session-updated-at right)
                       (ai-code-session-updated-at left)))))

(defun ai-code-session-update-metadata (id-or-buffer metadata)
  "Merge METADATA into the session identified by ID-OR-BUFFER."
  (when-let ((session (ai-code-session-get id-or-buffer)))
    (setf (ai-code-session-metadata session)
          (ai-code-session--merge-plists
           (ai-code-session-metadata session)
           metadata)
          (ai-code-session-updated-at session) (current-time))
    session))

(defun ai-code-session--status (session)
  "Return a simple status string for SESSION."
  (let ((buffer (ai-code-session-buffer session)))
    (if (and (buffer-live-p buffer)
             (when-let ((process (get-buffer-process buffer)))
               (process-live-p process)))
        "running"
      "stopped")))

(defun ai-code-session--branch (repo-root)
  "Return the current branch for REPO-ROOT, or nil."
  (when (and repo-root (file-directory-p repo-root))
    (let ((default-directory repo-root))
      (ignore-errors
        (magit-get-current-branch)))))

(defun ai-code-session--dirty-count (repo-root)
  "Return the dirty file count for REPO-ROOT, or nil."
  (when (and repo-root (file-directory-p repo-root))
    (let ((default-directory repo-root))
      (ignore-errors
        (length
         (magit-git-lines "status" "--porcelain" "--untracked-files=normal"))))))

(defun ai-code-session--parse-shortstat (line)
  "Return (INSERTIONS . DELETIONS) from a `git diff --shortstat' LINE, or nil."
  (when (and (stringp line) (string-match-p "changed" line))
    (cl-flet ((count (regexp)
                (if (string-match regexp line)
                    (string-to-number (match-string 1 line))
                  0)))
      (cons (count "\\([0-9]+\\) insertion")
            (count "\\([0-9]+\\) deletion")))))

(defun ai-code-session--diff-stat (repo-root)
  "Return (INSERTIONS . DELETIONS) of uncommitted changes in REPO-ROOT, or nil."
  (when (and repo-root (file-directory-p repo-root))
    (let ((default-directory repo-root))
      (ignore-errors
        (ai-code-session--parse-shortstat
         (car (magit-git-lines "diff" "--shortstat" "HEAD")))))))

(defun ai-code-session--default-metadata (session)
  "Return refreshed metadata plist for SESSION."
  (let* ((repo-root (ai-code-session-repo-root session))
         (metadata (ai-code-session-metadata session))
         (branch (ai-code-session--branch repo-root))
         (dirty-count (ai-code-session--dirty-count repo-root))
         (diff-stat (ai-code-session--diff-stat repo-root)))
    (list :branch (or branch (plist-get metadata :branch))
          :status (ai-code-session--status session)
          :dirty-count (or dirty-count (plist-get metadata :dirty-count) 0)
          :insertions (car diff-stat)
          :deletions (cdr diff-stat))))

(defun ai-code-session-refresh ()
  "Refresh session state and return the live session list."
  (dolist (session (copy-sequence (hash-table-values ai-code-session--sessions)))
    (let ((buffer (ai-code-session-buffer session)))
      (if (not (buffer-live-p buffer))
          (ai-code-session-unregister (ai-code-session-id session))
        (ai-code-session-update-metadata
         (ai-code-session-id session)
         (ai-code-session--default-metadata session)))))
  (ai-code-session-list))

;;; Prompts

(defconst ai-code-session--prompt-max-chars 2000
  "Most characters of a sent prompt kept in session metadata.")

(defun ai-code-session-note-prompt (buffer prompt)
  "Remember PROMPT as the latest prompt sent to the session in BUFFER."
  (when (stringp prompt)
    (ai-code-session-update-metadata
     buffer
     (list :last-prompt (substring prompt 0 (min (length prompt)
                                                  ai-code-session--prompt-max-chars))
           :last-prompt-at (float-time)))))

(defun ai-code-session--label-line-p (line)
  "Return non-nil when LINE is only a section label such as \"Scope:\"."
  (string-match-p "\\`[A-Z][A-Za-z ]*:\\'" line))

(defun ai-code-session--line-after (lines index)
  "Return the first non-empty line in LINES after INDEX, or nil."
  (seq-find (lambda (line) (not (string-empty-p line))) (nthcdr (1+ index) lines)))

(defun ai-code-session--prompt-goal (lines)
  "Return the first line of the Goal section in LINES, or nil."
  (when-let* ((index (seq-position lines "Goal:" (lambda (line _) (string-prefix-p "Goal:" line))))
              (rest (string-trim (substring (nth index lines) (length "Goal:")))))
    (if (string-empty-p rest)
        (ai-code-session--line-after lines index)
      rest)))

(defun ai-code-session--prompt-subject (lines)
  "Return the headline or comment a Scope section in LINES points at, or nil.
A subject follows a location line ending in a colon, such as
\"Org headline on line 2 (notes.org):\"."
  (when-let* ((index (seq-position lines "Scope:"))
              (location (ai-code-session--line-after lines index))
              ((string-suffix-p ":" location))
              ((not (ai-code-session--label-line-p location)))
              (subject (ai-code-session--line-after
                        lines (seq-position lines location))))
    (replace-regexp-in-string "\\`\\(?:\\*+\\|-\\)[ \t]+" "" subject)))

(defun ai-code-session--prompt-summary (prompt)
  "Return a one-line summary of PROMPT.
Structured prompts use their Goal line, preceded by the headline or
comment their Scope points at; other prompts use their first line."
  (let* ((lines (mapcar #'string-trim (split-string prompt "\n")))
         (main (or (ai-code-session--prompt-goal lines)
                   (seq-find (lambda (line)
                               (not (or (string-empty-p line)
                                        (ai-code-session--label-line-p line))))
                             lines)
                   ""))
         (subject (ai-code-session--prompt-subject lines)))
    (replace-regexp-in-string
     "[ \t]+" " " (string-join (delq nil (list subject main)) " — "))))

(defconst ai-code-session-dashboard-buffer-name "*AI Code Sessions*"
  "Buffer name used by the AI session dashboard.")

(defconst ai-code-session-dashboard-shortcuts-hint
  "Keys: RET visit session   r/g refresh   k kill session   D magit status"
  "Shortcut hint shown in the dashboard mode line.")

(defun ai-code-session-dashboard--repo-name (session)
  "Return a short repository name for SESSION."
  (when-let ((repo-root (ai-code-session-repo-root session)))
    (file-name-nondirectory (directory-file-name repo-root))))

(defun ai-code-session-dashboard--task-name (session)
  "Return a display task file name for SESSION."
  (when-let ((task-file (ai-code-session-task-file session)))
    (file-name-nondirectory task-file)))

(defun ai-code-session-dashboard--backend-label (backend)
  "Return a human-friendly label for BACKEND."
  (let ((text (cond
               ((symbolp backend) (symbol-name backend))
               ((stringp backend) backend)
               (t ""))))
    (capitalize (replace-regexp-in-string "[-_]+" " " text))))

(defconst ai-code-session-dashboard--attention-labels
  '((waiting . "needs input")
    (done . "done unseen"))
  "Status labels for sessions that need the user's attention.")

(defun ai-code-session-dashboard--status-label (metadata)
  "Return the Status column text for session METADATA.
It uses the same states as the tab-bar badge."
  (let ((status (plist-get metadata :status)))
    (cond
     ((equal status "stopped") "stopped")
     ((alist-get (plist-get metadata :attention)
                 ai-code-session-dashboard--attention-labels))
     ((plist-get metadata :working) "working")
     (status "idle")
     (t ""))))

(defun ai-code-session--format-age (seconds)
  "Return SECONDS as a short age such as \"45s\", \"3m\", \"2h\" or \"1d\"."
  (let ((seconds (max 0 (floor seconds))))
    (cond ((< seconds 60) (format "%ds" seconds))
          ((< seconds 3600) (format "%dm" (/ seconds 60)))
          ((< seconds 86400) (format "%dh" (/ seconds 3600)))
          (t (format "%dd" (/ seconds 86400))))))

(defun ai-code-session-dashboard--since (metadata &optional now)
  "Return how long the session in METADATA has been in its state, at NOW."
  (let ((time (cond ((equal (plist-get metadata :status) "stopped") nil)
                    ((plist-get metadata :attention)
                     (plist-get metadata :attention-since))
                    ((plist-get metadata :working)
                     (plist-get metadata :working-since))
                    (t (plist-get metadata :quiet-since)))))
    (if time
        (ai-code-session--format-age (- (or now (float-time)) time))
      "")))

(defun ai-code-session-dashboard--changes (metadata)
  "Return the uncommitted change size recorded in METADATA."
  (let ((files (or (plist-get metadata :dirty-count) 0))
        (insertions (or (plist-get metadata :insertions) 0))
        (deletions (or (plist-get metadata :deletions) 0)))
    (cond
     ((zerop files) "clean")
     ((and (zerop insertions) (zerop deletions))
      (format "%d file%s" files (if (= files 1) "" "s")))
     (t (format "%d file%s +%d −%d" files (if (= files 1) "" "s")
                insertions deletions)))))

(defconst ai-code-session-dashboard--prompt-width 60
  "Width of the last prompt summary in the dashboard.")

(defconst ai-code-session-dashboard--prompt-preview-lines 20
  "Most prompt lines shown in the dashboard tooltip.")

(defun ai-code-session-dashboard--prompt-preview (prompt)
  "Return the start of PROMPT for a tooltip, one short line per line."
  (let* ((lines (split-string (string-trim prompt) "\n"))
         (shown (seq-take lines ai-code-session-dashboard--prompt-preview-lines)))
    (concat (mapconcat (lambda (line) (truncate-string-to-width line 120 nil nil "…"))
                       shown "\n")
            (if (> (length lines) (length shown)) "\n…" ""))))

(defun ai-code-session-dashboard--prompt-cell (prompt)
  "Return the dashboard cell for the last PROMPT, with the prompt as tooltip."
  (if (or (not (stringp prompt)) (string-blank-p prompt))
      ""
    (propertize (truncate-string-to-width
                 (ai-code-session--prompt-summary prompt)
                 ai-code-session-dashboard--prompt-width nil nil "…")
                'help-echo (ai-code-session-dashboard--prompt-preview prompt))))

(defun ai-code-session-dashboard--entry (session)
  "Return the `tabulated-list-mode' entry for SESSION."
  (let* ((metadata (ai-code-session-metadata session))
         (branch (or (plist-get metadata :branch) ""))
         (status (ai-code-session-dashboard--status-label metadata)))
    (list (ai-code-session-id session)
          (vector
           (ai-code-session-id session)
           (or (ai-code-session-dashboard--repo-name session) "")
           (or (ai-code-session-dashboard--task-name session) "")
           (ai-code-session-dashboard--backend-label
            (ai-code-session-backend session))
           branch
           status
           (ai-code-session-dashboard--since metadata)
           (ai-code-session-dashboard--changes metadata)
           (ai-code-session-dashboard--prompt-cell
            (plist-get metadata :last-prompt))))))

(defun ai-code-session-dashboard--entries ()
  "Return dashboard entries for all active sessions."
  (mapcar #'ai-code-session-dashboard--entry
          (ai-code-session-refresh)))

(defun ai-code-session-dashboard--session-at-point ()
  "Return the dashboard session at point."
  (ai-code-session-get (tabulated-list-get-id)))

(defun ai-code-session-dashboard-refresh ()
  "Refresh the AI session dashboard."
  (interactive)
  (setq tabulated-list-entries (ai-code-session-dashboard--entries))
  (tabulated-list-print t))

(defun ai-code-session-dashboard-visit ()
  "Visit the session buffer on the current line."
  (interactive)
  (if-let* ((session (ai-code-session-dashboard--session-at-point))
            (buffer (ai-code-session-buffer session))
            ((buffer-live-p buffer)))
      (pop-to-buffer buffer)
    (user-error "No live AI session on this line")))

(defun ai-code-session-dashboard-kill-session ()
  "Kill the session on the current line, after confirmation when needed."
  (interactive)
  (let* ((session (or (ai-code-session-dashboard--session-at-point)
                      (user-error "No AI session on this line")))
         (buffer (ai-code-session-buffer session))
         (process (and (buffer-live-p buffer) (get-buffer-process buffer))))
    (when (and process
               (process-live-p process)
               (not (y-or-n-p (format "Kill AI session %s? "
                                      (ai-code-session-id session)))))
      (user-error "Session kill canceled"))
    (when (and process (process-live-p process))
      (delete-process process))
    (ai-code-session-unregister (ai-code-session-id session))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    (ai-code-session-dashboard-refresh)))

(defun ai-code-session-dashboard-open-diff ()
  "Open Magit status for the repository on the current line."
  (interactive)
  (if-let* ((session (ai-code-session-dashboard--session-at-point))
            (repo-root (ai-code-session-repo-root session)))
      (magit-status-setup-buffer repo-root)
    (user-error "No repository is associated with this session")))

(defvar ai-code-session-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'ai-code-session-dashboard-visit)
    (define-key map (kbd "r") #'ai-code-session-dashboard-refresh)
    (define-key map (kbd "g") #'ai-code-session-dashboard-refresh)
    (define-key map (kbd "k") #'ai-code-session-dashboard-kill-session)
    (define-key map (kbd "D") #'ai-code-session-dashboard-open-diff)
    map)
  "Keymap used by `ai-code-session-dashboard-mode'.")

(define-derived-mode ai-code-session-dashboard-mode tabulated-list-mode "AI Sessions"
  "Major mode for the AI session dashboard."
  (setq tabulated-list-format
        [("Session" 10 t)
         ("Repo" 18 t)
         ("Task file" 24 t)
         ("Backend" 14 t)
         ("Branch" 20 t)
         ("Status" 12 t)
         ("Since" 6 t)
         ("Changes" 18 t)
         ("Last prompt" 60 nil)])
  (setq tabulated-list-padding 2)
  ;; Keep shortcut hints visible at the bottom of the dashboard window.
  (setq-local mode-line-format
              (list " " (propertize ai-code-session-dashboard-shortcuts-hint
                                    'face 'mode-line)))
  (add-hook 'tabulated-list-revert-hook #'ai-code-session-dashboard-refresh nil t)
  (tabulated-list-init-header))

(defconst ai-code-session-dashboard--display-action
  '((display-buffer-reuse-window display-buffer-in-side-window)
    (side . bottom)
    (slot . 0)
    (window-height . fit-window-to-buffer)
    (preserve-size . (nil . t)))
  "Show the dashboard in a window across the bottom of the frame.")

;;;###autoload
(defun ai-code-session-dashboard ()
  "Toggle the AI session dashboard at the bottom of the frame."
  (interactive)
  (let ((buffer (get-buffer-create ai-code-session-dashboard-buffer-name)))
    (if-let* ((window (get-buffer-window buffer)))
        (quit-window nil window)
      (with-current-buffer buffer
        (ai-code-session-dashboard-mode)
        (ai-code-session-dashboard-refresh))
      (pop-to-buffer buffer ai-code-session-dashboard--display-action))))

(provide 'ai-code-session)

;;; ai-code-session.el ends here
