;;; ai-code-session.el --- AI session registry and dashboard -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This library tracks active AI coding sessions and provides a small query/update
;; API plus a simple dashboard for returning to active work.

;;; Code:

(require 'cl-lib)
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
  "Return DIRECTORY normalized as an absolute directory path."
  (when (and (stringp directory)
             (not (string-empty-p directory)))
    (file-name-as-directory (expand-file-name directory))))

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

(defun ai-code-session--default-metadata (session)
  "Return refreshed metadata plist for SESSION."
  (let* ((repo-root (ai-code-session-repo-root session))
         (metadata (ai-code-session-metadata session))
         (branch (ai-code-session--branch repo-root))
         (dirty-count (ai-code-session--dirty-count repo-root)))
    (list :branch (or branch (plist-get metadata :branch))
          :status (ai-code-session--status session)
          :dirty-count (or dirty-count (plist-get metadata :dirty-count) 0))))

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

(defconst ai-code-session-dashboard-buffer-name "*AI Code Sessions*"
  "Buffer name used by the AI session dashboard.")

(defconst ai-code-session-dashboard-shortcuts-hint
  "Keys: RET visit session   r/g refresh   k kill session   D magit status"
  "Shortcut hint shown in the dashboard mode line.")

(defun ai-code-session-dashboard--insert-footer ()
  "Insert dashboard help below the session list."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (insert (propertize ai-code-session-dashboard-shortcuts-hint
                        'face 'mode-line-inactive))
    (insert "\n")))

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

(defun ai-code-session-dashboard--entry (session)
  "Return the `tabulated-list-mode' entry for SESSION."
  (let* ((metadata (ai-code-session-metadata session))
         (branch (or (plist-get metadata :branch) ""))
         (status (or (plist-get metadata :status) ""))
         (dirty-count (number-to-string (or (plist-get metadata :dirty-count) 0))))
    (list (ai-code-session-id session)
          (vector
           (ai-code-session-id session)
           (or (ai-code-session-dashboard--repo-name session) "")
           (or (ai-code-session-dashboard--task-name session) "")
           (ai-code-session-dashboard--backend-label
            (ai-code-session-backend session))
           branch
           status
           dirty-count))))

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
  (tabulated-list-print t)
  (save-excursion
    (ai-code-session-dashboard--insert-footer)))

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
         ("Dirty files" 11 t)])
  (setq tabulated-list-padding 2)
  ;; Keep shortcut hints visible at the bottom of the dashboard window.
  (setq-local mode-line-format
              (list " " (propertize ai-code-session-dashboard-shortcuts-hint
                                    'face 'mode-line)))
  (add-hook 'tabulated-list-revert-hook #'ai-code-session-dashboard-refresh nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun ai-code-session-dashboard ()
  "Show the AI session dashboard."
  (interactive)
  (let ((buffer (get-buffer-create ai-code-session-dashboard-buffer-name)))
    (with-current-buffer buffer
      (ai-code-session-dashboard-mode)
      (ai-code-session-dashboard-refresh))
    (pop-to-buffer buffer)))

(provide 'ai-code-session)

;;; ai-code-session.el ends here
