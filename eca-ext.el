;;; eca-ext.el --- ECA session multiplexing and context extensions -*- lexical-binding: t; -*-

;; Author: davidwuchn
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; This file extends ECA functionality without modifying upstream package files.
;; It provides session multiplexing, workspace management, shared context, and
;; programmatic context helpers that can be used by ai-code's ECA integration.
;;
;; Session Multiplexing:
;;   (eca-list-sessions)                -- List all active sessions
;;   (eca-select-session)               -- Interactively select a session
;;   (eca-switch-to-session)            -- Switch to session and open chat buffer
;;   (eca-create-session-for-workspace) -- Create new session for workspace
;;
;; Workspace Management:
;;   (eca-list-workspace-folders)       -- List folders in current session
;;   (eca-add-workspace-folder)         -- Add folder to session
;;   (eca-remove-workspace-folder)      -- Remove folder from session
;;   (eca-workspace-folder-for-file)    -- Find which workspace owns a file
;;
;; Context Management:
;;   (eca-chat-add-file-context session file-path)
;;   (eca-chat-add-repo-map-context session)
;;   (eca-chat-add-cursor-context session file-path position)
;;   (eca-chat-add-clipboard-context session content)
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'eca nil t)
(require 'eca-util nil t)
(require 'eca-chat nil t)

(defvar eca--sessions nil)
(defvar eca--session-id-cache nil)
(defvar eca-config-directory nil)

(declare-function eca-session "eca-util" ())
(declare-function eca-get "eca-util" (alist key))
(declare-function eca-info "eca-util" (format-string &rest args))
(declare-function eca-create-session "eca-util" (workspace-folders))
(declare-function eca-delete-session "eca-util" (session))
(declare-function eca-assert-session-running "eca-util" (session))
(declare-function eca--session-id "eca-util" (session))
(declare-function eca--session-status "eca-util" (session))
(declare-function eca--session-workspace-folders "eca-util" (session))
(declare-function (setf eca--session-workspace-folders) "eca-util" (value session))
(declare-function eca--session-add-workspace-folder "eca-util" (session folder))
(declare-function eca--session-chats "eca-util" (session))
(declare-function eca-chat-open "eca-chat" (session))
(declare-function eca-chat--get-last-buffer "eca-chat" (session))
(declare-function eca-chat--add-context "eca-chat" (context-plist))
(declare-function eca-chat--with-current-buffer "eca-chat" (&rest body))
(declare-function eca-api-notify "eca-api" (session &rest args))
(declare-function projectile-project-root "projectile" (&optional dir))
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-root "project" (project))

(defun eca-ext--normalize-folder-path (path)
  "Return PATH as an expanded directory path without a trailing slash."
  (directory-file-name (expand-file-name path)))

;;; Session Multiplexing

(defun eca-list-sessions ()
  "Return a list of all active ECA sessions.
Each element is a plist with :id, :status, :workspace-folders, and :chat-count.
Return nil if ECA has no active sessions."
  (and (boundp 'eca--sessions)
       eca--sessions
       (mapcar (lambda (pair)
                 (let ((session (cdr pair)))
                   (list :id (eca--session-id session)
                         :status (eca--session-status session)
                         :workspace-folders (eca--session-workspace-folders session)
                         :chat-count (length (eca--session-chats session)))))
               eca--sessions)))

(defun eca-select-session (&optional session-id)
  "Select an ECA session by SESSION-ID or interactively.
Return the selected session or nil if canceled."
  (interactive)
  (let* ((sessions (eca-list-sessions))
         (choices (and sessions
                       (> (length sessions) 1)
                       (mapcar (lambda (s)
                                 (cons (format "Session %d: %s (%s) - %d chats"
                                               (plist-get s :id)
                                               (mapconcat #'identity
                                                          (plist-get s :workspace-folders)
                                                          ", ")
                                               (plist-get s :status)
                                               (plist-get s :chat-count))
                                       (plist-get s :id)))
                               sessions)))
         (session-id
          (or session-id
              (if (null sessions)
                  (progn
                    (message "No active ECA sessions")
                    nil)
                (if (= (length sessions) 1)
                    (plist-get (car sessions) :id)
                  (cdr (assoc (completing-read "Select ECA session: " choices nil t)
                              choices)))))))
    (when session-id
      (let ((session (condition-case nil
                         (eca-get eca--sessions session-id)
                       (error nil))))
        (if session
            (progn
              (setq eca--session-id-cache session-id)
              (when (called-interactively-p 'interactive)
                (eca-info "Switched to session %d" session-id))
              session)
          (user-error "Session %s not found (may have been deleted)" session-id))))))

(defun eca-switch-to-session (&optional session-id)
  "Switch to ECA session SESSION-ID and open its last chat buffer.
When called interactively, prompt for session selection."
  (interactive)
  (let ((session (eca-select-session session-id)))
    (when session
      (eca-chat-open session)
      (pop-to-buffer (eca-chat--get-last-buffer session))
      session)))

(defun eca-create-session-for-workspace (workspace-roots)
  "Create a new ECA session for WORKSPACE-ROOTS and switch to it.
Return the new session."
  (interactive (list (list (read-directory-name "Workspace root: "))))
  (let ((session (eca-create-session workspace-roots)))
    (unless session
      (user-error "Failed to create ECA session for %s"
                  (mapconcat #'identity workspace-roots ", ")))
    (eca-info "Created session %d for %s"
              (eca--session-id session)
              (mapconcat #'identity workspace-roots ", "))
    (when (called-interactively-p 'interactive)
      (eca-switch-to-session (eca--session-id session)))
    session))

;;; Workspace Management

(defun eca-list-workspace-folders (&optional session)
  "Return workspace folders for SESSION or the current session."
  (let ((sess (or session (eca-session))))
    (when sess
      (eca--session-workspace-folders sess))))

(defun eca-add-workspace-folder (folder &optional session)
  "Add FOLDER to SESSION's workspace.
SESSION defaults to the current session.  Return the expanded folder path."
  (interactive
   (let ((session (eca-session)))
     (unless session
       (user-error "No ECA session active"))
     (list (read-directory-name "Add workspace folder: ") session)))
  (let ((sess (or session (eca-session))))
    (unless sess
      (user-error "No ECA session active"))
    (let* ((folder (expand-file-name folder))
           (existing (eca--session-workspace-folders sess))
           (session-id (eca--session-id sess)))
      (unless (file-directory-p folder)
        (user-error "Directory does not exist: %s" folder))
      (when (member folder existing)
        (user-error "Folder already in workspace: %s" folder))
      (eca--session-add-workspace-folder sess folder)
      (eca-info "Added workspace folder to session %d: %s" session-id folder)
      folder)))

(defalias 'eca-chat-add-workspace-folder #'eca-add-workspace-folder
  "Alias for `eca-add-workspace-folder' for discoverability.")

(defun eca-add-workspace-folder-all-sessions (folder)
  "Add FOLDER to every active ECA session."
  (interactive "DAdd to all sessions: ")
  (let* ((folder (expand-file-name folder))
         (sessions (eca-list-sessions))
         (added 0)
         (skipped 0))
    (unless sessions
      (user-error "No active ECA sessions"))
    (unless (file-directory-p folder)
      (user-error "Directory does not exist: %s" folder))
    (dolist (info sessions)
      (let* ((session-id (plist-get info :id))
             (session (condition-case nil
                          (eca-get eca--sessions session-id)
                        (error nil)))
             (existing (when session (eca--session-workspace-folders session))))
        (if (member folder existing)
            (cl-incf skipped)
          (when session
            (eca--session-add-workspace-folder session folder)
            (cl-incf added)))))
    (eca-info "Added %s to %d session(s), skipped %d (already present)"
              folder added skipped)))

(defun eca-remove-workspace-folder (folder &optional session)
  "Remove FOLDER from SESSION's workspace.
SESSION defaults to the current session.  Return the removed folder."
  (interactive
   (let* ((session (eca-session))
          (folders (when session (eca--session-workspace-folders session))))
     (unless session
       (user-error "No ECA session active"))
     (unless folders
       (user-error "No workspace folders in session"))
     (list (completing-read "Remove workspace folder: " folders nil t) session)))
  (let ((sess (or session (eca-session))))
    (unless sess
      (user-error "No ECA session active"))
    (let* ((folder (expand-file-name folder))
           (existing (eca--session-workspace-folders sess))
           (session-id (eca--session-id sess)))
      (unless (member folder existing)
        (user-error "Folder not in workspace: %s" folder))
      (with-no-warnings
        (setf (eca--session-workspace-folders sess)
              (remove folder existing)))
      (when (fboundp 'eca-api-notify)
        (eca-api-notify
         sess
         :method "workspace/didChangeWorkspaceFolders"
         :params (list :event
                       (list :added []
                             :removed (vector
                                       (list :uri (concat "file://" folder)
                                             :name (file-name-nondirectory
                                                    (directory-file-name folder))))))))
      (eca-info "Removed workspace folder from session %d: %s" session-id folder)
      folder)))

(defun eca-workspace-folder-for-file (file-path &optional session)
  "Return the workspace folder containing FILE-PATH in SESSION.
Return nil if FILE-PATH does not belong to any workspace folder."
  (let* ((sess (or session (eca-session)))
         (folders (when sess (eca--session-workspace-folders sess)))
         (file-path (expand-file-name file-path)))
    (when folders
      (seq-find (lambda (folder)
                  (string-prefix-p (file-name-as-directory folder)
                                   (file-name-as-directory file-path)))
                folders))))

(defun eca-workspace-provenance (file-path &optional session)
  "Return workspace provenance plist for FILE-PATH in SESSION."
  (let ((workspace (eca-workspace-folder-for-file file-path session)))
    (when workspace
      (list :workspace workspace
            :relative-path (file-relative-name file-path workspace)
            :folder-name (file-name-nondirectory
                          (directory-file-name workspace))))))

;;; Context Management

(defun eca-chat-add-file-context (session file-path)
  "Add FILE-PATH as file context to SESSION."
  (eca-assert-session-running session)
  (let* ((file-path (expand-file-name file-path))
         (prov (eca-workspace-provenance file-path session))
         (context (list :type "file" :path file-path)))
    (when prov
      (setq context (append context (list :workspace prov))))
    (eca-chat--with-current-buffer (eca-chat--get-last-buffer session)
      (eca-chat--add-context context)
      (eca-chat-open session))))

(defun eca-chat-add-repo-map-context (session)
  "Add repository map context to SESSION."
  (eca-assert-session-running session)
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer session)
    (eca-chat--add-context (list :type "repoMap"))
    (eca-chat-open session)))

(defun eca-chat-add-cursor-context (session file-path position)
  "Add cursor context to SESSION for FILE-PATH at POSITION."
  (eca-assert-session-running session)
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer session)
    (save-restriction
      (widen)
      (goto-char position)
      (let* ((start (line-beginning-position))
             (line (line-number-at-pos start))
             (character (- position start))
             (file-path (expand-file-name file-path))
             (prov (eca-workspace-provenance file-path session))
             (context (list :type "cursor"
                            :path file-path
                            :position (list :start (list :line line :character character)
                                            :end (list :line line :character character)))))
        (when prov
          (setq context (append context (list :workspace prov))))
        (eca-chat--add-context context)))
    (eca-chat-open session)))

(defun eca-chat-add-clipboard-context (session content)
  "Add CONTENT as a temporary file context to SESSION."
  (eca-assert-session-running session)
  (let* ((temp-dir (file-name-as-directory
                    (or (bound-and-true-p eca-config-directory)
                        (expand-file-name "~/.eca"))))
         (tmp-subdir (expand-file-name "tmp" temp-dir))
         (temp-file (expand-file-name
                     (format "clipboard-%d-%d-%d.txt"
                             (emacs-pid)
                             (floor (float-time))
                             (random 1000000))
                     tmp-subdir)))
    (unless (file-directory-p tmp-subdir)
      (make-directory tmp-subdir t))
    (with-temp-file temp-file
      (insert content))
    (eca--register-temp-file temp-file session)
    (eca-chat--with-current-buffer (eca-chat--get-last-buffer session)
      (eca-chat--add-context (list :type "file" :path temp-file))
      (eca-chat-open session))
    (eca-info "Added clipboard context (%d chars)" (length content))))

(defun eca-chat-add-clipboard-context-now ()
  "Add current clipboard contents as context to the current ECA session."
  (interactive)
  (let ((session (eca-session)))
    (if session
        (let ((clip-content (current-kill 0 t)))
          (if (and clip-content (not (string-empty-p clip-content)))
              (eca-chat-add-clipboard-context session clip-content)
            (message "Clipboard is empty")))
      (user-error "No ECA session active"))))

;;; Temp File Management

(defvar eca--context-temp-files nil
  "List of temporary context files created by eca-ext.

Format: ((session-id . (file-path1 file-path2 ...)) ...).")

(defvar eca--temp-file-max-age (* 24 3600)
  "Max age in seconds before temp files are considered stale.
Set to nil to disable stale-file cleanup.")

(defun eca--cleanup-temp-context-files ()
  "Clean up all temporary context files created by eca-ext."
  (let ((count 0))
    (dolist (entry eca--context-temp-files)
      (dolist (file (cdr entry))
        (condition-case nil
            (when (and file (file-exists-p file))
              (delete-file file)
              (cl-incf count))
          (error nil))))
    (setq eca--context-temp-files nil)
    (when (and (fboundp 'eca-info) (> count 0))
      (eca-info "Cleaned up %d temporary context files" count))))

(defun eca--cleanup-stale-temp-files ()
  "Clean up temp files older than `eca--temp-file-max-age'."
  (when eca--temp-file-max-age
    (let ((now (float-time))
          (count 0))
      (dolist (entry eca--context-temp-files)
        (setcdr entry
                (cl-remove-if
                 (lambda (file)
                   (when (and file (file-exists-p file))
                     (let ((age (- now (float-time (nth 5 (file-attributes file))))))
                       (when (> age eca--temp-file-max-age)
                         (condition-case nil
                             (delete-file file)
                           (error nil))
                         (cl-incf count)
                         t))))
                 (cdr entry))))
      (when (and (fboundp 'eca-info) (> count 0))
        (eca-info "Cleaned up %d stale temp files (older than %d hours)"
                  count (/ eca--temp-file-max-age 3600))))))

(defun eca--register-temp-file (file-path &optional session)
  "Register FILE-PATH for cleanup on exit or session end."
  (when (and file-path (file-exists-p file-path))
    (let* ((sid (if session
                    (if (numberp session) session (eca--session-id session))
                  (when (boundp 'eca--session-id-cache)
                    eca--session-id-cache)))
           (entry (assoc sid eca--context-temp-files)))
      (if entry
          (push file-path (cdr entry))
        (push (cons sid (list file-path)) eca--context-temp-files)))
    file-path))

(defun eca--cleanup-session-temp-files (session)
  "Clean up temp files associated with SESSION."
  (let* ((sid (if (numberp session) session (eca--session-id session)))
         (entry (assoc sid eca--context-temp-files))
         (files (cdr entry))
         (count 0))
    (when files
      (dolist (file files)
        (condition-case nil
            (when (and file (file-exists-p file))
              (delete-file file)
              (cl-incf count))
          (error nil)))
      (setq eca--context-temp-files (assq-delete-all sid eca--context-temp-files))
      (when (and (fboundp 'eca-info) (> count 0))
        (eca-info "Cleaned up %d temp files for session %s" count sid)))))

(add-hook 'kill-emacs-hook #'eca--cleanup-temp-context-files)
(run-with-timer 3600 3600 #'eca--cleanup-stale-temp-files)

;;; Automatic Workspace / Session Management

(defcustom eca-auto-add-workspace-folder t
  "If non-nil, automatically add a file's project to the current workspace.
If the value is `prompt', ask before adding."
  :type '(choice (const :tag "Auto add" t)
                 (const :tag "Prompt before adding" prompt)
                 (const :tag "Disabled" nil))
  :group 'eca)

(defcustom eca-auto-switch-session 'prompt
  "If non-nil, automatically switch to the session matching the current project.
If the value is `prompt', ask before switching."
  :type '(choice (const :tag "Auto switch" t)
                 (const :tag "Prompt before switching" prompt)
                 (const :tag "Disabled" nil))
  :group 'eca)

(defcustom eca-auto-create-session nil
  "If non-nil, automatically create or extend sessions for new projects.
If the value is `prompt', ask before creating."
  :type '(choice (const :tag "Auto create" t)
                 (const :tag "Prompt before creating" prompt)
                 (const :tag "Disabled" nil))
  :group 'eca)

(defcustom eca-auto-sync-workspace t
  "If non-nil, automatically sync workspace folders when project changes."
  :type 'boolean
  :group 'eca)

(defvar eca--last-project-root nil
  "Track the last project root seen by ECA auto-switch logic.")

(defvar eca--shared-context nil
  "Plist of shared context items available to all sessions.
Keys currently used are :files and :repo-maps.

- :files contains a list of absolute file paths.
- :repo-maps contains a list of absolute repository root directories.")

(defun eca-ext--file-project-root (file-path)
  "Return a project root for FILE-PATH using projectile, project.el, or fallback."
  (when file-path
    (or (when (fboundp 'projectile-project-root)
          (ignore-errors
            (projectile-project-root (file-name-directory file-path))))
        (when (fboundp 'project-current)
          (ignore-errors
            (let ((proj (project-current nil (file-name-directory file-path))))
              (when proj
                (project-root proj)))))
        (file-name-directory file-path))))

(defun eca-ext--session-for-project-root (project-root)
  "Find the ECA session whose workspace contains PROJECT-ROOT."
  (let* ((root (eca-ext--normalize-folder-path project-root))
         (sessions (eca-list-sessions)))
    (cl-dolist (info sessions)
      (let* ((session-id (plist-get info :id))
             (folders (plist-get info :workspace-folders))
             (match (cl-find root folders
                             :test (lambda (lhs rhs)
                                     (string= lhs (eca-ext--normalize-folder-path rhs))))))
        (when match
          (cl-return session-id))))))

(defun eca-ext--auto-add-workspace-hook ()
  "Auto-add the current file's project root to the current ECA workspace.
If the project is already present in the workspace, do nothing."
  (when (and eca-auto-add-workspace-folder
             buffer-file-name
             (featurep 'eca)
             (eca-session))
    (let* ((project-root (eca-ext--file-project-root buffer-file-name))
           (session (eca-session))
           (workspace-folders (eca--session-workspace-folders session))
           (in-workspace (and project-root
                              (member (eca-ext--normalize-folder-path project-root)
                                      (mapcar (lambda (folder)
                                                (eca-ext--normalize-folder-path folder))
                                              workspace-folders)))))
      (when (and project-root (not in-workspace))
        (let ((root (eca-ext--normalize-folder-path project-root)))
          (pcase eca-auto-add-workspace-folder
            ('t
             (eca--session-add-workspace-folder session root)
             (message "Auto-added project to ECA session %d: %s"
                      (eca--session-id session) root))
            ('prompt
             (when (y-or-n-p (format "Add project to ECA workspace? (%s) " root))
               (eca--session-add-workspace-folder session root)))))))))

(defun eca-ext--auto-switch-session-hook (&optional _frame)
  "Auto-switch ECA sessions when the active project changes."
  (when (and eca-auto-switch-session
             buffer-file-name
             (featurep 'eca)
             (eca-list-sessions))
    (let* ((project-root (eca-ext--file-project-root buffer-file-name))
           (current-session (ignore-errors (eca-session)))
           (current-session-id (when current-session
                                 (ignore-errors (eca--session-id current-session)))))
      (when (and project-root
                 (not (string= project-root eca--last-project-root)))
        (let ((target-session (eca-ext--session-for-project-root project-root))
              (root (eca-ext--normalize-folder-path project-root)))
          (setq eca--last-project-root root)
          (when (and target-session
                     (not (eq target-session current-session-id)))
            (pcase eca-auto-switch-session
              ('t
               (eca-switch-to-session target-session)
               (message "Auto-switched to ECA session %d for %s"
                        target-session root))
              ('prompt
               (when (y-or-n-p (format "Switch to session %d for %s? "
                                       target-session root))
                 (eca-switch-to-session target-session))))))))))

(defun eca-ext--auto-create-session-hook ()
  "Auto-create or extend ECA sessions when visiting a project without one."
  (when (and eca-auto-create-session
             buffer-file-name
             (featurep 'eca))
    (let* ((project-root (eca-ext--file-project-root buffer-file-name))
           (existing-session (when project-root
                               (eca-ext--session-for-project-root project-root)))
           (any-sessions (eca-list-sessions)))
      (when (and project-root (not existing-session))
        (let ((root (eca-ext--normalize-folder-path project-root)))
          (pcase eca-auto-create-session
            ('t
             (if any-sessions
                 (let ((session (eca-session)))
                   (if session
                       (progn
                         (eca--session-add-workspace-folder session root)
                         (message "Auto-added %s to current ECA session" root))
                     (let ((session (eca-create-session (list root))))
                       (when session
                         (message "Auto-created ECA session %d for %s"
                                  (eca--session-id session) root)))))
               (let ((session (eca-create-session (list root))))
                 (when session
                   (message "Auto-created ECA session %d for %s"
                            (eca--session-id session) root)
                   (eca-chat-open session)))))
            ('prompt
             (when (y-or-n-p (format "Create ECA session for %s? " root))
               (let ((session (eca-create-session (list root))))
                 (when session
                   (eca-chat-open session)))))))))))

(defun eca-ext--auto-sync-workspace-hook (&optional _frame)
  "Auto-sync the current project's root into the current ECA workspace."
  (when (and eca-auto-sync-workspace
             buffer-file-name
             (featurep 'eca)
             (eca-session))
    (let* ((project-root (eca-ext--file-project-root buffer-file-name)))
      (when project-root
        (let* ((root (eca-ext--normalize-folder-path project-root))
               (session (eca-session))
               (folders (eca--session-workspace-folders session))
               (in-workspace (member root
                                     (mapcar #'eca-ext--normalize-folder-path
                                             folders))))
          (unless in-workspace
            (eca--session-add-workspace-folder session root)
            (message "Auto-synced workspace: added %s" root)))))))

(with-eval-after-load 'eca
  (add-hook 'find-file-hook #'eca-ext--auto-add-workspace-hook)
  (add-hook 'find-file-hook #'eca-ext--auto-create-session-hook 90)
  (add-hook 'window-buffer-change-functions #'eca-ext--auto-switch-session-hook)
  (add-hook 'window-buffer-change-functions #'eca-ext--auto-sync-workspace-hook))

;;; Shared Context

(defun eca-share-file-context (file-path)
  "Add FILE-PATH to the shared context for all ECA sessions."
  (interactive "fShare file across sessions: ")
  (let ((file-path (expand-file-name file-path)))
    (setq eca--shared-context
          (plist-put
           eca--shared-context
           :files
           (cl-adjoin file-path (plist-get eca--shared-context :files) :test #'string=)))
    (message "Shared file across all ECA sessions: %s" file-path)))

(defun eca-share-repo-map-context (project-root)
  "Add PROJECT-ROOT repo map to the shared context for all ECA sessions."
  (interactive "DShare repo map across sessions: ")
  (let ((root (expand-file-name project-root)))
    (setq eca--shared-context
          (plist-put
           eca--shared-context
           :repo-maps
           (cl-adjoin root (plist-get eca--shared-context :repo-maps) :test #'string=)))
    (message "Shared repo map across all ECA sessions: %s" root)))

(defun eca-apply-shared-context (session)
  "Apply shared context to SESSION."
  (interactive (list (eca-session)))
  (unless session
    (user-error "No ECA session active"))
  (let ((files (plist-get eca--shared-context :files))
        (repo-maps (plist-get eca--shared-context :repo-maps)))
    (dolist (file files)
      (when (file-exists-p file)
        (eca-chat-add-file-context session file)))
    (dolist (root repo-maps)
      (when (file-directory-p root)
        (unless (member (eca-ext--normalize-folder-path root)
                        (mapcar #'eca-ext--normalize-folder-path
                                (or (eca-list-workspace-folders session) '())))
          (when (fboundp 'eca-add-workspace-folder)
            (eca-add-workspace-folder root session)))
        (eca-chat-add-repo-map-context session)))
    (message "Applied shared context to session %d: %d files, %d repo maps"
             (eca--session-id session)
             (length files)
             (length repo-maps))))

(defun eca-clear-shared-context ()
  "Clear all shared context items."
  (interactive)
  (setq eca--shared-context nil)
  (message "Cleared shared context"))

;;; Session Dashboard

(defvar eca-session-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'eca-session-dashboard-switch)
    (define-key map (kbd "d") #'eca-session-dashboard-delete)
    (define-key map (kbd "w") #'eca-session-dashboard-list-folders)
    (define-key map (kbd "g") #'eca-session-dashboard-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for the ECA session dashboard.")

(define-derived-mode eca-session-dashboard-mode tabulated-list-mode "ECA Sessions"
  "Major mode for viewing and managing ECA sessions."
  (setq tabulated-list-format [("ID" 4 t)
                               ("Status" 12 t)
                               ("Workspaces" 40 t)
                               ("Chats" 6 t)])
  (setq tabulated-list-sort-key (cons "ID" nil))
  (tabulated-list-init-header))

(defun eca-session-dashboard ()
  "Open a dashboard showing all ECA sessions."
  (interactive)
  (let ((buffer (get-buffer-create "*ECA Sessions*")))
    (with-current-buffer buffer
      (eca-session-dashboard-mode)
      (eca-session-dashboard-refresh))
    (pop-to-buffer buffer)))

(defun eca-session-dashboard-refresh ()
  "Refresh the session dashboard."
  (interactive)
  (let ((sessions (eca-list-sessions)))
    (setq tabulated-list-entries
          (mapcar (lambda (info)
                    (list (plist-get info :id)
                          (vector (number-to-string (plist-get info :id))
                                  (format "%s" (plist-get info :status))
                                  (string-join (plist-get info :workspace-folders) ", ")
                                  (number-to-string (plist-get info :chat-count)))))
                  sessions))
    (tabulated-list-print t)))

(defun eca-session-dashboard-switch ()
  "Switch to the ECA session on the current dashboard line."
  (interactive)
  (let ((session-id (tabulated-list-get-id)))
    (when session-id
      (eca-switch-to-session session-id)
      (quit-window))))

(defun eca-session-dashboard-delete ()
  "Delete the ECA session on the current dashboard line."
  (interactive)
  (let ((session-id (tabulated-list-get-id)))
    (when (and session-id
               (y-or-n-p (format "Delete session %d? " session-id)))
      (let ((session (eca-get eca--sessions session-id)))
        (when session
          (eca-delete-session session)
          (eca-session-dashboard-refresh))))))

(defun eca-session-dashboard-list-folders ()
  "List workspace folders for the ECA session on the current dashboard line."
  (interactive)
  (let ((session-id (tabulated-list-get-id)))
    (when session-id
      (let* ((session (eca-get eca--sessions session-id))
             (folders (when session (eca--session-workspace-folders session))))
        (message "Session %d workspaces: %s" session-id
                 (string-join folders " | "))))))

(provide 'eca-ext)

;; Local Variables:
;; package-lint-main-file: "ai-code.el"
;; End:

;;; eca-ext.el ends here
