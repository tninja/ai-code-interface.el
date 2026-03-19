;;; ai-code-eca.el --- ECA backend bridge for ai-code  -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: davidwuchn
;; Version: 0.2
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, code, assistant, eca
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; ECA backend bridge for ai-code with:
;;   - Session management (list, switch, which)
;;   - Workspace management (add, list, remove, sync)
;;   - Context commands (file, cursor, repo-map, clipboard)
;;   - Shared context with auto-apply on session switch
;;   - Integrated into ai-code-menu (C-c a) when ECA selected
;;
;; When ECA is selected as the ai-code backend, ECA items appear
;; directly in ai-code-menu under the "ECA" group.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'package)
(require 'subr-x)
(require 'tabulated-list)
(require 'ai-code-backends)
(require 'ai-code-input nil t)
(require 'transient nil t)
(require 'eca nil t)
(require 'eca-util nil t)
(require 'eca-chat nil t)
(require 'eca-process nil t)

;;; ==============================================================================
;;; Binary Upgrade Configuration
;;; ==============================================================================

(defcustom ai-code-eca-upgrade-auto-enabled nil
  "When non-nil, check for ECA binary updates once per day at idle."
  :type 'boolean
  :group 'eca)

(defcustom ai-code-eca-upgrade-auto-idle-seconds 30
  "Seconds of idle time before the daily ECA binary update check runs."
  :type 'integer
  :group 'eca)

(declare-function eca "eca" (&optional arg))
(declare-function eca-session "eca-util" ())
(declare-function eca-vals "eca-util" (map))
(declare-function eca-chat-open "eca-chat" (session))
(declare-function eca-chat-send-prompt "eca-chat" (message))
(declare-function eca-chat--get-last-buffer "eca-chat" (session))
(declare-function eca-info "eca-util" (format-string &rest args))
(declare-function eca--session-id "eca-util" (session))
(declare-function eca--session-status "eca-util" (session))
(declare-function eca--session-workspace-folders "eca-util" (session))
(declare-function eca-chat-add-workspace-root "eca-chat" ())

(declare-function transient-append-suffix "transient" (prefix loc suffix &optional face))
(declare-function transient-remove-suffix "transient" (prefix suffix))

;;; Core Commands

;;;###autoload
(defun ai-code-eca-start (&optional arg)
  "Start or resume an ECA session.
With prefix ARG, force new session."
  (interactive "P")
  (ai-code-eca--ensure-available)
  (let ((current-prefix-arg arg))
    (call-interactively #'eca)))

;;;###autoload
(defun ai-code-eca-switch (&optional force-prompt)
  "Switch to ECA chat buffer.
With FORCE-PROMPT (prefix arg), force new session."
  (interactive "P")
  (ai-code-eca--ensure-available)
  (if force-prompt
      (ai-code-eca-start '(16))
    (let ((session (eca-session)))
      (if session
          (progn
            (eca-chat-open session)
            (pop-to-buffer (eca-chat--get-last-buffer session)))
        (ai-code-eca-start nil)))))

(defun ai-code-eca--ensure-available ()
  "Ensure ECA package and functions are available."
  (unless (require 'eca nil t)
    (user-error "ECA not available. Install with: M-x package-install RET eca RET"))
  (dolist (fn '(eca eca-session eca-chat-open eca-chat--get-last-buffer))
    (unless (fboundp fn)
      (user-error "ECA missing function: %s. Reinstall eca package" fn))))

;;; Menu Integration - ECA group in ai-code-menu

(defvar ai-code-eca--menu-group-added nil
  "Track whether ECA group has been added to ai-code-menu.")

(declare-function ai-code-eca-switch-session "ai-code-eca" (&optional session-id))
(declare-function ai-code-eca-add-workspace-folder "ai-code-eca" (folder &optional session))
(declare-function ai-code-eca-remove-workspace-folder "ai-code-eca" (folder &optional session))
(declare-function ai-code-eca-share-file-context "ai-code-eca" (file-path))
(declare-function ai-code-eca-share-repo-map-context "ai-code-eca" (project-root))
(declare-function ai-code-eca-clear-shared-context "ai-code-eca" ())
(declare-function ai-code-eca-chat-add-clipboard-context-now "ai-code-eca" ())
(declare-function eca-workspaces "eca" ())

;;;###autoload
(transient-define-prefix ai-code-eca-menu ()
  "ECA session management menu."
  ["ECA"
   ("E" "Start ECA (C-u: pick project)" ai-code-eca-create-session-for-workspace)
   ("W" "Switch session" ai-code-eca-switch-session)
   ("D" "Dashboard" eca-workspaces)
   ("A" "Add project" ai-code-eca-add-workspace-folder)
   ("X" "Remove project" ai-code-eca-remove-workspace-folder)
   ("F" "Share file" ai-code-eca-share-file-context)
   ("M" "Share repo map" ai-code-eca-share-repo-map-context)
   ("Y" "Clear shared" ai-code-eca-clear-shared-context)
   ("B" "Add clipboard" ai-code-eca-chat-add-clipboard-context-now)])

(defun ai-code-eca--add-menu-group ()
  "Add ECA commands directly to ai-code-menu layouts."
  (when (and (featurep 'transient)
             (not ai-code-eca--menu-group-added))
    (condition-case err
        (progn
          (dolist (prefix '(ai-code-menu-default ai-code-menu-2-columns))
            (when (commandp prefix)
              (transient-append-suffix prefix '(0 -1)
                ["ECA"
                 ("E" "Start ECA (C-u: pick project)" ai-code-eca-create-session-for-workspace)
                 ("W" "Switch session" ai-code-eca-switch-session)
                 ("D" "Dashboard" eca-workspaces)
                 ("A" "Add project" ai-code-eca-add-workspace-folder)
                 ("X" "Remove project" ai-code-eca-remove-workspace-folder)
                 ("F" "Share file" ai-code-eca-share-file-context)
                 ("M" "Share repo map" ai-code-eca-share-repo-map-context)
                 ("Y" "Clear shared" ai-code-eca-clear-shared-context)
                 ("B" "Add clipboard" ai-code-eca-chat-add-clipboard-context-now)])))
          (setq ai-code-eca--menu-group-added t))
      (error
       (message "Failed to add ECA menu: %s" (error-message-string err))))))

(defun ai-code-eca--remove-menu-group ()
  "Remove ECA commands from ai-code-menu layouts."
  (when ai-code-eca--menu-group-added
    (condition-case err
        (progn
          (dolist (prefix '(ai-code-menu-default ai-code-menu-2-columns))
            (when (commandp prefix)
              (dolist (key '("B" "Y" "M" "F" "X" "A" "D" "W" "E"))
                (condition-case nil
                    (transient-remove-suffix prefix key)
                  (error nil)))))
          (setq ai-code-eca--menu-group-added nil))
      (error
       (message "Failed to remove ECA menu: %s" (error-message-string err))))))

(with-eval-after-load 'ai-code
  (advice-add 'ai-code-set-backend :after
              (lambda (backend)
                (if (eq backend 'eca)
                    (ai-code-eca--add-menu-group)
                  (ai-code-eca--remove-menu-group)))))


;;; ============================================================
;;; Session Multiplexing & Workspace Management
;;; ============================================================

(declare-function eca-create-session "eca-util" (workspace-folders))
(declare-function eca-delete-session "eca-util" (session))
(declare-function eca-assert-session-running "eca-util" (session))
(declare-function (setf eca--session-workspace-folders) "eca-util" (value session))
(declare-function eca--session-add-workspace-folder "eca-util" (session folder))
(declare-function eca--session-chats "eca-util" (session))
(declare-function eca-process-start "eca-process" (session on-ready on-message))
(declare-function eca--initialize "eca" (session))
(declare-function eca--handle-message "eca" (session message))
(declare-function eca-chat--add-context "eca-chat" (context-plist))
(declare-function eca-chat--with-current-buffer "eca-chat" (&rest body))
(declare-function eca-api-notify "eca-api" (session &rest args))
(declare-function projectile-project-root "projectile" (&optional dir))
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-root "project" (project))

(defun ai-code-eca--normalize-folder-path (path)
  "Return PATH as an expanded directory path without a trailing slash."
  (directory-file-name (expand-file-name path)))

;;; Session Multiplexing

(defun ai-code-eca-list-sessions ()
  "Return a list of all active ECA sessions.
Each element is a plist with :id, :status, :workspace-folders, and :chat-count.
Return nil if ECA has no active sessions."
  (and (boundp 'eca--sessions)
       eca--sessions
       (mapcar (lambda (session)
                 (list :id (eca--session-id session)
                       :status (eca--session-status session)
                       :workspace-folders (eca--session-workspace-folders session)
                       :chat-count (length (eca--session-chats session))))
               (eca-vals eca--sessions))))

(defun ai-code-eca-select-session (&optional session-id)
  "Select an ECA session by SESSION-ID or interactively.
Return the selected session or nil if canceled."
  (interactive)
  (let* ((sessions (ai-code-eca-list-sessions))
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

(defun ai-code-eca-switch-to-session (&optional session-id)
  "Switch to ECA session SESSION-ID and open its last chat buffer.
When called interactively, prompt for session selection.
Auto-apply shared context if any."
  (interactive)
  (let ((session (ai-code-eca-select-session session-id)))
    (when session
      (ai-code-eca--apply-shared-context-internal session)
      (eca-chat-open session)
      (pop-to-buffer (eca-chat--get-last-buffer session))
      session)))

(defun ai-code-eca-create-session-for-workspace (&optional arg)
  "Start ECA session.
Without ARG, use current project root (reuse existing session if available).
With ARG (C-u), prompt for workspace root and create NEW session."
  (interactive "P")
  (if (equal arg '(4))
      ;; With C-u: create NEW session for specified workspace
      (let* ((workspace-root (read-directory-name "Workspace root: "))
             (session (eca-create-session (list workspace-root))))
        (when session
          (pcase (eca--session-status session)
            ('stopped
             (eca-process-start session
                                (lambda ()
                                  (eca--initialize session))
                                (-partial #'eca--handle-message session)))
            ('started
             (eca-chat-open session))
            ('starting
             (eca-info "ECA server is already starting")))
          session))
    ;; Without C-u: use eca (reuses existing session if available)
    (call-interactively #'eca)))

;;; Workspace Management

(defun ai-code-eca-list-workspace-folders (&optional session)
  "Return workspace folders for SESSION or the current session."
  (let ((sess (or session (eca-session))))
    (when sess
      (eca--session-workspace-folders sess))))

(defun ai-code-eca-add-workspace-folder (folder &optional session)
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

(defalias 'ai-code-eca-chat-add-workspace-folder #'ai-code-eca-add-workspace-folder
  "Alias for `eca-add-workspace-folder' for discoverability.")

(defun ai-code-eca-remove-workspace-folder (folder &optional session)
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
    (let* ((folder (directory-file-name (expand-file-name folder)))
           (existing-raw (eca--session-workspace-folders sess))
           (existing (mapcar (lambda (f) (directory-file-name (expand-file-name f))) existing-raw))
           (session-id (eca--session-id sess)))
      (unless (member folder existing)
        (user-error "Folder not in workspace: %s" folder))
      (let ((raw-folder (nth (seq-position existing folder) existing-raw)))
        (with-no-warnings
          (setf (eca--session-workspace-folders sess)
                (remove raw-folder existing-raw)))
        (when (fboundp 'eca-api-notify)
          (eca-api-notify
           sess
           :method "workspace/didChangeWorkspaceFolders"
           :params (list :event
                         (list :added []
                               :removed (vector
                                         (list :uri (concat "file://" raw-folder)
                                               :name (file-name-nondirectory
                                                      (directory-file-name raw-folder))))))))
        (eca-info "Removed workspace folder from session %d: %s" session-id raw-folder)
        raw-folder))))

(defun ai-code-eca-workspace-folder-for-file (file-path &optional session)
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

(defun ai-code-eca-workspace-provenance (file-path &optional session)
  "Return workspace provenance plist for FILE-PATH in SESSION."
  (let ((workspace (ai-code-eca-workspace-folder-for-file file-path session)))
    (when workspace
      (list :workspace workspace
            :relative-path (file-relative-name file-path workspace)
            :folder-name (file-name-nondirectory
                          (directory-file-name workspace))))))

;;; Context Management

(defun ai-code-eca-chat-add-file-context (session file-path)
  "Add FILE-PATH as file context to SESSION."
  (eca-assert-session-running session)
  (let* ((file-path (expand-file-name file-path))
         (prov (ai-code-eca-workspace-provenance file-path session))
         (context (list :type "file" :path file-path)))
    (when prov
      (setq context (append context (list :workspace prov))))
    (eca-chat--with-current-buffer (eca-chat--get-last-buffer session)
      (eca-chat--add-context context)
      (eca-chat-open session))))

(defun ai-code-eca-chat-add-repo-map-context (session)
  "Add repository map context to SESSION."
  (eca-assert-session-running session)
  (eca-chat--with-current-buffer (eca-chat--get-last-buffer session)
    (eca-chat--add-context (list :type "repoMap"))
    (eca-chat-open session)))

(defun ai-code-eca-chat-add-clipboard-context (session content)
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
    (ai-code-eca--register-temp-file temp-file session)
    (eca-chat--with-current-buffer (eca-chat--get-last-buffer session)
      (eca-chat--add-context (list :type "file" :path temp-file))
      (eca-chat-open session))
    (eca-info "Added clipboard context (%d chars)" (length content))))

(defun ai-code-eca-chat-add-clipboard-context-now ()
  "Add current clipboard contents as context to the current ECA session."
  (interactive)
  (let ((session (eca-session)))
    (if session
        (let ((clip-content (current-kill 0 t)))
          (if (and clip-content (not (string-empty-p clip-content)))
              (ai-code-eca-chat-add-clipboard-context session clip-content)
            (message "Clipboard is empty")))
      (user-error "No ECA session active"))))

;;; Temp File Management

(defvar ai-code-eca--context-temp-files nil
  "List of temporary context files created by ai-code-eca.

Format: ((session-id . (file-path1 file-path2 ...)) ...).")

(defvar ai-code-eca--temp-file-max-age (* 24 3600)
  "Max age in seconds before temp files are considered stale.
Set to nil to disable stale-file cleanup.")

(defun ai-code-eca--cleanup-temp-context-files ()
  "Clean up all temporary context files created by ai-code-eca."
  (let ((count 0))
    (dolist (entry ai-code-eca--context-temp-files)
      (dolist (file (cdr entry))
        (condition-case nil
            (when (and file (file-exists-p file))
              (delete-file file)
              (cl-incf count))
          (error nil))))
    (setq ai-code-eca--context-temp-files nil)
    (when (and (fboundp 'eca-info) (> count 0))
      (eca-info "Cleaned up %d temporary context files" count))))

(defun ai-code-eca--cleanup-stale-temp-files ()
  "Clean up temp files older than `ai-code-eca--temp-file-max-age'."
  (when ai-code-eca--temp-file-max-age
    (let ((now (float-time))
          (count 0))
      (dolist (entry ai-code-eca--context-temp-files)
        (setcdr entry
                (cl-remove-if
                 (lambda (file)
                   (when (and file (file-exists-p file))
                     (let ((age (- now (float-time (nth 5 (file-attributes file))))))
                       (when (> age ai-code-eca--temp-file-max-age)
                         (condition-case nil
                             (delete-file file)
                           (error nil))
                         (cl-incf count)
                         t))))
                 (cdr entry))))
      (when (and (fboundp 'eca-info) (> count 0))
        (eca-info "Cleaned up %d stale temp files (older than %d hours)"
                  count (/ ai-code-eca--temp-file-max-age 3600))))))

(defun ai-code-eca--register-temp-file (file-path &optional session)
  "Register FILE-PATH for cleanup on exit or session end."
  (when (and file-path (file-exists-p file-path))
    (let* ((sid (if session
                    (if (numberp session) session (eca--session-id session))
                  (when (boundp 'eca--session-id-cache)
                    eca--session-id-cache)))
           (entry (assoc sid ai-code-eca--context-temp-files)))
      (if entry
          (push file-path (cdr entry))
        (push (cons sid (list file-path)) ai-code-eca--context-temp-files)))
    file-path))

(add-hook 'kill-emacs-hook #'ai-code-eca--cleanup-temp-context-files)
(run-with-timer 3600 3600 #'ai-code-eca--cleanup-stale-temp-files)

;;; Automatic Workspace / Session Management

(defcustom ai-code-eca-auto-add-workspace-folder t
  "If non-nil, automatically add a file's project to the current workspace.
If the value is `prompt', ask before adding."
  :type '(choice (const :tag "Auto add" t)
                 (const :tag "Prompt before adding" prompt)
                 (const :tag "Disabled" nil))
  :group 'eca)

(defcustom ai-code-eca-auto-switch-session 'prompt
  "If non-nil, automatically switch to the session matching the current project.
If the value is `prompt', ask before switching."
  :type '(choice (const :tag "Auto switch" t)
                 (const :tag "Prompt before switching" prompt)
                 (const :tag "Disabled" nil))
  :group 'eca)

(defcustom ai-code-eca-auto-create-session nil
  "If non-nil, automatically create or extend sessions for new projects.
If the value is `prompt', ask before creating."
  :type '(choice (const :tag "Auto create" t)
                 (const :tag "Prompt before creating" prompt)
                 (const :tag "Disabled" nil))
  :group 'eca)

(defvar ai-code-eca--last-project-root nil
  "Track the last project root seen by ECA auto-switch logic.")

(defvar ai-code-eca--shared-context nil
  "Plist of shared context items available to all sessions.
Keys currently used are :files and :repo-maps.

- :files contains a list of absolute file paths.
- :repo-maps contains a list of absolute repository root directories.")

(defun ai-code-eca--file-project-root (file-path)
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

(defun ai-code-eca--session-for-project-root (project-root)
  "Find the ECA session whose workspace contains PROJECT-ROOT."
  (let* ((root (ai-code-eca--normalize-folder-path project-root))
         (sessions (ai-code-eca-list-sessions)))
    (cl-dolist (info sessions)
      (let* ((session-id (plist-get info :id))
             (folders (plist-get info :workspace-folders))
             (match (cl-find root folders
                             :test (lambda (lhs rhs)
                                     (string= lhs (ai-code-eca--normalize-folder-path rhs))))))
        (when match
          (cl-return session-id))))))

(defun ai-code-eca--auto-add-workspace-hook ()
  "Auto-add the current file's project root to the current ECA workspace.
If the project is already present in the workspace, do nothing."
  (when (and ai-code-eca-auto-add-workspace-folder
             buffer-file-name
             (featurep 'eca)
             (eca-session))
    (let* ((project-root (ai-code-eca--file-project-root buffer-file-name))
           (session (eca-session))
           (workspace-folders (eca--session-workspace-folders session))
           (in-workspace (and project-root
                              (member (ai-code-eca--normalize-folder-path project-root)
                                      (mapcar (lambda (folder)
                                                (ai-code-eca--normalize-folder-path folder))
                                              workspace-folders)))))
      (when (and project-root (not in-workspace))
        (let ((root (ai-code-eca--normalize-folder-path project-root)))
          (pcase ai-code-eca-auto-add-workspace-folder
            ('t
             (eca--session-add-workspace-folder session root)
             (message "Auto-added project to ECA session %d: %s"
                      (eca--session-id session) root))
            ('prompt
             (when (y-or-n-p (format "Add project to ECA workspace? (%s) " root))
               (eca--session-add-workspace-folder session root)))))))))

(defun ai-code-eca--auto-switch-session-hook (&optional _frame)
  "Auto-switch ECA sessions when the active project changes."
  (when (and ai-code-eca-auto-switch-session
             buffer-file-name
             (featurep 'eca)
             (ai-code-eca-list-sessions))
    (let* ((project-root (ai-code-eca--file-project-root buffer-file-name))
           (current-session (ignore-errors (eca-session)))
           (current-session-id (when current-session
                                 (ignore-errors (eca--session-id current-session)))))
      (when (and project-root
                 (not (string= project-root ai-code-eca--last-project-root)))
        (let ((target-session (ai-code-eca--session-for-project-root project-root))
              (root (ai-code-eca--normalize-folder-path project-root)))
          (setq ai-code-eca--last-project-root root)
          (when (and target-session
                     (not (eq target-session current-session-id)))
            (pcase ai-code-eca-auto-switch-session
              ('t
               (ai-code-eca-switch-to-session target-session)
               (message "Auto-switched to ECA session %d for %s"
                        target-session root))
              ('prompt
               (when (y-or-n-p (format "Switch to session %d for %s? "
                                       target-session root))
                 (ai-code-eca-switch-to-session target-session))))))))))

(defun ai-code-eca--auto-create-session-hook ()
  "Auto-create or extend ECA sessions when visiting a project without one."
  (when (and ai-code-eca-auto-create-session
             buffer-file-name
             (featurep 'eca))
    (let* ((project-root (ai-code-eca--file-project-root buffer-file-name))
           (existing-session (when project-root
                               (ai-code-eca--session-for-project-root project-root)))
           (any-sessions (ai-code-eca-list-sessions)))
      (when (and project-root (not existing-session))
        (let ((root (ai-code-eca--normalize-folder-path project-root)))
          (pcase ai-code-eca-auto-create-session
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

(with-eval-after-load 'eca
  (add-hook 'find-file-hook #'ai-code-eca--auto-add-workspace-hook)
  (add-hook 'find-file-hook #'ai-code-eca--auto-create-session-hook 90)
  (add-hook 'window-buffer-change-functions #'ai-code-eca--auto-switch-session-hook))

;;; Shared Context

(defun ai-code-eca-share-file-context (file-path)
  "Add FILE-PATH to the shared context for all ECA sessions."
  (interactive "fShare file across sessions: ")
  (let ((file-path (expand-file-name file-path)))
    (setq ai-code-eca--shared-context
          (plist-put
           ai-code-eca--shared-context
           :files
           (cl-adjoin file-path (plist-get ai-code-eca--shared-context :files) :test #'string=)))
    (message "Shared file across all ECA sessions: %s" file-path)))

(defun ai-code-eca-share-repo-map-context (project-root)
  "Add PROJECT-ROOT repo map to the shared context for all ECA sessions."
  (interactive "DShare repo map across sessions: ")
  (let ((root (expand-file-name project-root)))
    (setq ai-code-eca--shared-context
          (plist-put
           ai-code-eca--shared-context
           :repo-maps
           (cl-adjoin root (plist-get ai-code-eca--shared-context :repo-maps) :test #'string=)))
    (message "Shared repo map across all ECA sessions: %s" root)))

(defun ai-code-eca--apply-shared-context-internal (session)
  "Apply shared context to SESSION without interactive checks."
  (let ((files (plist-get ai-code-eca--shared-context :files))
        (repo-maps (plist-get ai-code-eca--shared-context :repo-maps)))
    (when (or files repo-maps)
      (dolist (file files)
        (when (file-exists-p file)
          (ai-code-eca-chat-add-file-context session file)))
      (dolist (root repo-maps)
        (when (file-directory-p root)
          (unless (member (ai-code-eca--normalize-folder-path root)
                          (mapcar #'ai-code-eca--normalize-folder-path
                                  (or (ai-code-eca-list-workspace-folders session) '())))
            (when (fboundp 'ai-code-eca-add-workspace-folder)
              (ai-code-eca-add-workspace-folder root session)))
          (ai-code-eca-chat-add-repo-map-context session)))
      (message "Applied shared context: %d files, %d repo maps"
               (length files) (length repo-maps)))))

(defun ai-code-eca-clear-shared-context ()
  "Clear all shared context items."
  (interactive)
  (setq ai-code-eca--shared-context nil)
  (message "Cleared shared context"))

;;; Backend interface functions for ai-code-backends.el

(defun ai-code-eca-send (line)
  "Send LINE to ECA chat."
  (interactive "sECA> ")
  (ai-code-eca--ensure-available)
  (let ((session (eca-session)))
    (if session
        (progn
          (eca-chat-open session)
          (eca-chat-send-prompt line))
      (user-error "No ECA session. Run M-x ai-code-eca-start first"))))

(defun ai-code-eca-resume (&optional _arg)
  "Resume/switch to ECA chat buffer.
ARG is ignored (for backend interface compatibility)."
  (interactive "P")
  (ai-code-eca-switch))

(defun ai-code-eca-upgrade-package ()
  "Upgrade ECA Emacs package.

If installed via package-vc, uses package-vc-upgrade.
Otherwise uses package.el to refresh and reinstall."
  (interactive)
  (cond
   ((and (featurep 'package-vc)
         (alist-get 'eca package-vc-selected-packages))
    (message "Upgrading ECA via package-vc...")
    (package-vc-upgrade 'eca)
    (message "ECA upgraded. Restart Emacs or re-evaluate for changes."))
   ((package-installed-p 'eca)
    (if (y-or-n-p "Refresh package archives and upgrade ECA? ")
        (progn
          (package-refresh-contents)
          (package-install 'eca)
          (message "ECA upgraded successfully"))
      (message "Upgrade cancelled")))
   (t
    (user-error "ECA is not installed as a package"))))

(defun ai-code-eca-install-skills ()
  "Install skills for ECA by prompting for a skills repo URL.
ECA manages skills as files under ~/.eca/ or project .eca/ directory."
  (interactive)
  (ai-code-eca--ensure-available)
  (let* ((url (read-string "Skills repo URL for ECA: " nil nil
                           "https://github.com/obra/superpowers"))
         (prompt (format
                  "Install the skill from %s for this ECA session. Read the repository README to understand the installation instructions and follow them. Set up the skill files under the appropriate directory (e.g. ~/.eca/ or the project .eca/ directory) so they are available in future sessions."
                  url)))
    (ai-code-eca-send prompt)))

;;; Aliases for menu compatibility

(defalias 'ai-code-eca-switch-session 'ai-code-eca-switch-to-session
  "Alias for `ai-code-eca-switch-to-session' for menu compatibility.")

;;; ==============================================================================
;;; Binary Upgrade System
;;; ==============================================================================

(defvar ai-code-eca-upgrade--pinned-version "0.115.0"
  "Pinned fallback version when binary and GitHub API are unavailable.")

(defun ai-code-eca-upgrade--resolve-version ()
  "Return the current eca version string as \"X.Y.Z\"."
  (cl-flet ((parse-semver (raw)
              (and (stringp raw)
                   (string-match "\\([0-9]+\\.[0-9]+\\.[0-9]+\\)" raw)
                   (match-string 1 raw))))
    (or
     (when-let* ((bin (executable-find "eca"))
                 (raw (string-trim
                       (shell-command-to-string
                        (concat (shell-quote-argument bin) " --version")))))
       (parse-semver raw))
     (when-let* ((curl (or (executable-find "curl") (executable-find "curl.exe")))
                 (raw  (string-trim
                        (shell-command-to-string
                         (format "%s -s -f --max-time 5 %s"
                                 (shell-quote-argument curl)
                                 "https://api.github.com/repos/editor-code-assistant/eca/releases/latest"))))
                 ((not (string-blank-p raw)))
                 ((string-match "\"tag_name\"\\s-*:\\s-*\"\\([^\"]+\\)\"" raw)))
       (parse-semver (match-string 1 raw)))
     (when (featurep 'package)
       (when-let* ((pkg-desc (assq 'eca package-alist))
                   (ver-list (package-desc-version (cadr pkg-desc))))
         (package-version-join ver-list)))
     ai-code-eca-upgrade--pinned-version)))

(defvar ai-code-eca-upgrade--last-check-file
  (expand-file-name "eca/eca-update-check" user-emacs-directory)
  "Timestamp file for last auto-update check.")

(defun ai-code-eca-upgrade--check-due-p ()
  "Return non-nil when more than 24 h have passed since last check."
  (not (and (file-exists-p ai-code-eca-upgrade--last-check-file)
            (let* ((attrs (file-attributes ai-code-eca-upgrade--last-check-file))
                   (mtime (file-attribute-modification-time attrs))
                   (age   (float-time (time-subtract (current-time) mtime))))
              (< age 86400)))))

(with-eval-after-load 'eca-process

  (setopt eca-server-install-path (expand-file-name (if (eq system-type 'windows-nt) "eca/eca.exe" "eca/eca")
                                                    user-emacs-directory)
          eca-server-version-file-path (expand-file-name "eca/eca-version" user-emacs-directory))

  (let* ((vfile (or (bound-and-true-p eca-server-version-file-path)
                    (expand-file-name "eca/eca-version" user-emacs-directory)))
         (version (ai-code-eca-upgrade--resolve-version)))
    (unless (file-exists-p vfile)
      (make-directory (file-name-directory vfile) t)
      (write-region version nil vfile nil 'silent))
    (setq eca-process--latest-server-version version))

  (defun ai-code-eca-upgrade--curl-download-string (url)
    "Rewrite URL to /releases/latest for faster download."
    (let* ((fast-url (replace-regexp-in-string
                      "/repos/\\([^/]+/[^/]+\\)/releases$"
                      "/repos/\\1/releases/latest"
                      url))
           (curl-cmd (or (executable-find "curl")
                         (executable-find "curl.exe"))))
      (unless curl-cmd
        (error "Curl not found"))
      (let ((output (shell-command-to-string
                     (format "%s -L -s -S -f --compressed %s"
                             (shell-quote-argument curl-cmd)
                             (shell-quote-argument fast-url)))))
        (when (string-blank-p output)
          (error "Curl failed to download from %s" fast-url))
        (if (string-match-p "^\\s-*\\[" output)
            output
          (concat "[" output "]")))))

  (advice-add 'eca--curl-download-string :override #'ai-code-eca-upgrade--curl-download-string)

  (setopt eca-server-download-method 'curl)

  (cl-defun ai-code-eca-upgrade--curl-download-file (&key url path on-done)
    "Async binary download using aria2c/wget/curl."
    (let* ((aria2 (executable-find "aria2c"))
           (wget  (executable-find "wget"))
           (curl  (or (executable-find "curl") (executable-find "curl.exe")))
           (cmd   (cond
                   (aria2 (list aria2
                                "-c"
                                "-x" "16"
                                "-s" "16"
                                "-k" "1M"
                                "--file-allocation=none"
                                "--summary-interval=0"
                                "--auto-file-renaming=false"
                                "-d" (file-name-directory path)
                                "-o" (file-name-nondirectory path)
                                url))
                   (wget  (list wget "-c" "-O" path url))
                   (curl  (list curl "-C" "-" "-L" "-f" "-o" path url))
                   (t (error "No downloader found (aria2c/wget/curl)")))))
      (eca-info "Fast-downloading %s -> %s [%s]"
                url path (file-name-nondirectory (car cmd)))
      (make-process
       :name     "eca-download"
       :command  cmd
       :noquery  t
       :sentinel (lambda (proc _event)
                   (unless (process-live-p proc)
                     (if (= 0 (process-exit-status proc))
                         (progn
                           (eca-info "Download complete: %s" path)
                           (funcall on-done))
                       (message "eca download failed (exit %d): %s"
                                (process-exit-status proc) path)))))))

  (advice-add 'eca--curl-download-file :override #'ai-code-eca-upgrade--curl-download-file)

  (defun ai-code-eca-upgrade--auto-maybe ()
    "Run binary upgrade silently if a day has passed since last check."
    (when (and ai-code-eca-upgrade-auto-enabled
               (featurep 'eca-process)
               (ai-code-eca-upgrade--check-due-p))
      (condition-case err
          (let ((display-buffer-alist
                 (cons '("\\*eca-update\\*" (display-buffer-no-window))
                       display-buffer-alist)))
            (ai-code-eca-upgrade-binary t))
        (error (message "eca auto-update check failed: %s"
                        (error-message-string err))))
      (make-directory (file-name-directory ai-code-eca-upgrade--last-check-file) t)
      (write-region "" nil ai-code-eca-upgrade--last-check-file nil 'silent)))

  (run-with-idle-timer ai-code-eca-upgrade-auto-idle-seconds t
                        #'ai-code-eca-upgrade--auto-maybe))

;;;###autoload
(defun ai-code-eca-upgrade-binary (&optional silent)
  "Check for a newer ECA binary and update if available.

Compares installed version against latest GitHub release.
If update available, downloads platform zip, verifies SHA256,
installs new binary into `eca-server-install-path'.

Progress shown in *eca-update* buffer.
With prefix arg or SILent non-nil, suppress buffer."
  (interactive "P")
  (let* ((buf (and (not silent) (get-buffer-create "*eca-update*")))
         (log (lambda (fmt &rest args)
                (when buf
                  (with-current-buffer buf
                    (goto-char (point-max))
                    (insert (apply #'format fmt args) "\n")
                    (when-let* ((win (get-buffer-window buf t)))
                      (set-window-point win (point-max)))))
                (apply #'message fmt args))))
    (when buf
      (with-current-buffer buf
        (erase-buffer)
        (insert "=== ECA Binary Update ===\n\n"))
      (display-buffer buf))
    (let ((curl (or (executable-find "curl") (executable-find "curl.exe"))))
      (unless curl (user-error "ai-code-eca-upgrade-binary: curl not found"))
      (funcall log "Checking latest version from GitHub...")
      (let* ((api-url "https://api.github.com/repos/editor-code-assistant/eca/releases/latest")
             (raw (string-trim
                   (shell-command-to-string
                    (format "%s -s -f --max-time 10 %s"
                            (shell-quote-argument curl)
                            (shell-quote-argument api-url)))))
             (_ (when (string-blank-p raw)
                  (user-error "ai-code-eca-upgrade-binary: failed to reach GitHub API")))
             (_ (unless (string-match "\"tag_name\"\\s-*:\\s-*\"\\([^\"]+\\)\"" raw)
                  (user-error "ai-code-eca-upgrade-binary: could not parse tag_name")))
             (latest (match-string 1 raw))
             (latest-bare (if (string-match "^v?\\(.*\\)" latest)
                              (match-string 1 latest)
                            latest))
             (installed (ai-code-eca-upgrade--resolve-version))
             (installed-bare (if (string-match "^v?\\(.*\\)" installed)
                                 (match-string 1 installed)
                               installed)))
        (funcall log "Latest:    %s" latest)
        (funcall log "Installed: %s" installed-bare)
        (if (not (string-version-lessp installed-bare latest-bare))
            (funcall log "\nAlready up to date.")
          (unless (fboundp 'eca-process--download-url)
            (user-error "ai-code-eca-upgrade-binary: eca-process not loaded"))
          (let* ((zip-url   (eca-process--download-url latest))
                 (store-path eca-server-install-path)
                 (zip-path  (concat store-path ".zip"))
                 (sha-url   (concat zip-url ".sha256"))
                 (sha-path  (concat zip-path ".sha256"))
                 (old-path  (concat store-path ".old"))
                 (temp-dir  (concat (file-name-directory store-path) "eca-update-temp")))
            (funcall log "\nDownloading %s ..." zip-url)
            (make-directory (file-name-directory store-path) t)
            (let ((exit (call-process curl nil buf t
                                      "-L" "-f" "--progress-bar"
                                      "-o" zip-path zip-url)))
              (unless (= exit 0)
                (user-error "ai-code-eca-upgrade-binary: download failed (curl exit %d)" exit)))
            (funcall log "Download complete.")
            (funcall log "\nVerifying SHA256...")
            (call-process curl nil nil nil
                          "-s" "-f" "--max-time" "5" "-o" sha-path sha-url)
            (let* ((sha-available (and (file-exists-p sha-path)
                                       (> (file-attribute-size
                                           (file-attributes sha-path)) 0)))
                   (shasum-cmd
                    (cond
                     ((executable-find "sha256sum") "sha256sum")
                     ((executable-find "shasum")    "shasum -a 256")
                     (t nil))))
              (cond
               ((not sha-available)
                (funcall log "Warning: SHA256 asset not available — skipping verification."))
               ((not shasum-cmd)
                (funcall log "Warning: sha256sum/shasum not found — skipping verification."))
               (t
                (let* ((expected (car (split-string
                                       (with-temp-buffer
                                         (insert-file-contents sha-path)
                                         (buffer-string)))))
                       (actual (car (split-string
                                     (shell-command-to-string
                                      (format "%s %s"
                                              shasum-cmd
                                              (shell-quote-argument zip-path)))))))
                  (if (string= expected actual)
                      (funcall log "SHA256 OK (%s)." actual)
                    (user-error "SHA256 mismatch — expected %s, got %s. Aborting."
                                expected actual))))))
            (funcall log "\nExtracting...")
            (unless (executable-find "unzip")
              (user-error "unzip not found — please install unzip"))
            (when (file-exists-p temp-dir)
              (delete-directory temp-dir t))
            (make-directory temp-dir t)
            (let ((exit (call-process "unzip" nil buf t
                                      "-o" zip-path "-d" temp-dir)))
              (unless (= exit 0)
                (user-error "ai-code-eca-upgrade-binary: unzip failed (exit %d)" exit)))
            (let ((new-bin (expand-file-name
                            (file-name-nondirectory store-path) temp-dir)))
              (unless (file-exists-p new-bin)
                (user-error "ai-code-eca-upgrade-binary: expected binary not found after extraction: %s" new-bin))
              (when (file-exists-p old-path)
                (ignore-errors (delete-file old-path)))
              (when (file-exists-p store-path)
                (rename-file store-path old-path))
              (rename-file new-bin store-path)
              (set-file-modes store-path #o0700))
            (ignore-errors (delete-directory temp-dir t))
            (ignore-errors (delete-file zip-path))
            (ignore-errors (delete-file sha-path))
            (ignore-errors (delete-file old-path))
            (write-region latest nil
                          eca-server-version-file-path nil 'silent)
            (setq eca-process--latest-server-version latest)
            (funcall log "\nECA binary updated to v%s. Done." latest)))))))

(defun ai-code-eca-upgrade-show ()
  "Show ECA upgrade status and offer manual upgrade.
Pops up a buffer with version info and buttons to check/update."
  (interactive)
  (let* ((installed (ai-code-eca-upgrade--resolve-version))
         (buf (get-buffer-create "*eca-update*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== ECA Upgrade ===\n\n")
      (insert (format "Installed version: %s\n\n" installed))
      (insert "Actions:\n")
      (insert-text-button "Upgrade binary"
                          'action (lambda (_)
                                    (call-interactively #'ai-code-eca-upgrade-binary))
                          'follow-link t
                          'help-echo "Upgrade ECA binary from GitHub")
      (insert "\n")
      (insert-text-button "Upgrade package"
                          'action (lambda (_)
                                    (call-interactively #'ai-code-eca-upgrade-package))
                          'follow-link t
                          'help-echo "Upgrade ECA Emacs package")
      (insert "\n")
      (insert-text-button "Force re-download"
                          'action (lambda (_)
                                    (when (yes-or-no-p "Re-download even if up to date? ")
                                      (call-interactively #'ai-code-eca-upgrade-binary)))
                          'follow-link t
                          'help-echo "Force re-download and reinstall binary")
      (insert "\n\n")
      (insert "Auto-update is currently: ")
      (insert-text-button (if ai-code-eca-upgrade-auto-enabled "ENABLED" "DISABLED")
                          'action (lambda (_)
                                    (customize-set-variable 'ai-code-eca-upgrade-auto-enabled
                                                            (not ai-code-eca-upgrade-auto-enabled))
                                    (ai-code-eca-upgrade-show))
                          'follow-link t
                          'help-echo "Click to toggle auto-update")
      (insert "\n")
      (goto-char (point-min)))
    (display-buffer buf '(display-buffer-same-window))))

;;;###autoload
(defun ai-code-eca-upgrade (&optional arg)
  "Upgrade ECA.
Without ARG, upgrade the ECA binary from GitHub releases.
With \\[universal-argument] (C-u), upgrade the ECA Emacs package.
With \\[universal-argument] \\[universal-argument] (C-u C-u), show upgrade status."
  (interactive "P")
  (cond
   ((equal arg '(16)) (ai-code-eca-upgrade-show))
   ((equal arg '(4))  (ai-code-eca-upgrade-package))
   (t                 (ai-code-eca-upgrade-binary))))

(provide 'ai-code-eca)

;;; ai-code-eca.el ends here
