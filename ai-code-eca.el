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
(require 'package)
(require 'subr-x)
(require 'ai-code-backends)
(require 'ai-code-input nil t)
(require 'eca-ext nil t)
(require 'transient nil t)

(declare-function eca "eca" (&optional arg))
(declare-function eca-session "eca-util" ())
(declare-function eca-chat-open "eca-chat" (session))
(declare-function eca-chat-send-prompt "eca-chat" (session message))
(declare-function eca-chat--get-last-buffer "eca-chat" (session))
(declare-function eca-info "eca-util" (format-string &rest args))
(declare-function eca--session-id "eca-util" (session))
(declare-function eca--session-status "eca-util" (session))
(declare-function eca--session-workspace-folders "eca-util" (session))
(declare-function eca-chat-add-workspace-root "eca-chat" ())

(declare-function eca-list-sessions "eca-ext" ())
(declare-function eca-switch-to-session "eca-ext" (&optional session-id))
(declare-function eca-list-workspace-folders "eca-ext" (&optional session))
(declare-function eca-add-workspace-folder "eca-ext" (folder &optional session))
(declare-function eca-add-workspace-folder-all-sessions "eca-ext" (folder))
(declare-function eca-remove-workspace-folder "eca-ext" (folder &optional session))
(declare-function eca-chat-add-file-context "eca-ext" (session file-path))
(declare-function eca-chat-add-repo-map-context "eca-ext" (session))
(declare-function eca-chat-add-cursor-context "eca-ext" (session file-path position))
(declare-function eca-chat-add-clipboard-context "eca-ext" (session content))
(declare-function eca-share-file-context "eca-ext" (file-path))
(declare-function eca-share-repo-map-context "eca-ext" (project-root))
(declare-function eca-apply-shared-context "eca-ext" (session))
(declare-function eca-clear-shared-context "eca-ext" ())

(declare-function transient-append-suffix "transient" (prefix loc suffix &optional face))
(declare-function transient-remove-suffix "transient" (prefix suffix))

;;; Customization

(defvar eca-auto-switch-session nil
  "If non-nil, auto-switch ECA session based on project.")

(defcustom eca-auto-apply-shared-context t
  "If non-nil, automatically apply shared context when switching ECA sessions."
  :type 'boolean
  :group 'ai-code)

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

;;;###autoload
(defun ai-code-eca-send (line)
  "Send LINE to ECA chat."
  (interactive "sECA> ")
  (ai-code-eca--ensure-available)
  (let ((session (eca-session)))
    (if session
        (progn
          (eca-chat-open session)
          (eca-chat-send-prompt session line))
      (user-error "No ECA session. Run M-x ai-code-eca-start first"))))

(defun ai-code-eca--ensure-available ()
  "Ensure ECA package and functions are available."
  (unless (require 'eca nil t)
    (user-error "ECA not available. Install with: M-x package-install RET eca RET"))
  (dolist (fn '(eca eca-session eca-chat-open eca-chat--get-last-buffer))
    (unless (fboundp fn)
      (user-error "ECA missing function: %s. Reinstall eca package" fn))))

(defun ai-code-eca--ensure-chat-buffer (session)
  "Ensure ECA chat buffer for SESSION exists and return it."
  (let ((buf (eca-chat--get-last-buffer session)))
    (unless (and buf (get-buffer-window buf))
      (eca-chat-open session))
    buf))

;;; Session Management

(defun ai-code-eca-get-sessions ()
  "Get list of ECA sessions."
  (require 'eca-ext nil t)
  (when (fboundp 'eca-list-sessions)
    (eca-list-sessions)))

(defun ai-code-eca-list-sessions ()
  "List ECA sessions."
  (interactive)
  (let ((sessions (ai-code-eca-get-sessions)))
    (if sessions
        (message "ECA sessions: %s"
                 (mapconcat (lambda (s)
                              (format "#%d: %s (%d chats)"
                                      (plist-get s :id)
                                      (string-join (plist-get s :workspace-folders) ", ")
                                      (plist-get s :chat-count)))
                            sessions " | "))
      (message "No ECA sessions found"))))

(defun ai-code-eca-switch-session (&optional session-id)
  "Switch to ECA SESSION-ID."
  (interactive)
  (require 'eca-ext nil t)
  (unless (fboundp 'eca-switch-to-session)
    (user-error "Session switching requires eca-ext.el"))
  (eca-switch-to-session session-id))

(defun ai-code-eca-which-session ()
  "Show current ECA session info."
  (interactive)
  (let ((session (eca-session)))
    (if session
        (let ((id (eca--session-id session))
              (folders (eca--session-workspace-folders session)))
          (message "ECA session %d: %s" id
                   (if folders (mapconcat #'identity folders ", ") "no workspace")))
      (message "No active ECA session"))))

;;; Workspace Management

(defun ai-code-eca-add-workspace-folder ()
  "Add workspace folder to ECA."
  (interactive)
  (unless (fboundp 'eca-chat-add-workspace-root)
    (user-error "Workspace management requires eca-ext.el"))
  (eca-chat-add-workspace-root))

(defun ai-code-eca-list-workspace-folders ()
  "List workspace folders in ECA."
  (interactive)
  (require 'eca-ext nil t)
  (let ((folders (eca-list-workspace-folders)))
    (if folders
        (message "Workspace folders: %s" (mapconcat #'identity folders "\n"))
      (message "No workspace folders"))))

(defun ai-code-eca-remove-workspace-folder (folder)
  "Remove FOLDER from ECA workspace.
Prevents removal of the last folder to keep session context."
  (interactive
   (let* ((folders (eca-list-workspace-folders)))
     (when (= (length folders) 1)
       (user-error "Cannot remove last workspace folder - session needs at least one"))
     (list (completing-read "Remove folder: " folders nil t))))
  (require 'eca-ext nil t)
  (eca-remove-workspace-folder folder))

;;;###autoload
(defun ai-code-eca-sync-project-workspaces ()
  "Sync current project roots to ECA session workspace.
Adds any project roots not already in the workspace."
  (interactive)
  (let ((session (eca-session)))
    (unless session
      (user-error "No ECA session active"))
    (let* ((project-roots (or (when (fboundp 'projectile-project-root)
                                (ignore-errors (list (projectile-project-root))))
                              (when (fboundp 'project-roots)
                                (ignore-errors (project-roots (project-current))))
                              (when buffer-file-name
                                (list (file-name-directory buffer-file-name)))))
           (existing (eca-list-workspace-folders session))
           (added 0))
      (dolist (root project-roots)
        (let ((root (expand-file-name root)))
          (unless (member root existing)
            (eca-add-workspace-folder root session)
            (cl-incf added))))
      (if (> added 0)
          (message "Added %d project roots to session %d workspace"
                   added (eca--session-id session))
        (message "All project roots already in session %d workspace"
                 (eca--session-id session))))))

;;;###autoload
(defun ai-code-eca-add-workspace-folder-all-sessions (folder)
  "Add FOLDER to all active ECA sessions.
Useful for shared libraries that should be available in all projects."
  (interactive "DAdd to all sessions: ")
  (eca-add-workspace-folder-all-sessions folder))

;;; Context Commands

(defun ai-code-eca-add-file-context (file-path)
  "Add FILE-PATH to ECA context."
  (interactive "fAdd file to ECA context: ")
  (require 'eca-ext nil t)
  (unless (fboundp 'eca-chat-add-file-context)
    (user-error "Context features require eca-ext.el"))
  (let ((session (eca-session)))
    (when session
      (ai-code-eca--ensure-chat-buffer session)
      (eca-chat-add-file-context session file-path)
      (eca-info "Added file context: %s" file-path))))

(defun ai-code-eca-add-cursor-context ()
  "Add cursor context to ECA."
  (interactive)
  (require 'eca-ext nil t)
  (unless (fboundp 'eca-chat-add-cursor-context)
    (user-error "Context features require eca-ext.el"))
  (let ((session (eca-session)))
    (when (and session buffer-file-name)
      (ai-code-eca--ensure-chat-buffer session)
      (eca-chat-add-cursor-context session buffer-file-name (point))
      (eca-info "Added cursor context: %s:%d" buffer-file-name (point)))))

(defun ai-code-eca-add-repo-map-context ()
  "Add repo map context to ECA."
  (interactive)
  (require 'eca-ext nil t)
  (unless (fboundp 'eca-chat-add-repo-map-context)
    (user-error "Context features require eca-ext.el"))
  (let ((session (eca-session)))
    (when session
      (ai-code-eca--ensure-chat-buffer session)
      (eca-chat-add-repo-map-context session)
      (eca-info "Added repo map context"))))

(defun ai-code-eca-add-clipboard-context ()
  "Add clipboard content to ECA context."
  (interactive)
  (require 'eca-ext nil t)
  (unless (fboundp 'eca-chat-add-clipboard-context)
    (user-error "Context features require eca-ext.el"))
  (let ((session (eca-session))
        (clip-content (gui-get-selection 'CLIPBOARD)))
    (when (and session clip-content)
      (ai-code-eca--ensure-chat-buffer session)
      (eca-chat-add-clipboard-context session clip-content)
      (eca-info "Added clipboard context (%d chars)" (length clip-content)))))

;;; Shared Context (auto-applies on session switch)

(advice-add 'eca-switch-to-session :after
            (lambda (&rest _)
              (when (and eca-auto-apply-shared-context
                         (fboundp 'eca-apply-shared-context)
                         (fboundp 'eca-session))
                (let ((session (eca-session)))
                  (when session
                    (condition-case nil
                        (eca-apply-shared-context session)
                      (error nil)))))))

;;; Menu Integration - Dynamic ECA group in ai-code-menu

(defvar ai-code-eca--menu-group-added nil
  "Track whether ECA group has been added to ai-code-menu.")

(defun ai-code-eca--add-menu-group ()
  "Add ECA group to ai-code-menu."
  (when (and (featurep 'transient)
             (not ai-code-eca--menu-group-added))
    (condition-case err
        (progn
          ;; Use coordinate list: '(0 -1) = last item at top level
          (transient-append-suffix 'ai-code-menu '(0 -1)
            ["ECA"
             ("?" "Which session" ai-code-eca-which-session)
             ("W" "Switch session" ai-code-eca-switch-session)
             ("A" "Add folder" ai-code-eca-add-workspace-folder)
             ("L" "List folders" ai-code-eca-list-workspace-folders)
             ("D" "Remove folder" ai-code-eca-remove-workspace-folder)
             ("F" "Add file context" ai-code-eca-add-file-context)
             ("Y" "Add cursor context" ai-code-eca-add-cursor-context)
             ("M" "Add repo map" ai-code-eca-add-repo-map-context)
             ("B" "Add clipboard" ai-code-eca-add-clipboard-context)])
          (setq ai-code-eca--menu-group-added t))
      (error
       (message "Failed to add ECA group: %s" (error-message-string err))))))

(defun ai-code-eca--remove-menu-group ()
  "Remove ECA group from ai-code-menu."
  (when ai-code-eca--menu-group-added
    (condition-case nil
        (progn
          (transient-remove-suffix 'ai-code-menu "?")
          (setq ai-code-eca--menu-group-added nil))
      (error nil))))

(with-eval-after-load 'ai-code
  (advice-add 'ai-code-set-backend :after
              (lambda (backend)
                (if (eq backend 'eca)
                    (ai-code-eca--add-menu-group)
                  (ai-code-eca--remove-menu-group)))))

(provide 'ai-code-eca)

;;; ai-code-eca.el ends here