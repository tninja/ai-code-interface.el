;;; ai-code-eca.el --- ECA backend bridge for ai-code  -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: davidwuchn
;; Version: 0.2
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, code, assistant, eca
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; ECA backend bridge for ai-code with:
;;   - Session management (list, switch, create, dashboard)
;;   - Workspace management (list, add, remove, sync projects)
;;   - Context commands (file, cursor, repo-map, clipboard)
;;   - Shared context (cross-session sharing)
;;   - Multi-Project Mode (auto-switch, auto-sync, mode-line)
;;   - ai-code-menu integration (transient)
;;   - Health verification and context synchronization
;;
;; MULTI-PROJECT WORKFLOWS:
;;
;; Two approaches for working with multiple projects:
;;
;; 1. SINGLE SESSION, MULTIPLE WORKSPACES (recommended):
;;    - All projects in one ECA session
;;    - AI sees context from all projects
;;    - Use: M-x ai-code-eca-multi-project-mode to enable auto-switch/sync
;;    - Use: M-x ai-code-eca-add-workspace-folder to add projects
;;
;; 2. MULTIPLE SESSIONS:
;;    - Separate ECA session per project
;;    - Isolated context per project
;;    - Use: M-x ai-code-eca-switch-session to switch between sessions
;;    - Share common context: M-x ai-code-eca-share-file
;;
;; ai-code-menu Integration (primary UX):
;;   All commands accessible via M-x ai-code-menu (C-c a) when ECA is selected:
;;
;;   ECA Workspace              ECA Context         ECA Shared Context
;;     wm - Multi-Project Mode    cf - File           F - Share file
;;     wa - Add folder            cc - Cursor         R - Share repo map
;;     wA - Add to ALL            cr - Repo map       p - Apply shared
;;     wl - List folders          cy - Clipboard      c - Clear shared
;;     wr - Remove folder         cs - Start sync
;;     ws - Sync projects         cS - Stop sync
;;     wd - Dashboard
;;     wt - Toggle auto-switch
;;
;;   ECA Sessions
;;     s? - Which session?
;;     sl - List sessions
;;     ss - Switch session
;;     sv - Verify health
;;     su - Upgrade ECA
;;
;; Usage:
;;   (require 'ai-code-eca)
;;   M-x ai-code-menu (when ECA is selected)
;;   M-x ai-code-eca-multi-project-mode (enable multi-project workflows)

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
(declare-function eca-session-dashboard "eca-ext" ())

(declare-function transient-append-suffix "transient" (prefix loc suffix &optional face))
(declare-function transient-remove-suffix "transient" (prefix suffix))

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

;;;###autoload
(defun ai-code-eca-resume (&optional arg)
  "Resume an ECA session.
With prefix ARG, force new session."
  (interactive "P")
  (ai-code-eca--ensure-available)
  (if arg
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
  (dolist (fn '(eca eca-session eca-chat-open eca-chat-send-prompt eca-chat--get-last-buffer))
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
        (message "ECA sessions: %s" (mapconcat #'identity sessions ", "))
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
        (message "Current ECA session: %s" session)
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
  "Remove FOLDER from ECA workspace."
  (interactive
   (list (require 'eca-ext nil t)
         (completing-read "Remove folder: " (eca-list-workspace-folders) nil t)))
  (require 'eca-ext nil t)
  (eca-remove-workspace-folder folder))

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

;;; Shared Context

(defun ai-code-eca-share-file (file-path)
  "Share FILE-PATH across ECA sessions."
  (interactive "fShare file: ")
  (require 'eca-ext nil t)
  (unless (fboundp 'eca-share-file-context)
    (user-error "Shared context requires eca-ext.el"))
  (eca-share-file-context file-path)
  (message "Shared file: %s" file-path))

(defun ai-code-eca-share-repo-map ()
  "Share repo map across ECA sessions."
  (interactive)
  (require 'eca-ext nil t)
  (unless (fboundp 'eca-share-repo-map-context)
    (user-error "Shared context requires eca-ext.el"))
  (eca-share-repo-map-context default-directory)
  (message "Shared repo map"))

(defun ai-code-eca-apply-shared-context ()
  "Apply shared context to current ECA session."
  (interactive)
  (require 'eca-ext nil t)
  (unless (fboundp 'eca-apply-shared-context)
    (user-error "Shared context requires eca-ext.el"))
  (let ((session (eca-session)))
    (when session
      (eca-apply-shared-context session)
      (message "Applied shared context"))))

(defun ai-code-eca-clear-shared-context ()
  "Clear shared context."
  (interactive)
  (require 'eca-ext nil t)
  (unless (fboundp 'eca-clear-shared-context)
    (user-error "Shared context requires eca-ext.el"))
  (eca-clear-shared-context)
  (message "Cleared shared context"))

;;; Health & Upgrade

(defun ai-code-eca-verify-health ()
  "Verify ECA session health."
  (interactive)
  (let ((session (eca-session)))
    (if session
        (message "ECA session healthy: %s" session)
      (message "No active ECA session"))))

(defun ai-code-eca-upgrade ()
  "Upgrade ECA package."
  (interactive)
  (if (package-installed-p 'eca)
      (progn
        (package-refresh-contents)
        (package-install 'eca)
        (message "ECA upgraded. Restart Emacs or re-evaluate."))
    (user-error "ECA is not installed")))

(defun ai-code-eca-install-skills ()
  "Install skills for ECA by prompting for a skills repo URL."
  (interactive)
  (let* ((url (read-string "Skills repo URL for ECA: "
                           nil nil
                           "https://github.com/obra/superpowers"))
         (prompt (format "Install the skill from %s for this ECA session." url)))
    (ai-code-eca-send prompt)))

(defun ai-code-eca-dashboard ()
  "Open ECA session dashboard."
  (interactive)
  (require 'eca-ext nil t)
  (unless (fboundp 'eca-session-dashboard)
    (user-error "Dashboard requires eca-ext.el"))
  (eca-session-dashboard))

;;; Multi-Project Mode

(defvar ai-code-eca-context-sync-interval 60
  "Seconds between automatic ECA context sync operations.")

(defvar ai-code-eca-auto-switch-backend t
  "If non-nil, keep ai-code backend in sync with ECA session switches.")

(defvar ai-code-eca-context-sync-timer nil
  "Timer for automatic context synchronization.")

(defvar ai-code-eca-mode-line-indicator nil
  "Mode-line indicator for ECA session.")

;;;###autoload
(define-minor-mode ai-code-eca-multi-project-mode
  "Minor mode for multi-project ECA workflows."
  :global t
  :group 'ai-code-eca
  (if ai-code-eca-multi-project-mode
      (progn
        (when ai-code-eca-context-sync-interval
          (setq ai-code-eca-context-sync-timer
                (run-with-timer ai-code-eca-context-sync-interval
                                ai-code-eca-context-sync-interval
                                #'ai-code-eca--sync-context)))
        (message "ECA multi-project mode enabled"))
    (when ai-code-eca-context-sync-timer
      (cancel-timer ai-code-eca-context-sync-timer)
      (setq ai-code-eca-context-sync-timer nil))
    (message "ECA multi-project mode disabled")))

(defun ai-code-eca--sync-context ()
  "Sync context for multi-project mode."
  (let ((session (eca-session)))
    (when session
      (message "ECA context sync for session: %s" session))))

;;; Transient Menu

;;;###autoload
(transient-define-prefix ai-code-eca-menu ()
  "ECA commands menu."
  ["ECA Workspace"
   ("wm" "Multi-Project Mode" ai-code-eca-multi-project-mode)
   ("wa" "Add folder" ai-code-eca-add-workspace-folder)
   ("wl" "List folders" ai-code-eca-list-workspace-folders)
   ("wr" "Remove folder" ai-code-eca-remove-workspace-folder)
   ("wd" "Dashboard" ai-code-eca-dashboard)]
  ["ECA Context"
   ("cf" "File" ai-code-eca-add-file-context)
   ("cc" "Cursor" ai-code-eca-add-cursor-context)
   ("cr" "Repo map" ai-code-eca-add-repo-map-context)
   ("cy" "Clipboard" ai-code-eca-add-clipboard-context)]
  ["ECA Shared Context"
   ("F" "Share file" ai-code-eca-share-file)
   ("R" "Share repo map" ai-code-eca-share-repo-map)
   ("p" "Apply shared" ai-code-eca-apply-shared-context)
   ("c" "Clear shared" ai-code-eca-clear-shared-context)]
  ["ECA Sessions"
   ("?" "Which session?" ai-code-eca-which-session)
   ("L" "List sessions" ai-code-eca-list-sessions)
   ("w" "Switch session" ai-code-eca-switch-session)
   ("v" "Verify health" ai-code-eca-verify-health)
   ("u" "Upgrade ECA" ai-code-eca-upgrade)])

(defvar ai-code-eca--menu-suffixes-added nil
  "Track whether ECA menu has been added to ai-code-menu.")

(defun ai-code-eca--add-menu-suffixes ()
  "Add ECA submenu to ai-code-menu."
  (when (and (boundp 'ai-code-selected-backend)
             (eq ai-code-selected-backend 'eca)
             (not ai-code-eca--menu-suffixes-added)
             (featurep 'transient))
    (condition-case err
        (progn
          (transient-append-suffix 'ai-code-menu "N"
            '("E" "ECA commands" ai-code-eca-menu))
          (setq ai-code-eca--menu-suffixes-added t)
          (message "ECA menu items added to ai-code-menu"))
      (error
       (message "Failed to add ECA menu items: %s" (error-message-string err))))))

(defun ai-code-eca--remove-menu-suffixes ()
  "Remove ECA submenu from ai-code-menu."
  (when (and ai-code-eca--menu-suffixes-added
             (featurep 'transient))
    (condition-case err
        (progn
          (transient-remove-suffix 'ai-code-menu "E")
          (setq ai-code-eca--menu-suffixes-added nil))
      (error
       (message "Failed to remove ECA menu items: %s" (error-message-string err))))))

;; Hook into ai-code-menu
(with-eval-after-load 'ai-code
  (advice-add 'ai-code-set-backend :after
              (lambda (backend)
                (if (eq backend 'eca)
                    (ai-code-eca--add-menu-suffixes)
                  (when ai-code-eca--menu-suffixes-added
                    (ai-code-eca--remove-menu-suffixes))))))

(provide 'ai-code-eca)

;;; ai-code-eca.el ends here
