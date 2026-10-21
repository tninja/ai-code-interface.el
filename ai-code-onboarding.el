;;; ai-code-onboarding.el --- Quickstart onboarding for AI Code -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides a small onboarding surface for first-run AI Code usage.

;;; Code:

(require 'button)

(declare-function ai-code--backend-spec "ai-code-backends" (key))
(declare-function ai-code-ask-question "ai-code-discussion" (arg))
(declare-function ai-code-cli-start "ai-code-backends" (&optional arg))
(declare-function ai-code-code-change "ai-code-change" (arg))
(declare-function ai-code-current-backend-label "ai-code-backends" ())
(declare-function ai-code-open-prompt-file "ai-code-prompt-mode" ())
(declare-function ai-code-select-backend "ai-code-backends" ())
(declare-function ai-code-backends-infra--find-session-buffers "ai-code-backends-infra" (prefix directory))
(declare-function ai-code-backends-infra--session-working-directory "ai-code-backends-infra" ())

(defvar ai-code-selected-backend)

(defgroup ai-code-onboarding nil
  "Onboarding helpers for AI Code."
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-onboarding-auto-show t
  "When non-nil, show the quickstart automatically for first-run usage."
  :type 'boolean
  :group 'ai-code-onboarding)

;;;###autoload
(defcustom ai-code-onboarding-seen nil
  "Whether the user has already seen the onboarding quickstart."
  :type 'boolean
  :group 'ai-code-onboarding)

(defconst ai-code-onboarding-buffer-name "*AI Code Quick Start*"
  "Buffer name used for the onboarding quickstart.")

(define-derived-mode ai-code-onboarding-mode special-mode "AI Code Quick Start"
  "Major mode for the AI Code onboarding quickstart buffer.")

(defun ai-code-onboarding--current-backend-label ()
  "Return the current backend label, or a safe fallback."
  (condition-case nil
      (or (ai-code-current-backend-label) "<none>")
    (error "<none>")))

(defun ai-code-onboarding--backend-spec ()
  "Return the current backend spec plist, or nil."
  (when ai-code-selected-backend
    (cdr (ai-code--backend-spec ai-code-selected-backend))))

(defun ai-code-onboarding--session-prefix ()
  "Return the CLI session prefix for the current backend, or nil."
  (plist-get (ai-code-onboarding--backend-spec) :cli))

(defun ai-code-onboarding--session-available-p ()
  "Return non-nil when a CLI session exists for the current backend."
  (when (and (fboundp 'ai-code-backends-infra--session-working-directory)
             (fboundp 'ai-code-backends-infra--find-session-buffers))
    (let ((working-dir (ai-code-backends-infra--session-working-directory))
          (session-prefix (ai-code-onboarding--session-prefix)))
      (and working-dir
           session-prefix
           (ai-code-backends-infra--find-session-buffers session-prefix working-dir)))))

(defun ai-code-onboarding--session-status-line ()
  "Return a short session status line for the current backend."
  (cond
   ((null ai-code-selected-backend)
    "Session: no backend selected")
   ((not (ai-code-onboarding--session-prefix))
    "Session: status unavailable for this backend")
   ((not (and (fboundp 'ai-code-backends-infra--session-working-directory)
              (fboundp 'ai-code-backends-infra--find-session-buffers)))
    "Session: status unavailable")
   ((ai-code-onboarding--session-available-p)
    "Session: available")
   (t
    "Session: not started")))

(defun ai-code-onboarding--insert-heading (title)
  "Insert section TITLE."
  (insert title "\n")
  (insert (make-string (length title) ?-) "\n"))

(defun ai-code-onboarding--insert-line (text)
  "Insert TEXT followed by a newline."
  (insert text "\n"))

(defun ai-code-onboarding--insert-command-button (label command)
  "Insert a LABEL button for COMMAND."
  (insert-text-button
   label
   'follow-link t
   'action (lambda (_button)
             (call-interactively command)))
  (insert "  "))

(defun ai-code-onboarding--readme-path ()
  "Return the local README path for the package."
  (expand-file-name "README.org"
                    (file-name-directory
                     (or load-file-name
                         (locate-library "ai-code")
                         default-directory))))

(defun ai-code-onboarding--open-readme ()
  "Open the package README."
  (interactive)
  (find-file-other-window (ai-code-onboarding--readme-path)))

(defun ai-code-onboarding--render ()
  "Render the onboarding quickstart buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (ai-code-onboarding--insert-line "AI Code Quick Start")
    (insert "\n")
    (ai-code-onboarding--insert-line
     (format "Current backend: %s" (ai-code-onboarding--current-backend-label)))
    (ai-code-onboarding--insert-line (ai-code-onboarding--session-status-line))
    (ai-code-onboarding--insert-line
     "You only need three keys to get productive.")
    (insert "\n")
    (ai-code-onboarding--insert-heading "Start Here")
    (ai-code-onboarding--insert-line "a  Start AI session")
    (ai-code-onboarding--insert-line "z  Switch back to the AI session")
    (ai-code-onboarding--insert-line "s  Switch backend")
    (insert "\n")
    (ai-code-onboarding--insert-heading "Most Useful Actions")
    (ai-code-onboarding--insert-line "c  Change current function or selected region")
    (ai-code-onboarding--insert-line "q  Ask about the current function or file")
    (ai-code-onboarding--insert-line "i  Implement TODO at point")
    (insert "\n")
    (ai-code-onboarding--insert-heading "How Context Works")
    (ai-code-onboarding--insert-line "Active region wins.")
    (ai-code-onboarding--insert-line
     "Otherwise the current function is used when available.")
    (ai-code-onboarding--insert-line
     "C-u adds broader context such as clipboard or file/repository context.")
    (insert "\n")
    (ai-code-onboarding--insert-heading "Try It Now")
    (ai-code-onboarding--insert-command-button "Start Session" #'ai-code-cli-start)
    (ai-code-onboarding--insert-command-button "Ask About This Function" #'ai-code-ask-question)
    (ai-code-onboarding--insert-command-button "Change Selected Code" #'ai-code-code-change)
    (ai-code-onboarding--insert-command-button "Open Prompt File" #'ai-code-open-prompt-file)
    (ai-code-onboarding--insert-command-button "Switch Backend" #'ai-code-select-backend)
    (insert "\n\n")
    (ai-code-onboarding--insert-heading "More")
    (ai-code-onboarding--insert-command-button "Do Not Show Again" #'ai-code-onboarding-disable-auto-show)
    (ai-code-onboarding--insert-command-button "Show README" #'ai-code-onboarding--open-readme)
    (ai-code-onboarding--insert-command-button "Close" #'quit-window)
    (insert "\n")))

(defun ai-code-onboarding--persist-seen-state ()
  "Persist onboarding seen state for interactive sessions."
  (unless noninteractive
    (customize-save-variable 'ai-code-onboarding-seen t)))

(defun ai-code-onboarding--restore-origin-context (origin-window origin-buffer)
  "Restore ORIGIN-WINDOW and ORIGIN-BUFFER after auto-showing quickstart."
  (when (window-live-p origin-window)
    (select-window origin-window))
  (when (buffer-live-p origin-buffer)
    (set-buffer origin-buffer)))

;;;###autoload
(defun ai-code-onboarding-open-quickstart ()
  "Open the onboarding quickstart buffer."
  (interactive)
  (setq ai-code-onboarding-seen t)
  (ai-code-onboarding--persist-seen-state)
  (let ((buffer (get-buffer-create ai-code-onboarding-buffer-name)))
    (with-current-buffer buffer
      (ai-code-onboarding-mode)
      (ai-code-onboarding--render)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun ai-code-onboarding--persist-disable-state ()
  "Persist onboarding opt-out state for interactive sessions."
  (unless noninteractive
    (customize-save-variable 'ai-code-onboarding-auto-show nil)
    (ai-code-onboarding--persist-seen-state)))

;;;###autoload
(defun ai-code-onboarding-disable-auto-show ()
  "Disable future auto-display of the onboarding quickstart."
  (interactive)
  (setq ai-code-onboarding-auto-show nil
        ai-code-onboarding-seen t)
  (ai-code-onboarding--persist-disable-state)
  (quit-window))

(defun ai-code-onboarding-maybe-show-quickstart ()
  "Open quickstart once when auto-show is enabled."
  (when (and ai-code-onboarding-auto-show
             (not ai-code-onboarding-seen))
    (let ((origin-window (selected-window))
          (origin-buffer (current-buffer)))
      (ai-code-onboarding-open-quickstart)
      (ai-code-onboarding--restore-origin-context origin-window origin-buffer))))

(defun ai-code-onboarding-backend-hint ()
  "Return a backend-specific next-step hint."
  (let* ((spec (ai-code-onboarding--backend-spec))
         (agent-file (plist-get spec :agent-file)))
    (concat
     (format "Backend switched to %s. Next: a to start, g to edit config"
             (ai-code-onboarding--current-backend-label))
     (if agent-file
         (format ", G to open %s." agent-file)
       "."))))

(defun ai-code-onboarding-show-backend-switch-hint ()
  "Show a backend-specific next-step hint."
  (message "%s" (ai-code-onboarding-backend-hint)))

(provide 'ai-code-onboarding)

;;; ai-code-onboarding.el ends here
