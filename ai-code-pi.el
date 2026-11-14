;;; ai-code-pi.el --- Thin wrapper for Pi coding agent -*- lexical-binding: t; -*-

;; Author: realazy
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Provide Pi coding agent integration by reusing
;; `ai-code-backends-infra'.  See https://pi.dev/.

;;; Code:

(require 'cl-lib)
(require 'ai-code-backends)
(require 'ai-code-backends-infra)
(require 'ai-code-mcp-agent)

(defgroup ai-code-pi nil
  "Pi coding agent integration via `ai-code-backends-infra'."
  :group 'tools
  :prefix "ai-code-pi-")

(defcustom ai-code-pi-program "pi"
  "Path to the Pi coding agent executable."
  :type 'string
  :group 'ai-code-pi)

(defcustom ai-code-pi-program-switches nil
  "Command-line switches to pass to Pi on startup."
  :type '(repeat string)
  :group 'ai-code-pi)

(defcustom ai-code-pi-tui-mode 'regular
  "Pi TUI mode passed with --tui-mode on startup.
`regular' renders on the terminal's normal screen, so the transcript
becomes buffer scrollback that Emacs can scroll, search, and number.
`fullscreen' uses Pi's alternate-screen viewport: the session buffer then
holds only the visible screen, and history is reachable only through
Pi's own scrolling.  nil passes no flag and keeps Pi's configured mode.
An explicit --tui-mode in `ai-code-pi-program-switches' takes precedence."
  :type '(choice (const :tag "Regular (normal screen scrollback)" regular)
                 (const :tag "Fullscreen (alternate screen)" fullscreen)
                 (const :tag "Use Pi's setting" nil))
  :group 'ai-code-pi)

(defcustom ai-code-pi-multiline-input-sequence "\e[13;2u"
  "Terminal sequence Pi recognizes as Shift+Enter for multiline input.
The default uses the Kitty keyboard protocol sequence documented by Pi."
  :type 'string
  :group 'ai-code-pi)

(defconst ai-code-pi--session-prefix "pi"
  "Session prefix used in Pi buffer names.")

(defvar ai-code-pi--processes (make-hash-table :test 'equal)
  "Hash table mapping Pi session keys to processes.")

(defun ai-code-pi--tui-mode-switch-p (switch)
  "Return non-nil when SWITCH selects Pi's TUI mode."
  (string-match-p "\\`--tui-mode\\(?:=\\|\\'\\)" switch))

(defun ai-code-pi--startup-switches (switches)
  "Return SWITCHES with `ai-code-pi-tui-mode' applied.
SWITCHES that already select a TUI mode are returned unchanged."
  (if (or (null ai-code-pi-tui-mode)
          (cl-some #'ai-code-pi--tui-mode-switch-p switches))
      switches
    (append switches
            (list "--tui-mode" (symbol-name ai-code-pi-tui-mode)))))

;;;###autoload
(defun ai-code-pi-start (&optional arg)
  "Start Pi using `ai-code-backends-infra' logic.
With prefix ARG, prompt for CLI args using
`ai-code-pi-program-switches', plus the `ai-code-pi-tui-mode' flag, as
the default input."
  (interactive "P")
  (ai-code-backends-infra--start-cli-session
   (list :program ai-code-pi-program
         :switches (ai-code-pi--startup-switches ai-code-pi-program-switches)
         :label "Pi"
         :process-table ai-code-pi--processes
         :session-prefix ai-code-pi--session-prefix
         :escape-function #'ai-code-pi-send-escape
         :multiline-input-sequence ai-code-pi-multiline-input-sequence
         :prepare-launch
         (lambda (working-dir argv)
           (ai-code-mcp-agent-prepare-launch 'pi working-dir argv)))
   arg))

;;;###autoload
(defun ai-code-pi-switch-to-buffer (&optional force-prompt)
  "Switch to a Pi session buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (ai-code-backends-infra--cli-switch-to-buffer
   "Pi" ai-code-pi--session-prefix force-prompt))

;;;###autoload
(defun ai-code-pi-send-command (line)
  "Send LINE to Pi.
When called interactively, prompt for the command."
  (interactive "sPi> ")
  (ai-code-backends-infra--cli-send-command
   "Pi" ai-code-pi--session-prefix line))

;;;###autoload
(defun ai-code-pi-send-escape ()
  "Send Escape to Pi to cancel its current operation."
  (interactive)
  (ai-code-backends-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-pi-resume (&optional arg)
  "Open Pi's session picker and resume a previous session.
When the active region is a session UUID and ARG is nil, resume that
session directly.  Otherwise ARG is passed to the underlying start function."
  (interactive "P")
  (let* ((session-id (and (null arg)
                          (ai-code-backends-infra--selected-session-id)))
         (ai-code-pi-program-switches
          (append ai-code-pi-program-switches
                  (if session-id
                      (list "--session" session-id)
                    '("--resume")))))
    (ai-code-pi-start arg)
    (unless session-id
      (ai-code-backends-infra--cli-show-resume-picker
       ai-code-pi--session-prefix))))

(provide 'ai-code-pi)

;;; ai-code-pi.el ends here
