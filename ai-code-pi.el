;;; ai-code-pi.el --- Thin wrapper for Pi coding agent -*- lexical-binding: t; -*-

;; Author: realazy
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Provide Pi coding agent integration by reusing
;; `ai-code-backends-infra'.  See https://pi.dev/.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
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

(defcustom ai-code-pi-project-trust nil
  "Project-local trust flag passed to Pi on startup.
When nil, pass no trust flag so Pi uses its own saved trust decisions and
prompt.  `approve' passes `--approve' and `no-approve' passes
`--no-approve'.  An explicit `-a', `--approve', `-na', or `--no-approve' in
`ai-code-pi-program-switches' takes precedence."
  :type '(choice (const :tag "Use Pi's trust prompt/settings" nil)
                 (const :tag "Approve project resources (--approve)" approve)
                 (const :tag "Ignore project resources (--no-approve)"
                        no-approve))
  :group 'ai-code-pi)

(defcustom ai-code-pi-follow-up-sequence "\e[13;3u"
  "Terminal sequence Pi recognizes as Alt+Enter to submit or queue a follow-up.
In Pi, Alt+Enter submits immediately when idle and queues a follow-up when
streaming or compacting."
  :type 'string
  :group 'ai-code-pi)

(defcustom ai-code-pi-compose-sequence "\x07"
  "Terminal sequence Pi recognizes as Ctrl+G to open its external editor."
  :type 'string
  :group 'ai-code-pi)

(defcustom ai-code-pi-send-while-busy 'steer
  "How `ai-code-pi-send-command' submits prompts to Pi.
`steer' sends standard Return, which steers Pi immediately when busy.
`followup' sends `ai-code-pi-follow-up-sequence' (Alt+Enter), which submits
immediately when idle and queues a follow-up until the current turn finishes."
  :type '(choice (const :tag "Steer immediately with Return" steer)
                 (const :tag "Queue follow-up with Alt+Enter" followup))
  :group 'ai-code-pi)

(defconst ai-code-pi--session-prefix "pi"
  "Session prefix used in Pi buffer names.")

(defconst ai-code-pi--slash-commands
  '("/model" "/thinking" "/scoped-models" "/tree" "/compact"
    "/mcp" "/reload" "/settings" "/login" "/fork" "/new" "/resume")
  "Common built-in Pi interactive slash commands offered for completion.")

(defvar ai-code-pi--processes (make-hash-table :test 'equal)
  "Hash table mapping Pi session keys to processes.")

(defun ai-code-pi--tui-mode-switch-p (switch)
  "Return non-nil when SWITCH selects Pi's TUI mode."
  (string-match-p "\\`--tui-mode\\(?:=\\|\\'\\)" switch))

(defun ai-code-pi--project-trust-switch-p (switch)
  "Return non-nil when SWITCH selects Pi's project trust mode."
  (member switch '("-a" "--approve" "-na" "--no-approve")))

(defun ai-code-pi--project-trust-switches (switches)
  "Return project-trust flag list to append to SWITCHES."
  (pcase ai-code-pi-project-trust
    ('nil nil)
    ('approve
     (unless (cl-some #'ai-code-pi--project-trust-switch-p switches)
       '("--approve")))
    ('no-approve
     (unless (cl-some #'ai-code-pi--project-trust-switch-p switches)
       '("--no-approve")))
    (_ (user-error "Unsupported `ai-code-pi-project-trust': %S"
                   ai-code-pi-project-trust))))

(defun ai-code-pi--startup-switches (switches)
  "Return SWITCHES with `ai-code-pi-tui-mode' and trust options applied.
SWITCHES that already select a TUI mode or project trust mode take
precedence over the corresponding option."
  (let ((with-tui
         (if (or (null ai-code-pi-tui-mode)
                 (cl-some #'ai-code-pi--tui-mode-switch-p switches))
             switches
           (append switches
                   (list "--tui-mode" (symbol-name ai-code-pi-tui-mode))))))
    (append with-tui (ai-code-pi--project-trust-switches with-tui))))

(defun ai-code-pi--submit-return-fn (mode)
  "Return the terminal submit function for Pi send MODE."
  (pcase mode
    ('steer #'ai-code-backends-infra--terminal-send-return)
    ('followup #'ai-code-pi-terminal-send-follow-up)
    (_ (user-error "Unsupported `ai-code-pi-send-while-busy': %S" mode))))

(defun ai-code-pi--viewport-submit ()
  "Submit restored Pi editor-viewport input per `ai-code-pi-send-while-busy'."
  (funcall (ai-code-pi--submit-return-fn ai-code-pi-send-while-busy)))

(defun ai-code-pi--configure-session-buffer (buffer _process _instance-name)
  "Configure Pi-specific keybindings and viewport submission in BUFFER."
  (with-current-buffer buffer
    (define-key (ai-code-backends-infra--session-keymap)
                (kbd "M-<return>")
                #'ai-code-pi-terminal-send-follow-up)
    (setq-local ai-code-editor-viewport--submit-function
                #'ai-code-pi--viewport-submit)))

(defun ai-code-pi--session-buffer-p (buffer)
  "Return non-nil when BUFFER is a live Pi session buffer."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (equal ai-code-backends-infra--session-prefix
                ai-code-pi--session-prefix))))

;;;###autoload
(defun ai-code-pi-start (&optional arg)
  "Start Pi using `ai-code-backends-infra' logic.
With prefix ARG, prompt for CLI args using
`ai-code-pi-program-switches', plus the `ai-code-pi-tui-mode' and
`ai-code-pi-project-trust' flags, as the default input."
  (interactive "P")
  (ai-code-backends-infra--start-cli-session
   (list :program ai-code-pi-program
         :switches (ai-code-pi--startup-switches ai-code-pi-program-switches)
         :label "Pi"
         :process-table ai-code-pi--processes
         :session-prefix ai-code-pi--session-prefix
         :escape-function #'ai-code-pi-send-escape
         :multiline-input-sequence ai-code-pi-multiline-input-sequence
         :post-start-fn #'ai-code-pi--configure-session-buffer
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
  "Send LINE to Pi according to `ai-code-pi-send-while-busy'.
When called interactively, prompt for the command."
  (interactive "sPi> ")
  (let ((ai-code-backends-infra--submit-return-function
         (ai-code-pi--submit-return-fn ai-code-pi-send-while-busy)))
    (ai-code-backends-infra--cli-send-command
     "Pi" ai-code-pi--session-prefix line)))

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

;;;###autoload
(defun ai-code-pi-terminal-send-follow-up ()
  "Send `ai-code-pi-follow-up-sequence' to Pi in the current session buffer."
  (interactive)
  (when (fboundp 'ai-code-mcp-agent-touch-session)
    (ai-code-mcp-agent-touch-session (current-buffer)))
  (ai-code-backends-infra--terminal-send-string
   ai-code-pi-follow-up-sequence))

;;;###autoload
(defun ai-code-pi-send-follow-up (line)
  "Send LINE to Pi using `ai-code-pi-follow-up-sequence' (Alt+Enter).
Submits immediately when Pi is idle and queues a follow-up when busy."
  (interactive "sPi follow-up> ")
  (let ((ai-code-backends-infra--submit-return-function
         #'ai-code-pi-terminal-send-follow-up))
    (ai-code-backends-infra--cli-send-command
     "Pi" ai-code-pi--session-prefix line)))

;;;###autoload
(defun ai-code-pi-compose (&optional force-prompt)
  "Open Pi's external editor viewport for the active project session.
When FORCE-PROMPT is non-nil, prompt to select among multiple Pi sessions."
  (interactive "P")
  (let* ((source-buffer (current-buffer))
         (in-pi-session (ai-code-pi--session-buffer-p source-buffer))
         (working-dir (and (or force-prompt (not in-pi-session))
                           (ai-code-backends-infra--session-working-directory)))
         (buffer (if (and in-pi-session (not force-prompt))
                     source-buffer
                   (ai-code-backends-infra--resolve-session-buffer
                    nil
                    "No Pi session for this project"
                    ai-code-pi--session-prefix
                    working-dir
                    force-prompt
                    source-buffer))))
    (when working-dir
      (with-current-buffer buffer
        (ai-code-backends-infra--remember-session-buffer
         ai-code-pi--session-prefix working-dir buffer)))
    (unless (eq buffer source-buffer)
      (ai-code-mcp-agent-refresh-source-context buffer source-buffer))
    (ai-code-mcp-agent-touch-session buffer)
    (with-current-buffer buffer
      (ai-code-backends-infra--terminal-send-string
       ai-code-pi-compose-sequence))
    (setq ai-code-backends-infra--last-accessed-buffer buffer)))

;;;###autoload
(defun ai-code-pi-slash-command (command)
  "Send slash COMMAND to Pi using standard Return.
Leading `/' is added when omitted."
  (interactive
   (list (completing-read "Pi slash command: "
                          ai-code-pi--slash-commands
                          nil nil "/")))
  (let ((trimmed (string-trim (or command ""))))
    (when (string-empty-p trimmed)
      (user-error "Pi slash command cannot be empty"))
    (let ((normalized (if (string-prefix-p "/" trimmed)
                          trimmed
                        (concat "/" trimmed)))
          (ai-code-backends-infra--submit-return-function
           #'ai-code-backends-infra--terminal-send-return))
      (ai-code-backends-infra--cli-send-command
       "Pi" ai-code-pi--session-prefix normalized))))

(provide 'ai-code-pi)

;;; ai-code-pi.el ends here
