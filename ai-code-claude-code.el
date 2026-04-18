;;; ai-code-claude-code.el --- Thin wrapper for Claude Code CLI  -*- lexical-binding: t; -*-

;; Author: Kang Tu, Yoav Orot
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `ai-code-backends-infra' to run Claude Code CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;; This is an alternative to the external claude-code.el package, using the
;; same terminal infrastructure as other backends (codex, gemini, grok, etc.).
;;
;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)
(require 'ai-code-mcp-agent)

(defgroup ai-code-claude-code nil
  "Claude Code CLI integration via `ai-code-backends-infra'."
  :group 'tools
  :prefix "ai-code-claude-code-")

(defcustom ai-code-claude-code-program "claude"
  "Path to the Claude Code CLI executable."
  :type 'string
  :group 'ai-code-claude-code)

(defcustom ai-code-claude-code-program-switches nil
  "Command line switches to pass to Claude Code CLI on startup."
  :type '(repeat string)
  :group 'ai-code-claude-code)

(defcustom ai-code-claude-code-no-flicker nil
  "Enable experimental flicker-free terminal renderer in Claude Code.
When non-nil, set CLAUDE_CODE_NO_FLICKER=1 which uses full-screen
redraw rendering.  This can break vterm scrollback because the
screen-clearing sequences overwrite the scrollback buffer.  Leave
nil unless you specifically need flicker-free rendering and do not
rely on scrolling back through terminal history."
  :type 'boolean
  :group 'ai-code-claude-code)

(defcustom ai-code-claude-code-multiline-input-sequence "\e\r"
  "Terminal sequence used for multiline input in Claude Code sessions.
This mirrors the newline sequence Claude Code expects from `/terminal-setup'."
  :type 'string
  :group 'ai-code-claude-code)

(defconst ai-code-claude-code--session-prefix "claude"
  "Session prefix used in Claude Code CLI buffer names.")

(defvar ai-code-claude-code--processes (make-hash-table :test 'equal)
  "Hash table mapping Claude Code session keys to processes.")

;;;###autoload
(defun ai-code-claude-code (&optional arg)
  "Start Claude Code (uses `ai-code-backends-infra' logic).
With prefix ARG, prompt for CLI args using
`ai-code-claude-code-program-switches' as the default input."
  (interactive "P")
  (let* ((working-dir (ai-code-backends-infra--session-working-directory))
         (resolved (ai-code-backends-infra--resolve-start-command
                    ai-code-claude-code-program
                    ai-code-claude-code-program-switches
                    arg
                    "Claude Code"))
         (command (plist-get resolved :command))
         (mcp-launch (ai-code-mcp-agent-prepare-launch 'claude-code working-dir command))
         (launch-command (or (plist-get mcp-launch :command) command))
         (cleanup-fn (plist-get mcp-launch :cleanup-fn))
         (mcp-post-start-fn (plist-get mcp-launch :post-start-fn))
         ;; Wrap post-start-fn to conditionally disable strip-alternate-screen.
         ;; strip-alternate-screen (PR #298) strips \e[?1049h/l so scrollback
         ;; is preserved, but it causes the Claude Code badge to repeat
         ;; because the Ink/React TUI redraws the full screen every frame.
         ;;
         ;; - vterm: KEEP strip-alternate-screen enabled (with throttling).
         ;;   libvterm's alternate screen has no scrollback ring, so letting
         ;;   \e[?1049h through truncates scrollback to vterm-max-scrollback
         ;;   (~1000 lines).
         ;; - eat/ghostel: DISABLE strip-alternate-screen.  eat saves the
         ;;   entire buffer on alternate-screen entry and restores on exit,
         ;;   so scrollback is never lost.
         (post-start-fn
          (lambda (buffer process instance-name)
            (with-current-buffer buffer
              (if (eq ai-code-backends-infra-terminal-backend 'vterm)
                  ;; Explicitly re-enable for vterm to guard against stale
                  ;; buffer-local nil from a previous session or backend switch.
                  (setq-local ai-code-backends-infra-strip-alternate-screen t)
                (setq-local ai-code-backends-infra-strip-alternate-screen nil)))
            (when mcp-post-start-fn
              (funcall mcp-post-start-fn buffer process instance-name))))
         (env-vars (append (list "TERM_PROGRAM=emacs"
                                "FORCE_CODE_TERMINAL=true")
                          (when ai-code-claude-code-no-flicker
                            (list "CLAUDE_CODE_NO_FLICKER=1")))))
    (ai-code-backends-infra--toggle-or-create-session
     working-dir
     nil
     ai-code-claude-code--processes
     launch-command
     #'ai-code-claude-code-send-escape
     cleanup-fn
     nil
     ai-code-claude-code--session-prefix
     nil
     env-vars
     ai-code-claude-code-multiline-input-sequence
     post-start-fn)))

;;;###autoload
(defun ai-code-claude-code-switch-to-buffer (&optional force-prompt)
  "Switch to the Claude Code CLI buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--switch-to-session-buffer
     nil
     "No Claude Code session for this project"
     ai-code-claude-code--session-prefix
     working-dir
     force-prompt)))

;;;###autoload
(defun ai-code-claude-code-send-command (line)
  "Send LINE to Claude Code CLI."
  (interactive "sClaude Code> ")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--send-line-to-session
     nil
     "No Claude Code session for this project"
     line
     ai-code-claude-code--session-prefix
     working-dir)))

;;;###autoload
(defun ai-code-claude-code-send-escape ()
  "Send escape key to Claude Code CLI."
  (interactive)
  (ai-code-backends-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-claude-code-resume (&optional arg)
  "Resume a previous Claude Code CLI session.
With prefix ARG, prompt for additional CLI args."
  (interactive "P")
  (let ((ai-code-claude-code-program-switches
         (append ai-code-claude-code-program-switches '("--resume"))))
    (ai-code-claude-code arg)))

(provide 'ai-code-claude-code)

;;; ai-code-claude-code.el ends here
