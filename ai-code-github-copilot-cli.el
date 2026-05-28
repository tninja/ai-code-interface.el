;;; ai-code-github-copilot-cli.el --- Thin wrapper for GitHub Copilot CLI  -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `ai-code-backends-infra' to run GitHub Copilot CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)
(require 'ai-code-mcp-agent)

(defgroup ai-code-github-copilot-cli nil
  "GitHub Copilot CLI integration via `ai-code-backends-infra'."
  :group 'tools
  :prefix "ai-code-github-copilot-cli-")

(defcustom ai-code-github-copilot-cli-program "copilot"
  "Path to the GitHub Copilot CLI executable."
  :type 'string
  :group 'ai-code-github-copilot-cli)

(defcustom ai-code-github-copilot-cli-program-switches nil
  "Command line switches to pass to GitHub Copilot CLI on startup."
  :type '(repeat string)
  :group 'ai-code-github-copilot-cli)

(defcustom ai-code-github-copilot-cli-extra-env-vars '("TERM_PROGRAM=vscode")
  "Extra environment variables passed to the GitHub Copilot CLI terminal session.
By default, `TERM_PROGRAM=vscode' is set so that Copilot CLI recognizes the
terminal as VS Code-compatible and enables multiline input support via
`/terminal-setup' (Shift+Enter and Ctrl+Enter)."
  :type '(repeat string)
  :group 'ai-code-github-copilot-cli)

(defcustom ai-code-github-copilot-cli-multiline-input-sequence "\r\n"
  "Terminal sequence used for multiline input in GitHub Copilot CLI sessions.
This mirrors the VS Code `workbench.action.terminal.sendSequence' binding
that `/terminal-setup' installs for Shift+Enter and Ctrl+Enter."
  :type 'string
  :group 'ai-code-github-copilot-cli)

(defvar ghostel-full-redraw)

(defconst ai-code-github-copilot-cli--session-prefix "copilot"
  "Session prefix used in GitHub Copilot CLI buffer names.")

(defvar ai-code-github-copilot-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping Copilot session keys to processes.")

;;;###autoload
(defun ai-code-github-copilot-cli (&optional arg)
  "Start GitHub Copilot CLI using `ai-code-backends-infra' logic.
With prefix ARG, prompt for CLI args using
`ai-code-github-copilot-cli-program-switches' as the default input."
  (interactive "P")
  (ai-code-backends-infra--start-cli-session
   (list :program ai-code-github-copilot-cli-program
         :switches ai-code-github-copilot-cli-program-switches
         :label "Copilot"
         :process-table ai-code-github-copilot-cli--processes
         :session-prefix ai-code-github-copilot-cli--session-prefix
         :escape-function #'ai-code-github-copilot-cli-send-escape
         :env-vars ai-code-github-copilot-cli-extra-env-vars
         :multiline-input-sequence
         ai-code-github-copilot-cli-multiline-input-sequence
         :prepare-launch
         (lambda (working-dir command)
           (let* ((mcp-launch
                   (ai-code-mcp-agent-prepare-launch 'github-copilot-cli
                                                     working-dir
                                                     command))
                  (mcp-post-start-fn (plist-get mcp-launch :post-start-fn)))
             (list
              :command (plist-get mcp-launch :command)
              :cleanup-fn (plist-get mcp-launch :cleanup-fn)
              :post-start-fn
              ;; Copilot redraws via alternate-screen sequences, so keep the
              ;; scrollback injection hook before attaching MCP session metadata.
              (lambda (buffer process instance-name)
                (with-current-buffer buffer
                  (setq ai-code-backends-infra--sync-redraw-scrollback t)
                  (when (eq ai-code-backends-infra-terminal-backend 'ghostel)
                    (setq-local ghostel-full-redraw t)))
                (when mcp-post-start-fn
                  (funcall mcp-post-start-fn buffer process instance-name)))))))
   arg))

;;;###autoload
(defun ai-code-github-copilot-cli-switch-to-buffer (&optional force-prompt)
  "Switch to the GitHub Copilot CLI buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--switch-to-session-buffer
     nil
     "No Copilot session for this project"
     ai-code-github-copilot-cli--session-prefix
     working-dir
     force-prompt)))

;;;###autoload
(defun ai-code-github-copilot-cli-send-command (line)
  "Send LINE to GitHub Copilot CLI."
  (interactive "sCopilot> ")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--send-line-to-session
     nil
     "No Copilot session for this project"
     line
     ai-code-github-copilot-cli--session-prefix
     working-dir)))

;;;###autoload
(defun ai-code-github-copilot-cli-send-escape ()
  "Send escape key to GitHub Copilot CLI."
  (interactive)
  (ai-code-backends-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-github-copilot-cli-resume (&optional arg)
  "Resume a previous GitHub Copilot CLI session.
Argument ARG is passed to the start command."
  (interactive "P")
  (let ((ai-code-github-copilot-cli-program-switches (append ai-code-github-copilot-cli-program-switches '("--resume"))))
    (ai-code-github-copilot-cli arg)
    ;; Send empty string to trigger terminal processing and ensure CLI session picker appears
    (let* ((working-dir (ai-code-backends-infra--session-working-directory))
           (buffer (ai-code-backends-infra--select-session-buffer
                    ai-code-github-copilot-cli--session-prefix
                    working-dir)))
      (when buffer
        (with-current-buffer buffer
          (sit-for 0.5)
          (ai-code-backends-infra--terminal-send-string "")
          (goto-char (point-min)))))))

(provide 'ai-code-github-copilot-cli)

;;; ai-code-github-copilot-cli.el ends here
