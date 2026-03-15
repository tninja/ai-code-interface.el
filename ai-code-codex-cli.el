;;; ai-code-codex-cli.el --- Thin wrapper for Codex CLI  -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `ai-code-backends-infra' to run Codex CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)
(require 'ai-code-mcp-agent)

(defgroup ai-code-codex-cli nil
  "Codex CLI integration via `ai-code-backends-infra'."
  :group 'tools
  :prefix "ai-code-codex-cli-")

(defcustom ai-code-codex-cli-program "codex"
  "Path to the Codex CLI executable."
  :type 'string
  :group 'ai-code-codex-cli)

(defcustom ai-code-codex-cli-program-switches nil
  "Command line switches to pass to Codex CLI on startup."
  :type '(repeat string)
  :group 'ai-code-codex-cli)

(defconst ai-code-codex-cli--session-prefix "codex"
  "Session prefix used in Codex CLI buffer names.")

(defvar ai-code-codex-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping Codex session keys to processes.")

;;;###autoload
(defun ai-code-codex-cli (&optional arg)
  "Start Codex (uses `ai-code-backends-infra' logic).
With prefix ARG, prompt for CLI args using
`ai-code-codex-cli-program-switches' as the default input."
  (interactive "P")
  (let* ((working-dir (ai-code-backends-infra--session-working-directory))
         (resolved (ai-code-backends-infra--resolve-start-command
                    ai-code-codex-cli-program
                    ai-code-codex-cli-program-switches
                    arg
                    "Codex"))
         (command (plist-get resolved :command))
         (mcp-launch (ai-code-mcp-agent-prepare-launch 'codex working-dir command))
         (launch-command (or (plist-get mcp-launch :command) command))
         (cleanup-fn (plist-get mcp-launch :cleanup-fn))
         (post-start-fn (plist-get mcp-launch :post-start-fn)))
    (ai-code-backends-infra--toggle-or-create-session
     working-dir
     nil
     ai-code-codex-cli--processes
     launch-command
     #'ai-code-codex-cli-send-escape
     cleanup-fn
     nil
     ai-code-codex-cli--session-prefix
     nil
     nil
     nil
     post-start-fn)))

;;;###autoload
(defun ai-code-codex-cli-switch-to-buffer (&optional force-prompt)
  "Switch to the Codex CLI buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--switch-to-session-buffer
     nil
     "No Codex session for this project"
     ai-code-codex-cli--session-prefix
     working-dir
     force-prompt)))

;;;###autoload
(defun ai-code-codex-cli-send-command (line)
  "Send LINE to Codex CLI."
  (interactive "sCodex> ")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--send-line-to-session
     nil
     "No Codex session for this project"
     line
     ai-code-codex-cli--session-prefix
     working-dir)))

;;;###autoload
(defun ai-code-codex-cli-send-escape ()
  "Send escape key to Codex CLI."
  (interactive)
  (ai-code-backends-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-codex-cli-resume (&optional arg)
  "Resume a previous Codex CLI session."
  (interactive "P")
  (let ((ai-code-codex-cli-program-switches (append ai-code-codex-cli-program-switches '("resume"))))
    (ai-code-codex-cli arg)))

(provide 'ai-code-codex-cli)

;;; ai-code-codex-cli.el ends here
