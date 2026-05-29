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
  "Start Codex using `ai-code-backends-infra' logic.
With prefix ARG, prompt for CLI args using
`ai-code-codex-cli-program-switches' as the default input."
  (interactive "P")
  (ai-code-backends-infra--start-cli-session
   (list :program ai-code-codex-cli-program
         :switches ai-code-codex-cli-program-switches
         :label "Codex"
         :process-table ai-code-codex-cli--processes
         :session-prefix ai-code-codex-cli--session-prefix
         :escape-function #'ai-code-codex-cli-send-escape
         :prepare-launch
         (lambda (working-dir command)
           (ai-code-mcp-agent-prepare-launch 'codex working-dir command)))
   arg))

;;;###autoload
(defun ai-code-codex-cli-switch-to-buffer (&optional force-prompt)
  "Switch to the Codex CLI buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (ai-code-backends-infra--cli-switch-to-buffer
   "Codex" ai-code-codex-cli--session-prefix force-prompt))

;;;###autoload
(defun ai-code-codex-cli-send-command (line)
  "Send LINE to Codex CLI."
  (interactive "sCodex> ")
  (ai-code-backends-infra--cli-send-command
   "Codex" ai-code-codex-cli--session-prefix line))

;;;###autoload
(defun ai-code-codex-cli-send-escape ()
  "Send escape key to Codex CLI."
  (interactive)
  (ai-code-backends-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-codex-cli-resume (&optional arg)
  "Resume a previous Codex CLI session.
Argument ARG is passed to the start command."
  (interactive "P")
  (let ((ai-code-codex-cli-program-switches (append ai-code-codex-cli-program-switches '("resume"))))
    (ai-code-codex-cli arg)))

(provide 'ai-code-codex-cli)

;;; ai-code-codex-cli.el ends here
