;;; ai-code-open-interpreter-cli.el --- Thin wrapper for Open Interpreter CLI  -*- lexical-binding: t; -*-

;; Author: swithin chan, Kang Tu, AI coding agent
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `ai-code-backends-infra' to run Open Interpreter CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)
(require 'ai-code-mcp-agent)

(defgroup ai-code-open-interpreter-cli nil
  "Open Interpreter CLI integration via `ai-code-backends-infra'."
  :group 'tools
  :prefix "ai-code-open-interpreter-cli-")

(defcustom ai-code-open-interpreter-cli-program "interpreter"
  "Path to the Open Interpreter CLI executable."
  :type 'string
  :group 'ai-code-open-interpreter-cli)

(defcustom ai-code-open-interpreter-cli-program-switches nil
  "Command line switches to pass to Open Interpreter CLI on startup."
  :type '(repeat string)
  :group 'ai-code-open-interpreter-cli)

(defconst ai-code-open-interpreter-cli--session-prefix "open-interpreter"
  "Session prefix used in Open Interpreter CLI buffer names.")

(defvar ai-code-open-interpreter-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping Open Interpreter session keys to processes.")

;;;###autoload
(defun ai-code-open-interpreter-cli (&optional arg)
  "Start Open Interpreter using `ai-code-backends-infra' logic.
With prefix ARG, prompt for CLI args using
`ai-code-open-interpreter-cli-program-switches' as the default input."
  (interactive "P")
  (ai-code-backends-infra--start-cli-session
   (list :program ai-code-open-interpreter-cli-program
         :switches ai-code-open-interpreter-cli-program-switches
         :label "Open Interpreter"
         :process-table ai-code-open-interpreter-cli--processes
         :session-prefix ai-code-open-interpreter-cli--session-prefix
         :escape-function #'ai-code-open-interpreter-cli-send-escape
         :prepare-launch
         (lambda (working-dir command)
           (ai-code-mcp-agent-prepare-launch 'open-interpreter working-dir command)))
   arg))

;;;###autoload
(defun ai-code-open-interpreter-cli-switch-to-buffer (&optional force-prompt)
  "Switch to the Open Interpreter CLI buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (ai-code-backends-infra--cli-switch-to-buffer
   "Open Interpreter" ai-code-open-interpreter-cli--session-prefix force-prompt))

;;;###autoload
(defun ai-code-open-interpreter-cli-send-command (line)
  "Send LINE to Open Interpreter CLI."
  (interactive "sOpen Interpreter> ")
  (ai-code-backends-infra--cli-send-command
   "Open Interpreter" ai-code-open-interpreter-cli--session-prefix line))

;;;###autoload
(defun ai-code-open-interpreter-cli-send-escape ()
  "Send escape key to Open Interpreter CLI."
  (interactive)
  (ai-code-backends-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-open-interpreter-cli-resume (&optional arg)
  "Resume a previous Open Interpreter CLI session.
Argument ARG is passed to the start command."
  (interactive "P")
  (let ((ai-code-open-interpreter-cli-program-switches
         (append ai-code-open-interpreter-cli-program-switches '("resume" "--last"))))
    (ai-code-open-interpreter-cli arg)))

(provide 'ai-code-open-interpreter-cli)

;;; ai-code-open-interpreter-cli.el ends here
