;;; ai-code-cursor-cli.el --- Thin wrapper for Cursor CLI  -*- lexical-binding: t; -*-

;; Author: donneyluck <donneyluck@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `ai-code-backends-infra' to run Cursor CLI (cursor-agent).
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)

(defgroup ai-code-cursor-cli nil
  "Cursor CLI integration via `ai-code-backends-infra'."
  :group 'tools
  :prefix "ai-code-cursor-cli-")

(defcustom ai-code-cursor-cli-program "cursor-agent"
  "Path to the Cursor CLI executable (cursor-agent)."
  :type 'string
  :group 'ai-code-cursor-cli)

(defcustom ai-code-cursor-cli-program-switches nil
  "Command line switches to pass to Cursor CLI on startup."
  :type '(repeat string)
  :group 'ai-code-cursor-cli)

(defconst ai-code-cursor-cli--session-prefix "cursor"
  "Session prefix used in Cursor CLI buffer names.")

(defvar ai-code-cursor-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping Cursor session keys to processes.")

;;;###autoload
(defun ai-code-cursor-cli (&optional arg)
  "Start Cursor CLI using `ai-code-backends-infra' logic.
With prefix ARG, prompt for CLI args using
`ai-code-cursor-cli-program-switches' as the default input."
  (interactive "P")
  (ai-code-backends-infra--start-cli-session
   (list :program ai-code-cursor-cli-program
         :switches ai-code-cursor-cli-program-switches
         :label "Cursor"
         :process-table ai-code-cursor-cli--processes
         :session-prefix ai-code-cursor-cli--session-prefix
         :escape-function #'ai-code-cursor-cli-send-escape)
   arg))

;;;###autoload
(defun ai-code-cursor-cli-switch-to-buffer (&optional force-prompt)
  "Switch to the Cursor CLI buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (ai-code-backends-infra--cli-switch-to-buffer
   "Cursor" ai-code-cursor-cli--session-prefix force-prompt))

;;;###autoload
(defun ai-code-cursor-cli-send-command (line)
  "Send LINE to Cursor CLI."
  (interactive "sCursor> ")
  (ai-code-backends-infra--cli-send-command
   "Cursor" ai-code-cursor-cli--session-prefix line))

;;;###autoload
(defun ai-code-cursor-cli-send-escape ()
  "Send escape key to Cursor CLI."
  (interactive)
  (ai-code-backends-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-cursor-cli-resume (&optional arg)
  "Resume a previous Cursor CLI session.
Argument ARG is passed to the start command."
  (interactive "P")
  (let ((ai-code-cursor-cli-program-switches (append ai-code-cursor-cli-program-switches '("resume"))))
    (ai-code-cursor-cli arg)
    (ai-code-backends-infra--cli-show-resume-picker
     ai-code-cursor-cli--session-prefix)))

(provide 'ai-code-cursor-cli)

;;; ai-code-cursor-cli.el ends here
