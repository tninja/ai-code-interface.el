;;; ai-code-gemini-cli.el --- Thin wrapper for Gemini CLI  -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `ai-code-backends-infra' to run Gemini CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)

(defgroup ai-code-gemini-cli nil
  "Gemini CLI integration via `ai-code-backends-infra'."
  :group 'tools
  :prefix "ai-code-gemini-cli-")

(defcustom ai-code-gemini-cli-program "gemini"
  "Path to the Gemini CLI executable."
  :type 'string
  :group 'ai-code-gemini-cli)

(defcustom ai-code-gemini-cli-program-switches nil
  "Command line switches to pass to Gemini CLI on startup."
  :type '(repeat string)
  :group 'ai-code-gemini-cli)

(defconst ai-code-gemini-cli--session-prefix "gemini"
  "Session prefix used in Gemini CLI buffer names.")

(defvar ai-code-gemini-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping Gemini session keys to processes.")

;;;###autoload
(defun ai-code-gemini-cli (&optional arg)
  "Start Gemini using `ai-code-backends-infra' logic.
With prefix ARG, prompt for CLI args using
`ai-code-gemini-cli-program-switches' as the default input."
  (interactive "P")
  (ai-code-backends-infra--start-cli-session
   (list :program ai-code-gemini-cli-program
         :switches ai-code-gemini-cli-program-switches
         :label "Gemini"
         :process-table ai-code-gemini-cli--processes
         :session-prefix ai-code-gemini-cli--session-prefix
         :escape-function #'ai-code-gemini-cli-send-escape)
   arg))

;;;###autoload
(defun ai-code-gemini-cli-switch-to-buffer (&optional force-prompt)
  "Switch to the Gemini CLI buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (ai-code-backends-infra--cli-switch-to-buffer
   "Gemini" ai-code-gemini-cli--session-prefix force-prompt))

;;;###autoload
(defun ai-code-gemini-cli-send-command (line)
  "Send LINE to Gemini CLI."
  (interactive "sGemini> ")
  (ai-code-backends-infra--cli-send-command
   "Gemini" ai-code-gemini-cli--session-prefix line))

;;;###autoload
(defun ai-code-gemini-cli-send-escape ()
  "Send escape key to Gemini CLI."
  (interactive)
  (ai-code-backends-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-gemini-cli-resume (&optional arg)
  "Resume a previous Gemini CLI session.
Argument ARG is passed to the start command."
  (interactive "P")
  (let ((ai-code-gemini-cli-program-switches (append ai-code-gemini-cli-program-switches '("--resume"))))
    (ai-code-gemini-cli arg)))

(provide 'ai-code-gemini-cli)

;;; ai-code-gemini-cli.el ends here
