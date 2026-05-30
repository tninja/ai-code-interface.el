;;; ai-code-antigravity-cli.el --- Thin wrapper for Antigravity CLI  -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `ai-code-backends-infra' to run Antigravity CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)

(defgroup ai-code-antigravity-cli nil
  "Antigravity CLI integration via `ai-code-backends-infra'."
  :group 'tools
  :prefix "ai-code-antigravity-cli-")

(defcustom ai-code-antigravity-cli-program "agy"
  "Path to the Antigravity CLI executable."
  :type 'string
  :group 'ai-code-antigravity-cli)

(defcustom ai-code-antigravity-cli-program-switches nil
  "Command line switches to pass to Antigravity CLI on startup."
  :type '(repeat string)
  :group 'ai-code-antigravity-cli)

(defconst ai-code-antigravity-cli--session-prefix "antigravity"
  "Session prefix used in Antigravity CLI buffer names.")

(defvar ai-code-antigravity-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping Antigravity CLI session keys to processes.")

;;;###autoload
(defun ai-code-antigravity-cli (&optional arg)
  "Start Antigravity CLI using `ai-code-backends-infra' logic.
With prefix ARG, prompt for CLI args using
`ai-code-antigravity-cli-program-switches' as the default input."
  (interactive "P")
  (ai-code-backends-infra--start-cli-session
   (list :program ai-code-antigravity-cli-program
         :switches ai-code-antigravity-cli-program-switches
         :label "Antigravity"
         :process-table ai-code-antigravity-cli--processes
         :session-prefix ai-code-antigravity-cli--session-prefix
         :escape-function #'ai-code-antigravity-cli-send-escape)
   arg))

;;;###autoload
(defun ai-code-antigravity-cli-switch-to-buffer (&optional force-prompt)
  "Switch to the Antigravity CLI buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (ai-code-backends-infra--cli-switch-to-buffer
   "Antigravity" ai-code-antigravity-cli--session-prefix force-prompt))

;;;###autoload
(defun ai-code-antigravity-cli-send-command (line)
  "Send LINE to Antigravity CLI."
  (interactive "sAntigravity> ")
  (ai-code-backends-infra--cli-send-command
   "Antigravity" ai-code-antigravity-cli--session-prefix line))

;;;###autoload
(defun ai-code-antigravity-cli-send-escape ()
  "Send escape key to Antigravity CLI."
  (interactive)
  (ai-code-backends-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-antigravity-cli-resume (&optional arg)
  "Resume a previous Antigravity CLI session.
Argument ARG is passed to the start command."
  (interactive "P")
  (let ((ai-code-antigravity-cli-program-switches
         (append ai-code-antigravity-cli-program-switches '("--continue"))))
    (ai-code-antigravity-cli arg)))

(provide 'ai-code-antigravity-cli)

;;; ai-code-antigravity-cli.el ends here
