;;; ai-code-muse-cli.el --- Thin wrapper for Muse Code CLI  -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `ai-code-backends-infra' to run Muse Code CLI.
;; Provides interactive commands for the AI Code suite.
;;
;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)

(defgroup ai-code-muse-cli nil
  "Muse Code CLI integration via `ai-code-backends-infra'."
  :group 'tools
  :prefix "ai-code-muse-cli-")

(defcustom ai-code-muse-cli-program "muse"
  "Path to the Muse Code CLI executable."
  :type 'string
  :group 'ai-code-muse-cli)

(defcustom ai-code-muse-cli-program-switches nil
  "Command line switches to pass to Muse Code CLI on startup."
  :type '(repeat string)
  :group 'ai-code-muse-cli)

(defconst ai-code-muse-cli--session-prefix "muse"
  "Session prefix used in Muse Code CLI buffer names.")

(defvar ai-code-muse-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping Muse Code session keys to processes.")

;;;###autoload
(defun ai-code-muse-cli (&optional arg)
  "Start Muse Code CLI using `ai-code-backends-infra' logic.
With prefix ARG, prompt for CLI args using
`ai-code-muse-cli-program-switches' as the default input."
  (interactive "P")
  (ai-code-backends-infra--start-cli-session
   (list :program ai-code-muse-cli-program
         :switches ai-code-muse-cli-program-switches
         :label "Muse Code"
         :process-table ai-code-muse-cli--processes
         :session-prefix ai-code-muse-cli--session-prefix
         :escape-function #'ai-code-muse-cli-send-escape)
   arg))

;;;###autoload
(defun ai-code-muse-cli-switch-to-buffer (&optional force-prompt)
  "Switch to the Muse Code CLI buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (ai-code-backends-infra--cli-switch-to-buffer
   "Muse Code" ai-code-muse-cli--session-prefix force-prompt))

;;;###autoload
(defun ai-code-muse-cli-send-command (line)
  "Send LINE to Muse Code CLI."
  (interactive "sMuse Code> ")
  (ai-code-backends-infra--cli-send-command
   "Muse Code" ai-code-muse-cli--session-prefix line))

;;;###autoload
(defun ai-code-muse-cli-send-escape ()
  "Send escape key to Muse Code CLI."
  (interactive)
  (ai-code-backends-infra--terminal-send-escape))

;;;###autoload
(defun ai-code-muse-cli-resume (&optional arg)
  "Resume a previous Muse Code CLI session.
Argument ARG is passed to the start command."
  (interactive "P")
  (let ((ai-code-muse-cli-program-switches
         (append ai-code-muse-cli-program-switches '("resume"))))
    (ai-code-muse-cli arg)))

(provide 'ai-code-muse-cli)

;;; ai-code-muse-cli.el ends here
