;;; ai-code-aider-cli.el --- Thin wrapper for Aider CLI  -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `ai-code-backends-infra' to run Aider CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)

(defgroup ai-code-aider-cli nil
  "Aider CLI integration via `ai-code-backends-infra'."
  :group 'tools
  :prefix "ai-code-aider-cli-")

(defcustom ai-code-aider-cli-program "aider"
  "Path to the Aider CLI executable."
  :type 'string
  :group 'ai-code-aider-cli)

(defcustom ai-code-aider-cli-program-switches nil
  "Command line switches to pass to Aider CLI on startup."
  :type '(repeat string)
  :group 'ai-code-aider-cli)

(defconst ai-code-aider-cli--session-prefix "aider"
  "Session prefix used in Aider CLI buffer names.")

(defvar ai-code-aider-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping Aider session keys to processes.")

;;;###autoload
(defun ai-code-aider-cli (&optional arg)
  "Start Aider using `ai-code-backends-infra' logic.
With prefix ARG, prompt for CLI args using
`ai-code-aider-cli-program-switches' as the default input."
  (interactive "P")
  (ai-code-backends-infra--start-cli-session
   (list :program ai-code-aider-cli-program
         :switches ai-code-aider-cli-program-switches
         :label "Aider"
         :process-table ai-code-aider-cli--processes
         :session-prefix ai-code-aider-cli--session-prefix
         :escape-function #'ai-code-aider-cli-send-escape)
   arg))

;;;###autoload
(defun ai-code-aider-cli-switch-to-buffer (&optional force-prompt)
  "Switch to the Aider CLI buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--switch-to-session-buffer
     nil
     "No Aider session for this project"
     ai-code-aider-cli--session-prefix
     working-dir
     force-prompt)))

;;;###autoload
(defun ai-code-aider-cli-send-command (line)
  "Send LINE to Aider CLI."
  (interactive "sAider> ")
  (let ((working-dir (ai-code-backends-infra--session-working-directory)))
    (ai-code-backends-infra--send-line-to-session
     nil
     "No Aider session for this project"
     line
     ai-code-aider-cli--session-prefix
     working-dir)))

;;;###autoload
(defun ai-code-aider-cli-send-escape ()
  "Send escape key to Aider CLI."
  (interactive)
  (ai-code-backends-infra--terminal-send-escape))

(provide 'ai-code-aider-cli)

;;; ai-code-aider-cli.el ends here
