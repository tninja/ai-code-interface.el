;;; ai-code-goose-cli.el --- Thin wrapper for Goose CLI -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Run Goose using the shared terminal session infrastructure.
;; See https://goose-docs.ai/docs/guides/goose-cli-commands/.

;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)

(defgroup ai-code-goose-cli nil
  "Goose CLI integration."
  :group 'ai-code
  :prefix "ai-code-goose-cli-")

(defcustom ai-code-goose-cli-program "goose"
  "Path to the Goose CLI executable."
  :type 'string
  :group 'ai-code-goose-cli)

(defcustom ai-code-goose-cli-program-switches nil
  "Command-line options to pass after the Goose session subcommand."
  :type '(repeat string)
  :group 'ai-code-goose-cli)

(defcustom ai-code-goose-cli-multiline-input-sequence "\C-j"
  "Terminal sequence for inserting a newline in Goose.
Match this to GOOSE_CLI_NEWLINE_KEY if customized in Goose."
  :type 'string
  :group 'ai-code-goose-cli)

(defconst ai-code-goose-cli--session-prefix "goose"
  "Session prefix used in Goose buffer names.")

(defvar ai-code-goose-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping Goose session keys to processes.")

;;;###autoload
(defun ai-code-goose-cli (&optional arg)
  "Start an interactive Goose session.
With prefix ARG, prompt for command-line arguments and session settings."
  (interactive "P")
  (ai-code-backends-infra--start-cli-session
   (list :program ai-code-goose-cli-program
         :switches (cons "session" ai-code-goose-cli-program-switches)
         :label "Goose"
         :process-table ai-code-goose-cli--processes
         :session-prefix ai-code-goose-cli--session-prefix
         :multiline-input-sequence ai-code-goose-cli-multiline-input-sequence)
   arg))

;;;###autoload
(defun ai-code-goose-cli-switch-to-buffer (&optional force-prompt)
  "Switch to a Goose session buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (ai-code-backends-infra--cli-switch-to-buffer
   "Goose" ai-code-goose-cli--session-prefix force-prompt))

;;;###autoload
(defun ai-code-goose-cli-send-command (line)
  "Send LINE to Goose."
  (interactive "sGoose> ")
  (ai-code-backends-infra--cli-send-command
   "Goose" ai-code-goose-cli--session-prefix line))

;;;###autoload
(defun ai-code-goose-cli-resume (&optional arg)
  "Resume the latest Goose session.
With prefix ARG, prompt for options such as --session-id or --name."
  (interactive "P")
  (let ((ai-code-goose-cli-program-switches
         (cons "--resume" ai-code-goose-cli-program-switches)))
    (ai-code-goose-cli arg)))

(provide 'ai-code-goose-cli)

;;; ai-code-goose-cli.el ends here
