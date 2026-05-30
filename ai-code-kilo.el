;;; ai-code-kilo.el --- Thin wrapper for Kilo  -*- lexical-binding: t; -*-

;; Author: sirmacik <sirmacik@wioo.waw.pl>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Thin wrapper that reuses `ai-code-backends-infra' to run Kilo.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;; Kilo is a fork of Opencode, an open-source alternative to Claude Code
;; that provides HTTP server APIs and customization features (LSP, custom
;; LLM providers, etc.)
;; See: https://kilo.dev/
;;
;;; Code:

(require 'ai-code-backends)
(require 'ai-code-backends-infra)


(defgroup ai-code-kilo nil
  "Kilo integration via `ai-code-backends-infra.el'."
  :group 'tools
  :prefix "ai-code-kilo-")

(defcustom ai-code-kilo-program "kilo"
  "Path to the Kilo executable."
  :type 'string
  :group 'ai-code-kilo)

(defcustom ai-code-kilo-program-switches nil
  "Command line switches to pass to Kilo on startup."
  :type '(repeat string)
  :group 'ai-code-kilo)

(defcustom ai-code-kilo-extra-env-vars
  '("OTUI_USE_ALTERNATE_SCREEN=main-screen")
  "Extra environment variables passed to the Kilo terminal session.
`OTUI_USE_ALTERNATE_SCREEN=main-screen' avoids the alternate screen
buffer so that terminal scrollback is partially preserved."
  :type '(repeat string)
  :group 'ai-code-kilo)

(defconst ai-code-kilo--session-prefix "kilo"
  "Session prefix used in Kilo buffer names.")

(defvar ai-code-kilo--processes (make-hash-table :test 'equal)
  "Hash table mapping Kilo session keys to processes.")

;;;###autoload
(defun ai-code-kilo (&optional arg)
  "Start Kilo using `ai-code-backends-infra' logic.
With prefix ARG, prompt for CLI args using
`ai-code-kilo-program-switches' as the default input."
  (interactive "P")
  (ai-code-backends-infra--start-cli-session
   (list :program ai-code-kilo-program
         :switches ai-code-kilo-program-switches
         :label "Kilo"
         :process-table ai-code-kilo--processes
         :session-prefix ai-code-kilo--session-prefix
         :env-vars ai-code-kilo-extra-env-vars)
   arg))

;;;###autoload
(defun ai-code-kilo-switch-to-buffer (&optional force-prompt)
  "Switch to the Kilo buffer.
When FORCE-PROMPT is non-nil, prompt to select a session."
  (interactive "P")
  (ai-code-backends-infra--cli-switch-to-buffer
   "Kilo" ai-code-kilo--session-prefix force-prompt))

;;;###autoload
(defun ai-code-kilo-send-command (line)
  "Send LINE to Kilo.
When called interactively, prompts for the command."
  (interactive "sKilo> ")
  (ai-code-backends-infra--cli-send-command
   "Kilo" ai-code-kilo--session-prefix line))

;;;###autoload
(defun ai-code-kilo-resume (&optional arg)
  "Resume a previous Kilo session.

This command starts Kilo with the --continue flag to resume
a specific past session.  The CLI will present an interactive list of past
sessions to choose from.

If current buffer belongs to a project, start in the project's root
directory.  Otherwise start in the directory of the current buffer file,
or the current value of `default-directory' if no project and no buffer file.

With double prefix ARG (\\[universal-argument] \\[universal-argument]),
prompt for the project directory."
  (interactive "P")
  (let ((ai-code-kilo-program-switches
         (append ai-code-kilo-program-switches '("--continue"))))
    (ai-code-kilo arg)
    (ai-code-backends-infra--cli-show-resume-picker
     ai-code-kilo--session-prefix)))

(provide 'ai-code-kilo)

;;; ai-code-kilo.el ends here
