;;; ai-code-github-copilot-cli.el --- Thin wrapper for Github Copilot CLI  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Thin wrapper that reuses `claude-code' to run Github Copilot CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'claude-code)

(defvar claude-code-program)
(defvar claude-code-program-switches)
(declare-function claude-code "claude-code" (&optional arg extra-switches force-prompt force-switch-to-buffer))
(declare-function claude-code-resume "claude-code" (&optional arg))
(declare-function claude-code-switch-to-buffer "claude-code" (&optional arg))
(declare-function claude-code-send-command "claude-code" (line))


(defgroup ai-code-github-copilot-cli nil
  "Github Copilot CLI integration via `claude-code'."
  :group 'tools
  :prefix "github-copilot-cli-")

(defcustom github-copilot-cli-program "copilot"
  "Path to the Github Copilot CLI executable."
  :type 'string
  :group 'ai-code-github-copilot-cli)

;;;###autoload
(defun github-copilot-cli (&optional arg)
  "Start Github Copilot CLI (reuses `claude-code' startup logic)."
  (interactive "P")
  (let ((claude-code-program github-copilot-cli-program) ; override dynamically
        (claude-code-program-switches nil))         ; optional e.g.: '("exec" "--non-interactive")
    (claude-code arg)))

;;;###autoload
(defun github-copilot-cli-switch-to-buffer ()
  (interactive)
  (claude-code-switch-to-buffer))

;;;###autoload
(defun github-copilot-cli-send-command (line)
  (interactive "sCopilot> ")
  (claude-code-send-command line))

(provide 'ai-code-github-copilot-cli)

;;; ai-code-github-copilot-cli.el ends here
