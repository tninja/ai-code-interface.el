;;; ai-code-agent-shell.el --- agent-shell backend bridge for ai-code -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;; Bridge ai-code backend contracts (:start/:switch/:send/:resume)
;; to the external agent-shell package.
;;
;;; Code:

(require 'ai-code-backends)

(declare-function agent-shell "agent-shell" (&optional arg))
(declare-function agent-shell--shell-buffer "agent-shell" (&key viewport-buffer no-error no-create))
(declare-function agent-shell-queue-request "agent-shell" (prompt))

(defvar agent-shell-session-strategy)

(defgroup ai-code-agent-shell nil
  "Agent-shell backend bridge for ai-code."
  :group 'tools
  :prefix "ai-code-agent-shell-")

(defun ai-code-agent-shell--ensure-available ()
  "Ensure `agent-shell' can be used."
  (unless (require 'agent-shell nil t)
    (user-error "Agent-shell backend is not available; please install agent-shell"))
  (dolist (fn '(agent-shell agent-shell--shell-buffer agent-shell-queue-request))
    (unless (fboundp fn)
      (user-error "Agent-shell backend missing required function: %s" fn))))

;;;###autoload
(defun ai-code-agent-shell (&optional arg)
  "Start or reuse an agent-shell session.
With prefix ARG, forward the prefix to `agent-shell'."
  (interactive "P")
  (ai-code-agent-shell--ensure-available)
  (let ((current-prefix-arg arg))
    (call-interactively #'agent-shell)))

;;;###autoload
(defun ai-code-agent-shell-switch-to-buffer (&optional force-prompt)
  "Switch to an existing agent-shell buffer.
When FORCE-PROMPT is non-nil, prompt to choose a shell."
  (interactive "P")
  (ai-code-agent-shell--ensure-available)
  (if force-prompt
      (let ((current-prefix-arg '(16)))
        (call-interactively #'agent-shell))
    (let ((buffer (agent-shell--shell-buffer :no-create t :no-error t)))
      (if buffer
          (pop-to-buffer buffer)
        (user-error "No agent-shell session for this project")))))

;;;###autoload
(defun ai-code-agent-shell-send-command (line)
  "Send LINE to agent-shell."
  (interactive "sAgent Shell> ")
  (ai-code-agent-shell--ensure-available)
  (let ((buffer (agent-shell--shell-buffer :no-create t :no-error t)))
    (if buffer
        (with-current-buffer buffer
          (agent-shell-queue-request line))
      (user-error "No agent-shell session for this project"))))

;;;###autoload
(defun ai-code-agent-shell-resume (&optional arg)
  "Resume an agent-shell ACP session.
Without ARG use `latest' strategy, with ARG use `prompt' strategy."
  (interactive "P")
  (ai-code-agent-shell--ensure-available)
  (let ((agent-shell-session-strategy (if arg 'prompt 'latest))
        (current-prefix-arg '(4)))
    (call-interactively #'agent-shell)))

(provide 'ai-code-agent-shell)

;;; ai-code-agent-shell.el ends here
