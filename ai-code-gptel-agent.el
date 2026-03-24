;;; ai-code-gptel-agent.el --- gptel-agent backend bridge for ai-code -*- lexical-binding: t; -*-

;; Author: davidwuchn
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Bridge ai-code backend contracts (:start/:switch/:send)
;; to the external gptel-agent package.

;;; Code:

(require 'ai-code-backends)

(declare-function gptel-agent "gptel-agent" (&optional project-dir agent-preset))
(declare-function gptel-send "gptel" (&optional arg))
(declare-function ai-code--git-root "ai-code-file" (&optional dir))

(defun ai-code-gptel-agent--get-buffer ()
  "Get gptel-agent buffer for current project, or nil."
  (let* ((project-root (or (and (fboundp 'ai-code--git-root) (ai-code--git-root))
                           default-directory))
         (project-name (file-name-nondirectory
                        (directory-file-name project-root)))
         (buf-name (format "*gptel-agent:%s*" project-name)))
    (get-buffer buf-name)))

(defun ai-code-gptel-agent--ensure-available ()
  "Ensure gptel-agent is available."
  (unless (require 'gptel-agent nil t)
    (user-error "gptel-agent backend requires gptel-agent package")))

;;;###autoload
(defun ai-code-gptel-agent (&optional arg)
  "Start gptel-agent session for current project.
With prefix ARG, prompt for project directory."
  (interactive "P")
  (ai-code-gptel-agent--ensure-available)
  (let ((current-prefix-arg arg))
    (call-interactively #'gptel-agent)))

;;;###autoload
(defun ai-code-gptel-agent-switch-to-buffer (&optional _arg)
  "Switch to gptel-agent buffer for current project."
  (interactive "P")
  (ai-code-gptel-agent--ensure-available)
  (if-let ((buffer (ai-code-gptel-agent--get-buffer)))
      (pop-to-buffer buffer)
    (user-error "No gptel-agent session for this project. Use 'a' to start one.")))

;;;###autoload
(defun ai-code-gptel-agent-send-command (prompt)
  "Send PROMPT to gptel-agent buffer for current project."
  (interactive "sGPTel Agent> ")
  (ai-code-gptel-agent--ensure-available)
  (if-let ((buffer (ai-code-gptel-agent--get-buffer)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert prompt)
        (gptel-send))
    (user-error "No gptel-agent session for this project. Use 'a' to start one.")))

(provide 'ai-code-gptel-agent)

;;; ai-code-gptel-agent.el ends here