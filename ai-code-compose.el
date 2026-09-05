;;; ai-code-compose.el --- Editable compose buffer for AI prompts -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Optional full-buffer editor for long AI prompts.  This file only provides
;; the compose UI; `ai-code--confirm-and-send' decides when to use it.
;; Existing ai-code commands continue to own context assembly, prompt suffixes,
;; harness behavior, backend selection, and sending.

;;; Code:

(require 'subr-x)
(require 'ai-code-prompt-mode)

;;;###autoload
(defcustom ai-code-use-compose-buffer nil
  "When non-nil, use a compose buffer for prompts longer than two lines.

`ai-code--confirm-and-send' keeps its existing two-line threshold.  Prompts
with two lines or fewer keep their existing input UI.  For prompts longer
than two lines, enabling this option replaces the previous minibuffer
`read-string' editor with `ai-code-compose-read'."
  :type 'boolean
  :group 'ai-code)

(defvar-local ai-code-compose--result nil
  "Accepted text from the current compose buffer.")

(defvar-local ai-code-compose--cancelled nil
  "Non-nil when the current compose buffer was cancelled.")

(defvar-local ai-code-compose--prompt-label nil
  "Prompt label shown in the current compose buffer header line.")

(defconst ai-code-compose-buffer-name "*AI Compose*"
  "Name of the temporary compose buffer.")

(defun ai-code-compose-accept ()
  "Accept the current compose buffer and return to the calling command."
  (interactive)
  (let ((text (string-trim-right
               (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-empty-p (string-trim text))
      (user-error "Nothing to send"))
    (setq ai-code-compose--result text
          ai-code-compose--cancelled nil)
    (exit-recursive-edit)))

(defun ai-code-compose-cancel ()
  "Cancel the current compose buffer and return to the calling command."
  (interactive)
  (setq ai-code-compose--result nil
        ai-code-compose--cancelled t)
  (exit-recursive-edit))

(defun ai-code-compose--header-line ()
  "Return the header line for the current compose buffer."
  (substitute-command-keys
   (concat "AI Compose"
           (when (and ai-code-compose--prompt-label
                      (not (string-empty-p ai-code-compose--prompt-label)))
             (format " — %s" (string-trim ai-code-compose--prompt-label)))
           "    \\[ai-code-compose-accept] send"
           "    \\[ai-code-compose-cancel] cancel")))

(define-derived-mode ai-code-compose-mode text-mode "AI Compose"
  "Major mode for editing a long prompt before an ai-code command uses it."
  (visual-line-mode 1)
  (setq-local header-line-format '(:eval (ai-code-compose--header-line)))
  ;; Reuse the prompt-file completion implementation rather than maintaining
  ;; a second path/symbol discovery stack for compose buffers.
  (add-hook 'completion-at-point-functions #'ai-code--prompt-filepath-capf nil t)
  (add-hook 'post-self-insert-hook
            #'ai-code--prompt-auto-trigger-filepath-completion nil t))

(define-key ai-code-compose-mode-map (kbd "C-c C-c") #'ai-code-compose-accept)
(define-key ai-code-compose-mode-map (kbd "C-c C-k") #'ai-code-compose-cancel)

(defun ai-code-compose--new-buffer ()
  "Kill any stale compose buffer, then return a fresh compose buffer."
  (when-let ((stale (get-buffer ai-code-compose-buffer-name)))
    (when (buffer-live-p stale)
      (kill-buffer stale)))
  (generate-new-buffer ai-code-compose-buffer-name))

(defun ai-code-compose-read (prompt &optional initial-input _candidate-list)
  "Edit and return prompt text in a temporary compose buffer.
PROMPT is shown in the header line and INITIAL-INPUT seeds the buffer.
_CANDIDATE-LIST is accepted for compatibility with `ai-code-read-string'.
Return nil when the user cancels.  The function is synchronous so existing
callers can keep their current prompt-building and sending flow unchanged."
  (let* ((origin-directory default-directory)
         (compose-buffer (ai-code-compose--new-buffer))
         result
         cancelled)
    (unwind-protect
        (save-window-excursion
          (pop-to-buffer compose-buffer)
          (ai-code-compose-mode)
          (setq-local default-directory origin-directory)
          (setq-local ai-code-compose--prompt-label prompt)
          (when initial-input
            (insert initial-input))
          (goto-char (point-max))
          (recursive-edit)
          (setq result ai-code-compose--result
                cancelled ai-code-compose--cancelled))
      (when (buffer-live-p compose-buffer)
        (kill-buffer compose-buffer)))
    (unless cancelled
      result)))

(provide 'ai-code-compose)

;;; ai-code-compose.el ends here
