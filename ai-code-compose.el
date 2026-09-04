;;; ai-code-compose.el --- Editable compose buffer for AI prompts -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Optional full-buffer editor for long AI prompts.  The compose buffer only
;; edits and returns text; existing ai-code commands continue to own context
;; assembly, prompt suffixes, harness behavior, backend selection, and sending.
;;
;; Compose intentionally reuses the existing `ai-code--confirm-and-send'
;; threshold: it is eligible only when INITIAL-PROMPT is longer than five
;; lines.  Which interactive command produced the prompt is irrelevant.

;;; Code:

(require 'subr-x)
(require 'ai-code-prompt-mode)

;;;###autoload
(defcustom ai-code-use-compose-buffer nil
  "When non-nil, edit long confirmation prompts in a compose buffer.

This replaces the existing minibuffer `read-string' editing only when
`ai-code--confirm-and-send' receives an initial prompt longer than five lines.
Shorter prompts keep their existing input UI, and the originating command does
not affect compose eligibility."
  :type 'boolean
  :group 'ai-code)

(defvar ai-code-compose--eligible-p nil
  "Dynamically bound non-nil while editing an eligible long prompt.")

(defvar-local ai-code-compose--result nil
  "Accepted text from the current compose buffer.")

(defvar-local ai-code-compose--cancelled nil
  "Non-nil when the current compose buffer was cancelled.")

(defvar-local ai-code-compose--prompt-label nil
  "Prompt label shown in the current compose buffer header line.")

(defun ai-code-compose--long-prompt-p (prompt)
  "Return non-nil when PROMPT exceeds the existing five-line threshold."
  (and prompt
       (> (length (split-string prompt "\n")) 5)))

(defun ai-code-compose-should-use-p ()
  "Return non-nil when the current prompt should use a compose buffer."
  (and ai-code-use-compose-buffer
       ai-code-compose--eligible-p))

(defun ai-code-compose--confirm-and-send-around
    (original prompt-label initial-prompt)
  "Call ORIGINAL with compose eligibility derived from INITIAL-PROMPT.
PROMPT-LABEL and INITIAL-PROMPT are the arguments of
`ai-code--confirm-and-send'."
  (let ((ai-code-compose--eligible-p
         (ai-code-compose--long-prompt-p initial-prompt)))
    (funcall original prompt-label initial-prompt)))

(with-eval-after-load 'ai-code-input
  (unless (advice-member-p #'ai-code-compose--confirm-and-send-around
                           'ai-code--confirm-and-send)
    (advice-add 'ai-code--confirm-and-send :around
                #'ai-code-compose--confirm-and-send-around)))

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

(defun ai-code-compose-read (prompt &optional initial-input _candidate-list)
  "Edit and return prompt text in a temporary compose buffer.
PROMPT is shown in the header line and INITIAL-INPUT seeds the buffer.
_CANDIDATE-LIST is accepted for compatibility with `ai-code-read-string'.
Return nil when the user cancels.  The function is synchronous so existing
callers can keep their current prompt-building and sending flow unchanged."
  (let* ((origin-directory default-directory)
         (compose-buffer (generate-new-buffer "*AI Compose*"))
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
