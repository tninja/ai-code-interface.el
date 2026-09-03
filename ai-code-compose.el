;;; ai-code-compose.el --- Editable compose buffer for AI prompts -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Optional full-buffer editor for free-form AI prompts.  The compose buffer
;; only edits and returns text; existing ai-code commands continue to own
;; context assembly, prompt suffixes, harness behavior, backend selection, and
;; sending.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;###autoload
(defcustom ai-code-use-compose-buffer nil
  "When non-nil, edit supported free-form AI prompts in a compose buffer.
The existing minibuffer/Helm input remains the default when this option is nil.
Only commands listed in `ai-code-compose-buffer-commands' use the compose
buffer, so short inputs such as commit messages and backend choices are not
affected."
  :type 'boolean
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-compose-buffer-commands
  '(ai-code-send-command
    ai-code-code-change
    ai-code-ask-question
    ai-code-implement-todo
    ai-code-investigate-exception
    ai-code-refactor-book-method
    ai-code-send-quick-prompt
    ai-code-search-notes-with-ai)
  "Interactive commands whose free-form prompts may use a compose buffer.
This list is consulted only when `ai-code-use-compose-buffer' is non-nil.
Add commands here when their `ai-code-read-string' input is natural-language
prompt text that benefits from full Emacs editing."
  :type '(repeat function)
  :group 'ai-code)

(defvar-local ai-code-compose--result nil
  "Accepted text from the current compose buffer.")

(defvar-local ai-code-compose--cancelled nil
  "Non-nil when the current compose buffer was cancelled.")

(defvar-local ai-code-compose--prompt-label nil
  "Prompt label shown in the current compose buffer header line.")

(defun ai-code-compose-should-use-p ()
  "Return non-nil when the current command should use a compose buffer."
  (and ai-code-use-compose-buffer
       (cl-some (lambda (command)
                  (and command
                       (memq command ai-code-compose-buffer-commands)))
                (list this-command
                      (and (boundp 'real-this-command)
                           real-this-command)))))

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

(define-derived-mode ai-code-compose-mode text-mode "AI Compose"
  "Major mode for editing a free-form prompt before an ai-code command uses it."
  (visual-line-mode 1)
  (setq-local header-line-format
              (substitute-command-keys
               (concat "AI Compose"
                       (when (and ai-code-compose--prompt-label
                                  (not (string-empty-p ai-code-compose--prompt-label)))
                         (format " — %s" (string-trim ai-code-compose--prompt-label)))
                       "    \\[ai-code-compose-accept] send"
                       "    \\[ai-code-compose-cancel] cancel"))))

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
          (setq-local default-directory origin-directory)
          (setq-local ai-code-compose--prompt-label prompt)
          (ai-code-compose-mode)
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
