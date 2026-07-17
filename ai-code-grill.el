;;; ai-code-grill.el --- Optional prompt clarification harness -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Provide an optional one-question-at-a-time clarification suffix for the main
;; prompt entry commands before their completed prompt is sent to an AI backend.

;;; Code:

(require 'subr-x)

(declare-function ai-code--git-root "ai-code-utils" (&optional dir))
(declare-function ai-code-prompt-context-cache
                  "ai-code-prompt-mode" (context))
(declare-function ai-code-prompt-context-origin-command
                  "ai-code-prompt-mode" (context))

(defvar ai-code--prompt-origin-command)
(defvar ai-code-prompt-suffix-functions)

;;;###autoload
(defcustom ai-code-grill-me-enabled nil
  "When non-nil, offer to clarify selected prompts before sending them.

The prompt is offered for `ai-code-code-change', `ai-code-ask-question',
`ai-code-implement-todo', and `ai-code-send-command'.  When accepted, the
request tells the AI backend to read the bundled grilling harness before
acting."
  :type 'boolean
  :group 'ai-code)

(defconst ai-code--grill-me-commands
  '(ai-code-code-change
    ai-code-ask-question
    ai-code-implement-todo
    ai-code-send-command)
  "Interactive commands that offer the grill-me harness.")

(defconst ai-code--grill-me-context-cache-key 'ai-code-grill-me-accepted
  "Prompt context cache key for the current Grill decision.")

(defun ai-code--grill-me-package-directory ()
  "Return the package installation directory for ai-code."
  (file-name-directory
   (file-truename
    (or (locate-library "ai-code")
        load-file-name
        buffer-file-name
        default-directory))))

(defun ai-code--grill-me-harness-file ()
  "Return the bundled grill-me harness file path."
  (expand-file-name "prompt/grilling.v1.md"
                    (ai-code--grill-me-package-directory)))

(defun ai-code--grill-me-prompt-path (file-path)
  "Return FILE-PATH formatted for prompt usage."
  (if-let ((git-root (ai-code--git-root)))
      (let ((git-root-abs
             (file-name-as-directory (file-truename git-root)))
            (file-abs (file-truename file-path)))
        (if (file-in-directory-p file-abs git-root-abs)
            (file-relative-name file-abs git-root-abs)
          file-path))
    file-path))

(defun ai-code--grill-me-reference-suffix ()
  "Return a short prompt suffix referencing the grill-me harness."
  (let ((file-path (ai-code--grill-me-harness-file)))
    (unless (file-readable-p file-path)
      (user-error "Grill-me harness is not readable: %s" file-path))
    (format
     "Read the local harness file: @%s. Use its instructions for this request. Apply them without repeating their full contents."
     (ai-code--grill-me-prompt-path file-path))))

(defun ai-code--with-grill-me-origin (orig-fun &rest args)
  "Call ORIG-FUN with ARGS while preserving the entry command."
  (let ((ai-code--prompt-origin-command
         (or ai-code--prompt-origin-command this-command)))
    (apply orig-fun args)))

(defun ai-code--grill-me-accepted-p (context)
  "Return non-nil when Grill was accepted for prompt CONTEXT."
  (gethash ai-code--grill-me-context-cache-key
           (ai-code-prompt-context-cache context)))

(defun ai-code--grill-me-suffix-provider (context)
  "Return the optional Grill suffix for prompt CONTEXT."
  (let ((accepted
         (and ai-code-grill-me-enabled
              (memq (ai-code-prompt-context-origin-command context)
                    ai-code--grill-me-commands)
              (y-or-n-p "Grill me before acting? "))))
    (puthash ai-code--grill-me-context-cache-key accepted
             (ai-code-prompt-context-cache context))
    (when accepted
      (ai-code--grill-me-reference-suffix))))

(add-hook 'ai-code-prompt-suffix-functions
          #'ai-code--grill-me-suffix-provider 20)

;; Remove the prompt-transform advice installed by pre-provider releases.
(when (advice-member-p 'ai-code--with-optional-grill-me
                       'ai-code--insert-prompt)
  (advice-remove 'ai-code--insert-prompt
                 'ai-code--with-optional-grill-me))

(defun ai-code--install-grill-me-command-advice ()
  "Install origin-preserving advice on available Grill commands."
  (dolist (command ai-code--grill-me-commands)
    (when (and (fboundp command)
               (not (advice-member-p #'ai-code--with-grill-me-origin command)))
      (advice-add command :around #'ai-code--with-grill-me-origin))))

(ai-code--install-grill-me-command-advice)

(dolist (feature '(ai-code-change ai-code-discussion ai-code))
  (with-eval-after-load feature
    (ai-code--install-grill-me-command-advice)))

(provide 'ai-code-grill)

;;; ai-code-grill.el ends here
