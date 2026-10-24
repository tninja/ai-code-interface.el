;;; ai-code-grow.el --- Discuss one next value step for an Org task -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Discuss one next independently useful, verifiable growth step for an existing
;; Org task headline.  The first turn is discussion-only; writing the result back
;; to the Org task requires explicit user approval.

;;; Code:

(require 'org)
(require 'ai-code-utils)

(declare-function ai-code--insert-prompt "ai-code-prompt-mode" (prompt-text))
(declare-function ai-code--git-root "ai-code-utils" (&optional dir))

(defun ai-code-grow--package-directory ()
  "Return the package directory containing `ai-code-grow.el'."
  (file-name-directory
   (file-truename
    (or (locate-library "ai-code-grow")
        load-file-name
        buffer-file-name
        default-directory))))

(defun ai-code-grow--harness-file ()
  "Return the bundled next-growth-step harness file."
  (expand-file-name "prompt/growing-design.v1.md"
                    (ai-code-grow--package-directory)))

(defun ai-code-grow--prompt-path (file)
  "Return FILE formatted for use in an AI prompt.
Use a repository-relative path when FILE is inside the current repository."
  (let ((root (ai-code--git-root)))
    (if (and root (file-in-directory-p file root))
        (file-relative-name file root)
      file)))

(defun ai-code-grow--heading-context ()
  "Return context for the Org headline containing point.
Signal a user error unless the current buffer is a saved Org file and point is
inside an Org headline subtree."
  (unless (and (derived-mode-p 'org-mode)
               (stringp buffer-file-name))
    (user-error "Grow Next Step must be run from a saved Org file"))
  (save-excursion
    (unless (ignore-errors (org-back-to-heading t) t)
      (user-error "Point is not inside an Org headline"))
    (list :file (expand-file-name buffer-file-name)
          :line (line-number-at-pos)
          :title (org-get-heading t t t t))))

(defun ai-code-grow--build-prompt (context)
  "Build the Grow Next Step discussion prompt from CONTEXT."
  (let ((harness (ai-code-grow--harness-file))
        (file (plist-get context :file))
        (line (plist-get context :line))
        (title (plist-get context :title)))
    (unless (file-readable-p harness)
      (user-error "Grow Next Step harness is not readable: %s" harness))
    (format
     (concat
      "Read the local harness file @%s and follow it for this request.\n"
      "Target the existing Org headline %S at line %d in @%s.\n\n"
      "Discuss exactly one recommended next value-growing step for that headline. "
      "If no child step exists yet, this is simply the first step. "
      "Use completed children, current code, tests, and task context to decide what should grow next. "
      "Do not generate a roadmap or later steps, and do not edit any file yet. "
      "Keep the response concise and end by asking whether I want to discuss the step further or write it back as a direct child TODO under the target headline.")
     (ai-code-grow--prompt-path harness)
     title line (ai-code-grow--prompt-path file))))

;;;###autoload
(defun ai-code-grow-next-step ()
  "Discuss one next value-growing step for the current Org headline.

The AI may inspect the repository for context, but the first turn is discussion
only.  It recommends one useful, independently verifiable next step and asks
whether to discuss further or write the result back as a direct child TODO.
Writing the task file requires explicit user approval in the AI conversation;
implementation remains a separate workflow, such as `ai-code-implement-todo'."
  (interactive)
  (let ((context (ai-code-grow--heading-context)))
    (when (buffer-modified-p)
      (save-buffer))
    (ai-code--insert-prompt (ai-code-grow--build-prompt context))))

(provide 'ai-code-grow)

;;; ai-code-grow.el ends here
