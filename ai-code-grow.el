;;; ai-code-grow.el --- Value-growing Org task breakdown for AI Code -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Break an existing Org headline into an ordered sequence of sub-tasks that
;; grow software through independently useful, verifiable states.

;;; Code:

(require 'org)
(require 'transient)
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
  "Return the bundled growing-design harness file."
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
    (user-error "Grow Org Heading must be run from a saved Org file"))
  (save-excursion
    (unless (ignore-errors (org-back-to-heading t) t)
      (user-error "Point is not inside an Org headline"))
    (list :file (expand-file-name buffer-file-name)
          :line (line-number-at-pos)
          :title (org-get-heading t t t t))))

(defun ai-code-grow--build-prompt (context)
  "Build the Grow Org Heading prompt from CONTEXT."
  (let ((harness (ai-code-grow--harness-file))
        (file (plist-get context :file))
        (line (plist-get context :line))
        (title (plist-get context :title)))
    (unless (file-readable-p harness)
      (user-error "Growing Design harness is not readable: %s" harness))
    (format
     (concat
      "Read the local harness file @%s and follow it for this request.\n"
      "Target the existing Org headline %S at line %d in @%s.\n\n"
      "Break down that headline into an ordered sequence of value-growing child TODO sub-headlines. "
      "Create or revise only sub-tasks under that headline; preserve the parent headline and its existing description. "
      "Each sub-task must leave the software in a useful, independently verifiable state and build naturally on the previous step. "
      "Do not modify program code, tests, configuration, or other files, and do not implement any sub-task. Stop after updating the Org breakdown.")
     (ai-code-grow--prompt-path harness)
     title line (ai-code-grow--prompt-path file))))

;;;###autoload
(defun ai-code-grow-heading ()
  "Break the current Org headline into value-growing sub-tasks.

The AI may inspect the repository for context, but it may modify only sub-tasks
under the current Org headline.  The generated steps should each deliver a
useful, independently verifiable software state.  Implementation remains a
separate workflow, such as `ai-code-implement-todo'."
  (interactive)
  (let ((context (ai-code-grow--heading-context)))
    (when (buffer-modified-p)
      (save-buffer))
    (ai-code--insert-prompt (ai-code-grow--build-prompt context))))

;;;###autoload
(with-eval-after-load 'ai-code
  (dolist (prefix '(ai-code-menu-default ai-code-menu-2-columns))
    (when (and (commandp prefix)
               (not (ignore-errors (transient-get-suffix prefix "y"))))
      (transient-append-suffix
       prefix '(0 -1)
       ["Growth"
        ("y" "Grow Org Heading" ai-code-grow-heading)]))))

(provide 'ai-code-grow)

;;; ai-code-grow.el ends here
