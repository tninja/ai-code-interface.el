;;; ai-code-grow.el --- Evolutionary design workflow for AI Code -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Grow software through one independently valuable, verifiable increment at a
;; time.  The workflow keeps the evolving design in an Org task file and asks
;; the AI to update that design before implementation begins.

;;; Code:

(require 'org)
(require 'subr-x)
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

(defun ai-code-grow--current-task-file ()
  "Return the current saved Org task file or signal a user error."
  (unless (and (derived-mode-p 'org-mode)
               (stringp buffer-file-name))
    (user-error "Grow Design must be run from a saved Org task file"))
  (expand-file-name buffer-file-name))

(defun ai-code-grow--ensure-design-heading ()
  "Ensure the current Org buffer has a top-level Growing Design heading.
Insert it immediately before Code Change when that heading exists.
Return non-nil when a heading was inserted."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\* Growing Design[ \t]*$" nil t)
        nil
      (goto-char (point-min))
      (if (re-search-forward "^\\* Code Change[ \t]*$" nil t)
          (goto-char (match-beginning 0))
        (goto-char (point-max))
        (unless (bolp)
          (insert "\n"))
        (insert "\n"))
      (insert "* Growing Design\n\n")
      t)))

(defun ai-code-grow--build-prompt (task-file)
  "Build the Grow Design prompt for TASK-FILE."
  (let ((harness (ai-code-grow--harness-file)))
    (unless (file-readable-p harness)
      (user-error "Growing Design harness is not readable: %s" harness))
    (format
     (concat
      "Read the local harness file @%s and follow it for this request.\n"
      "The living design document is the Org task file @%s.\n\n"
      "Inspect the current repository, tests, and existing task-file evidence as needed. "
      "Update only the task file's top-level * Growing Design section. "
      "Do not modify program code, tests, configuration, or other project files. "
      "Select or refine only one next growth increment, make its delivered user value and verification explicit, then stop.")
     (ai-code-grow--prompt-path harness)
     (ai-code-grow--prompt-path task-file))))

;;;###autoload
(defun ai-code-grow-design ()
  "Evolve the current task file toward one next valuable software increment.

The command creates a top-level `Growing Design' section when needed, saves the
task file so the AI can read the latest contents, and sends a design-only
prompt.  The AI may inspect the repository but must update only the task file;
implementation remains a separate workflow, such as `ai-code-implement-todo'."
  (interactive)
  (let ((task-file (ai-code-grow--current-task-file)))
    (ai-code-grow--ensure-design-heading)
    (when (buffer-modified-p)
      (save-buffer))
    (ai-code--insert-prompt (ai-code-grow--build-prompt task-file))))

;;;###autoload
(with-eval-after-load 'ai-code
  (unless (ignore-errors
            (transient-get-suffix 'ai-code--menu-agile-development "y"))
    (transient-append-suffix
      'ai-code--menu-agile-development "t"
      '("y" "Grow Design" ai-code-grow-design))))

(provide 'ai-code-grow)

;;; ai-code-grow.el ends here
