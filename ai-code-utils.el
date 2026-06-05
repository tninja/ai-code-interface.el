;;; ai-code-utils.el --- Shared utility functions for ai-code -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/tninja/ai-code-interface.el
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Shared utility functions used across multiple ai-code modules.
;; Contains path detection, text formatting, and context helpers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit)
(require 'project)

(declare-function projectile-project-root "projectile")
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-root "project" (project))

(defvar ai-code--repo-context-info (make-hash-table :test #'equal)
  "Hash table storing context info lists per Git repository root.")

;;; Path Utilities

(defconst ai-code-files-dir-name ".ai.code.files"
  "Directory name for storing AI task files.")

(defun ai-code--git-root (&optional dir)
  "Return the normalized Git repository root path, or nil.
Calls `magit-toplevel' with optional DIR argument and applies
`file-truename' to resolve symlinks.  Returns nil when not inside
a Git repository or when `magit-toplevel' signals an error."
  (condition-case nil
      (let ((root (magit-toplevel dir)))
        (when root (file-truename root)))
    (error nil)))

(defun ai-code--worktree-main-repo-root ()
  "Return the main repository root when inside a git worktree, or nil.
Uses git-common-dir to find the shared .git directory and derives the
main repo root from it."
  (condition-case nil
      (let* ((git-common-dir (magit-git-string "rev-parse" "--git-common-dir"))
             (git-dir (magit-git-string "rev-parse" "--git-dir")))
        (when (and git-common-dir git-dir
                   (not (string= (file-truename git-common-dir)
                                 (file-truename git-dir))))
          (file-truename (expand-file-name ".." git-common-dir))))
    (error nil)))

(defun ai-code--project-root ()
  "Return the current project root using Projectile first, then Git."
  (or (and (fboundp 'projectile-project-root)
           (ignore-errors (projectile-project-root)))
      (ai-code--git-root)))

(defun ai-code--session-project-root ()
  "Return the best available project root for the current session.
Tries project.el first, then Git root, then `default-directory'."
  (or (when-let ((project (ignore-errors (project-current nil default-directory))))
        (expand-file-name (project-root project)))
      (ai-code--git-root)
      (expand-file-name default-directory)))

(defun ai-code--get-files-directory ()
  "Get the task directory path.
If inside a git worktree, return `.ai.code.files/' under the main
repository root so task files are shared across worktrees.
If in a regular git repository, return `.ai.code.files/' under git root.
Otherwise, return the current `default-directory'."
  (let ((root (or (ai-code--worktree-main-repo-root)
                  (ai-code--git-root))))
    (if root
        (expand-file-name ai-code-files-dir-name root)
      default-directory)))

(defun ai-code--ensure-files-directory ()
  "Ensure the task directory exists and return its path."
  (let ((ai-code-files-dir (ai-code--get-files-directory)))
    (unless (file-directory-p ai-code-files-dir)
      (make-directory ai-code-files-dir t))
    ai-code-files-dir))

;;; Text Utilities

(defun ai-code--get-clipboard-text ()
  "Return the current clipboard contents as a plain string, or nil if unavailable."
  (let* ((selection (when (fboundp 'gui-get-selection)
                      (or (let ((text (gui-get-selection 'CLIPBOARD 'UTF8_STRING)))
                            (and (stringp text) (not (string-empty-p text)) text))
                          (let ((text (gui-get-selection 'CLIPBOARD 'STRING)))
                            (and (stringp text) (not (string-empty-p text)) text)))))
         (kill-text (condition-case nil
                        (current-kill 0 t)
                      (error nil))))
    (let ((text (or selection kill-text)))
      (when (stringp text)
        (substring-no-properties text)))))

(defun ai-code--get-window-files ()
  "Get a list of unique file paths from all visible windows."
  (let ((files nil))
    (dolist (window (window-list))
      (let ((buffer (window-buffer window)))
        (when (and buffer (buffer-file-name buffer))
          (cl-pushnew (buffer-file-name buffer) files :test #'string=))))
    files))

(defun ai-code--get-context-files-string ()
  "Get a string of files in the current window for context.
The current buffer's file is always first."
  (if (not buffer-file-name)
      ""
    (let* ((current-buffer-file-name buffer-file-name)
           (all-buffer-files (ai-code--get-window-files))
           (other-buffer-files (remove current-buffer-file-name all-buffer-files))
           (sorted-files (cons current-buffer-file-name other-buffer-files)))
      (if sorted-files
          (concat "\nFiles:\n" (mapconcat #'identity sorted-files "\n"))
        ""))))

(defun ai-code--format-repo-context-info ()
  "Return formatted repository context string or nil.
Includes stored context entries for the current Git repository if available."
  (when (and (boundp 'ai-code--repo-context-info)
             ai-code--repo-context-info)
    (let ((repo-root (ai-code--git-root)))
      (when repo-root
        (let ((entries (gethash repo-root ai-code--repo-context-info)))
           (when entries
             (concat "\nStored repository context:\n"
                    (mapconcat (lambda (ctx)
                                 (concat "  - " ctx))
                               (reverse entries)
                               "\n"))))))))

(provide 'ai-code-utils)

;;; ai-code-utils.el ends here
