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
(declare-function magit-git-string "magit-git" (&rest args))
(declare-function which-function "which-func" ())
(declare-function treesit-available-p "treesit")
(declare-function treesit-parser-list "treesit" (&optional buffer))
(declare-function treesit-defun-at-point "treesit" ())
(declare-function treesit-defun-name "treesit" (node))
(declare-function treesit-node-type "treesit" (node))
(declare-function treesit-node-start "treesit" (node))
(declare-function treesit-node-end "treesit" (node))
(declare-function treesit-node-text "treesit" (node &optional no-property))
(declare-function treesit-node-parent "treesit" (node))
(declare-function treesit-node-child-by-field-name "treesit" (node field-name))

(defvar ai-code--repo-context-info (make-hash-table :test #'equal)
  "Hash table storing context info lists per Git repository root.")

(defvar-local ai-code--session-project-root-override nil
  "Explicit session project root associated with the current buffer.")

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
Uses the current buffer override first, then Git root, project.el, and
finally `default-directory'."
  (or ai-code--session-project-root-override
      (ai-code--git-root)
      (when-let ((project (ignore-errors (project-current nil default-directory))))
        (expand-file-name (project-root project)))
      (expand-file-name default-directory)))

(defun ai-code--set-session-project-root (root)
  "Associate the current buffer's AI sessions with ROOT."
  (setq-local ai-code--session-project-root-override
              (file-truename (expand-file-name root))))

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

;;; Semantic Scope & Tree-sitter Utilities

(defun ai-code--treesit-available-p (&optional buffer)
  "Return non-nil if Tree-sitter is available and active for BUFFER.
BUFFER defaults to `current-buffer'."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (fboundp 'treesit-parser-list)
       (condition-case nil
           (consp (treesit-parser-list (or buffer (current-buffer))))
         (error nil))))

(defun ai-code--treesit-defun-at-point (&optional pos)
  "Return the Tree-sitter defun/method AST node at POS, or nil.
POS defaults to `point'."
  (when (ai-code--treesit-available-p)
    (save-excursion
      (when pos (goto-char pos))
      (and (fboundp 'treesit-defun-at-point)
           (ignore-errors (treesit-defun-at-point))))))

(defun ai-code--treesit-node-name (node)
  "Return the symbol/identifier name string for Tree-sitter NODE, or nil."
  (when node
    (or (and (fboundp 'treesit-defun-name)
             (ignore-errors (treesit-defun-name node)))
        (and (fboundp 'treesit-node-child-by-field-name)
             (when-let ((name-child (or (treesit-node-child-by-field-name node "name")
                                        (treesit-node-child-by-field-name node "identifier"))))
               (and (fboundp 'treesit-node-text)
                    (treesit-node-text name-child t)))))))

(defun ai-code--treesit-enclosing-class-node (&optional node)
  "Find the enclosing class, struct, trait, or interface node for NODE.
If NODE is omitted, searches upward from the defun at point."
  (when (ai-code--treesit-available-p)
    (let ((current (or node (ai-code--treesit-defun-at-point))))
      (while (and current
                  (fboundp 'treesit-node-parent)
                  (let ((parent (treesit-node-parent current)))
                    (setq current parent)
                    (and current
                         (fboundp 'treesit-node-type)
                         (not (string-match-p
                               "\\`\\(?:class\\|struct\\|impl\\|interface\\|trait\\|module\\|object\\)\\_>"
                               (or (treesit-node-type current) "")))))))
      current)))

(defun ai-code--treesit-node-header (node &optional max-lines)
  "Extract the signature / header line(s) of Tree-sitter NODE.
MAX-LINES defaults to 3."
  (when (and node (fboundp 'treesit-node-start) (fboundp 'treesit-node-end))
    (let* ((start (treesit-node-start node))
           (end (min (treesit-node-end node)
                     (save-excursion
                       (goto-char start)
                       (forward-line (or max-lines 3))
                       (point))))
           (text (buffer-substring-no-properties start end)))
      (string-trim text))))

(defun ai-code--current-function-name ()
  "Return the name of the function/method at point.
Prefers Tree-sitter AST detection when available, and falls back to
`which-function'."
  (or (when (ai-code--treesit-available-p)
        (when-let ((node (ai-code--treesit-defun-at-point)))
          (ai-code--treesit-node-name node)))
      (when (fboundp 'which-function)
        (ignore-errors (which-function)))))

(defun ai-code--current-scope-context (&optional pos)
  "Return a plist describing the semantic scope at POS (defaults to point).
Includes `:function-name', `:class-name', `:class-header', and `:range'."
  (save-excursion
    (when pos (goto-char pos))
    (if (ai-code--treesit-available-p)
        (let* ((defun-node (ai-code--treesit-defun-at-point))
               (class-node (ai-code--treesit-enclosing-class-node defun-node))
               (func-name (or (and defun-node (ai-code--treesit-node-name defun-node))
                              (when (fboundp 'which-function) (which-function))))
               (class-name (and class-node (ai-code--treesit-node-name class-node)))
               (class-header (and class-node (ai-code--treesit-node-header class-node)))
               (range (when (and defun-node
                                 (fboundp 'treesit-node-start)
                                 (fboundp 'treesit-node-end))
                        (ignore-errors
                          (cons (treesit-node-start defun-node)
                                (treesit-node-end defun-node))))))
          (list :function-name func-name
                :class-name class-name
                :class-header class-header
                :range range))
      (list :function-name (when (fboundp 'which-function) (which-function))
            :class-name nil
            :class-header nil
            :range nil))))

(provide 'ai-code-utils)

;;; ai-code-utils.el ends here
