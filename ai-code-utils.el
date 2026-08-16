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

(defconst ai-code--treesit-enclosing-type-node-types
  '("class" "class_definition" "class_declaration" "class_specifier"
    "struct" "struct_definition" "struct_declaration" "struct_specifier"
    "struct_item" "impl_item" "interface_declaration" "trait_item"
    "object_declaration" "object_definition" "record_declaration"
    "enum_declaration")
  "Tree-sitter node types that can contain a method or function.")

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

(defun ai-code--treesit-enclosing-type-node-p (node-type)
  "Return non-nil when NODE-TYPE represents a class-like container."
  (and (stringp node-type)
       (member node-type ai-code--treesit-enclosing-type-node-types)))

(defun ai-code--treesit-class-like-node-p (node)
  "Return non-nil when Tree-sitter NODE is a class-like container."
  (and node
       (fboundp 'treesit-node-type)
       (when-let ((node-type (ignore-errors (treesit-node-type node))))
         (ai-code--treesit-enclosing-type-node-p node-type))))

(defun ai-code--treesit-enclosing-class-node (&optional node)
  "Find the enclosing class, struct, trait, or interface node for NODE.
If NODE is omitted, searches upward from the defun at point."
  (when (ai-code--treesit-available-p)
    (let ((current (or node (ai-code--treesit-defun-at-point)))
          enclosing-node)
      (while (and current (not enclosing-node)
                  (fboundp 'treesit-node-parent))
        (setq current (treesit-node-parent current))
        (when (and current
                   (fboundp 'treesit-node-type)
                   (ai-code--treesit-enclosing-type-node-p
                    (treesit-node-type current)))
          (setq enclosing-node current)))
      enclosing-node)))

(defun ai-code--treesit-node-header (node)
  "Extract the signature or declaration header of Tree-sitter NODE.
When the grammar exposes a body field, stop immediately before it.
Otherwise, return the first source line of NODE."
  (when (and node (fboundp 'treesit-node-start) (fboundp 'treesit-node-end))
    (let* ((start (treesit-node-start node))
           (body-node
            (and (fboundp 'treesit-node-child-by-field-name)
                 (ignore-errors
                   (treesit-node-child-by-field-name node "body"))))
           (end (if (and body-node (fboundp 'treesit-node-start))
                    (treesit-node-start body-node)
                  (min (treesit-node-end node)
                       (save-excursion
                         (goto-char start)
                         (line-end-position)))))
           (text (buffer-substring-no-properties start end)))
      (string-trim text))))

(defun ai-code--current-function-name ()
  "Return the name of the function/method at point.
Prefers Tree-sitter AST detection when available, and falls back to
`which-function'."
  (if (ai-code--treesit-available-p)
      (let ((node (ai-code--treesit-defun-at-point)))
        (unless (ai-code--treesit-class-like-node-p node)
          (or (and node (ai-code--treesit-node-name node))
              (when (fboundp 'which-function)
                (ignore-errors (which-function))))))
    (when (fboundp 'which-function)
      (ignore-errors (which-function)))))

(defun ai-code--current-scope-context (&optional pos)
  "Return a plist describing the semantic scope at POS (defaults to point).
Includes names and headers for the function and its class-like container,
plus the function's buffer range under `:range'."
  (save-excursion
    (when pos (goto-char pos))
    (if (ai-code--treesit-available-p)
        (let* ((raw-defun-node (ai-code--treesit-defun-at-point))
               (raw-node-is-class
                (ai-code--treesit-class-like-node-p raw-defun-node))
               (defun-node (unless raw-node-is-class raw-defun-node))
               (class-node (if raw-node-is-class
                               raw-defun-node
                             (ai-code--treesit-enclosing-class-node
                              defun-node)))
               (func-name
                (or (and defun-node (ai-code--treesit-node-name defun-node))
                    (when (and (null raw-defun-node)
                               (fboundp 'which-function))
                      (ignore-errors (which-function)))))
               (class-name (and class-node (ai-code--treesit-node-name class-node)))
               (class-header (and class-node (ai-code--treesit-node-header class-node)))
               (function-header (and defun-node
                                     (ai-code--treesit-node-header defun-node)))
               (class-range
                (when (and class-node
                           (fboundp 'treesit-node-start)
                           (fboundp 'treesit-node-end))
                  (ignore-errors
                    (cons (treesit-node-start class-node)
                          (treesit-node-end class-node)))))
               (range (when (and defun-node
                                 (fboundp 'treesit-node-start)
                                 (fboundp 'treesit-node-end))
                        (ignore-errors
                          (cons (treesit-node-start defun-node)
                                (treesit-node-end defun-node))))))
          (list :function-name func-name
                :class-name class-name
                :class-header class-header
                :class-range class-range
                :function-header function-header
                :range range))
      (list :function-name (when (fboundp 'which-function)
                             (ignore-errors (which-function)))
            :class-name nil
            :class-header nil
            :class-range nil
            :function-header nil
            :range nil))))

(defun ai-code--empty-scope-context ()
  "Return an empty semantic scope context plist."
  (list :function-name nil
        :class-name nil
        :class-header nil
        :class-range nil
        :function-header nil
        :range nil))

(defun ai-code--region-semantic-bounds (beg end)
  "Return first and last non-whitespace positions between BEG and END.
END is treated as an exclusive buffer position.  Return nil when the
region is empty or contains only whitespace."
  (when (< beg end)
    (let ((start (save-excursion
                   (goto-char beg)
                   (when (re-search-forward "\\S-" end t)
                     (match-beginning 0))))
          (finish (save-excursion
                    (goto-char end)
                    (when (re-search-backward "\\S-" beg t)
                      (match-beginning 0)))))
      (when (and start finish)
        (cons start finish)))))

(defun ai-code--scope-context-same-range-p (first second range-key name-key)
  "Return non-nil when FIRST and SECOND identify the same semantic scope.
RANGE-KEY selects the preferred identity range.  NAME-KEY supplies the
fallback identity when neither context has a range."
  (let ((first-range (plist-get first range-key))
        (second-range (plist-get second range-key))
        (first-name (plist-get first name-key))
        (second-name (plist-get second name-key)))
    (cond
     ((and first-range second-range) (equal first-range second-range))
     ((or first-range second-range) nil)
     (t (and first-name second-name (equal first-name second-name))))))

(defun ai-code--class-only-scope-context (context)
  "Return only the class-like container fields from CONTEXT."
  (list :function-name nil
        :class-name (plist-get context :class-name)
        :class-header (plist-get context :class-header)
        :class-range (plist-get context :class-range)
        :function-header nil
        :range nil))

(defun ai-code--scope-context-for-region (beg end)
  "Return semantic context shared by the selected region from BEG to END.
Use full function context only when both non-whitespace endpoints belong
to the same function.  For a region spanning sibling functions, retain
only their common class-like container context."
  (if-let ((bounds (ai-code--region-semantic-bounds beg end)))
      (let* ((first-context (ai-code--current-scope-context (car bounds)))
             (last-context (ai-code--current-scope-context (cdr bounds))))
        (cond
         ((ai-code--scope-context-same-range-p
           first-context last-context :range :function-name)
          first-context)
         ((ai-code--scope-context-same-range-p
           first-context last-context :class-range :class-name)
          (ai-code--class-only-scope-context first-context))
         (t (ai-code--empty-scope-context))))
    (ai-code--empty-scope-context)))

(defun ai-code--format-scope-context (context)
  "Return human-readable semantic scope lines for CONTEXT.
CONTEXT is a plist returned by `ai-code--current-scope-context'."
  (let* ((function-name (plist-get context :function-name))
         (class-name (plist-get context :class-name))
         (class-header (plist-get context :class-header))
         (function-header (plist-get context :function-header))
         (range (plist-get context :range))
         (start-line (and (consp range)
                          (integer-or-marker-p (car range))
                          (ignore-errors (line-number-at-pos (car range)))))
         (end-line (and (consp range)
                        (integer-or-marker-p (cdr range))
                        (ignore-errors (line-number-at-pos (cdr range))))))
    (mapconcat
     #'identity
     (delq nil
           (list
            (when class-name (format "Enclosing class: %s" class-name))
            (when class-header (format "Class definition: %s" class-header))
            (when function-name (format "Function: %s" function-name))
            (when function-header
              (format "Function definition: %s" function-header))
            (when (and start-line end-line)
              (format "Function range: lines %d-%d" start-line end-line))))
     "\n")))

(provide 'ai-code-utils)

;;; ai-code-utils.el ends here
