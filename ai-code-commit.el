;;; ai-code-commit.el --- Commit current changes workflow for AI Code -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides a small Git commit workflow for AI Code.
;; It stages the current working tree, optionally generates a commit message
;; from the staged diff, commits the change, and can push the current branch.

;;; Code:

(require 'magit)
(require 'subr-x)
(require 'ai-code-input)
(require 'ai-code-prompt-mode)

(declare-function ai-code--git-root "ai-code-utils" (&optional dir))
(declare-function ai-code-call-gptel-sync "ai-code-prompt-mode" (question))
(declare-function ai-code-read-string "ai-code-input" (prompt &optional initial-input candidate-list))
(declare-function magit-anything-modified-p "magit" ())
(declare-function magit-call-git "magit-git" (&rest args))
(declare-function magit-get-current-branch "magit-git" ())
(declare-function magit-git-lines "magit-git" (&rest args))
(declare-function magit-git-output "magit-git" (&rest args))
(declare-function magit-git-string "magit-git" (&rest args))

(defun ai-code-commit--validate-repository ()
  "Return the current Git root or signal a user error."
  (or (ai-code--git-root)
      (user-error "Not in a git repository")))

(defun ai-code-commit--generate-message (diff)
  "Generate a concise commit message from staged DIFF."
  (unless (fboundp 'ai-code-call-gptel-sync)
    (user-error "GPTel commit message generation is not available"))
  (let ((message
         (ai-code-call-gptel-sync
          (concat
           "Generate a concise Git commit message for the staged diff below.\n\n"
           "Return only the commit message. Use imperative mood. Keep the first "
           "line under 72 characters. Do not use Markdown.\n\n"
           diff))))
    (setq message (and message (string-trim message)))
    (unless (and message (not (string-empty-p message)))
      (user-error "Could not generate commit message"))
    message))

(defun ai-code-commit--default-remote ()
  "Return a suitable remote name for pushing the current branch."
  (cond
   ((ignore-errors (magit-git-string "remote" "get-url" "origin"))
    "origin")
   ((car (magit-git-lines "remote")))
   (t
    (user-error "No Git remote configured"))))

(defun ai-code-commit--push-current-branch ()
  "Push the current branch, setting an upstream when needed."
  (let ((branch (magit-get-current-branch)))
    (unless branch
      (user-error "Current branch is not available"))
    (if (ignore-errors
          (magit-git-string "rev-parse"
                            "--abbrev-ref"
                            "--symbolic-full-name"
                            "@{upstream}"))
        (unless (zerop (magit-call-git "push"))
          (user-error "Git push failed"))
      (let ((remote (ai-code-commit--default-remote)))
        (unless (zerop (magit-call-git "push" "-u" remote branch))
          (user-error "Git push failed"))))))

;;;###autoload
(defun ai-code-git-commit-current-changes ()
  "Commit the current working-tree changes and optionally push them.
Prompt for a commit message first.  When the message is empty, stage all
changes, generate a message from the exact staged diff, and let the user edit
that generated message before committing.  Finally ask whether to push the
current branch to its remote."
  (interactive)
  (let* ((root (ai-code-commit--validate-repository))
         (default-directory root))
    (unless (magit-anything-modified-p)
      (user-error "No changes to commit"))
    (let ((commit-message
           (ai-code-read-string "Commit message (empty = AI generate): ")))
      (unless (zerop (magit-call-git "add" "-A"))
        (user-error "Git add failed"))
      (when (string-empty-p (or commit-message ""))
        (let* ((diff (magit-git-output "diff" "--cached"))
               (generated (ai-code-commit--generate-message diff)))
          (setq commit-message
                (ai-code-read-string "Commit message: " generated))))
      (setq commit-message (string-trim (or commit-message "")))
      (when (string-empty-p commit-message)
        (user-error "Commit message cannot be empty"))
      (unless (zerop (magit-call-git "commit" "-m" commit-message))
        (user-error "Git commit failed"))
      (message "Committed: %s" commit-message)
      (when (y-or-n-p "Push to remote? ")
        (ai-code-commit--push-current-branch)))))

(provide 'ai-code-commit)

;;; ai-code-commit.el ends here
