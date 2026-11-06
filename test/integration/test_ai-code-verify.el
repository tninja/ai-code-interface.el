;;; test_ai-code-verify.el --- Real Git verification identity tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Exercise code observations against actual Git repositories and worktrees.
;; No agent or external service is invoked.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'ai-code-verify)

(defun ai-code-verify-integration--git (&rest args)
  "Run Git with ARGS, failing the test on errors."
  (with-temp-buffer
    (unless (zerop (apply #'process-file "git" nil t nil args))
      (ert-fail (buffer-string)))
    (string-trim (buffer-string))))

(defmacro ai-code-verify-integration--with-repository (&rest body)
  "Run BODY in a temporary repository containing a tracked binary file."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory (make-temp-file "verify-state-" t)))
          (default-directory root))
     (unwind-protect
         (progn
           (ai-code-verify-integration--git "init" "-q" "--initial-branch=main")
           (ai-code-verify-integration--git "config" "user.name" "Verify Test")
           (ai-code-verify-integration--git "config" "user.email" "verify@example.invalid")
           (with-temp-file "code.bin" (insert "base\0one\n"))
           (ai-code-verify-integration--git "add" "code.bin")
           (ai-code-verify-integration--git "commit" "-qm" "Initial code")
           ,@body)
       (dolist (buffer (buffer-list))
         (when-let ((file (buffer-file-name buffer)))
           (when (string-prefix-p root file)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory root t))))

(ert-deftest ai-code-verify-integration-tracked-content-and-staging ()
  "Detect same-size binary edits, staging and commits without changing Git."
  (ai-code-verify-integration--with-repository
    (let* ((clean (ai-code-verify--capture-code-state root))
           (index (ai-code-verify-integration--git "ls-files" "--stage")))
      (should-not (plist-get clean :error))
      (should (ai-code-verify--same-code-state-p
               clean (ai-code-verify--capture-code-state root)))
      (with-temp-file "code.bin" (insert "base\0two\n"))
      (let ((dirty (ai-code-verify--capture-code-state root)))
        (with-temp-file "code.bin" (insert "base\0six\n"))
        (let ((changed (ai-code-verify--capture-code-state root)))
          (should (equal (plist-get dirty :head) (plist-get changed :head)))
          (should (equal (plist-get dirty :status) (plist-get changed :status)))
          (should-not (ai-code-verify--same-code-state-p dirty changed))
          (should (equal index (ai-code-verify-integration--git "ls-files" "--stage")))
          (ai-code-verify-integration--git "add" "code.bin")
          (let ((staged (ai-code-verify--capture-code-state root)))
            (should-not (ai-code-verify--same-code-state-p changed staged))
            (should-not (equal (plist-get changed :staged)
                               (plist-get staged :staged)))
            (ai-code-verify-integration--git "commit" "-qm" "Updated code")
            (should-not (ai-code-verify--same-code-state-p
                         staged (ai-code-verify--capture-code-state root))))))
      (should-not (ai-code-verify--same-code-state-p
                   clean (ai-code-verify--capture-code-state root))))))

(ert-deftest ai-code-verify-integration-observation-is-read-only ()
  "Do not refresh the index, run external diff tools, or save editor buffers."
  (ai-code-verify-integration--with-repository
    (with-temp-file "code.bin" (insert "disk\0two\n"))
    (with-temp-file "untracked.txt" (insert "not part of reviewed content\n"))
    (ai-code-verify-integration--git "config" "diff.external" "must-not-run")
    (let* ((buffer (find-file-noselect "code.bin"))
           (index (expand-file-name ".git/index" root))
           (index-hash (with-temp-buffer
                         (insert-file-contents-literally index)
                         (secure-hash 'sha256 (current-buffer))))
           before after)
      (with-current-buffer buffer (goto-char (point-max)) (insert "unsaved"))
      (setq before (ai-code-verify--capture-code-state root)
            after (ai-code-verify--capture-code-state root))
      (should-not (plist-get before :error))
      (should (ai-code-verify--same-code-state-p before after))
      (should (assoc (buffer-file-name buffer) (plist-get before :unsaved)))
      (with-current-buffer buffer (insert " more"))
      (should-not (ai-code-verify--same-code-state-p
                   before (ai-code-verify--capture-code-state root)))
      (should (equal index-hash
                     (with-temp-buffer
                       (insert-file-contents-literally index)
                       (secure-hash 'sha256 (current-buffer)))))
      (should (equal "disk\0two\n"
                     (with-temp-buffer
                       (insert-file-contents "code.bin") (buffer-string))))
      (should (string-match-p "untracked contents"
                              (ai-code-verify--code-state-text before))))))

(ert-deftest ai-code-verify-integration-worktree-and-unborn ()
  "Keep worktrees distinct and never invent a usable identity for unborn HEAD."
  (ai-code-verify-integration--with-repository
    (let ((linked (expand-file-name "linked" root))
          (main (ai-code-verify--capture-code-state root)))
      (ai-code-verify-integration--git "worktree" "add" "-qb" "feature" linked "HEAD")
      (let ((worktree (ai-code-verify--capture-code-state linked)))
        (should-not (plist-get worktree :error))
        (should (equal (plist-get main :head) (plist-get worktree :head)))
        (should-not (ai-code-verify--same-code-state-p main worktree))))
    (ai-code-verify-integration--git "checkout" "--orphan" "unborn")
    (let ((state (ai-code-verify--capture-code-state root)))
      (should (plist-get state :error))
      (should-not (ai-code-verify--same-code-state-p state state)))))

(ert-deftest ai-code-verify-integration-compose-refresh-and-staleness ()
  "Reobserve after editing Compose and detect further edits from the source buffer."
  (ai-code-verify-integration--with-repository
    (with-temp-buffer
      (let ((initial (ai-code-verify--capture-code-state root)) sent)
        (cl-letf (((symbol-function 'ai-code-compose-read)
                   (lambda (_label _prompt)
                     (with-temp-file "code.bin" (insert "after\0edit\n"))
                     "Discuss compatibility only."))
                  ((symbol-function 'ai-code--insert-prompt)
                   (lambda (prompt) (setq sent prompt))))
          (ai-code-verify--confirm-and-send "Review" initial))
        (should (string-match-p "Current code observation after Compose" sent))
        (should (string-match-p "changed while Compose was open" sent))
        (should (string-match-p (plist-get ai-code-verify--last-code-state :unstaged) sent))
        (should (eq (ai-code-verify-check-code-state) 'unchanged))
        (with-temp-file "code.bin" (insert "later\0edit\n"))
        (should (eq (ai-code-verify-check-code-state) 'changed))))))

(ert-deftest ai-code-verify-integration-bound-worktree-scope ()
  "Observe the task's execution worktree while preserving the source directory."
  (ai-code-verify-integration--with-repository
    (let ((linked (expand-file-name "linked" root)) sent)
      (ai-code-verify-integration--git "worktree" "add" "-qb" "feature" linked "HEAD")
      (with-temp-buffer
        (setq-local ai-code--session-project-root-override linked)
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Current branch"))
                  ((symbol-function 'ai-code-read-string) (lambda (&rest _) "Preserve behavior"))
                  ((symbol-function 'ai-code-compose-read) (lambda (_label prompt) prompt))
                  ((symbol-function 'ai-code--insert-prompt) (lambda (prompt) (setq sent prompt))))
          (ai-code-verify-current-work))
        (should (string-match-p "Current branch: feature" sent))
        (should (file-equal-p (plist-get ai-code-verify--last-code-state :root) linked))
        (should (eq (ai-code-verify-check-code-state) 'unchanged))
        (should (equal default-directory root))))))

(provide 'test-ai-code-verify-integration)
;;; test_ai-code-verify.el ends here
