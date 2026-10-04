;;; test_ai-code-annotate-magit.el --- Magit review regression tests -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Real Git and Magit tests using the optional, unmodified annotate package.

;;; Code:

(require 'ert)
(require 'annotate)
(require 'ai-code-annotate-magit)
(require 'ai-code-annotation)
(require 'ai-code-utils)
(require 'ai-code-prompt-mode)

(defvar ai-code-prompt-suffix-functions)

(defun ai-code-annotate-magit-test--git (&rest args)
  "Run Git ARGS in the test worktree and return its output."
  (with-temp-buffer
    (unless (zerop (apply #'process-file "git" nil t nil args))
      (error "Git failed: %s" (buffer-string)))
    (string-trim (buffer-string))))

(defmacro ai-code-annotate-magit-test--repository (&rest body)
  "Run BODY in a real temporary Git repository and Magit status buffer."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "annotate-review-test-" t))
          (default-directory (file-name-as-directory directory))
          (ai-code-annotate-magit-file (expand-file-name "reviews" directory))
          (annotate-file (expand-file-name "source-notes" directory))
          (magit-refresh-verbose nil)
          (magit-auto-revert-mode nil)
          (magit-git-global-arguments '("--no-pager"))
          buffer)
     (unwind-protect
         (progn
           (ai-code-annotate-magit-test--git "init" "-q")
           (ai-code-annotate-magit-test--git "config" "user.name" "Review Test")
           (ai-code-annotate-magit-test--git "config" "user.email" "review@example.test")
           (with-temp-file "example.txt" (insert "first\nold\nlast\n"))
           (ai-code-annotate-magit-test--git "add" "example.txt")
           (ai-code-annotate-magit-test--git "commit" "-qm" "Initial")
           (with-temp-file "example.txt" (insert "first\nnew\nlast\n"))
           (setq buffer (magit-status-setup-buffer default-directory))
           (with-current-buffer buffer
             (ai-code-annotate-magit-mode 1)
             ;; Accept the real compose buffer unchanged instead of waiting.
             (cl-letf (((symbol-function 'recursive-edit) #'ai-code-compose-accept)
                       ((symbol-function 'exit-recursive-edit) #'ignore))
               ,@body)))
       (dolist (live (buffer-list))
         (with-current-buffer live
           (when (and (string-prefix-p directory default-directory)
                      (not (eq live (get-buffer " *load*"))))
             (set-buffer-modified-p nil)
             (kill-buffer live))))
       (delete-directory directory t))))

(defun ai-code-annotate-magit-test--select (text)
  "Select line containing TEXT in the current real Magit diff."
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line)
  (set-mark (line-end-position))
  (setq transient-mark-mode t mark-active t))

(ert-deftest ai-code-annotate-magit-source-line-mapping ()
  (let ((hunk "@@ -10,3 +20,3 @@\n before\n-deleted\n+added\n after\n"))
    (let ((start (string-match "-deleted" hunk)))
      (should (equal (ai-code--annotate-magit-line-ranges hunk start (+ start 8))
                     '((11 . 11) nil))))
    (let ((start (string-match "+added" hunk)))
      (should (equal (ai-code--annotate-magit-line-ranges hunk start (+ start 6))
                     '(nil (21 . 21)))))
    (should (equal (ai-code--annotate-magit-line-ranges hunk 0 (length hunk))
                   '((10 . 12) (20 . 22))))))

(ert-deftest ai-code-annotate-magit-empty-side-and-no-newline-marker ()
  (let ((hunk "@@ -0,0 +1 @@\n+added\n\\ No newline at end of file\n"))
    (should (equal (ai-code--annotate-magit-line-ranges hunk 0 (length hunk))
                   '(nil (1 . 1)))))
  (let ((hunk "@@ -1 +0,0 @@\n-deleted\n"))
    (should (equal (ai-code--annotate-magit-line-ranges hunk 0 (length hunk))
                   '((1 . 1) nil)))))

(ert-deftest ai-code-annotate-magit-persists-and-restores-real-magit-hunk ()
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "-old")
    (let* ((text-before (buffer-string))
           (note (ai-code--annotate-magit-snapshot)))
      (should (equal (plist-get note :file) "example.txt"))
      (should (equal (plist-get note :old-lines) '(2 . 2)))
      (should-not (plist-get note :new-lines))
      (ai-code--annotate-magit-save-note note "Keep the old behavior.\nExplain the change.")
      (should (file-exists-p ai-code-annotate-magit-file))
      (should-not (file-exists-p annotate-file))
      (should (equal (buffer-string) text-before))
      (should ai-code--annotate-magit-overlays)
      (magit-refresh-buffer)
      (should ai-code--annotate-magit-overlays)
      (should (string-match-p "Old lines: 2; new lines: none"
                              (ai-code-annotate-magit-review-string)))
      (should (string-match-p "Status: MATCHED" (ai-code-annotate-magit-review-string)))
      (let ((root default-directory))
        (kill-buffer buffer)
        (setq buffer (magit-status-setup-buffer root))
        (with-current-buffer buffer
          (ai-code-annotate-magit-mode 1)
          (should ai-code--annotate-magit-overlays)
          (should (string-match-p "Keep the old behavior" (ai-code-annotate-magit-review-string))))))))

(ert-deftest ai-code-annotate-magit-changed-hunks-are-retained-but-not-reattached ()
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "+new")
    (let ((note (ai-code--annotate-magit-snapshot)))
      (ai-code--annotate-magit-save-note note "Check this replacement")
      (with-temp-file "example.txt" (insert "first\ndifferent\nlast\n"))
      (magit-refresh-buffer)
      (should-not ai-code--annotate-magit-overlays)
      (let ((report (ai-code-annotate-magit-review-string)))
        (should (string-match-p "UNMATCHED" report))
        (should (string-match-p "+new" report))
        (should (string-match-p "Check this replacement" report)))
      (ai-code-annotate-magit-delete (plist-get note :id))
      (should-not (ai-code--annotate-magit-read)))))

(ert-deftest ai-code-annotate-magit-staging-does-not-misattach-unstaged-notes ()
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "+new")
    (ai-code--annotate-magit-save-note (ai-code--annotate-magit-snapshot) "Review unstaged change")
    (ai-code-annotate-magit-test--git "add" "example.txt")
    (magit-refresh-buffer)
    (should-not ai-code--annotate-magit-overlays)
    (should (string-match-p "UNMATCHED" (ai-code-annotate-magit-review-string)))))

(ert-deftest ai-code-annotate-magit-whole-hunk-and-editor-roundtrip ()
  (ai-code-annotate-magit-test--repository
    (goto-char (plist-get (car (ai-code--annotate-magit-hunks)) :start))
    (setq mark-active nil)
    (let ((note (ai-code--annotate-magit-snapshot)))
      (should (equal (plist-get note :old-lines) '(1 . 3)))
      (should (equal (plist-get note :new-lines) '(1 . 3))))
    (ai-code-annotate-magit-annotate)
    (should (derived-mode-p 'ai-code-annotate-magit-edit-mode))
    (insert "A multiline review\nwith a second line")
    (ai-code-annotate-magit-edit-save)
    (should (eq (current-buffer) buffer))
    (should (= (length (ai-code--annotate-magit-read)) 1))
    (goto-char (overlay-start (car ai-code--annotate-magit-overlays)))
    (ai-code-annotate-magit-annotate)
    (erase-buffer)
    (insert "Updated comment")
    (ai-code-annotate-magit-edit-save)
    (should (= (length (ai-code--annotate-magit-read)) 1))
    (ai-code-annotate-magit-copy-review)
    (should (string-match-p "Updated comment" (current-kill 0)))
    (should (= (length (ai-code--annotate-magit-read)) 1))))

(ert-deftest ai-code-annotate-magit-editor-survives-origin-refresh ()
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "+new")
    (ai-code-annotate-magit-annotate)
    (let ((editor (current-buffer)))
      (insert "Based on the original snapshot")
      (with-current-buffer buffer
        (with-temp-file "example.txt" (insert "first\nchanged while reviewing\nlast\n"))
        (magit-refresh-buffer))
      (with-current-buffer editor (ai-code-annotate-magit-edit-save)))
    (should-not ai-code--annotate-magit-overlays)
    (should (string-match-p "UNMATCHED" (ai-code-annotate-magit-review-string)))))

(ert-deftest ai-code-annotate-magit-multiple-buffers-merge-saved-notes ()
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "-old")
    (ai-code--annotate-magit-save-note (ai-code--annotate-magit-snapshot) "Deletion note")
    (let ((diff (magit-diff-unstaged)))
      (with-current-buffer diff
        (ai-code-annotate-magit-mode 1)
        (ai-code-annotate-magit-test--select "+new")
        (ai-code--annotate-magit-save-note (ai-code--annotate-magit-snapshot) "Addition note")
        (should (= (length (ai-code--annotate-magit-read)) 2))
        (should (= (length ai-code--annotate-magit-overlays) 2))))
    (should (= (length ai-code--annotate-magit-overlays) 2))))

(ert-deftest ai-code-annotate-magit-rejects-cross-hunk-selection ()
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "+new")
    (set-mark (point-max))
    (should-error (ai-code--annotate-magit-snapshot) :type 'user-error)))

(ert-deftest ai-code-annotate-magit-corrupt-database-is-never-overwritten ()
  (let* ((directory (make-temp-file "annotate-corrupt-" t))
         (ai-code-annotate-magit-file (expand-file-name "reviews" directory)))
    (unwind-protect
        (progn
          (with-temp-file ai-code-annotate-magit-file (insert "broken database"))
          (should-error (ai-code--annotate-magit-save-note '(:id "a") "A note") :type 'user-error)
          (with-temp-buffer
            (insert-file-contents ai-code-annotate-magit-file)
            (should (equal (buffer-string) "broken database"))))
      (delete-directory directory t))))

(ert-deftest ai-code-annotate-magit-corrupt-database-does-not-break-refresh ()
  "An unreadable database is reported without breaking Magit refresh."
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "+new")
    (ai-code--annotate-magit-save-note (ai-code--annotate-magit-snapshot) "Before corruption")
    (should ai-code--annotate-magit-overlays)
    (with-temp-file ai-code-annotate-magit-file (insert "broken database"))
    (let ((start (with-current-buffer (messages-buffer) (point-max))))
      (magit-refresh-buffer)
      (should ai-code-annotate-magit-mode)
      (should-not ai-code--annotate-magit-overlays)
      (should (string-match-p "Cannot read"
                              (with-current-buffer (messages-buffer)
                                (buffer-substring start (point-max))))))))

(ert-deftest ai-code-annotate-magit-disable-only-removes-display ()
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "+new")
    (ai-code--annotate-magit-save-note (ai-code--annotate-magit-snapshot) "Retained note")
    (ai-code-annotate-magit-mode -1)
    (should-not ai-code--annotate-magit-overlays)
    (should-not (memq #'ai-code--annotate-magit-refresh magit-refresh-buffer-hook))
    (should (= (length (ai-code--annotate-magit-read)) 1))
    (ai-code-annotate-magit-mode 1)
    (should ai-code--annotate-magit-overlays)))

(ert-deftest ai-code-annotate-magit-rename-retains-both-file-paths ()
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--git "mv" "example.txt" "renamed.txt")
    (ai-code-annotate-magit-test--git "add" "renamed.txt")
    (magit-refresh-buffer)
    (ai-code-annotate-magit-test--select "+new")
    (let ((note (ai-code--annotate-magit-snapshot)))
      (should (equal (plist-get note :file) "renamed.txt"))
      (should (equal (plist-get note :old-file) "example.txt"))
      (ai-code--annotate-magit-save-note note "Review the rename and replacement")
      (should (string-match-p "Old file: example.txt" (ai-code-annotate-magit-review-string)))
      (magit-refresh-buffer)
      (should ai-code--annotate-magit-overlays))))

(ert-deftest ai-code-annotate-magit-revision-buffer-roundtrip ()
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--git "add" "example.txt")
    (ai-code-annotate-magit-test--git "commit" "-qm" "Replace line")
    (let ((revision (magit-revision-setup-buffer "HEAD" nil nil)))
      (with-current-buffer revision
        (ai-code-annotate-magit-mode 1)
        (ai-code-annotate-magit-test--select "+new")
        (let ((note (ai-code--annotate-magit-snapshot)))
          (should (eq (car (plist-get note :context)) 'committed))
          (ai-code--annotate-magit-save-note note "Review this committed change")
          (magit-refresh-buffer)
          (should ai-code--annotate-magit-overlays)
          (should (string-match-p "Status: MATCHED" (ai-code-annotate-magit-review-string))))))))

(ert-deftest ai-code-annotate-magit-branch-switch-keeps-unstaged-notes-matched ()
  "The same unstaged hunk carried to another branch is still the same change."
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "+new")
    (ai-code--annotate-magit-save-note (ai-code--annotate-magit-snapshot) "Original branch review")
    (ai-code-annotate-magit-test--git "checkout" "-qb" "another-branch")
    (magit-refresh-buffer)
    (should ai-code--annotate-magit-overlays)
    (should (string-match-p "Status: MATCHED" (ai-code-annotate-magit-review-string)))))

(ert-deftest ai-code-annotate-magit-unrelated-commit-keeps-unstaged-notes-matched ()
  "Committing another file must not detach notes on an unchanged hunk."
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "+new")
    (ai-code--annotate-magit-save-note (ai-code--annotate-magit-snapshot) "Still relevant")
    (with-temp-file "other.txt" (insert "other\n"))
    (ai-code-annotate-magit-test--git "add" "other.txt")
    (ai-code-annotate-magit-test--git "commit" "-qm" "Unrelated")
    (magit-refresh-buffer)
    (should ai-code--annotate-magit-overlays)
    (should (string-match-p "Status: MATCHED" (ai-code-annotate-magit-review-string)))))

(ert-deftest ai-code-annotate-magit-keys-do-not-shadow-magit ()
  "Mode keys must be unbound in Magit status and diff buffers."
  (ai-code-annotate-magit-test--repository
    (let ((keys (cl-loop for command in '(ai-code-annotate-magit-annotate
                                          ai-code-annotate-magit-delete
                                          ai-code-annotate-magit-review
                                          ai-code-annotate-magit-copy-review)
                         append (where-is-internal command ai-code-annotate-magit-mode-map))))
      (should (= (length keys) 4))
      (dolist (view (list buffer (magit-diff-unstaged)))
        (with-current-buffer view
          (ai-code-annotate-magit-mode -1)
          (dolist (key keys)
            (should-not (key-binding key))))))))

(ert-deftest ai-code-annotate-magit-removed-file-retains-deletion-note ()
  (ai-code-annotate-magit-test--repository
    (delete-file "example.txt")
    (magit-refresh-buffer)
    (ai-code-annotate-magit-test--select "-old")
    (let ((note (ai-code--annotate-magit-snapshot)))
      (should-not (plist-get note :new-lines))
      (ai-code--annotate-magit-save-note note "Do not remove this behavior")
      (should ai-code--annotate-magit-overlays)
      (should (string-match-p "new lines: none" (ai-code-annotate-magit-review-string))))))

(ert-deftest ai-code-annotate-magit-worktree-review-isolation ()
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "+new")
    (ai-code--annotate-magit-save-note (ai-code--annotate-magit-snapshot) "Original worktree only")
    (let* ((other-root (expand-file-name "other-tree" directory))
           (database ai-code-annotate-magit-file)
           other)
      (ai-code-annotate-magit-test--git "worktree" "add" "-qb" "other" other-root)
      (with-temp-file (expand-file-name "example.txt" other-root)
        (insert "first\nnew\nlast\n"))
      (setq other (magit-status-setup-buffer other-root))
      (with-current-buffer other
        (setq-local ai-code-annotate-magit-file database)
        (ai-code-annotate-magit-mode 1)
        (should-not ai-code--annotate-magit-overlays)
        (should-error (ai-code-annotate-magit-review-string) :type 'user-error)))))

(ert-deftest ai-code-annotate-magit-no-source-files-are-changed ()
  (ai-code-annotate-magit-test--repository
    (let ((patch (ai-code-annotate-magit-test--git "diff")))
      (ai-code-annotate-magit-test--select "+new")
      (ai-code--annotate-magit-save-note (ai-code--annotate-magit-snapshot) "Review only")
      (should (equal patch (ai-code-annotate-magit-test--git "diff")))
      (should-not annotate-mode)
      (should-not (memq #'annotate-save-annotations kill-buffer-hook)))))

(ert-deftest ai-code-annotate-magit-ambiguous-hunks-do-not-reattach ()
  (let ((note '(:file "f" :context (unstaged) :hunk "same")))
    (should-not (ai-code--annotate-magit-match note (list note note)))))

(ert-deftest ai-code-annotation-empty-worktree-does-not-send ()
  "An empty worktree reports no annotations without sending a prompt."
  (ai-code-annotate-magit-test--repository
    (cl-letf (((symbol-function 'ai-code--write-prompt-to-file-and-send)
               (lambda (&rest _) (ert-fail "Unexpected AI handoff"))))
      (should (string-match-p "No code annotations"
                              (error-message-string
                               (should-error (ai-code--annotation-send-all)
                                             :type 'user-error)))))))

(ert-deftest ai-code-annotation-combines-source-and-hunks-as-suggestions ()
  "Use ordinary annotate overlays and saved Magit notes in one prompt."
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "+new")
    (ai-code--annotate-magit-save-note (ai-code--annotate-magit-snapshot) "Review new line")
    (let ((source (find-file-noselect "example.txt")) prompt)
      (with-current-buffer source
        (annotate-create-annotation 1 6 "Explain first" nil 0 nil "source-id")
        (setq-local annotate-mode t))
      (cl-letf (((symbol-function 'ai-code--write-prompt-to-file-and-send)
                 (lambda (text)
                   (should-not ai-code-prompt-suffix-functions)
                   (setq prompt text))))
        (let ((ai-code-prompt-suffix-functions '(ignore)))
          (ai-code--annotation-send-all)))
      (should (string-match-p "Explain first" prompt))
      (should (string-match-p "Review new line" prompt))
      (should (string-match-p (regexp-quote "@@ -1,3 +1,3 @@") prompt))
      (should (string-match-p "Current source lines: 1-1" prompt))
      (should (string-match-p "suggestions ONLY" prompt))
      (should (string-match-p "Do NOT modify files" prompt))
      (should (string-match-p "Wait for the user" prompt))
      (should-not (string-match-p "Apply these review comments" prompt))
      (should (equal (with-temp-buffer (insert-file-contents "example.txt") (buffer-string))
                     "first\nnew\nlast\n")))))

(ert-deftest ai-code-annotation-live-source-overrides-saved-and-deduplicates ()
  "Live multiline notes appear once and override obsolete saved notes."
  (ai-code-annotate-magit-test--repository
    (let* ((file (expand-file-name "example.txt"))
           (source (find-file-noselect file)))
      (with-temp-file annotate-file
        (prin1 (list (annotate-make-record
                      file (list '(1 6 "obsolete" "first" 0 nil "old")) nil))
               (current-buffer)))
      (with-current-buffer source
        (annotate-create-annotation 1 10 "live multiline" nil 0 nil "live-id")
        (setq-local annotate-mode t))
      (let* ((records (ai-code--annotation-sources default-directory nil))
             (notes (plist-get (car records) :notes)))
        (should (= (length records) 1))
        (should (= (length notes) 1))
        (should (equal (annotate-annotation-string (car notes)) "live multiline")))
      (with-current-buffer source (mapc #'delete-overlay (annotate-all-annotations)))
      (should-not (ai-code--annotation-sources default-directory nil)))))

(ert-deftest ai-code-annotation-saved-source-stale-missing-and-outside ()
  "Retain stale and missing source notes but exclude other worktrees."
  (ai-code-annotate-magit-test--repository
    (with-temp-file annotate-file
      (prin1 (list
              (annotate-make-record (expand-file-name "example.txt")
                                   (list '(1 6 "saved" "first" 0 nil "saved-id")
                                         '(7 10 "stale" "old" 0 nil "stale-id")) nil)
              (annotate-make-record (expand-file-name "missing.txt")
                                   (list '(1 5 "missing" "gone" 0 nil "missing-id")) nil)
              (annotate-make-record (expand-file-name "../outside.txt")
                                   (list '(1 5 "outside" "code" 0 nil "outside-id")) nil))
             (current-buffer)))
    (let* ((sources (ai-code--annotation-sources default-directory nil))
           (report (mapconcat #'ai-code--annotation-source-report sources "\n")))
      (should (= (length sources) 2))
      (should (string-match-p "MATCHED disk" report))
      (should (string-match-p "UNMATCHED" report))
      (should (string-match-p "Current source lines: unknown" report))
      (should (string-match-p "missing-id" report))
      (should-not (string-match-p "outside-id" report)))))

(ert-deftest ai-code-annotation-unchecked-hunks-remain-in-source-buffer-handoff ()
  "Snapshots stay accessible from a source buffer, without a false UNMATCHED."
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "-old")
    (ai-code--annotate-magit-save-note (ai-code--annotate-magit-snapshot) "Keep snapshot")
    (with-temp-buffer
      (org-mode)
      (insert "* Heading\n")
      (cl-letf (((symbol-function 'ai-code--write-prompt-to-file-and-send)
                 (lambda (prompt)
                   (should (string-match-p "Keep snapshot" prompt))
                   (should (string-match-p "Status: NOT CHECKED" prompt))
                   (should-not (string-match-p "Status: UNMATCHED" prompt)))))
        (ai-code--annotation-send-all)))))

(ert-deftest ai-code-annotation-broken-source-database-is-not-silenced ()
  "Database errors must not be mistaken for an absence of annotations."
  (ai-code-annotate-magit-test--repository
    (with-temp-file annotate-file (insert "(broken"))
    (cl-letf (((symbol-function 'ai-code--write-prompt-to-file-and-send)
               (lambda (&rest _) (ert-fail "Unexpected AI handoff"))))
      (should-error (ai-code--annotation-send-all)))))

(ert-deftest ai-code-annotation-legacy-source-database-is-read ()
  "Legacy records list annotations after the file name, without a checksum."
  (ai-code-annotate-magit-test--repository
    (with-temp-file annotate-file
      (prin1 `((,(expand-file-name "example.txt") (1 6 "legacy single"))
               (,(expand-file-name "missing.txt") (1 5 "legacy first") (7 9 "legacy second")))
             (current-buffer)))
    (let (prompt)
      (cl-letf (((symbol-function 'ai-code--write-prompt-to-file-and-send)
                 (lambda (text) (setq prompt text))))
        (ai-code--annotation-send-all))
      (should (string-match-p "legacy single" prompt))
      (should (string-match-p "legacy first" prompt))
      (should (string-match-p "legacy second" prompt))
      (should (= (cl-count-if (lambda (line) (string-prefix-p "Note ID: legacy" line))
                              (split-string prompt "\n"))
                 3)))))

(ert-deftest ai-code-annotation-malformed-source-record-is-skipped-not-fatal ()
  "A record that cannot be rendered is marked skipped; others are still sent."
  (ai-code-annotate-magit-test--repository
    (with-temp-file annotate-file
      (prin1 (list (annotate-make-record (expand-file-name "example.txt")
                                         (list '(1 6 "good" "first" 0 nil "good-id")) nil)
                   (annotate-make-record (expand-file-name "bad.txt")
                                         (list '(1 5 nil)) nil))
             (current-buffer)))
    (let (prompt)
      (cl-letf (((symbol-function 'ai-code--write-prompt-to-file-and-send)
                 (lambda (text) (setq prompt text))))
        (ai-code--annotation-send-all))
      (should (string-match-p "good-id" prompt))
      (should (string-match-p "bad\\.txt\n\nSKIPPED" prompt)))))

(ert-deftest ai-code-annotation-compose-buffer-reviews-prompt-before-handoff ()
  "The prompt is sent only as accepted in the compose buffer, never on cancel."
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "+new")
    (ai-code--annotate-magit-save-note (ai-code--annotate-magit-snapshot) "Review new line")
    (let (sent)
      (cl-letf (((symbol-function 'ai-code-compose-read)
                 (lambda (_label initial)
                   (should (string-match-p "Review new line" initial))
                   (should (string-match-p "suggestions ONLY" initial))
                   "EDITED PROMPT"))
                ((symbol-function 'ai-code--write-prompt-to-file-and-send)
                 (lambda (text) (setq sent text))))
        (ai-code--annotation-send-all))
      (should (equal sent "EDITED PROMPT")))
    (cl-letf (((symbol-function 'ai-code-compose-read) #'ignore)
              ((symbol-function 'ai-code--write-prompt-to-file-and-send)
               (lambda (&rest _) (ert-fail "Sent after cancel"))))
      (ai-code--annotation-send-all))))

(ert-deftest ai-code-annotation-menu-offers-ordered-actions-and-routes-by-buffer ()
  "The menu lists six ordered actions; edits go to Magit or annotate.el."
  (ai-code-annotate-magit-test--repository
    (let (choice labels calls)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (setq labels (all-completions "" table))
                   (should (eq (completion-metadata-get
                                (completion-metadata "" table nil) 'display-sort-function)
                               #'identity))
                   (cl-find choice labels :test #'string-prefix-p)))
                ((symbol-function 'ai-code-annotate-magit-annotate)
                 (lambda () (interactive) (push 'magit-edit calls)))
                ((symbol-function 'ai-code-annotate-magit-delete)
                 (lambda (&optional _) (interactive) (push 'magit-delete calls)))
                ((symbol-function 'annotate-annotate)
                 (lambda (&optional _) (interactive) (push 'source-edit calls)))
                ((symbol-function 'annotate-delete-annotation)
                 (lambda (&optional _) (interactive) (push 'source-delete calls))))
        (setq choice "1.")
        (ai-code-address-code-annotation)
        (setq choice "2.")
        (ai-code-address-code-annotation)
        (with-current-buffer (find-file-noselect "example.txt")
          (setq choice "1.")
          (ai-code-address-code-annotation)
          (should annotate-mode)
          (annotate-create-annotation 1 6 "here" nil 0 nil "here-id")
          (goto-char 2)
          (setq choice "2.")
          (ai-code-address-code-annotation)
          (goto-char (point-max))
          (should-error (ai-code-address-code-annotation) :type 'user-error)))
      (should (equal labels '("1. Add / Edit annotation"
                              "2. Delete annotation"
                              "3. Clear all annotations"
                              "4. View annotation"
                              "5. Send current annotation to AI"
                              "6. Send all annotations to AI")))
      (should (equal (nreverse calls)
                     '(magit-edit magit-delete source-edit source-delete))))))

(ert-deftest ai-code-annotation-send-current-sends-only-note-at-point ()
  "Send current includes only the Magit or source note at point."
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "-old")
    (ai-code--annotate-magit-save-note (ai-code--annotate-magit-snapshot) "Magit other")
    (ai-code-annotate-magit-test--select "+new")
    (ai-code--annotate-magit-save-note (ai-code--annotate-magit-snapshot) "Magit here")
    (deactivate-mark)
    (goto-char (point-min))
    (search-forward "+ne")
    (let (sent)
      (cl-letf (((symbol-function 'ai-code--write-prompt-to-file-and-send)
                 (lambda (text) (push text sent))))
        (ai-code--annotation-send-current)
        (with-current-buffer (find-file-noselect "example.txt")
          (annotate-create-annotation 1 6 "Source here" nil 0 nil "here-id")
          (annotate-create-annotation 7 10 "Source other" nil 0 nil "other-id")
          (setq-local annotate-mode t)
          (goto-char 2)
          (ai-code--annotation-send-current)
          (goto-char (point-max))
          (should-error (ai-code--annotation-send-current) :type 'user-error)))
      (setq sent (nreverse sent))
      (should (= (length sent) 2))
      (should (string-match-p "Magit here" (nth 0 sent)))
      (should-not (string-match-p "Magit other" (nth 0 sent)))
      (should (string-match-p "suggestions ONLY" (nth 0 sent)))
      (should (string-match-p "Source here" (nth 1 sent)))
      (should-not (string-match-p "Source other" (nth 1 sent)))
      (should-not (string-match-p "Magit here" (nth 1 sent))))))

(ert-deftest ai-code-annotation-clear-all-removes-only-this-worktree-after-confirmation ()
  "Clear all asks first, then deletes this worktree's source and Magit notes."
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "+new")
    (ai-code--annotate-magit-save-note (ai-code--annotate-magit-snapshot) "Magit note")
    (with-temp-file annotate-file
      (prin1 (list (annotate-make-record (expand-file-name "missing.txt")
                                         (list '(1 5 "saved" "code" 0 nil "saved-id")) nil)
                   (annotate-make-record (expand-file-name "../outside.txt")
                                         (list '(1 5 "outside" "code" 0 nil "outside-id")) nil))
             (current-buffer)))
    (with-current-buffer (find-file-noselect "example.txt")
      (annotate-create-annotation 1 6 "live" nil 0 nil "live-id")
      (setq-local annotate-mode t))
    (cl-letf (((symbol-function 'yes-or-no-p) #'ignore))
      (ai-code--annotation-clear-all))
    (should (= (length (ai-code--annotation-sources default-directory nil)) 2))
    (should (ai-code--annotate-magit-read))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (prompt) (should (string-match-p "all 3 annotations" prompt)) t)))
      (ai-code--annotation-clear-all))
    (should-not (ai-code--annotation-sources default-directory nil))
    (should-not (ai-code--annotate-magit-read))
    (should (equal (mapcar (lambda (record)
                             (file-name-nondirectory (annotate-filename-from-dump record)))
                           (annotate-load-annotation-data))
                   '("outside.txt")))))

(ert-deftest ai-code-annotation-view-shows-all-without-sending ()
  "View displays every worktree annotation read-only and sends nothing."
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "+new")
    (ai-code--annotate-magit-save-note (ai-code--annotate-magit-snapshot) "Magit view")
    (with-current-buffer (find-file-noselect "example.txt")
      (annotate-create-annotation 1 6 "Source view" nil 0 nil "view-id")
      (setq-local annotate-mode t))
    (cl-letf (((symbol-function 'ai-code--write-prompt-to-file-and-send)
               (lambda (&rest _) (ert-fail "View must not send"))))
      (ai-code--annotation-view))
    (with-current-buffer "*AI Code Annotations*"
      (should (string-match-p "Source view" (buffer-string)))
      (should (string-match-p "Magit view" (buffer-string)))
      (should buffer-read-only))))

(ert-deftest ai-code-annotation-real-dispatch-suppresses-edit-suffix-and-org-write ()
  "Sending must not append implementation or Org-write prompts."
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-test--select "+new")
    (ai-code--annotate-magit-save-note (ai-code--annotate-magit-snapshot) "Consider this")
    (let ((ai-code-prompt-suffix-functions
           (list (lambda (_context) "IMPLEMENT AUTOMATICALLY")))
          sent sender caller)
      (with-temp-buffer
        (setq caller (current-buffer))
        (org-mode)
        (insert "* Review\n")
        (setq-local ai-code-prompt-suffix-functions
                    (list (lambda (_context) "LOCAL IMPLEMENT")))
        (cl-letf (((symbol-function 'ai-code--get-ai-code-prompt-file-path) (lambda () nil))
                  ((symbol-function 'ai-code--send-prompt)
                   (lambda (text) (setq sent text sender (current-buffer))))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) (ert-fail "Unexpected Org summary offer"))))
          (ai-code--annotation-send-all)))
      ;; The caller, not a killed temporary buffer, is the MCP source buffer.
      (should (eq sender caller))
      (should (string-match-p "Consider this" sent))
      (should-not (string-match-p "IMPLEMENT AUTOMATICALLY" sent))
      (should-not (string-match-p "LOCAL IMPLEMENT" sent))
      (should-not (string-match-p "append a concise result summary" sent))
      (should (string-match-p "Wait for the user" sent))
      (should (= (length ai-code-prompt-suffix-functions) 1)))))

(ert-deftest ai-code-annotation-outside-git-reviews-only-current-file ()
  "Outside Git, annotations from other files must not leak into the prompt."
  (let* ((directory (make-temp-file "ai-code-annotation-test-" t))
         (default-directory (file-name-as-directory directory))
         (annotate-file (expand-file-name "notes"))
         (file (expand-file-name "one.txt")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "one"))
          (with-temp-file annotate-file
            (prin1 (list (annotate-make-record file (list '(1 4 "current" "one")) nil)
                         (annotate-make-record (expand-file-name "two.txt")
                                              (list '(1 4 "other" "two")) nil))
                   (current-buffer)))
          (with-temp-buffer
            (setq buffer-file-name file)
            (cl-letf (((symbol-function 'recursive-edit) #'ai-code-compose-accept)
                      ((symbol-function 'exit-recursive-edit) #'ignore)
                      ((symbol-function 'ai-code--write-prompt-to-file-and-send)
                       (lambda (text)
                         (should (string-match-p "current" text))
                         (should-not (string-match-p "other" text)))))
              (ai-code--annotation-send-all))))
      (delete-directory directory t))))

(ert-deftest ai-code-annotation-old-annotate-asks-for-upgrade ()
  "Old annotate.el without annotation IDs (< 2.5.0) must stop with a hint."
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-mode -1)
    (cl-letf (((symbol-function 'annotate-id-from-dump) nil)
              ((symbol-function 'ai-code--write-prompt-to-file-and-send)
               (lambda (&rest _) (ert-fail "Unexpected AI handoff"))))
      (should (string-match-p "annotate.el to 2.5.0"
                              (error-message-string
                               (should-error (ai-code-address-code-annotation)
                                             :type 'user-error))))
      (should-error (ai-code-annotate-magit-mode 1) :type 'user-error)
      (should-not ai-code-annotate-magit-mode))))

(ert-deftest ai-code-annotation-absent-dependency-leaves-magit-mode-disabled ()
  "A missing dependency must not leave the Magit overlay mode active."
  (ai-code-annotate-magit-test--repository
    (ai-code-annotate-magit-mode -1)
    (let ((original (symbol-function 'require)))
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &rest args)
                   (if (eq feature 'annotate) nil (apply original feature args)))))
        (should-error (ai-code-annotate-magit-mode 1) :type 'user-error)
        (should-not ai-code-annotate-magit-mode)))))

(provide 'test_ai-code-annotate-magit)
;;; test_ai-code-annotate-magit.el ends here
