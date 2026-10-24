;;; test_ai-code-magit.el --- Magit hunk workflow tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Exercise real Magit section objects, without invoking an agent.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-diff)
(require 'magit-status)
(require 'ai-code-change)
(require 'ai-code-discussion)
(require 'ai-code-agile)
(require 'ai-code-file)
(require 'ai-code-magit nil t)

(defmacro ai-code-magit-test--with-diff (&rest body)
  "Build a real Magit section tree and evaluate BODY at its first hunk."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (setq major-mode 'magit-status-mode)
     (let ((magit-section-set-visibility-hook nil)
           (magit-section-insert-hook nil)
           (transient-mark-mode t)
           first-hunk second-hunk file-section)
       (magit-insert-section (root)
         (magit-insert-section (unstaged)
           (magit-insert-heading "Unstaged changes")
           (setq file-section
                 (magit-insert-section (file "src/example.el" nil
                                       :source "src/old.el"
                                       :header "diff --git a/src/old.el b/src/example.el\n--- a/src/old.el\n+++ b/src/example.el\n")
                   (magit-insert-heading "modified src/example.el")
                   (setq first-hunk
                         (magit-insert-section (hunk '(nil (1 1) (1 1)))
                           (magit-insert-heading "@@ -1 +1 @@")
                           (insert "-old-value\n+new-value\n")))
                   (setq second-hunk
                         (magit-insert-section (hunk '(nil (20 1) (20 1)))
                           (magit-insert-heading "@@ -20 +20 @@")
                           (insert "-old-other\n+new-other\n")))))))
       (goto-char (oref first-hunk start))
       (cl-letf (((symbol-function 'magit-toplevel)
                  (lambda (&optional _) "/tmp/ai-code-magit-project/"))
                 ((symbol-function 'ai-code--git-root)
                  (lambda (&optional _) "/tmp/ai-code-magit-project/"))
                 ((symbol-function 'magit-rev-parse)
                  (lambda (&rest _) "0123456789abcdef")))
         ,@body))))

(ert-deftest ai-code-magit-context-current-hunk ()
  "Capture one hunk with paths, provenance, and no neighboring changes."
  (ai-code-magit-test--with-diff
    (let* ((context (ai-code-magit-context))
           (text (plist-get context :text)))
      (should (equal (plist-get context :root) "/tmp/ai-code-magit-project/"))
      (should (eq (plist-get context :type) 'unstaged))
      (should (string-match-p "index -> working tree" text))
      (should (string-match-p "0123456789abcdef" text))
      (should (string-match-p "src/old.el" text))
      (should (string-match-p "src/example.el" text))
      (should (string-match-p "new-value" text))
      (should-not (string-match-p "new-other" text)))))

(ert-deftest ai-code-magit-context-multiple-hunks ()
  "Use Magit's sibling selection, in display order."
  (ai-code-magit-test--with-diff
    (set-mark (oref first-hunk start))
    (goto-char (oref second-hunk start))
    (setq mark-active t)
    (let ((text (plist-get (ai-code-magit-context) :text)))
      (should (string-match-p "new-value" text))
      (should (string-match-p "new-other" text))
      (should (< (string-match "new-value" text)
                 (string-match "new-other" text))))))

(ert-deftest ai-code-magit-context-partial-hunk ()
  "Keep the complete hunk but identify the selected text separately."
  (ai-code-magit-test--with-diff
    (goto-char (oref first-hunk content))
    (forward-line 1)
    (set-mark (point))
    (end-of-line)
    (setq mark-active t)
    (let ((text (plist-get (ai-code-magit-context) :text)))
      (should (string-match-p "Selected excerpt (focus only" text))
      (should (string-match-p "old-value" text))
      (should (string-match-p "new-value" text))
      (should-not (string-match-p "new-other" text)))))

(ert-deftest ai-code-magit-context-staged ()
  "Never describe the index as the working-tree source."
  (ai-code-magit-test--with-diff
    (oset (oref file-section parent) type 'staged)
    (let ((context (ai-code-magit-context)))
      (should (eq (plist-get context :type) 'staged))
      (should (string-match-p "HEAD -> index" (plist-get context :text))))))

(ert-deftest ai-code-magit-context-history ()
  "Preserve a revision buffer's ref and classify it as historical."
  (ai-code-magit-test--with-diff
    (setq major-mode 'magit-revision-mode)
    (setq-local magit-buffer-refname "abc123")
    (let ((context (ai-code-magit-context)))
      (should (eq (plist-get context :type) 'committed))
      (should (string-match-p "abc123" (plist-get context :text))))))

(ert-deftest ai-code-magit-context-rejects-non-hunk ()
  "Do not silently turn a file heading into a whole-file request."
  (ai-code-magit-test--with-diff
    (goto-char (oref file-section start))
    (should-error (ai-code-magit-context) :type 'user-error)))

(ert-deftest ai-code-magit-context-rejects-invalid-selection ()
  "Do not silently drop selected text spanning incompatible sections."
  (ai-code-magit-test--with-diff
    (set-mark (oref first-hunk content))
    (goto-char (oref second-hunk content))
    (setq mark-active t)
    (should-error (ai-code-magit-context) :type 'user-error)))

(ert-deftest ai-code-magit-context-rejects-pseudo-hunk ()
  "A chmod or rename pseudo-hunk is not a textual patch."
  (ai-code-magit-test--with-diff
    (oset first-hunk value '(chmod))
    (should-error (ai-code-magit-context) :type 'user-error)))

(defun ai-code-magit-test--prompt (command &optional argument)
  "Capture the prompt generated by COMMAND with ARGUMENT."
  (let (result)
    (cl-letf (((symbol-function 'ai-code--insert-prompt)
               (lambda (text) (setq result text)))
              ((symbol-function 'ai-code-read-string)
               (lambda (&rest _) "Keep compatibility"))
              ((symbol-function 'ai-code--format-repo-context-info)
               (lambda () "Existing task context"))
              ((symbol-function 'ai-code--get-clipboard-text)
               (lambda () "Clipboard constraint")))
      (if (memq command '(ai-code-ask-question ai-code-code-change))
          (funcall command argument)
        (funcall command)))
    result))

(ert-deftest ai-code-magit-explain-hunk-read-only ()
  "Explain changes, not merely the resulting source."
  (ai-code-magit-test--with-diff
    (let ((prompt (ai-code-magit-test--prompt #'ai-code-explain)))
      (should (string-match-p "behavior" prompt))
      (should (string-match-p "new-value" prompt))
      (should (eq (ai-code--simple-classify-prompt-code-change prompt)
                  'non-code-change)))))

(ert-deftest ai-code-magit-ask-question-hunk-read-only ()
  "Questions preserve user text, clipboard and existing task context."
  (ai-code-magit-test--with-diff
    (let ((prompt (ai-code-magit-test--prompt #'ai-code-ask-question t)))
      (dolist (text '("Keep compatibility" "Clipboard constraint"
                      "Existing task context" "new-value"))
        (should (string-match-p text prompt)))
      (should (eq (ai-code--simple-classify-prompt-code-change prompt)
                  'non-code-change)))))

(ert-deftest ai-code-magit-change-hunk-rereads-source ()
  "Change feedback targets current source without staging or committing."
  (ai-code-magit-test--with-diff
    (let ((prompt (ai-code-magit-test--prompt #'ai-code-code-change t)))
      (dolist (text '("Keep compatibility" "Clipboard constraint"
                      "Re-read" "Do not stage" "new-value"))
        (should (string-match-p text prompt)))
      (should (eq (ai-code--simple-classify-prompt-code-change prompt)
                  'code-change)))))

(ert-deftest ai-code-magit-tests-add-coverage-without-fake-red ()
  "Test an existing change without forcing Red/Green or source fixes."
  (ai-code-magit-test--with-diff
    (let ((prompt (ai-code-magit-test--prompt #'ai-code-tdd-cycle)))
      (dolist (text '("existing tests" "Run" "Do not modify production code"
                      "Do not claim a test-first" "new-value"))
        (should (string-match-p text prompt))))))

(ert-deftest ai-code-magit-tests-disable-conflicting-auto-tdd ()
  "The hunk test request must not append a strict implementation TDD loop."
  (ai-code-magit-test--with-diff
    (let ((ai-code-auto-test-type 'tdd)
          (seen 'unset))
      (cl-letf (((symbol-function 'ai-code--insert-prompt)
                 (lambda (_) (setq seen ai-code-auto-test-type))))
        (ai-code-tdd-cycle))
      (should-not seen)
      (should (eq ai-code-auto-test-type 'tdd)))))

(ert-deftest ai-code-magit-history-write-commands-refuse ()
  "Historical diffs are references, not current editing targets."
  (ai-code-magit-test--with-diff
    (setq major-mode 'magit-revision-mode)
    (dolist (command '(ai-code-code-change ai-code-tdd-cycle))
      (should-error (ai-code-magit-test--prompt command) :type 'user-error))))

(ert-deftest ai-code-magit-add-context-captures-hunk-before-prompting ()
  "Store the actual snapshot, not just a filename or the Magit buffer."
  (ai-code-magit-test--with-diff
    (let ((ai-code--repo-context-info (make-hash-table :test 'equal)))
      (ai-code-add-context)
      (let ((entry (car (gethash "/tmp/ai-code-magit-project/"
                                 ai-code--repo-context-info))))
        (should (string-match-p "new-value" entry))
        (should (string-match-p "snapshot" entry))
        (should-not (string-match-p "new-other" entry))))))

(ert-deftest ai-code-magit-copy-context-copies-patch-not-branch ()
  "The context menu's copy action should include the selected patch."
  (ai-code-magit-test--with-diff
    (let (copied)
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (text &rest _) (setq copied text))))
        (ai-code-copy-buffer-file-name-to-clipboard))
      (should (string-match-p "new-value" copied)))))

(ert-deftest ai-code-magit-context-menu-preserves-snapshot ()
  "Capture multiple hunks before the context menu can alter the selection."
  (dolist (action '("Copy context" "Add context"))
    (ai-code-magit-test--with-diff
      (set-mark (oref first-hunk start))
      (goto-char (oref second-hunk start))
      (setq mark-active t)
      (let ((ai-code--repo-context-info (make-hash-table :test 'equal))
            copied)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _)
                     (setq mark-active nil)
                     (goto-char (oref second-hunk start))
                     action))
                  ((symbol-function 'kill-new)
                   (lambda (text &rest _) (setq copied text)))
                  ((symbol-function 'ai-code-list-context) #'ignore))
          (ai-code-context-action nil))
        (let ((text (or copied (car (gethash "/tmp/ai-code-magit-project/"
                                            ai-code--repo-context-info)))))
          (should (string-match-p "new-value" text))
          (should (string-match-p "new-other" text)))))))

(ert-deftest ai-code-magit-context-real-revision-identity ()
  "Capture Magit's revision and resolved OID, not merely current HEAD."
  (ai-code-magit-test--with-diff
    (setq major-mode 'magit-revision-mode)
    (setq-local magit-buffer-revision "release~2")
    (setq-local magit-buffer-revision-oid "abcdef0123456789")
    (let ((text (plist-get (ai-code-magit-context) :text)))
      (should (string-match-p "release~2" text))
      (should (string-match-p "abcdef0123456789" text)))))

(ert-deftest ai-code-magit-context-diff-range-and-direction ()
  "Preserve a displayed base and reversal instead of assuming index to disk."
  (ai-code-magit-test--with-diff
    (setq major-mode 'magit-diff-mode)
    (setq-local magit-buffer-diff-type 'unstaged)
    (setq-local magit-buffer-diff-range "HEAD")
    (setq-local magit-buffer-diff-range-oids '("0123456789abcdef"))
    (setq-local magit-buffer-diff-args '("-R" "--ignore-space-change"))
    (let ((text (plist-get (ai-code-magit-context) :text)))
      (should (string-match-p (regexp-quote "range=\"HEAD\"") text))
      (should (string-match-p "-R" text))
      (should-not (string-match-p "index -> working tree" text)))))

(ert-deftest ai-code-magit-read-only-disables-implementation-harness ()
  "Read-only commands suppress persistent TDD suffixes for their handoff."
  (ai-code-magit-test--with-diff
    (dolist (command '(ai-code-explain ai-code-ask-question))
      (let ((ai-code-auto-test-type 'tdd)
            (seen 'unset))
        (cl-letf (((symbol-function 'ai-code--insert-prompt)
                   (lambda (_) (setq seen ai-code-auto-test-type)))
                  ((symbol-function 'ai-code-read-string)
                   (lambda (&rest _) "Why?")))
          (if (eq command 'ai-code-ask-question)
              (funcall command nil)
            (funcall command)))
        (should-not seen)
        (should (eq ai-code-auto-test-type 'tdd))))))

(ert-deftest ai-code-magit-context-washed-addition-and-deletion ()
  "Extract real Magit parser output, preserving /dev/null and patch headers."
  (dolist (addition '(t nil))
    (ai-code-magit-test--with-diff
      (erase-buffer)
      (magit-insert-section (root)
        (magit-insert-section (unstaged)
          (magit-insert-heading "Unstaged changes")
          (let ((start (point)))
            (insert
             (if addition
                 (concat "diff --git added.el added.el\n"
                         "new file mode 100644\n"
                         "index 0000000..e69de29\n"
                         "--- /dev/null\n+++ added.el\n"
                         "@@ -0,0 +1 @@\n+hello\n")
               (concat "diff --git deleted.el deleted.el\n"
                       "deleted file mode 100644\n"
                       "index e69de29..0000000\n"
                       "--- deleted.el\n+++ /dev/null\n"
                       "@@ -1 +0,0 @@\n-goodbye\n")))
            (goto-char start)
            (magit-diff-wash-diffs nil))))
      (goto-char (point-min))
      (re-search-forward "^@@")
      (let ((text (plist-get (ai-code-magit-context) :text)))
        (should (string-match-p (if addition "Path: added.el" "Path: deleted.el") text))
        (should (string-match-p (regexp-quote (if addition "--- /dev/null" "+++ /dev/null")) text))
        (should (string-match-p (if addition "hello" "goodbye") text))))))

(ert-deftest ai-code-magit-snapshot-survives-prompt-path-preprocessing ()
  "Keep patch tokens literal when captured context is reused in a prompt."
  (ai-code-magit-test--with-diff
    (let ((ai-code--repo-context-info (make-hash-table :test 'equal)))
      (ai-code-add-context)
      (let* ((stored (ai-code--format-repo-context-info))
             (prompt (concat "outside " stored))
             (patch "diff --git a/src/old.el b/src/example.el"))
        (cl-letf (((symbol-function 'ai-code--process-word-for-filepath)
                   (lambda (word _) (concat "@" word))))
          (let ((processed (ai-code--preprocess-prompt-text prompt)))
            (should (string-prefix-p "@outside " processed))
            (should (string-match-p (regexp-quote patch) processed))
            (should (string-match-p (regexp-quote "-old-value\n+new-value")
                                    processed))))))))

(provide 'test_ai-code-magit)
;;; test_ai-code-magit.el ends here
