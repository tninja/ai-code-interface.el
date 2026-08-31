;;; test_ai-code-prompt-editing.el --- Tests for ai-code-prompt-editing -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for editing support in AI prompt task files: @ reference gating,
;; navigation, long-form writing, and reuse of the existing prompt corpus.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'ai-code-prompt-editing)
(require 'ai-code-prompt-mode)

(defmacro ai-code-test-with-prompt-text (text &rest body)
  "Insert TEXT into a temp buffer, leave point at end, then run BODY.
Point is left after the final character of TEXT, which is how the
auto-trigger hook sees the buffer right after a sigil is typed."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (goto-char (point-max))
     ,@body))

(defmacro ai-code-test-with-task-org (&rest body)
  "Run BODY in an `org-mode' buffer holding a deep task outline."
  (declare (indent 0))
  `(with-temp-buffer
     (let ((org-mode-hook nil))
       (org-mode))
     (insert "* Task Description\n"
             "Top level prose.\n"
             "* Investigation\n"
             "** Root cause\n"
             "*** Candidate A\n"
             "**** Deep detail\n"
             "Deep prose here.\n"
             "* Code Change\n")
     (goto-char (point-min))
     ,@body))

(defmacro ai-code-test-with-corpus (&rest body)
  "Create a throwaway repository with a task-file corpus, then run BODY.
Binds `corpus-root' to the repository root and `corpus-dir' to its task
directory."
  (declare (indent 0))
  `(let* ((corpus-root (make-temp-file "ai-code-corpus" t))
          (corpus-dir (expand-file-name ".ai.code.files" corpus-root)))
     (unwind-protect
         (progn
           (make-directory corpus-dir t)
           (with-temp-file (expand-file-name "one.org" corpus-dir)
             (insert "* Investigation\n"
                     "Refactor the payment retry logic.\n"
                     "* Code Change\n"
                     "Add a regression test for retries.\n"))
           (with-temp-file (expand-file-name "two.org" corpus-dir)
             (insert "* Investigation\n"
                     "Investigate the flaky upload test.\n"))
           ,@body)
       (delete-directory corpus-root t))))


;;;; Reference gating

;;; Scenario: Java annotation inside a src block does not trigger

(ert-deftest ai-code-test-prompt-reference-rejects-annotation-in-src-block ()
  "A Java annotation typed inside a src block is not a file reference."
  (ai-code-test-with-prompt-text
      "Look at this:\n#+begin_src java\n@Override\n"
    ;; Point sits on the empty line after "@Override"; place it just after
    ;; the "@" the user would have typed.
    (goto-char (point-min))
    (should (search-forward "@" nil t))
    (should-not (ai-code--prompt-reference-position-p))))

;;; Scenario: Annotation in an example block does not trigger

(ert-deftest ai-code-test-prompt-reference-rejects-annotation-in-example-block ()
  "An annotation typed inside an example block is not a file reference."
  (ai-code-test-with-prompt-text
      "#+begin_example\n@Deprecated"
    (should-not (ai-code--prompt-reference-position-p))))

(ert-deftest ai-code-test-prompt-reference-accepts-after-closed-block ()
  "A reference typed after a block has been closed still triggers."
  (ai-code-test-with-prompt-text
      "#+begin_src java\ncode();\n#+end_src\nNow check @"
    (should (ai-code--prompt-reference-position-p))))

;;; Scenario: Email-like text does not trigger

(ert-deftest ai-code-test-prompt-reference-rejects-email-like-at ()
  "An \"@\" following a word character is part of a word, not a reference."
  (ai-code-test-with-prompt-text "mail tninja@"
    (should-not (ai-code--prompt-reference-position-p))))

;;; Scenario: Real reference at word start triggers

(ert-deftest ai-code-test-prompt-reference-accepts-after-whitespace ()
  "An \"@\" following whitespace starts a file reference."
  (ai-code-test-with-prompt-text "Please read @"
    (should (ai-code--prompt-reference-position-p))))

(ert-deftest ai-code-test-prompt-reference-accepts-at-beginning-of-line ()
  "An \"@\" at the beginning of a line starts a file reference."
  (ai-code-test-with-prompt-text "Please read:\n@"
    (should (ai-code--prompt-reference-position-p))))

(ert-deftest ai-code-test-prompt-reference-accepts-after-open-paren ()
  "An \"@\" after a punctuation delimiter starts a file reference."
  (ai-code-test-with-prompt-text "see (@"
    (should (ai-code--prompt-reference-position-p))))

(ert-deftest ai-code-test-prompt-reference-accepts-at-buffer-start ()
  "An \"@\" as the very first character starts a file reference."
  (ai-code-test-with-prompt-text "@"
    (should (ai-code--prompt-reference-position-p))))

;;; Scenario: the auto-trigger path honours the guard

(ert-deftest ai-code-test-prompt-auto-trigger-skips-annotation-in-src-block ()
  "The inline auto-trigger must not fire for an annotation in a src block."
  (let ((started nil))
    (cl-letf (((symbol-function 'ai-code--prompt-start-inline-reference-completion)
               (lambda () (setq started t))))
      (ai-code-test-with-prompt-text "#+begin_src java\n@"
        (let ((before (buffer-string)))
          (ai-code--prompt-auto-trigger-filepath-completion)
          (should-not started)
          (should (string= before (buffer-string))))))))

(ert-deftest ai-code-test-prompt-auto-trigger-starts-inline-completion ()
  "A valid sigil starts inline completion without opening a minibuffer."
  (let ((started nil))
    (cl-letf (((symbol-function 'ai-code--prompt-start-inline-reference-completion)
               (lambda () (setq started t)))
              ((symbol-function 'completing-read)
               (lambda (&rest _)
                 (ert-fail "Auto-trigger called completing-read"))))
      (ai-code-test-with-prompt-text "read @"
        (let ((before (buffer-string)))
          (ai-code--prompt-auto-trigger-filepath-completion)
          (should started)
          (should (string= before (buffer-string))))))))

(ert-deftest ai-code-test-prompt-capf-skips-annotation-in-src-block ()
  "The capf path must not offer candidates for an annotation in a src block."
  (cl-letf (((symbol-function 'ai-code--git-root) (lambda (&rest _) "/tmp/repo/"))
            ((symbol-function 'ai-code--prompt-filepath-candidates)
             (lambda () '("@src/main.el"))))
    (ai-code-test-with-prompt-text "#+begin_src java\n@Over"
      (should-not (ai-code--prompt-filepath-capf)))))

;;; Scenario: Explicit blocking selector remains available

(ert-deftest ai-code-test-prompt-complete-reference-ignores-guard ()
  "The explicit command completes even where auto-triggering is suppressed."
  (cl-letf (((symbol-function 'ai-code--git-root) (lambda (&rest _) "/tmp/repo/"))
            ((symbol-function 'ai-code--prompt-filepath-candidates)
             (lambda () '("@src/main.el")))
            ((symbol-function 'completing-read)
             (lambda (&rest _) "@src/main.el")))
    (ai-code-test-with-prompt-text "#+begin_src java\n"
      (ai-code-prompt-complete-reference)
      (should (string-match-p (regexp-quote "@src/main.el") (buffer-string))))))

(ert-deftest ai-code-test-prompt-complete-reference-replaces-partial-reference ()
  "The explicit command replaces an already-typed partial reference."
  (cl-letf (((symbol-function 'ai-code--git-root) (lambda (&rest _) "/tmp/repo/"))
            ((symbol-function 'ai-code--prompt-filepath-candidates)
             (lambda () '("@src/main.el")))
            ((symbol-function 'completing-read)
             (lambda (&rest _) "@src/main.el")))
    (ai-code-test-with-prompt-text "read @src/ma"
      (ai-code-prompt-complete-reference)
      (should (string= "read @src/main.el" (buffer-string))))))

(ert-deftest ai-code-test-prompt-complete-reference-preserves-ordinary-text ()
  "The explicit command does not delete text that lacks an @ sigil."
  (cl-letf (((symbol-function 'ai-code--git-root) (lambda (&rest _) "/tmp/repo/"))
            ((symbol-function 'ai-code--prompt-filepath-candidates)
             (lambda () '("@src/main.el")))
            ((symbol-function 'completing-read)
             (lambda (&rest _) "@src/main.el")))
    (ai-code-test-with-prompt-text "keep"
      (ai-code-prompt-complete-reference)
      (should (string= "keep@src/main.el" (buffer-string))))))

(ert-deftest ai-code-test-prompt-complete-reference-errors-without-repo ()
  "The explicit command reports a user error outside a repository."
  (cl-letf (((symbol-function 'ai-code--git-root) (lambda (&rest _) nil)))
    (ai-code-test-with-prompt-text "read "
      (should-error (ai-code-prompt-complete-reference) :type 'user-error))))

;;; Scenario: Inline, non-blocking completion inside prompt mode

;; Declared special so `let' binds it dynamically even when company is absent,
;; which is the case in continuous integration.  The helm option is deliberately
;; NOT declared here: the implementation must cope with it being void.
(defvar company-backends nil)
(defvar company-mode nil)

(ert-deftest ai-code-test-prompt-inline-completion-opts-out-of-helm ()
  "Setup registers the mode in helm's supported opt-out list."
  (let ((helm-mode-no-completion-in-region-in-modes nil))
    (with-temp-buffer
      (ai-code--prompt-setup-inline-completion)
      (should (memq 'ai-code-prompt-mode
                    helm-mode-no-completion-in-region-in-modes)))))

(ert-deftest ai-code-test-prompt-inline-completion-opt-out-is-idempotent ()
  "Repeated setup does not add duplicate entries to helm's opt-out list."
  (let ((helm-mode-no-completion-in-region-in-modes nil))
    (with-temp-buffer
      (ai-code--prompt-setup-inline-completion)
      (ai-code--prompt-setup-inline-completion)
      (should (= 1 (cl-count 'ai-code-prompt-mode
                             helm-mode-no-completion-in-region-in-modes))))))

(ert-deftest ai-code-test-prompt-inline-completion-adds-flex-locally ()
  "Setup appends the flex style buffer-locally without touching the default."
  (let ((global-styles (default-value 'completion-styles)))
    (with-temp-buffer
      (ai-code--prompt-setup-inline-completion)
      (should (memq 'flex completion-styles))
      (should (local-variable-p 'completion-styles))
      (should (equal global-styles (default-value 'completion-styles))))))

(ert-deftest ai-code-test-prompt-inline-completion-preserves-existing-styles ()
  "Setup keeps the styles that were already configured."
  (with-temp-buffer
    (let ((completion-styles '(basic partial-completion)))
      (ai-code--prompt-setup-inline-completion)
      (should (memq 'basic completion-styles))
      (should (memq 'partial-completion completion-styles)))))

(ert-deftest ai-code-test-prompt-inline-completion-survives-missing-company ()
  "Setup degrades silently when company is not installed."
  (cl-letf (((symbol-function 'fboundp)
             (lambda (sym) (not (memq sym '(company-mode))))))
    (with-temp-buffer
      ;; Must not signal even though company appears unavailable.
      (ai-code--prompt-setup-inline-completion)
      ;; The helm opt-out and flex style still apply.
      (should (memq 'flex completion-styles)))))

(ert-deftest ai-code-test-prompt-inline-completion-survives-void-helm-option ()
  "Setup must not signal when helm has never been loaded.
Regression test: the option is void until helm-mode loads, and signalling
here aborts activation of the whole major mode."
  (let ((had-value (boundp 'helm-mode-no-completion-in-region-in-modes))
        (saved (and (boundp 'helm-mode-no-completion-in-region-in-modes)
                    helm-mode-no-completion-in-region-in-modes)))
    (unwind-protect
        (progn
          (makunbound 'helm-mode-no-completion-in-region-in-modes)
          (with-temp-buffer
            (ai-code--prompt-setup-inline-completion)
            ;; The opt-out still records the mode so it applies once helm loads.
            (should (memq 'ai-code-prompt-mode
                          helm-mode-no-completion-in-region-in-modes))))
      (if had-value
          (setq helm-mode-no-completion-in-region-in-modes saved)
        (makunbound 'helm-mode-no-completion-in-region-in-modes)))))

(ert-deftest ai-code-test-prompt-inline-completion-starts-company-capf ()
  "Inline completion uses Company's CAPF backend when Company is active."
  (let ((company-mode t)
        backend)
    (cl-letf (((symbol-function 'company-begin-backend)
               (lambda (value) (setq backend value)))
              ((symbol-function 'completion-at-point)
               (lambda () (ert-fail "CAPF fallback was called"))))
      (ai-code--prompt-start-inline-reference-completion)
      (should (eq backend 'company-capf)))))

(ert-deftest ai-code-test-prompt-inline-completion-tolerates-no-company-candidates ()
  "Inline completion does not signal when Company finds no candidates."
  (let ((company-mode t))
    (cl-letf (((symbol-function 'company-begin-backend)
               (lambda (&rest _) (user-error "Cannot complete at point"))))
      (should-not (ai-code--prompt-start-inline-reference-completion)))))

(ert-deftest ai-code-test-prompt-inline-completion-falls-back-to-capf ()
  "Inline completion uses the standard CAPF UI when Company is inactive."
  (let ((company-mode nil)
        (called nil))
    (cl-letf (((symbol-function 'completion-at-point)
               (lambda () (setq called t))))
      (ai-code--prompt-start-inline-reference-completion)
      (should called))))


;;;; Navigation

;;; Scenario: Jump to a heading by outline path

(ert-deftest ai-code-test-prompt-nav-heading-candidates-use-outline-path ()
  "Candidates identify a deep heading by its full outline path."
  (ai-code-test-with-task-org
    (let* ((candidates (ai-code--prompt-heading-candidates))
           (labels (mapcar #'car candidates)))
      (should (cl-find-if (lambda (label)
                            (and (string-match-p "Investigation" label)
                                 (string-match-p "Root cause" label)
                                 (string-match-p "Deep detail" label)))
                          labels)))))

(ert-deftest ai-code-test-prompt-nav-heading-candidates-are-positions ()
  "Each candidate carries a buffer position for its heading."
  (ai-code-test-with-task-org
    (dolist (candidate (ai-code--prompt-heading-candidates))
      (should (integer-or-marker-p (cdr candidate))))))

(ert-deftest ai-code-test-prompt-nav-goto-heading-moves-point ()
  "Selecting a heading moves point to that heading."
  (ai-code-test-with-task-org
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (cl-find-if (lambda (label) (string-match-p "Deep detail" label))
                             (mapcar #'car collection)))))
      (ai-code-prompt-goto-heading)
      (should (string-match-p "Deep detail"
                              (buffer-substring-no-properties
                               (line-beginning-position) (line-end-position)))))))

(ert-deftest ai-code-test-prompt-nav-goto-heading-errors-without-headings ()
  "Jumping reports a user error when the buffer has no headings."
  (with-temp-buffer
    (let ((org-mode-hook nil)) (org-mode))
    (insert "just prose, no headings\n")
    (should-error (ai-code-prompt-goto-heading) :type 'user-error)))

;;; Scenario: Focus and restore a subtree

(ert-deftest ai-code-test-prompt-nav-focus-subtree-narrows ()
  "Focusing narrows the buffer to the enclosing subtree."
  (ai-code-test-with-task-org
    (goto-char (point-min))
    (should (search-forward "Deep prose here." nil t))
    (ai-code-prompt-focus-subtree)
    (should (buffer-narrowed-p))
    (let ((visible (buffer-string)))
      (should (string-match-p "Deep detail" visible))
      (should-not (string-match-p "Code Change" visible)))))

(ert-deftest ai-code-test-prompt-nav-focus-subtree-toggles-back ()
  "Focusing a second time restores the whole buffer."
  (ai-code-test-with-task-org
    (goto-char (point-min))
    (should (search-forward "Deep prose here." nil t))
    (ai-code-prompt-focus-subtree)
    (should (buffer-narrowed-p))
    (ai-code-prompt-focus-subtree)
    (should-not (buffer-narrowed-p))
    (should (string-match-p "Code Change" (buffer-string)))))

;;; Scenario: Task files fold by default, other org files do not

(ert-deftest ai-code-test-prompt-nav-task-file-is-recognised ()
  "A file under the task directory is recognised as a task file."
  (should (ai-code--prompt-task-file-p "/repo/.ai.code.files/issue-1.org")))

(ert-deftest ai-code-test-prompt-nav-plain-org-file-is-not-task-file ()
  "An Org file outside the task directory is not a task file."
  (should-not (ai-code--prompt-task-file-p "/repo/docs/notes.org")))

(ert-deftest ai-code-test-prompt-nav-nil-file-is-not-task-file ()
  "A buffer without a file is not treated as a task file."
  (should-not (ai-code--prompt-task-file-p nil)))

(ert-deftest ai-code-test-prompt-nav-folds-task-file-only ()
  "Startup folding runs for task files and is skipped elsewhere."
  (let ((folded nil))
    (cl-letf (((symbol-function 'org-content) (lambda (&rest _) (setq folded t))))
      (with-temp-buffer
        (setq buffer-file-name "/repo/.ai.code.files/issue-1.org")
        (ai-code--prompt-apply-startup-folding)
        (should folded)
        (setq buffer-file-name nil))
      (setq folded nil)
      (with-temp-buffer
        (setq buffer-file-name "/repo/docs/notes.org")
        (ai-code--prompt-apply-startup-folding)
        (should-not folded)
        (setq buffer-file-name nil)))))

(ert-deftest ai-code-test-prompt-nav-folding-can-be-disabled ()
  "Setting the folding option to nil disables startup folding."
  (let ((folded nil))
    (cl-letf (((symbol-function 'org-content) (lambda (&rest _) (setq folded t)))
              ((symbol-function 'org-overview) (lambda (&rest _) (setq folded t))))
      (with-temp-buffer
        (setq buffer-file-name "/repo/.ai.code.files/issue-1.org")
        (let ((ai-code-prompt-startup-folded nil))
          (ai-code--prompt-apply-startup-folding))
        (should-not folded)
        (setq buffer-file-name nil)))))


;;;; Writing

;;; Scenario: flyspell ignores code inside blocks

(ert-deftest ai-code-test-prompt-flyspell-skips-src-block ()
  "Spell checking is suppressed inside a source block."
  (with-temp-buffer
    (insert "#+begin_src java\nreturn foobarbaz;\n")
    (goto-char (point-min))
    (should (search-forward "foobarbaz" nil t))
    (should-not (ai-code--prompt-flyspell-verify))))

(ert-deftest ai-code-test-prompt-flyspell-skips-example-block ()
  "Spell checking is suppressed inside an example block."
  (with-temp-buffer
    (insert "#+begin_example\nENOSPC qqzz\n")
    (goto-char (point-min))
    (should (search-forward "qqzz" nil t))
    (should-not (ai-code--prompt-flyspell-verify))))

(ert-deftest ai-code-test-prompt-flyspell-checks-prose ()
  "Spell checking still applies to ordinary prose."
  (with-temp-buffer
    (insert "This is ordinary prose.\n")
    (goto-char (point-min))
    (should (search-forward "ordinary" nil t))
    (should (ai-code--prompt-flyspell-verify))))

(ert-deftest ai-code-test-prompt-flyspell-checks-prose-after-block ()
  "Spell checking resumes after a block has been closed."
  (with-temp-buffer
    (insert "#+begin_src java\ncode();\n#+end_src\nordinary prose\n")
    (goto-char (point-min))
    (should (search-forward "ordinary" nil t))
    (should (ai-code--prompt-flyspell-verify))))

;;; Scenario: Wrap pasted logs in an example block

(ert-deftest ai-code-test-prompt-wrap-region-in-block-wraps-example ()
  "Wrapping encloses the active region in an example block."
  (with-temp-buffer
    (insert "line one\nline two\n")
    (let ((transient-mark-mode t))
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (ai-code-prompt-wrap-region-in-block "example"))
    (let ((content (buffer-string)))
      (should (string-match-p "^#\\+begin_example$" content))
      (should (string-match-p "^#\\+end_example$" content))
      (should (string-match-p "line one" content))
      (should (string-match-p "line two" content)))))

(ert-deftest ai-code-test-prompt-wrap-region-keeps-content-order ()
  "Wrapping places the region text between the delimiters, in order."
  (with-temp-buffer
    (insert "payload\n")
    (let ((transient-mark-mode t))
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (ai-code-prompt-wrap-region-in-block "example"))
    (should (string= "#+begin_example\npayload\n#+end_example\n"
                     (buffer-string)))))

(ert-deftest ai-code-test-prompt-wrap-region-supports-src-block ()
  "Wrapping accepts a source block type with a language."
  (with-temp-buffer
    (insert "int x = 1;\n")
    (let ((transient-mark-mode t))
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (ai-code-prompt-wrap-region-in-block "src java"))
    (let ((content (buffer-string)))
      (should (string-match-p "^#\\+begin_src java$" content))
      (should (string-match-p "^#\\+end_src$" content)))))

(ert-deftest ai-code-test-prompt-wrap-region-errors-without-region ()
  "Wrapping reports a user error when no region is active."
  (with-temp-buffer
    (insert "text\n")
    (deactivate-mark)
    (should-error (ai-code-prompt-wrap-region-in-block "example")
                  :type 'user-error)))

;;; Scenario: Block marking is reachable

(ert-deftest ai-code-test-prompt-mark-block-has-binding ()
  "The block-marking command is reachable from the prompt mode map."
  (should (eq 'ai-code--mark-prompt-block
              (lookup-key ai-code-prompt-mode-map (kbd "C-c m")))))

(ert-deftest ai-code-test-prompt-bindings-do-not-shadow-org-ctrl-c-ctrl-c ()
  "The send binding on C-c C-c is preserved."
  (should (eq 'ai-code-prompt-send-block
              (lookup-key ai-code-prompt-mode-map (kbd "C-c C-c")))))

(ert-deftest ai-code-test-prompt-bindings-preserve-org-mark-subtree ()
  "Prompt mode preserves Org's subtree marking binding on C-c @."
  (should (eq 'org-mark-subtree
              (lookup-key ai-code-prompt-mode-map (kbd "C-c @")))))

(ert-deftest ai-code-test-prompt-complete-reference-has-nonconflicting-binding ()
  "Explicit reference completion remains available on C-c r."
  (should (eq 'ai-code-prompt-complete-reference
              (lookup-key ai-code-prompt-mode-map (kbd "C-c r")))))

;;; Scenario: writing mode is opt-in and degrades without its dependency

(ert-deftest ai-code-test-prompt-writing-mode-enables-visual-line ()
  "Enabling writing mode turns on visual line wrapping."
  (with-temp-buffer
    (ai-code-prompt-writing-mode 1)
    (should visual-line-mode)))

(ert-deftest ai-code-test-prompt-writing-mode-disables-cleanly ()
  "Disabling writing mode turns visual line wrapping back off."
  (with-temp-buffer
    (ai-code-prompt-writing-mode 1)
    (ai-code-prompt-writing-mode -1)
    (should-not visual-line-mode)))

(ert-deftest ai-code-test-prompt-writing-mode-survives-missing-fill-column ()
  "Writing mode does not signal when visual-fill-column is absent."
  (cl-letf (((symbol-function 'fboundp)
             (lambda (sym) (not (memq sym '(visual-fill-column-mode))))))
    (with-temp-buffer
      (ai-code-prompt-writing-mode 1)
      (should visual-line-mode))))


;;;; Reuse

;;; Scenario: Search current repo corpus by default

(ert-deftest ai-code-test-prompt-reuse-default-root-is-current-repo ()
  "Without a prefix argument the search covers the current repository only."
  (ai-code-test-with-corpus
    (cl-letf (((symbol-function 'ai-code--get-files-directory)
               (lambda () corpus-dir)))
      (should (equal (list corpus-dir) (ai-code--prompt-corpus-roots nil))))))

(ert-deftest ai-code-test-prompt-reuse-finds-lines-in-corpus ()
  "Corpus search returns prose lines from the task files."
  (ai-code-test-with-corpus
    (let ((lines (ai-code--prompt-corpus-lines (list corpus-dir))))
      (should (cl-find-if (lambda (line) (string-match-p "payment retry" line))
                          lines))
      (should (cl-find-if (lambda (line) (string-match-p "flaky upload" line))
                          lines)))))

(ert-deftest ai-code-test-prompt-reuse-excludes-short-fragments ()
  "Corpus search omits lines too short to be reusable prompts."
  (ai-code-test-with-corpus
    (with-temp-file (expand-file-name "short.org" corpus-dir)
      (insert "ok\n" "yes\n" "TODO\n"))
    (let ((lines (ai-code--prompt-corpus-lines (list corpus-dir))))
      (should-not (member "ok" lines))
      (should-not (member "yes" lines))
      (should-not (member "TODO" lines))
      ;; Every surviving candidate meets the configured minimum length.
      (dolist (line lines)
        (should (>= (length line) ai-code-prompt-corpus-min-length))))))

(ert-deftest ai-code-test-prompt-reuse-excludes-org-headings ()
  "Corpus search omits Org headings, which are structure rather than prompts."
  (ai-code-test-with-corpus
    (let ((lines (ai-code--prompt-corpus-lines (list corpus-dir))))
      (should-not (cl-find-if (lambda (line)
                                (string-match-p "^\\*+ " line))
                              lines)))))

(ert-deftest ai-code-test-prompt-reuse-deduplicates-lines ()
  "Identical prompt lines across files collapse to a single candidate."
  (ai-code-test-with-corpus
    (with-temp-file (expand-file-name "three.org" corpus-dir)
      (insert "Refactor the payment retry logic.\n"))
    (let* ((lines (ai-code--prompt-corpus-lines (list corpus-dir)))
           (matches (cl-count-if
                     (lambda (line) (string-match-p "payment retry" line))
                     lines)))
      (should (= 1 matches)))))

(ert-deftest ai-code-test-prompt-reuse-handles-missing-directory ()
  "A nonexistent corpus directory yields no candidates instead of an error."
  (should-not (ai-code--prompt-corpus-lines
               (list "/nonexistent/ai-code-corpus-dir"))))

;;; Scenario: Prefix argument widens to all known corpora

(ert-deftest ai-code-test-prompt-reuse-prefix-widens-to-all-roots ()
  "With a prefix argument every configured corpus root is searched."
  (ai-code-test-with-corpus
    (cl-letf (((symbol-function 'ai-code--get-files-directory)
               (lambda () corpus-dir)))
      (let ((ai-code-prompt-corpus-additional-roots '("/extra/corpus")))
        (let ((roots (ai-code--prompt-corpus-roots t)))
          (should (member corpus-dir roots))
          (should (member "/extra/corpus" roots)))))))

(ert-deftest ai-code-test-prompt-reuse-prefix-deduplicates-roots ()
  "A root listed twice is searched once."
  (ai-code-test-with-corpus
    (cl-letf (((symbol-function 'ai-code--get-files-directory)
               (lambda () corpus-dir)))
      (let ((ai-code-prompt-corpus-additional-roots (list corpus-dir)))
        (should (= 1 (length (ai-code--prompt-corpus-roots t))))))))

;;; Scenario: Selected historical prompt is inserted at point

(ert-deftest ai-code-test-prompt-reuse-inserts-selection-at-point ()
  "The chosen historical prompt is inserted at point."
  (ai-code-test-with-corpus
    (cl-letf (((symbol-function 'ai-code--get-files-directory)
               (lambda () corpus-dir))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (cl-find-if (lambda (line) (string-match-p "payment retry" line))
                             collection))))
      (with-temp-buffer
        (insert "prefix ")
        (ai-code-prompt-insert-from-history nil)
        (should (string-match-p "^prefix .*payment retry" (buffer-string)))))))

(ert-deftest ai-code-test-prompt-reuse-insert-errors-on-empty-corpus ()
  "An empty corpus reports a user error rather than inserting nothing."
  (let ((empty (make-temp-file "ai-code-empty-corpus" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--get-files-directory)
                   (lambda () empty)))
          (with-temp-buffer
            (should-error (ai-code-prompt-insert-from-history nil)
                          :type 'user-error)))
      (delete-directory empty t))))

;;; Scenario: Snippets are discoverable by description

(ert-deftest ai-code-test-prompt-reuse-snippet-candidates-use-names ()
  "Snippet candidates are labelled by name, not by key abbreviation."
  (let ((candidates (ai-code--prompt-snippet-candidates)))
    (should candidates)
    ;; "Commit Message" is the name of the snippet whose key is "commit".
    (should (cl-find-if (lambda (candidate)
                          (string-match-p "Commit Message" (car candidate)))
                        candidates))))

(ert-deftest ai-code-test-prompt-reuse-snippet-candidates-carry-keys ()
  "Each snippet candidate carries the key needed to expand it."
  (dolist (candidate (ai-code--prompt-snippet-candidates))
    (should (stringp (cdr candidate)))
    (should-not (string-empty-p (cdr candidate)))))

(ert-deftest ai-code-test-prompt-reuse-snippet-parses-name-and-key ()
  "Snippet metadata is parsed from the yasnippet header comments."
  (let ((file (make-temp-file "ai-code-snippet")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "# -*- mode: snippet -*-\n"
                    "# name: Explain Code\n"
                    "# key: explain\n"
                    "# --\n"
                    "Explain the following code.\n"))
          (let ((parsed (ai-code--prompt-parse-snippet-file file)))
            (should (equal "Explain Code" (car parsed)))
            (should (equal "explain" (cdr parsed)))))
      (delete-file file))))

;;; Scenario: Context helpers return live values

(ert-deftest ai-code-test-prompt-reuse-ctx-file-returns-relative-path ()
  "The file helper returns a repository-relative path."
  (cl-letf (((symbol-function 'ai-code--git-root) (lambda (&rest _) "/repo/")))
    (with-temp-buffer
      (setq buffer-file-name "/repo/src/main.el")
      (should (equal "src/main.el" (ai-code--prompt-ctx-file)))
      (setq buffer-file-name nil))))

(ert-deftest ai-code-test-prompt-reuse-ctx-file-nil-without-file ()
  "The file helper returns nil in a buffer with no file."
  (with-temp-buffer
    (should-not (ai-code--prompt-ctx-file))))

(ert-deftest ai-code-test-prompt-reuse-ctx-branch-returns-branch ()
  "The branch helper returns the current branch name."
  (cl-letf (((symbol-function 'magit-get-current-branch) (lambda () "kang_feat_11")))
    (should (equal "kang_feat_11" (ai-code--prompt-ctx-branch)))))

(ert-deftest ai-code-test-prompt-reuse-ctx-branch-nil-outside-repo ()
  "The branch helper returns nil when there is no branch."
  (cl-letf (((symbol-function 'magit-get-current-branch) (lambda () nil)))
    (should-not (ai-code--prompt-ctx-branch))))

(ert-deftest ai-code-test-prompt-reuse-ctx-defun-nil-without-function ()
  "The defun helper returns nil when point is not inside a function."
  (cl-letf (((symbol-function 'which-function) (lambda () nil)))
    (with-temp-buffer
      (should-not (ai-code--prompt-ctx-defun)))))

(ert-deftest ai-code-test-prompt-reuse-ctx-defun-returns-name ()
  "The defun helper returns the enclosing function name."
  (cl-letf (((symbol-function 'which-function) (lambda () "my-function")))
    (with-temp-buffer
      (should (equal "my-function" (ai-code--prompt-ctx-defun))))))

(provide 'test_ai-code-prompt-editing)

;;; test_ai-code-prompt-editing.el ends here
