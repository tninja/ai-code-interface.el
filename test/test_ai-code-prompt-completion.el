;;; test_ai-code-prompt-completion.el --- Tests for prompt history completion -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Covers the hand-written prompt completion source and the input history
;; file round-trip that feeds `ai-code-helm-read-string-with-history'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-prompt-completion)
(require 'ai-code-input)

;; Declared so the tests can bind it whether or not projectile is loaded.
(defvar projectile-known-projects nil)

(defun ai-code-prompt-completion-test--make-root (content &optional nested)
  "Create a temp project root holding a prompt file with CONTENT.
When NESTED is non-nil the file goes under `ai-code-files-dir-name'."
  (let* ((root (make-temp-file "ai-code-prompt-completion-root" t))
         (dir (if nested
                  (let ((nested-dir (expand-file-name ai-code-files-dir-name root)))
                    (make-directory nested-dir t)
                    nested-dir)
                root))
         (file (expand-file-name ai-code-prompt-file-name dir)))
    (with-temp-file file (insert content))
    root))

(defun ai-code-prompt-completion-test--make-file (content &optional suffix)
  "Write CONTENT to a new temp file ending in SUFFIX and return its name."
  (make-temp-file "ai-code-prompt-completion-file" nil (or suffix ".org")
                  content))

(defmacro ai-code-prompt-completion-test--with-roots (roots &rest body)
  "Run BODY with the prompt history index scoped to ROOTS only."
  (declare (indent 1))
  `(let ((ai-code-prompt-completion-roots ,roots)
         (ai-code-prompt-completion-files nil)
         (ai-code-prompt-completion-use-org-roam nil)
         (ai-code-prompt-fallback-directory nil)
         (ai-code-prompt-completion--index nil)
         (projectile-known-projects nil)
         (default-directory (make-temp-file "ai-code-prompt-completion-cwd" t)))
     ,@body))

(defconst ai-code-prompt-completion-test--corpus "\
* [2026-01-01 Thu 10:00]
:PROPERTIES:
:AGENT: Claude Code
:END:
Go ahead with the suggested refactoring

* [2026-01-02 Fri 10:00]
:PROPERTIES:
:AGENT: Claude Code
:END:
Goal:
Scope: current file
Instruction: do the thing

* [2026-01-03 Sat 10:00]
Go ahead with the suggested refactoring

* [2026-01-04 Sun 10:00]
investigate the issue and answer the question
"
  "Prompt history sample with a repeat, a generated prompt, and a bare body.")

(ert-deftest ai-code-prompt-completion-test-skips-generated-prompts ()
  "Prompts ai-code generated itself are not offered back as candidates."
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root
             ai-code-prompt-completion-test--corpus))
    (let ((candidates (nth 0 (ai-code-prompt-completion--ensure-index))))
      (should (member "Go ahead with the suggested refactoring" candidates))
      (should (member "investigate the issue and answer the question" candidates))
      (should-not (cl-some (lambda (candidate) (string-prefix-p "Goal:" candidate))
                           candidates)))))

(ert-deftest ai-code-prompt-completion-test-strips-property-drawer ()
  "The :PROPERTIES: drawer never leaks into a candidate."
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root
             ai-code-prompt-completion-test--corpus))
    (should-not (cl-some (lambda (candidate) (string-match-p ":AGENT:" candidate))
                         (nth 0 (ai-code-prompt-completion--ensure-index))))))

(ert-deftest ai-code-prompt-completion-test-drops-oversized-entries ()
  "Entries longer than `ai-code-prompt-completion-max-lines' are left out."
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root
             "* [2026-01-01 Thu 10:00]\nshort prompt\n
* [2026-01-02 Fri 10:00]\nline1\nline2\nline3\nline4\nline5\nline6\n"))
    (let ((candidates (nth 0 (ai-code-prompt-completion--ensure-index))))
      (should (equal candidates '("short prompt" "prompt"))))))

(ert-deftest ai-code-prompt-completion-test-orders-by-frequency ()
  "The prompt written most often sorts to the top of the popup."
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root
             ai-code-prompt-completion-test--corpus))
    (should (equal (car (nth 0 (ai-code-prompt-completion--ensure-index)))
                   "Go ahead with the suggested refactoring"))))

(ert-deftest ai-code-prompt-completion-test-indexes-every-root ()
  "Prompt files from several repositories land in one index."
  (let ((plain (ai-code-prompt-completion-test--make-root
                "* [2026-01-01 Thu 10:00]\nprompt from repo one\n"))
        (nested (ai-code-prompt-completion-test--make-root
                 "* [2026-01-01 Thu 10:00]\nprompt from repo two\n" t)))
    (ai-code-prompt-completion-test--with-roots (list plain nested)
      (let ((index (ai-code-prompt-completion--ensure-index)))
        (should (equal (nth 2 index) 2))
        (should (member "prompt from repo one" (nth 0 index)))
        (should (member "prompt from repo two" (nth 0 index)))))))

(ert-deftest ai-code-prompt-completion-test-ignores-pasted-headline-fragments ()
  "A pasted document whose lines start with \"* \" is not split into entries."
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root "\
* [2026-01-01 Thu 10:00]
:PROPERTIES:
:AGENT: Claude Code
:END:
summarize the design document

* Purpose
explain what this is for

* Important Modules
list the modules
"))
    (should (equal (nth 0 (ai-code-prompt-completion--ensure-index))
                   '("summarize the design document" "the design document"
                     "design document" "document")))))

(ert-deftest ai-code-prompt-completion-test-keeps-gptel-headline-entries ()
  "A generated headline still counts when the property drawer is present."
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root "\
* Refactor the parser for clarity
:PROPERTIES:
:AGENT: Claude Code
:END:
split the parser into smaller functions
"))
    (should (equal (car (nth 0 (ai-code-prompt-completion--ensure-index)))
                   "split the parser into smaller functions"))))

(ert-deftest ai-code-prompt-completion-test-skips-remote-roots ()
  "A remote project root is never probed, so indexing cannot hang on Tramp."
  (ai-code-prompt-completion-test--with-roots
      (list "/ssh:nowhere.invalid:/srv/repo" "/docker:container:/etc")
    (cl-letf (((symbol-function 'file-readable-p)
               (lambda (file)
                 (when (file-remote-p file)
                   (ert-fail (format "probed remote file %s" file)))
                 nil)))
      (should-not (ai-code-prompt-completion--files)))))

(ert-deftest ai-code-prompt-completion-test-capf-uses-word-bounds ()
  "The capf reports the same start position `cape-dict' would."
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root
             ai-code-prompt-completion-test--corpus))
    (with-temp-buffer
      (insert "Go")
      (let ((result (ai-code-prompt-completion-capf)))
        (should result)
        (should (equal (list (nth 0 result) (nth 1 result))
                       (list (car (bounds-of-thing-at-point 'word))
                             (cdr (bounds-of-thing-at-point 'word)))))
        (should (member "Go ahead with the suggested refactoring" (nth 2 result)))
        (should (eq (plist-get (nthcdr 3 result) :exclusive) 'no))))))

(ert-deftest ai-code-prompt-completion-test-completion-at-point-inserts-prompt ()
  "A prefix of a stored prompt completes to the whole prompt.
Drives the real `completion-at-point' so the capf, the candidate table
and the exit function are exercised together."
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root
             ai-code-prompt-completion-test--corpus))
    (with-temp-buffer
      (setq-local completion-at-point-functions
                  (list #'ai-code-prompt-completion-capf))
      (insert "please Go")
      (completion-at-point)
      (should (equal (buffer-string)
                     "please Go ahead with the suggested refactoring")))))

(ert-deftest ai-code-prompt-completion-test-recall-from-middle-word ()
  "A word from the middle of a prompt stands for the whole prompt."
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root
             ai-code-prompt-completion-test--corpus))
    (let ((index (ai-code-prompt-completion--ensure-index)))
      (should (member "refactoring" (nth 0 index)))
      (should (equal (gethash "refactoring" (nth 1 index))
                     "Go ahead with the suggested refactoring")))))

(ert-deftest ai-code-prompt-completion-test-skips-short-words ()
  "Words too short to be worth typing do not become candidates."
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root
             "* [2026-01-01 Thu 10:00]\ndo it now\n"))
    (should (equal (nth 0 (ai-code-prompt-completion--ensure-index))
                   '("do it now" "now")))))

(ert-deftest ai-code-prompt-completion-test-whole-line-before-its-tails ()
  "The full first line is offered ahead of the fragments cut out of it."
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root
             ai-code-prompt-completion-test--corpus))
    (let* ((candidates (nth 0 (ai-code-prompt-completion--ensure-index)))
           (whole (cl-position "Go ahead with the suggested refactoring"
                               candidates :test #'equal))
           (tail (cl-position "refactoring" candidates :test #'equal)))
      (should whole)
      (should tail)
      (should (< whole tail)))))

(ert-deftest ai-code-prompt-completion-test-mid-line-recall-keeps-one-copy ()
  "Recalling from a middle word rewrites the line instead of doubling it."
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root
             ai-code-prompt-completion-test--corpus))
    (with-temp-buffer
      (setq-local completion-at-point-functions
                  (list #'ai-code-prompt-completion-capf))
      (insert "Go ahead with the suggested refac")
      (completion-at-point)
      (should (equal (buffer-string)
                     "Go ahead with the suggested refactoring")))))

(ert-deftest ai-code-prompt-completion-test-keeps-unrelated-text-on-the-line ()
  "Text that is not the opening of the prompt stays where the user put it."
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root
             ai-code-prompt-completion-test--corpus))
    (with-temp-buffer
      (setq-local completion-at-point-functions
                  (list #'ai-code-prompt-completion-capf))
      (insert "TODO: refac")
      (completion-at-point)
      (should (equal (buffer-string)
                     "TODO: Go ahead with the suggested refactoring")))))

(ert-deftest ai-code-prompt-completion-test-keeps-org-line-prefix ()
  "An Org bullet or headline survives a rewrite back to the line start."
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root
             ai-code-prompt-completion-test--corpus))
    (dolist (prefix '("- " "** " "1. "))
      (with-temp-buffer
        (setq-local completion-at-point-functions
                    (list #'ai-code-prompt-completion-capf))
        (insert prefix "Go ahead with the suggested refac")
        (completion-at-point)
        (should (equal (buffer-string)
                       (concat prefix
                               "Go ahead with the suggested refactoring")))))))

(ert-deftest ai-code-prompt-completion-test-capf-quiet-without-word ()
  "With no word before point the capf declines, leaving other capfs alone."
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root
             ai-code-prompt-completion-test--corpus))
    (with-temp-buffer
      (insert "hello ")
      (should-not (ai-code-prompt-completion-capf)))))

(ert-deftest ai-code-prompt-completion-test-expands-truncated-candidate ()
  "Choosing a truncated candidate inserts the whole prompt it stands for."
  (let* ((long-line (concat "Please review the change and " (make-string 80 ?x)))
         (entry (concat long-line "\nthen summarize it")))
    (ai-code-prompt-completion-test--with-roots
        (list (ai-code-prompt-completion-test--make-root
               (format "* [2026-01-01 Thu 10:00]\n%s\n" entry)))
      (let ((candidate (car (nth 0 (ai-code-prompt-completion--ensure-index)))))
        (should (<= (length candidate) ai-code-prompt-completion--display-width))
        (with-temp-buffer
          (insert candidate)
          (ai-code-prompt-completion--exit candidate 'finished)
          (should (equal (buffer-string) entry)))))))

(ert-deftest ai-code-prompt-completion-test-exit-ignores-unfinished-status ()
  "A still-unique prefix is not expanded while the user keeps typing."
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root
             ai-code-prompt-completion-test--corpus))
    (with-temp-buffer
      (insert "Go ahead with the suggested refactoring")
      (ai-code-prompt-completion--exit "Go ahead with the suggested refactoring"
                                    'sole)
      (should (equal (buffer-string) "Go ahead with the suggested refactoring")))))

(ert-deftest ai-code-prompt-completion-test-refresh-picks-up-new-prompts ()
  "`ai-code-prompt-completion-refresh' re-reads prompt files from disk."
  (let ((root (ai-code-prompt-completion-test--make-root
               "* [2026-01-01 Thu 10:00]\nfirst prompt\n")))
    (ai-code-prompt-completion-test--with-roots (list root)
      (should (equal (nth 0 (ai-code-prompt-completion--ensure-index))
                     '("first prompt" "prompt")))
      (with-temp-file (expand-file-name ai-code-prompt-file-name root)
        (insert "* [2026-01-01 Thu 10:00]\nfirst prompt\n
* [2026-01-02 Fri 10:00]\nsecond prompt\n"))
      ;; The cache is deliberately sticky until asked to rebuild.
      (should (equal (nth 0 (ai-code-prompt-completion--ensure-index))
                     '("first prompt" "prompt")))
      (ai-code-prompt-completion-refresh)
      (should (member "second prompt"
                      (nth 0 (ai-code-prompt-completion--ensure-index)))))))

;;; Hand-curated prompt files

(ert-deftest ai-code-prompt-completion-test-extra-files-default-to-none ()
  "Nothing outside the history files is read until the user asks for it."
  (should-not (default-value 'ai-code-prompt-completion-files))
  (should-not (default-value 'ai-code-prompt-completion-use-org-roam)))

(ert-deftest ai-code-prompt-completion-test-indexes-extra-org-file ()
  "A prompt library kept by hand joins the candidates."
  (let ((file (ai-code-prompt-completion-test--make-file "\
* Explain
explain the current code to me

* Review
review this diff for logical errors
")))
    (ai-code-prompt-completion-test--with-roots nil
      (let* ((ai-code-prompt-completion-files (list file))
             (index (ai-code-prompt-completion--ensure-index)))
        (should (equal (nth 2 index) 1))
        (should (member "explain the current code to me" (nth 0 index)))
        (should (member "review this diff for logical errors" (nth 0 index)))))))

(ert-deftest ai-code-prompt-completion-test-extra-file-headline-is-a-prompt ()
  "A headline with no body under it is itself the prompt."
  (let ((file (ai-code-prompt-completion-test--make-file
               "* explain the current code\n* review this diff\n")))
    (ai-code-prompt-completion-test--with-roots nil
      (let* ((ai-code-prompt-completion-files (list file))
             (candidates (nth 0 (ai-code-prompt-completion--ensure-index))))
        (should (member "explain the current code" candidates))
        (should (member "review this diff" candidates))))))

(ert-deftest ai-code-prompt-completion-test-extra-file-without-headlines ()
  "A plain list of prompts is split on blank lines."
  (let ((file (ai-code-prompt-completion-test--make-file "\
explain the current code

write the missing tests
first, then the code
" ".txt")))
    (ai-code-prompt-completion-test--with-roots nil
      (let* ((ai-code-prompt-completion-files (list file))
             (index (ai-code-prompt-completion--ensure-index)))
        (should (member "explain the current code" (nth 0 index)))
        (should (equal (gethash "write the missing tests" (nth 1 index))
                       "write the missing tests\nfirst, then the code"))))))

(ert-deftest ai-code-prompt-completion-test-extra-file-is-not-filtered ()
  "A curated file keeps prompts the history filters would have dropped."
  (let ((file (ai-code-prompt-completion-test--make-file
               (concat "* Handoff\nGoal: hand off to the next agent\n\n"
                       "* Release\n"
                       (mapconcat (lambda (n) (format "step %d" n))
                                  (number-sequence 1 8) "\n")
                       "\n"))))
    (ai-code-prompt-completion-test--with-roots nil
      (let* ((ai-code-prompt-completion-files (list file))
             (index (ai-code-prompt-completion--ensure-index)))
        (should (member "Goal: hand off to the next agent" (nth 0 index)))
        (should (equal (length (split-string (gethash "step 1" (nth 1 index)) "\n"))
                       8))))))

(ert-deftest ai-code-prompt-completion-test-extra-files-join-history ()
  "Curated prompts and recorded history end up in the same index."
  (let ((file (ai-code-prompt-completion-test--make-file
               "* Curated\nrun the release checklist\n")))
    (ai-code-prompt-completion-test--with-roots
        (list (ai-code-prompt-completion-test--make-root
               ai-code-prompt-completion-test--corpus))
      (let* ((ai-code-prompt-completion-files (list file))
             (index (ai-code-prompt-completion--ensure-index)))
        (should (equal (nth 2 index) 2))
        (should (member "run the release checklist" (nth 0 index)))
        (should (member "investigate the issue and answer the question"
                        (nth 0 index)))))))

(ert-deftest ai-code-prompt-completion-test-skips-remote-extra-files ()
  "A remote curated file is never probed, so indexing cannot hang on Tramp."
  (ai-code-prompt-completion-test--with-roots nil
    (let ((ai-code-prompt-completion-files
           '("/ssh:nowhere.invalid:/srv/prompts.org")))
      (cl-letf (((symbol-function 'file-readable-p)
                 (lambda (file)
                   (when (file-remote-p file)
                     (ert-fail (format "probed remote file %s" file)))
                   nil)))
        (should-not (ai-code-prompt-completion--extra-files))))))

;;; Documents: any headline, at any depth

(ert-deftest ai-code-prompt-completion-test-indexes-headline-at-any-depth ()
  "A leaf section deep in a document is a prompt like a top-level one."
  (let ((file (ai-code-prompt-completion-test--make-file "\
* Notes
** Emacs
*** rewrite this function without recursion
")))
    (ai-code-prompt-completion-test--with-roots nil
      (let* ((ai-code-prompt-completion-files (list file))
             (candidates (nth 0 (ai-code-prompt-completion--ensure-index))))
        (should (member "rewrite this function without recursion" candidates))))))

(ert-deftest ai-code-prompt-completion-test-indexes-text-under-headline ()
  "A section offers the text written under it, and not its children's."
  (let ((file (ai-code-prompt-completion-test--make-file "\
* Review
check the error handling in this file
** Tests
check the tests cover the new branch
")))
    (ai-code-prompt-completion-test--with-roots nil
      (let* ((ai-code-prompt-completion-files (list file))
             (index (ai-code-prompt-completion--ensure-index)))
        (should (equal (gethash "check the error handling in this file"
                                (nth 1 index))
                       "check the error handling in this file"))
        (should (member "check the tests cover the new branch"
                        (nth 0 index)))))))

(ert-deftest ai-code-prompt-completion-test-indexes-diary-datetree ()
  "A diary offers what was written on a day, not the day it was written."
  (let ((file (ai-code-prompt-completion-test--make-file "\
* 2026
** 2026-01 January
*** 2026-01-02 Friday
**** Release
cut the release and write the announcement
")))
    (ai-code-prompt-completion-test--with-roots nil
      (let* ((ai-code-prompt-completion-files (list file))
             (candidates (nth 0 (ai-code-prompt-completion--ensure-index))))
        (should (member "cut the release and write the announcement" candidates))
        (should-not (cl-some (lambda (candidate)
                               (string-match-p "\\`202[0-9]" candidate))
                             candidates))))))

(ert-deftest ai-code-prompt-completion-test-headline-markup-is-not-prompt-text ()
  "Tags, a statistics cookie and a link wrapper are markup, not wording."
  (let ((file (ai-code-prompt-completion-test--make-file "\
* review this diff for logical errors [1/2]   :work:emacs:
* [[id:4e2f0e1a][write the missing tests first]]
")))
    (ai-code-prompt-completion-test--with-roots nil
      (let* ((ai-code-prompt-completion-files (list file))
             (candidates (nth 0 (ai-code-prompt-completion--ensure-index))))
        (should (member "review this diff for logical errors" candidates))
        (should (member "write the missing tests first" candidates))))))

(ert-deftest ai-code-prompt-completion-test-skips-short-headlines ()
  "A heading of a word or two is quicker typed than picked from a popup."
  (let ((file (ai-code-prompt-completion-test--make-file "\
* Note
* Scrum
* explain the current code
")))
    (ai-code-prompt-completion-test--with-roots nil
      (let* ((ai-code-prompt-completion-files (list file))
             (candidates (nth 0 (ai-code-prompt-completion--ensure-index))))
        (should (member "explain the current code" candidates))
        (should-not (member "Note" candidates))
        (should-not (member "Scrum" candidates))))))

(ert-deftest ai-code-prompt-completion-test-indexes-text-before-headlines ()
  "What a note says before its first headline is offered too."
  (let ((file (ai-code-prompt-completion-test--make-file "\
#+title: Prompt library
walk through the release checklist

* Review
review this diff for logical errors
")))
    (ai-code-prompt-completion-test--with-roots nil
      (let* ((ai-code-prompt-completion-files (list file))
             (candidates (nth 0 (ai-code-prompt-completion--ensure-index))))
        (should (member "walk through the release checklist" candidates))
        (should (member "review this diff for logical errors" candidates))))))

(ert-deftest ai-code-prompt-completion-test-strips-indented-drawer ()
  "An archived entry indents its drawer, which is still not prompt text."
  (let ((file (ai-code-prompt-completion-test--make-file "\
* Release
  :PROPERTIES:
  :ARCHIVE_TIME: 2026-01-02 Fri 10:00
  :END:
cut the release and write the announcement
")))
    (ai-code-prompt-completion-test--with-roots nil
      (let* ((ai-code-prompt-completion-files (list file))
             (candidates (nth 0 (ai-code-prompt-completion--ensure-index))))
        (should (member "cut the release and write the announcement"
                        candidates))))))

;;; Org-roam notes

(defmacro ai-code-prompt-completion-test--with-org-roam (files &rest body)
  "Run BODY with org-roam pretending to track FILES."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'org-roam-list-files) (lambda () ,files)))
     ,@body))

(ert-deftest ai-code-prompt-completion-test-indexes-org-roam-notes ()
  "Notes org-roam tracks are completed from like a curated file."
  (let ((note (ai-code-prompt-completion-test--make-file "\
:PROPERTIES:
:ID:       4e2f0e1a-0000-0000-0000-000000000000
:END:
#+title: Prompt library

* Release
walk through the release checklist
")))
    (ai-code-prompt-completion-test--with-roots nil
      (let ((ai-code-prompt-completion-use-org-roam t))
        (ai-code-prompt-completion-test--with-org-roam (list note)
          (should (member "walk through the release checklist"
                          (nth 0 (ai-code-prompt-completion--ensure-index)))))))))

(ert-deftest ai-code-prompt-completion-test-org-roam-header-is-not-a-prompt ()
  "The ID drawer and title a note opens with are not offered back."
  (let ((note (ai-code-prompt-completion-test--make-file "\
:PROPERTIES:
:ID:       4e2f0e1a-0000-0000-0000-000000000000
:END:
#+title: Prompt library

walk through the release checklist
")))
    (ai-code-prompt-completion-test--with-roots nil
      (let ((ai-code-prompt-completion-use-org-roam t))
        (ai-code-prompt-completion-test--with-org-roam (list note)
          (let ((candidates (nth 0 (ai-code-prompt-completion--ensure-index))))
            (should (member "walk through the release checklist" candidates))
            (should-not (cl-some (lambda (candidate)
                                   (string-match-p "#\\+title\\|:ID:" candidate))
                                 candidates))))))))

(ert-deftest ai-code-prompt-completion-test-org-roam-can-be-turned-off ()
  "With the option off org-roam is not consulted at all."
  (let ((consulted nil))
    (ai-code-prompt-completion-test--with-roots nil
      (ai-code-prompt-completion-test--with-org-roam
          (progn (setq consulted t) nil)
        (should-not (ai-code-prompt-completion--org-roam-files))))
    (should-not consulted)))

(ert-deftest ai-code-prompt-completion-test-org-roam-absent-is-quiet ()
  "Without org-roam installed the index is built from the other sources only."
  (let ((definition (and (fboundp 'org-roam-list-files)
                         (symbol-function 'org-roam-list-files))))
    (unwind-protect
        (progn
          (fmakunbound 'org-roam-list-files)
          (ai-code-prompt-completion-test--with-roots
              (list (ai-code-prompt-completion-test--make-root
                     ai-code-prompt-completion-test--corpus))
            (let ((ai-code-prompt-completion-use-org-roam t))
              (should-not (ai-code-prompt-completion--org-roam-files))
              (should (member "investigate the issue and answer the question"
                              (nth 0 (ai-code-prompt-completion--ensure-index)))))))
      (when definition (fset 'org-roam-list-files definition)))))

;;; Input history file round-trip

(ert-deftest ai-code-input-test-history-reports-unreadable-file ()
  "A history file that does not parse is reported, not silently emptied."
  (let ((file (make-temp-file "ai-code-history" nil ".el" "((((")))
    (unwind-protect
        (should (equal (ai-code--read-input-history file) '(nil)))
      (delete-file file))))

(ert-deftest ai-code-input-test-history-missing-file-is-readable ()
  "A missing history file starts an empty history rather than blocking writes."
  (let ((file (expand-file-name "absent-history.el"
                                (make-temp-file "ai-code-history" t))))
    (should (equal (ai-code--read-input-history file) '(t)))))

(ert-deftest ai-code-input-test-history-round-trips-propertized-text ()
  "Text properties are stripped so a saved history always reads back."
  (let ((file (make-temp-file "ai-code-history" nil ".el")))
    (unwind-protect
        (progn
          (ai-code--write-input-history
           file (list (propertize "fix the failing test" 'face 'bold)))
          (should (equal (ai-code--read-input-history file)
                         '(t "fix the failing test"))))
      (delete-file file))))

(ert-deftest ai-code-input-test-history-drops-ellipsis-artifacts ()
  "A history truncated by `print-length' reads back without its ellipsis."
  (let ((file (make-temp-file "ai-code-history" nil ".el"
                              "(\"kept prompt\" ... \"also kept\")")))
    (unwind-protect
        (should (equal (ai-code--read-input-history file)
                       '(t "kept prompt" "also kept")))
      (delete-file file))))

(ert-deftest ai-code-input-test-history-write-ignores-print-limits ()
  "Saving a long history is never truncated by ambient printer settings."
  (let ((file (make-temp-file "ai-code-history" nil ".el"))
        (print-length 5)
        (print-level 2)
        (entries (mapcar #'number-to-string (number-sequence 1 50))))
    (unwind-protect
        (progn
          (ai-code--write-input-history file entries)
          (should (equal (cdr (ai-code--read-input-history file)) entries)))
      (delete-file file))))

(ert-deftest ai-code-input-test-history-keeps-latest-entries-only ()
  "Saving caps the history at 1000 entries, newest first."
  (let ((file (make-temp-file "ai-code-history" nil ".el"))
        (entries (mapcar #'number-to-string (number-sequence 0 1200))))
    (unwind-protect
        (progn
          (ai-code--write-input-history file entries)
          (let ((saved (cdr (ai-code--read-input-history file))))
            (should (equal (length saved) 1000))
            (should (equal (car saved) "0"))))
      (delete-file file))))

(ert-deftest ai-code-input-test-unparsable-history-is-not-overwritten ()
  "Reading a corrupt history file must not replace it with a single entry."
  (let* ((user-emacs-directory (file-name-as-directory
                                (make-temp-file "ai-code-history-dir" t)))
         (file-name "corrupt-history.el")
         (file (expand-file-name file-name user-emacs-directory))
         (corrupt "(\"unterminated"))
    (with-temp-file file (insert corrupt))
    (cl-letf (((symbol-function 'helm-comp-read)
               (lambda (&rest _args) "a brand new prompt")))
      (should (equal (ai-code-helm-read-string-with-history "Prompt: " file-name)
                     "a brand new prompt")))
    (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                   corrupt))))

(ert-deftest ai-code-input-test-readable-history-gains-new-entry ()
  "A history file that parses is extended with the newest input on top."
  (let* ((user-emacs-directory (file-name-as-directory
                                (make-temp-file "ai-code-history-dir" t)))
         (file-name "good-history.el")
         (file (expand-file-name file-name user-emacs-directory)))
    (with-temp-file file (insert (prin1-to-string '("older prompt"))))
    (cl-letf (((symbol-function 'helm-comp-read)
               (lambda (&rest _args) "newest prompt")))
      (ai-code-helm-read-string-with-history "Prompt: " file-name))
    (should (equal (cdr (ai-code--read-input-history file))
                   '("newest prompt" "older prompt")))))

(ert-deftest ai-code-prompt-completion-test-dict-capf-works-without-cape ()
  "Without cape loaded the merged capf still offers prompt candidates."
  (should-not (fboundp 'cape-wrap-super))
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root
             ai-code-prompt-completion-test--corpus))
    (with-temp-buffer
      (insert "Go")
      (let ((result (ai-code-prompt-completion-dict-capf)))
        (should result)
        (should (member "Go ahead with the suggested refactoring"
                        (nth 2 result)))))))

(ert-deftest ai-code-prompt-completion-test-setup-installs-capf ()
  "Setup installs the capf buffer-locally and does not duplicate it."
  (with-temp-buffer
    (let ((ai-code-prompt-completion-enable-company nil))
      (ai-code-prompt-completion-setup)
      (ai-code-prompt-completion-setup))
    (should (equal (cl-remove-if-not
                    (lambda (f) (eq f #'ai-code-prompt-completion-dict-capf))
                    completion-at-point-functions)
                   (list #'ai-code-prompt-completion-dict-capf)))
    (should (local-variable-p 'completion-at-point-functions))))

(ert-deftest ai-code-prompt-completion-test-setup-leaves-company-alone ()
  "Setup does not start `company-mode' when the option is off."
  (with-temp-buffer
    (let ((ai-code-prompt-completion-enable-company nil)
          (started nil))
      (cl-letf (((symbol-function 'company-mode) (lambda (&rest _) (setq started t))))
        (ai-code-prompt-completion-setup))
      (should-not started))))

(ert-deftest ai-code-prompt-completion-test-prompt-mode-registers-capf ()
  "Entering `ai-code-prompt-mode' installs the capf with no user setup."
  (with-temp-buffer
    (ai-code-prompt-mode)
    (should (memq #'ai-code-prompt-completion-dict-capf
                  completion-at-point-functions))
    (should (local-variable-p 'completion-at-point-functions))
    ;; The popup front-end stays the user's choice.
    (should-not (bound-and-true-p company-mode))))

(ert-deftest ai-code-prompt-completion-test-prompt-mode-capf-completes ()
  "The capf installed by the mode completes a stored prompt end to end."
  (ai-code-prompt-completion-test--with-roots
      (list (ai-code-prompt-completion-test--make-root
             ai-code-prompt-completion-test--corpus))
    (with-temp-buffer
      (ai-code-prompt-mode)
      (insert "please Go")
      (completion-at-point)
      (should (equal (buffer-string)
                     "please Go ahead with the suggested refactoring")))))

(provide 'test_ai-code-prompt-completion)

;;; test_ai-code-prompt-completion.el ends here
