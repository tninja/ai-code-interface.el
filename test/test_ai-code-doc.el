;;; test_ai-code-doc.el --- Tests for ai-code-doc.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)

(defun ai-code-test--maybe-prefer-packaged-transient ()
  "Prefer the newest packaged Transient when one is installed."
  (let* ((pattern (expand-file-name "transient-*" package-user-dir))
         (candidates (sort (cl-remove-if-not #'file-directory-p
                                             (file-expand-wildcards pattern))
                           #'version<))
         (latest (car (last candidates))))
    (when latest
      (add-to-list 'load-path latest))))

(ai-code-test--maybe-prefer-packaged-transient)

(require 'ai-code-doc)
(require 'ai-code)

(defmacro ai-code-file-with-test-env (&rest body)
  "Set up a temporary environment for testing file operations.
This macro creates a temporary directory structure and ensures
everything is cleaned up afterward."
  `(let* ((test-dir (expand-file-name "test-file-ops/" temporary-file-directory))
          (default-directory test-dir))
     (unwind-protect
         (progn
           ;; Setup: Create test directory
           (make-directory test-dir t)
           ;; Execute test body
           ,@body)
       ;; Teardown: Clean up test directory
       (when (file-directory-p test-dir)
         (delete-directory test-dir t)))))

(ert-deftest ai-code-test-menu-agile-development-includes-derive-architecture-document-entry ()
  "Test that Agile Development menu exposes architecture document derivation."
  (let ((suffix (transient-get-suffix 'ai-code--menu-agile-development "A")))
    (should suffix)
    (should (eq (plist-get (cdr suffix) :command)
                'ai-code-derive-architecture-document))
    (should (equal (plist-get (cdr suffix) :description)
                   "Derive architecture document"))))

(ert-deftest ai-code-test-derive-architecture-document-dispatches-to-guardrails ()
  "Test that architecture document derivation dispatches to guardrails."
  (let (called)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 "Derive Architecture Guardrails"))
              ((symbol-function 'ai-code-derive-architecture-guardrails)
               (lambda ()
                 (setq called 'guardrails)))
              ((symbol-function 'ai-code-derive-ddd-context)
               (lambda ()
                 (setq called 'ddd-context))))
      (ai-code-derive-architecture-document))
    (should (eq called 'guardrails))))

(ert-deftest ai-code-test-derive-architecture-document-dispatches-to-ddd-context ()
  "Test that architecture document derivation dispatches to DDD context."
  (let (called)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 "Derive DDD Context for Repo"))
              ((symbol-function 'ai-code-derive-architecture-guardrails)
               (lambda ()
                 (setq called 'guardrails)))
              ((symbol-function 'ai-code-derive-ddd-context)
               (lambda ()
                 (setq called 'ddd-context))))
      (ai-code-derive-architecture-document))
    (should (eq called 'ddd-context))))

(ert-deftest ai-code-test-derive-architecture-document-dispatches-to-test-context ()
  "Test that architecture document derivation dispatches to Test Context."
  (let (called)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 "Derive Test Context Document"))
              ((symbol-function 'ai-code-derive-test-context)
               (lambda ()
                 (setq called 'test-context))))
      (ai-code-derive-architecture-document))
    (should (eq called 'test-context))))

(ert-deftest ai-code-test-derive-architecture-guardrails-creates-template-and-prompt ()
  "Test `ai-code-derive-architecture-guardrails' initializes the Org file and prompt."
  (let* ((tmp-root (make-temp-file "ai-code-guardrails" t))
         (target-file (expand-file-name ".ai.code.files/architecture/guardrails.org" tmp-root))
         captured-initial-prompt
         captured-final-prompt)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--git-root)
                   (lambda (&optional _dir)
                     tmp-root))
                  ((symbol-function 'read-string)
                   (lambda (&rest _args) "English"))
                  ((symbol-function 'ai-code-plain-read-string)
                   (lambda (prompt initial-input)
                     (should (equal prompt "Prompt: "))
                     (setq captured-initial-prompt initial-input)
                     initial-input))
                  ((symbol-function 'ai-code--insert-prompt)
                   (lambda (prompt)
                     (setq captured-final-prompt prompt))))
          (ai-code-derive-architecture-guardrails)
          (should (file-exists-p target-file))
          (with-temp-buffer
            (insert-file-contents target-file)
            (should (string-match-p (regexp-quote "#+TITLE: Architecture Guardrails")
                                    (buffer-string)))
            (should (string-match-p (regexp-quote "* Dependency Rules")
                                    (buffer-string)))
            (should (string-match-p (regexp-quote "* Required Validation")
                                    (buffer-string))))
          (should (string-match-p (regexp-quote "Derive a lightweight architecture guardrails document")
                                  captured-initial-prompt))
          (should (string-match-p (regexp-quote "current code, tests, docs, and filenames")
                                  captured-initial-prompt))
          (should (string-match-p (regexp-quote "Do not invent an ideal architecture")
                                  captured-initial-prompt))
          (should (string-match-p (regexp-quote "Keep it concise")
                                  captured-initial-prompt))
          (should (string-match-p (regexp-quote "@.ai.code.files/architecture/guardrails.org")
                                  captured-initial-prompt))
          (should (string-match-p (regexp-quote "Org-mode format")
                                  captured-initial-prompt))
          (should (string-match-p (regexp-quote "[[file:../../path/to/file::symbol_or_line][description]]")
                                  captured-initial-prompt))
          (should (equal captured-final-prompt captured-initial-prompt)))
      (ignore-errors (delete-directory tmp-root t)))))

(ert-deftest ai-code-test-derive-architecture-guardrails-preserves-existing-file ()
  "Test `ai-code-derive-architecture-guardrails' does not overwrite an existing file."
  (let* ((tmp-root (make-temp-file "ai-code-guardrails-existing" t))
         (files-dir (expand-file-name ".ai.code.files/architecture" tmp-root))
         (target-file (expand-file-name "guardrails.org" files-dir))
         (existing-content "#+TITLE: Existing guardrails\n"))
    (unwind-protect
        (progn
          (make-directory files-dir t)
          (write-region existing-content nil target-file nil 'silent)
          (cl-letf (((symbol-function 'ai-code--git-root)
                     (lambda (&optional _dir)
                       tmp-root))
                    ((symbol-function 'read-string)
                     (lambda (&rest _args) "English"))
                    ((symbol-function 'ai-code-plain-read-string)
                     (lambda (_prompt initial-input)
                       initial-input))
                    ((symbol-function 'ai-code--insert-prompt)
                     (lambda (_prompt) nil)))
            (ai-code-derive-architecture-guardrails))
          (should (file-exists-p target-file))
          (with-temp-buffer
            (insert-file-contents target-file)
            (should (string= (buffer-string) existing-content))))
      (ignore-errors (delete-directory tmp-root t)))))

(ert-deftest ai-code-test-derive-architecture-guardrails-errors-outside-git-repo ()
  "Test `ai-code-derive-architecture-guardrails' requires a git repository."
  (cl-letf (((symbol-function 'ai-code--git-root)
             (lambda (&optional _dir) nil)))
    (should-error (ai-code-derive-architecture-guardrails)
                  :type 'user-error)))

(ert-deftest ai-code-test-derive-architecture-guardrails-reports-cancelled-request ()
  "Test `ai-code-derive-architecture-guardrails' reports cancellation."
  (let* ((tmp-root (make-temp-file "ai-code-guardrails-cancel" t))
         captured-message
         insert-called)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--git-root)
                   (lambda (&optional _dir)
                     tmp-root))
                  ((symbol-function 'read-string)
                   (lambda (&rest _args) "English"))
                  ((symbol-function 'ai-code-plain-read-string)
                   (lambda (_prompt _initial-input)
                     nil))
                  ((symbol-function 'ai-code--insert-prompt)
                   (lambda (&rest _args)
                     (setq insert-called t)))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (setq captured-message
                           (apply #'format format-string args)))))
          (ai-code-derive-architecture-guardrails)
          (should-not insert-called)
          (should (equal captured-message
                         "Architecture guardrails request cancelled")))
      (ignore-errors (delete-directory tmp-root t)))))

(ert-deftest ai-code-test-derive-ddd-context-creates-target-file-and-sends-prompt ()
  "Derive DDD context should create the target file and send the default prompt."
  (ai-code-file-with-test-env
   (let (captured-read-prompt
         captured-initial-prompt
         inserted-prompt)
     (cl-letf (((symbol-function 'ai-code--git-root)
                (lambda (&optional _dir)
                  default-directory))
               ((symbol-function 'read-string)
                (lambda (&rest _args) "English"))
               ((symbol-function 'ai-code-plain-read-string)
                (lambda (prompt &optional initial-input)
                  (setq captured-read-prompt prompt
                        captured-initial-prompt initial-input)
                  initial-input))
               ((symbol-function 'ai-code--insert-prompt)
                (lambda (prompt)
                  (setq inserted-prompt prompt))))
       (ai-code-derive-ddd-context)
       (should (equal captured-read-prompt "Derive DDD context prompt: "))
       (should (string-match-p
                "Domain-Driven Design (DDD) style context document"
                captured-initial-prompt))
       (should (string-match-p
                "\\.ai\\.code\\.files/architecture/domain-context\\.org"
                captured-initial-prompt))
       (should (string-match-p "\\*\\* Notes and Uncertainties"
                               captured-initial-prompt))
       (should (string-match-p (regexp-quote "[[file:../../path/to/file::symbol_or_line][description_text]]")
                               captured-initial-prompt))
       (should (equal inserted-prompt captured-initial-prompt))
       (should (file-exists-p
                (expand-file-name ".ai.code.files/architecture/domain-context.org"
                                  default-directory)))))))

(ert-deftest ai-code-test-derive-ddd-context-includes-stored-repo-context ()
  "Derive DDD context should append stored repo context when present."
  (ai-code-file-with-test-env
   (let (inserted-prompt)
     (cl-letf (((symbol-function 'ai-code--git-root)
                (lambda (&optional _dir)
                  default-directory))
               ((symbol-function 'read-string)
                (lambda (&rest _args) "English"))
               ((symbol-function 'ai-code--format-repo-context-info)
                (lambda ()
                  "\nStored repository context:\n  - Preserve existing CLI UX"))
               ((symbol-function 'ai-code-plain-read-string)
                (lambda (_prompt &optional initial-input)
                  initial-input))
               ((symbol-function 'ai-code--insert-prompt)
                (lambda (prompt)
                  (setq inserted-prompt prompt))))
       (ai-code-derive-ddd-context)
       (should (string-match-p
                "Stored repository context:\n  - Preserve existing CLI UX"
                inserted-prompt))))))

(ert-deftest ai-code-test-derive-ddd-context-errors-outside-git-repo ()
  "Derive DDD context should require a Git repository."
  (cl-letf (((symbol-function 'ai-code--git-root)
             (lambda (&optional _dir)
               nil)))
    (should-error (ai-code-derive-ddd-context) :type 'user-error)))

(ert-deftest ai-code-test-derive-test-context-creates-target-file-and-sends-prompt ()
  "Derive Test Context should create the target file and send the default prompt."
  (ai-code-file-with-test-env
   (let (captured-read-prompt
         captured-initial-prompt
         inserted-prompt)
     (cl-letf (((symbol-function 'ai-code--git-root)
                (lambda (&optional _dir)
                  default-directory))
               ((symbol-function 'read-string)
                (lambda (&rest _args) "English"))
               ((symbol-function 'ai-code-plain-read-string)
                (lambda (prompt &optional initial-input)
                  (setq captured-read-prompt prompt
                        captured-initial-prompt initial-input)
                  initial-input))
               ((symbol-function 'ai-code--insert-prompt)
                (lambda (prompt)
                  (setq inserted-prompt prompt))))
       (ai-code-derive-test-context)
       (should (equal captured-read-prompt "Derive Test Context prompt: "))
       (should (string-match-p
                "Test Context and Verification Guide"
                captured-initial-prompt))
       (should (string-match-p
                "\\.ai\\.code\\.files/architecture/test-context\\.org"
                captured-initial-prompt))
       (should (string-match-p (regexp-quote "[[file:../../path/to/file::symbol_or_line][description_text]]")
                               captured-initial-prompt))
       (should (equal inserted-prompt captured-initial-prompt))
       (should (file-exists-p
                (expand-file-name ".ai.code.files/architecture/test-context.org"
                                  default-directory)))))))

(ert-deftest ai-code-test-derive-architecture-guardrails-asks-language ()
  "Test that `ai-code-derive-architecture-guardrails' asks for document language and appends it."
  (let* ((tmp-root (make-temp-file "ai-code-guardrails-lang" t))
         captured-language-prompt
         captured-language-default
         (mock-lang "French")
         captured-final-prompt)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--git-root)
                   (lambda (&optional _dir) tmp-root))
                  ((symbol-function 'read-string)
                   (lambda (prompt &optional initial-input &rest _args)
                     (setq captured-language-prompt prompt
                           captured-language-default initial-input)
                     mock-lang))
                  ((symbol-function 'ai-code-plain-read-string)
                   (lambda (_prompt initial-input) initial-input))
                  ((symbol-function 'ai-code--insert-prompt)
                   (lambda (prompt) (setq captured-final-prompt prompt))))
          (ai-code-derive-architecture-guardrails)
          (should (equal captured-language-prompt "Document language: "))
          (should (equal captured-language-default "English"))
          (should (string-match-p (regexp-quote "Generate the document in French.")
                                  captured-final-prompt)))
      (ignore-errors (delete-directory tmp-root t)))))

(ert-deftest ai-code-test-derive-ddd-context-asks-language ()
  "Test that `ai-code-derive-ddd-context' asks for document language and appends it."
  (ai-code-file-with-test-env
   (let (captured-language-prompt
         captured-language-default
         (mock-lang "Chinese")
         captured-final-prompt)
     (cl-letf (((symbol-function 'ai-code--git-root)
                (lambda (&optional _dir) default-directory))
               ((symbol-function 'read-string)
                (lambda (prompt &optional initial-input &rest _args)
                  (setq captured-language-prompt prompt
                        captured-language-default initial-input)
                  mock-lang))
               ((symbol-function 'ai-code-plain-read-string)
                (lambda (_prompt initial-input) initial-input))
               ((symbol-function 'ai-code--insert-prompt)
                (lambda (prompt) (setq captured-final-prompt prompt))))
       (ai-code-derive-ddd-context)
       (should (equal captured-language-prompt "Document language: "))
       (should (equal captured-language-default "English"))
       (should (string-match-p (regexp-quote "Generate the document in Chinese.")
                               captured-final-prompt))))))

(ert-deftest ai-code-test-derive-test-context-asks-language ()
  "Test that `ai-code-derive-test-context' asks for document language and appends it."
  (ai-code-file-with-test-env
   (let (captured-language-prompt
         captured-language-default
         (mock-lang "German")
         captured-final-prompt)
     (cl-letf (((symbol-function 'ai-code--git-root)
                (lambda (&optional _dir) default-directory))
               ((symbol-function 'read-string)
                (lambda (prompt &optional initial-input &rest _args)
                  (setq captured-language-prompt prompt
                        captured-language-default initial-input)
                  mock-lang))
               ((symbol-function 'ai-code-plain-read-string)
                (lambda (_prompt initial-input) initial-input))
               ((symbol-function 'ai-code--insert-prompt)
                (lambda (prompt) (setq captured-final-prompt prompt))))
       (ai-code-derive-test-context)
       (should (equal captured-language-prompt "Document language: "))
       (should (equal captured-language-default "English"))
       (should (string-match-p (regexp-quote "Generate the document in German.")
                               captured-final-prompt))))))

(provide 'test_ai-code-doc)
;;; test_ai-code-doc.el ends here
;;; test_ai-code-doc.el ends here
