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
  "Run BODY in a temporary environment for testing file operations.
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

(defun ai-code-test--doc-read-string (topic language)
  "Return a `read-string' stand-in answering TOPIC and LANGUAGE.
The document topic question is recognized by its prompt prefix."
  (lambda (prompt &rest _args)
    (if (string-prefix-p "Document topic" prompt) topic language)))

(defun ai-code-test--unit-test-read-string (topic language)
  "Return a `read-string' stand-in answering TOPIC and LANGUAGE.
The unit test topic question is recognized by its prompt prefix."
  (lambda (prompt &rest _args)
    (if (string-prefix-p "Unit test topic" prompt) topic language)))

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

(ert-deftest ai-code-test-derive-architecture-document-dispatches-to-topic-unit-tests ()
  "Test that architecture document derivation dispatches to topic unit tests."
  (let (called)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 "Derive Unit Test to Help Understand the Topic"))
              ((symbol-function 'ai-code-derive-topic-unit-tests)
               (lambda ()
                 (setq called 'topic-unit-tests))))
      (ai-code-derive-architecture-document))
    (should (eq called 'topic-unit-tests))))

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
                   (ai-code-test--doc-read-string "" "English"))
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
          (should (string-match-p (regexp-quote "[[file:../../path/to/file::symbol][description]]")
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
                     (ai-code-test--doc-read-string "" "English"))
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
                   (ai-code-test--doc-read-string "" "English"))
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
                (ai-code-test--doc-read-string "" "English"))
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
       (should-not (string-match-p "Scope this document to the topic"
                                   captured-initial-prompt))
       (should (string-match-p "\\*\\* Notes and Uncertainties"
                               captured-initial-prompt))
       (should (string-match-p (regexp-quote "[[file:../../path/to/file::symbol][description]]")
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
                (ai-code-test--doc-read-string "" "English"))
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
                (ai-code-test--doc-read-string "" "English"))
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
       (should (string-match-p (regexp-quote "[[file:../../path/to/file::symbol][description]]")
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
                     (if (string-prefix-p "Document topic" prompt)
                         ""
                       (setq captured-language-prompt prompt
                             captured-language-default initial-input)
                       mock-lang)))
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
                  (if (string-prefix-p "Document topic" prompt)
                      ""
                    (setq captured-language-prompt prompt
                          captured-language-default initial-input)
                    mock-lang)))
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
                  (if (string-prefix-p "Document topic" prompt)
                      ""
                    (setq captured-language-prompt prompt
                          captured-language-default initial-input)
                    mock-lang)))
               ((symbol-function 'ai-code-plain-read-string)
                (lambda (_prompt initial-input) initial-input))
               ((symbol-function 'ai-code--insert-prompt)
                (lambda (prompt) (setq captured-final-prompt prompt))))
       (ai-code-derive-test-context)
       (should (equal captured-language-prompt "Document language: "))
       (should (equal captured-language-default "English"))
       (should (string-match-p (regexp-quote "Generate the document in German.")
                               captured-final-prompt))))))

(ert-deftest ai-code-test-derive-ddd-context-inserts-into-prompt-mode-buffer ()
  "In `ai-code-prompt-mode' the DDD prompt is written at point instead of sent."
  (ai-code-file-with-test-env
   (let (sent-prompt)
     (cl-letf (((symbol-function 'ai-code--git-root)
                (lambda (&optional _dir) default-directory))
               ((symbol-function 'read-string)
                (ai-code-test--doc-read-string "" "English"))
               ((symbol-function 'ai-code-plain-read-string)
                (lambda (_prompt &optional initial-input) initial-input))
               ((symbol-function 'ai-code--insert-prompt)
                (lambda (prompt) (setq sent-prompt prompt)))
               ((symbol-function 'y-or-n-p) (lambda (&rest _args) t)))
       (with-temp-buffer
         (ai-code-prompt-mode)
         (insert "* Existing task\n** Sub task\n")
         (goto-char (point-max))
         (ai-code-derive-ddd-context)
         (should-not sent-prompt)
         (should (string-match-p "^\\*\\* Derive DDD Context for Repo \\["
                                 (buffer-string)))
         (should (string-match-p
                  (regexp-quote "Domain-Driven Design (DDD) style context document")
                  (buffer-string))))))))

(ert-deftest ai-code-test-doc-emit-prompt-declines-buffer-dump-and-sends-to-ai ()
  "Declining the question in `ai-code-prompt-mode' sends the prompt to the AI.
The buffer dump must stay opt-in, so a no answer falls back to the
behavior used outside `ai-code-prompt-mode'."
  (let (sent-prompt
        captured-question)
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (question)
                 (setq captured-question question)
                 nil))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (prompt) (setq sent-prompt prompt))))
      (with-temp-buffer
        (ai-code-prompt-mode)
        (insert "* Existing task\n")
        (goto-char (point-max))
        (ai-code--doc-emit-prompt "Derive Repository Map" "PROMPT BODY")
        (should captured-question)
        (should (equal sent-prompt "PROMPT BODY"))
        (should (equal (buffer-string) "* Existing task\n"))))))

(ert-deftest ai-code-test-derive-architecture-guardrails-inserts-into-prompt-mode-buffer ()
  "In `ai-code-prompt-mode' the guardrails prompt is written at point, not sent."
  (ai-code-file-with-test-env
   (let (sent-prompt)
     (cl-letf (((symbol-function 'ai-code--git-root)
                (lambda (&optional _dir) default-directory))
               ((symbol-function 'read-string)
                (ai-code-test--doc-read-string "" "English"))
               ((symbol-function 'ai-code-plain-read-string)
                (lambda (_prompt initial-input) initial-input))
               ((symbol-function 'ai-code--insert-prompt)
                (lambda (prompt) (setq sent-prompt prompt)))
               ((symbol-function 'y-or-n-p) (lambda (&rest _args) t)))
       (with-temp-buffer
         (ai-code-prompt-mode)
         (insert "* Existing task\n")
         (goto-char (point-max))
         (ai-code-derive-architecture-guardrails)
         (should-not sent-prompt)
         (should (string-match-p "^\\* Derive Architecture Guardrails \\["
                                 (buffer-string)))
         (should (string-match-p
                  (regexp-quote "Derive a lightweight architecture guardrails document")
                  (buffer-string))))))))

(ert-deftest ai-code-test-derive-ddd-context-scopes-to-topic ()
  "A non-empty document topic narrows the DDD prompt and its output file."
  (ai-code-file-with-test-env
   (let (captured-topic-prompt
         inserted-prompt)
     (cl-letf (((symbol-function 'ai-code--git-root)
                (lambda (&optional _dir) default-directory))
               ((symbol-function 'read-string)
                (lambda (prompt &rest _args)
                  (cond ((string-prefix-p "Document topic" prompt)
                         (setq captured-topic-prompt prompt)
                         "Prompt Pipeline")
                        (t "English"))))
               ((symbol-function 'ai-code-plain-read-string)
                (lambda (_prompt &optional initial-input) initial-input))
               ((symbol-function 'ai-code--insert-prompt)
                (lambda (prompt) (setq inserted-prompt prompt))))
       (ai-code-derive-ddd-context)
       (should (equal captured-topic-prompt
                      "Document topic (empty for whole repo): "))
       (should (string-match-p
                (regexp-quote "Scope this document to the topic: Prompt Pipeline")
                inserted-prompt))
       (should (string-match-p
                (regexp-quote ".ai.code.files/architecture/domain-context-prompt-pipeline-13b070827c7b.org")
                inserted-prompt))
       (should (file-exists-p
                (expand-file-name
                 ".ai.code.files/architecture/domain-context-prompt-pipeline-13b070827c7b.org"
                 default-directory)))))))

(ert-deftest ai-code-test-derive-architecture-guardrails-scopes-to-topic ()
  "A non-empty document topic narrows the guardrails prompt and its output file."
  (ai-code-file-with-test-env
   (let (inserted-prompt)
     (cl-letf (((symbol-function 'ai-code--git-root)
                (lambda (&optional _dir) default-directory))
               ((symbol-function 'read-string)
                (ai-code-test--doc-read-string "Git Integration" "English"))
               ((symbol-function 'ai-code-plain-read-string)
                (lambda (_prompt initial-input) initial-input))
               ((symbol-function 'ai-code--insert-prompt)
                (lambda (prompt) (setq inserted-prompt prompt))))
       (ai-code-derive-architecture-guardrails)
       (should (string-match-p
                (regexp-quote "Scope this document to the topic: Git Integration")
                inserted-prompt))
       (should (string-match-p
                (regexp-quote "@.ai.code.files/architecture/guardrails-git-integration-cd172722a457.org")
                inserted-prompt))
       (should (file-exists-p
                (expand-file-name
                 ".ai.code.files/architecture/guardrails-git-integration-cd172722a457.org"
                 default-directory)))))))

(ert-deftest ai-code-test-document-prompts-require-verified-org-links ()
  "Every derived document prompt asks for verified relative Org links."
  (dolist (builder '(ai-code--derive-ddd-context-prompt
                     ai-code--derive-test-context-prompt
                     ai-code--derive-c4-plantuml-prompt
                     ai-code--derive-repo-map-prompt
                     ai-code--build-architecture-guardrails-prompt))
    (let ((prompt (funcall builder "/tmp/repo")))
      (should (string-match-p
               (regexp-quote "[[file:../../path/to/file::symbol][description]]")
               prompt))
      (should (string-match-p
               (regexp-quote "fall back to ::<line-number> only when")
               prompt))
      (should (string-match-p
               (regexp-quote "Only link to paths and symbols you have actually confirmed")
               prompt))
      (should (string-match-p (regexp-quote "first mention in each section")
                              prompt)))))

(ert-deftest ai-code-test-org-link-instruction-prefix-follows-output-depth ()
  "The relative link prefix is derived from the document output depth."
  (should (string-match-p (regexp-quote "[[file:../../path/to/file::symbol]")
                          (ai-code--org-link-instruction "a/b/doc.org")))
  (should (string-match-p (regexp-quote "[[file:../path/to/file::symbol]")
                          (ai-code--org-link-instruction "a/doc.org")))
  (should (string-match-p (regexp-quote "[[file:path/to/file::symbol]")
                          (ai-code--org-link-instruction "doc.org"))))

(ert-deftest test-ai-code-doc--topic-file-name-distinguishes-lossy-slugs ()
  "Distinct topics must not update the same output document."
  (let* ((topics '("\u8ba4\u8bc1" "\u652f\u4ed8" "C++" "C#" "c#"))
         (paths (mapcar (lambda (topic)
                          (ai-code--topic-file-name "docs/domain-context.org"
                                                    topic))
                        topics)))
    (should (= (length topics) (length (delete-dups (copy-sequence paths)))))
    (dolist (path paths)
      (should (equal (file-name-directory path) "docs/"))
      (should (equal (file-name-extension path) "org")))
    (should (equal (car paths)
                   (ai-code--topic-file-name "docs/domain-context.org"
                                             (car topics))))))

(ert-deftest test-ai-code-doc--topic-file-name-preserves-repository-path ()
  "Whole-repository documents retain their established output path."
  (should (equal (ai-code--topic-file-name "docs/domain-context.org" nil)
                 "docs/domain-context.org")))

(ert-deftest test-ai-code-doc--github-links-follow-analyzed-revision ()
  "Pin feature-branch links and use local links for unpublished or dirty code."
  (let* ((repo (make-temp-file "ai-code-doc-git-" t))
         (default-directory (file-name-as-directory repo))
         (process-environment (append '("GIT_CONFIG_GLOBAL=/dev/null"
                                        "GIT_CONFIG_NOSYSTEM=1")
                                      process-environment)))
    (unwind-protect
        ;; Execute real Git queries even when other test files stub Magit.
        (cl-letf (((symbol-function 'magit-git-string)
                   (lambda (&rest args)
                     (with-temp-buffer
                       (when (zerop (apply #'process-file "git" nil t nil args))
                         (car (split-string (buffer-string) "\n" t))))))
                  ((symbol-function 'magit-git-success)
                   (lambda (&rest args)
                     (zerop (apply #'process-file "git" nil nil nil args)))))
          (cl-labels ((git (&rest args)
                        (should (zerop
                                 (apply #'process-file "git" nil nil nil
                                        "-c" "user.name=Document Test"
                                        "-c" "user.email=doc@example.com"
                                        "-c" "commit.gpgsign=false" args)))))
            (git "init" "-q" "-b" "main")
            (git "remote" "add" "origin" "ssh://git@github.com/org/repo.git")
            ;; An unborn repository cannot supply a source revision.
            (should-not (ai-code--doc-github-source-url))
            (with-temp-file "source.txt" (insert "Main definition\n"))
            (with-temp-file ".gitignore" (insert "generated.txt\n"))
            (git "add" ".")
            (git "commit" "-qm" "Initial source")
            (git "update-ref" "refs/remotes/origin/main" "HEAD")
            (git "checkout" "-qb" "feature")
            (with-temp-file "source.txt" (insert "Feature definition\n"))
            (git "commit" "-qam" "Feature source")
            ;; A commit on another remote does not prove origin has it.
            (git "update-ref" "refs/remotes/upstream/feature" "HEAD")
            (should-not (ai-code--doc-github-source-url))
            (cl-letf (((symbol-function 'read-string)
                       (lambda (&rest _) (ert-fail "Unexpected link question"))))
              (should (eq (ai-code--read-document-link-style) 'local)))
            (git "update-ref" "refs/remotes/origin/feature" "HEAD")
            (let* ((revision (magit-git-string "rev-parse" "HEAD"))
                   (source-url (concat "https://github.com/org/repo/blob/"
                                       revision)))
              (should (equal (ai-code--doc-github-source-url) source-url))
              (cl-letf (((symbol-function 'read-string)
                         (lambda (&rest _) "github")))
                (should (eq (ai-code--read-document-link-style) 'github)))
              (let ((prompt (ai-code--org-link-instruction "a/b/doc.org" 'github)))
                (should (string-match-p (regexp-quote (concat source-url "/"))
                                        prompt))
                (should-not (string-match-p "/blob/HEAD/" prompt))
                (should (string-match-p "untracked, ignored" prompt)))
              ;; New document files must not disable links to committed code.
              (with-temp-file "doc.org" (insert "Draft document\n"))
              (with-temp-file "generated.txt" (insert "Generated output\n"))
              (should (equal (ai-code--doc-github-source-url) source-url))
              (git "checkout" "--detach" "-q")
              (should (equal (ai-code--doc-github-source-url) source-url))
              ;; Both unstaged and staged edits invalidate committed line numbers.
              (with-temp-file "source.txt" (insert "Local definition\n"))
              (should-not (ai-code--doc-github-source-url))
              (git "add" "source.txt")
              (should-not (ai-code--doc-github-source-url))
              (let ((prompt (ai-code--org-link-instruction "a/b/doc.org" 'github)))
                (should-not (string-match-p "https://github.com" prompt))
                (should (string-match-p
                         (regexp-quote "[[file:../../path/to/file::symbol]")
                         prompt))))))
      (delete-directory repo t))))

(ert-deftest test-ai-code-doc--github-source-url-handles-git-failure ()
  "An unavailable Git command must leave local document links usable."
  (cl-letf (((symbol-function 'ai-code--doc-github-repo-url)
             (lambda () "https://github.com/org/repo"))
            ((symbol-function 'magit-git-string)
             (lambda (&rest _) (error "Git unavailable"))))
    (should-not (ai-code--doc-github-source-url))
    (should (string-match-p
             (regexp-quote "[[file:path/to/file::symbol]")
             (ai-code--org-link-instruction "doc.org" 'github)))))

(ert-deftest ai-code-test-derive-topic-unit-tests-builds-learning-test-prompt ()
  "Topic unit tests must be ordered, runnable, and create no Org document."
  (ai-code-file-with-test-env
   (let (captured-read-prompt
         captured-initial-prompt
         inserted-prompt)
     (cl-letf (((symbol-function 'ai-code--git-root)
                (lambda (&optional _dir) default-directory))
               ((symbol-function 'read-string)
                (ai-code-test--unit-test-read-string "Prompt Pipeline" "Chinese"))
               ((symbol-function 'ai-code-plain-read-string)
                (lambda (prompt &optional initial-input)
                  (setq captured-read-prompt prompt
                        captured-initial-prompt initial-input)
                  initial-input))
               ((symbol-function 'ai-code--insert-prompt)
                (lambda (prompt) (setq inserted-prompt prompt))))
       (ai-code-derive-topic-unit-tests)
       (should (equal captured-read-prompt "Derive topic unit tests prompt: "))
       (should (string-match-p (regexp-quote "topic: Prompt Pipeline")
                               captured-initial-prompt))
       (should (string-match-p (regexp-quote "the most basic entry point first")
                               captured-initial-prompt))
       (should (string-match-p (regexp-quote "the project's normal test command")
                               captured-initial-prompt))
       (should (string-match-p
                (regexp-quote "Write the test comments and any explanation in Chinese.")
                captured-initial-prompt))
       (should (equal inserted-prompt captured-initial-prompt))
       ;; Learning tests are source code, so no architecture document is created.
       (should-not (file-exists-p
                    (expand-file-name ".ai.code.files/architecture"
                                      default-directory)))))))

(ert-deftest ai-code-test-derive-topic-unit-tests-separates-stored-repo-context ()
  "Stored repository context must not run into the language directive.
`ai-code--format-repo-context-info' ends without a newline, so the
directive has to supply the separator itself or the last context entry is
corrupted."
  (ai-code-file-with-test-env
   (let (inserted-prompt)
     (cl-letf (((symbol-function 'ai-code--git-root)
                (lambda (&optional _dir) default-directory))
               ((symbol-function 'read-string)
                (ai-code-test--unit-test-read-string "Prompt Pipeline" "English"))
               ((symbol-function 'ai-code--format-repo-context-info)
                (lambda ()
                  "\nStored repository context:\n  - Preserve existing CLI UX"))
               ((symbol-function 'ai-code-plain-read-string)
                (lambda (_prompt &optional initial-input) initial-input))
               ((symbol-function 'ai-code--insert-prompt)
                (lambda (prompt) (setq inserted-prompt prompt))))
       (ai-code-derive-topic-unit-tests)
       (should (string-match-p
                (regexp-quote "  - Preserve existing CLI UX\nWrite the test comments")
                inserted-prompt))))))

(ert-deftest ai-code-test-derive-topic-unit-tests-requires-a-topic ()
  "An empty topic gives the backend nothing to teach, so it must be rejected."
  (ai-code-file-with-test-env
   (let (inserted-prompt)
     (cl-letf (((symbol-function 'ai-code--git-root)
                (lambda (&optional _dir) default-directory))
               ((symbol-function 'read-string)
                (ai-code-test--unit-test-read-string "   " "English"))
               ((symbol-function 'ai-code-plain-read-string)
                (lambda (_prompt &optional initial-input) initial-input))
               ((symbol-function 'ai-code--insert-prompt)
                (lambda (prompt) (setq inserted-prompt prompt))))
       (should-error (ai-code-derive-topic-unit-tests) :type 'user-error)
       (should-not inserted-prompt)))))

(ert-deftest ai-code-test-derive-topic-unit-tests-errors-outside-git-repo ()
  "Deriving topic unit tests should require a Git repository."
  (cl-letf (((symbol-function 'ai-code--git-root)
             (lambda (&optional _dir) nil)))
    (should-error (ai-code-derive-topic-unit-tests) :type 'user-error)))

(provide 'test_ai-code-doc)
;;; test_ai-code-doc.el ends here
;;; test_ai-code-doc.el ends here
