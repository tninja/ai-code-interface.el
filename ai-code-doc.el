;;; ai-code-doc.el --- Architecture document generation for AI code interface -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides functionality to derive and manage various architecture
;; and verification documents in the AI Code Interface package.

;;; Code:

(require 'subr-x)
(require 'ai-code-utils)
(require 'ai-code-input)
(require 'ai-code-prompt-mode)

(declare-function ai-code-plain-read-string "ai-code-input" (prompt &optional initial-input history default inherit-input-method))
(declare-function ai-code--insert-prompt "ai-code-prompt-mode" (prompt-text))
(declare-function ai-code--format-repo-context-info "ai-code-utils")
(declare-function ai-code--git-root "ai-code-utils" (&optional dir))
(declare-function ai-code--ensure-files-directory "ai-code-utils")

(defconst ai-code--architecture-document-choices
  '(("Derive Architecture Guardrails" . ai-code-derive-architecture-guardrails)
    ("Derive DDD Context for Repo" . ai-code-derive-ddd-context)
    ("Derive Test Context Document" . ai-code-derive-test-context))
  "Choices for `ai-code-derive-architecture-document`.")

;;;###autoload
(defun ai-code-derive-architecture-document ()
  "Derive an architecture document by selecting one of the available options."
  (interactive)
  (let* ((default-choice (caar ai-code--architecture-document-choices))
         (choice (completing-read "Derive architecture document: "
                                  (mapcar #'car ai-code--architecture-document-choices)
                                  nil t nil nil default-choice))
         (command (alist-get choice ai-code--architecture-document-choices
                             nil nil #'string=)))
    (funcall command)))

(defun ai-code--read-document-language ()
  "Ask the user which language they want to use in the doc.
Default value is English."
  (let ((lang (read-string "Document language: " "English")))
    (if (string-empty-p lang) "English" lang)))

(defun ai-code--append-document-language (base-prompt)
  "Prompt for the language and append it to BASE-PROMPT."
  (concat base-prompt (format "\nGenerate the document in %s." (ai-code--read-document-language))))

(defconst ai-code-ddd-context-output-relative-path
  ".ai.code.files/architecture/domain-context.org"
  "Repository-relative path for the derived DDD context document.")

(defconst ai-code-test-context-output-relative-path
  ".ai.code.files/architecture/test-context.org"
  "Repository-relative path for the derived Test Context document.")

(defconst ai-code-file--architecture-guardrails-file-name
  "guardrails.org"
  "File name for derived architecture guardrails.")

(defconst ai-code-file--architecture-guardrails-directory-name
  "architecture"
  "Directory name for derived architecture guardrails.")

(defconst ai-code-file--architecture-guardrails-template
  (mapconcat #'identity
             '("#+TITLE: Architecture Guardrails"
               ""
               "* Purpose"
               ""
               "* Important Modules / Areas"
               ""
               "* Dependency Rules"
               ""
               "* State and Ownership Rules"
               ""
               "* AI Change Rules"
               ""
               "* Required Validation"
               ""
               "* Notes and Uncertainties"
               "")
             "\n")
  "Initial Org template for architecture guardrails.")

(defun ai-code--derive-ddd-context-prompt (git-root)
  "Build and return a formatted DDD context derivation prompt string for GIT-ROOT."
  (concat
   "Derive a lightweight Domain-Driven Design (DDD) style context document for this existing repository.\n"
   "Do not assume the repository already follows DDD today.\n"
   "Do not invent an ideal architecture.\n"
   "Infer domain terms, bounded context candidates, core flows, invariants, and testing ideas from the actual code, tests, docs, filenames, and existing behavior.\n"
   "Mark uncertainty explicitly.\n"
   "Keep the output practical, concise, and useful for future AI coding tasks.\n"
   "Do not suggest large refactors unless you list them separately as optional future ideas.\n"
   "When referencing any code file, function, variable, or type, you MUST provide a relative Org-mode link in the format [[file:../../path/to/file::symbol_or_line][description_text]] pointing to its definition in the codebase (relative to the .ai.code.files/architecture/ output directory).\n"
   (format "Repository root: %s\n" git-root)
   (format "Create or update the Org file at %s.\n\n"
            ai-code-ddd-context-output-relative-path)
   "Use this structure:\n"
   "* Domain Context\n\n"
   "** Purpose\n"
   "** Ubiquitous Language\n"
   "** Main Domain Concepts\n"
   "** Bounded Context Candidates\n"
   "** Core Flows\n"
   "** Domain Invariants / Business Rules\n"
   "** Testing Ideas\n"
   "** Notes and Uncertainties"))

(defun ai-code--derive-test-context-prompt (git-root)
  "Build and return Test Context prompt for GIT-ROOT."
  (concat
   "Derive a lightweight Test Context and Verification Guide document for this existing repository.\n"
   "Analyze the existing tests, test runner configuration, and mocking/verification patterns.\n"
   "Explain how the tests demonstrate and safeguard core business invariants.\n"
   "Keep the output practical, concise, and useful for future AI coding tasks.\n"
   "When referencing any test file, source file, test case, function, variable, or type, you MUST provide a relative Org-mode link in the format [[file:../../path/to/file::symbol_or_line][description_text]] pointing to its definition in the codebase (relative to the .ai.code.files/architecture/ output directory).\n"
   (format "Repository root: %s\n" git-root)
   (format "Create or update the Org file at %s.\n\n"
            ai-code-test-context-output-relative-path)
   "Use this structure:\n"
   "* Test Context and Verification Guide\n\n"
   "** Purpose\n"
   "** Test Runner & Tooling\n"
   "** Folder Structure & Organization\n"
   "** Key Mocking & Fixture Patterns\n"
   "** Business Rules Derived from Tests\n"
   "** Coverage Gaps & Actionable Testing Ideas\n"
   "** Notes and Uncertainties"))

(defun ai-code--architecture-guardrails-relative-path ()
  "Return the repo-relative path for the architecture guardrails file."
  (concat ai-code-files-dir-name "/"
          ai-code-file--architecture-guardrails-directory-name "/"
          ai-code-file--architecture-guardrails-file-name))

(defun ai-code--architecture-guardrails-file-path ()
  "Return the absolute path for the architecture guardrails file."
  (expand-file-name ai-code-file--architecture-guardrails-file-name
                    (expand-file-name
                     ai-code-file--architecture-guardrails-directory-name
                     (ai-code--ensure-files-directory))))

(defun ai-code--ensure-architecture-guardrails-file ()
  "Create the architecture guardrails file with a starter template if missing."
  (let ((target-file (ai-code--architecture-guardrails-file-path)))
    (unless (file-directory-p (file-name-directory target-file))
      (make-directory (file-name-directory target-file) t))
    (unless (file-exists-p target-file)
      (with-temp-file target-file
        (insert ai-code-file--architecture-guardrails-template)))
    target-file))

(defun ai-code--build-architecture-guardrails-prompt (git-root)
  "Build the default prompt to derive architecture guardrails for GIT-ROOT."
  (let ((relative-path (ai-code--architecture-guardrails-relative-path)))
    (mapconcat
     #'identity
     (list "Derive a lightweight architecture guardrails document for this existing repository."
           (format "Repository path: %s" git-root)
           (format "Write or update @%s in Org-mode format." relative-path)
           ""
           "Infer practical module boundaries, dependency rules, state ownership rules, and validation expectations from the current code, tests, docs, and filenames."
           "Do not invent an ideal architecture."
           "Do not force DDD, Hexagonal Architecture, or Clean Architecture onto the repository."
           "Prefer simple, practical rules over abstract architecture theory."
           "Mark uncertain conclusions clearly."
           "Focus on what helps future AI coding sessions avoid breaking boundaries or introducing messy dependencies."
           "Do not suggest large refactors unless clearly separated as optional future ideas."
           "Keep it concise, practical, and small enough to reuse in future AI prompts."
           "When referencing any code file, folder, module, function, variable, or type, you MUST provide a relative Org-mode link in the format [[file:../../path/to/file::symbol_or_line][description]] pointing to its definition in the codebase (relative to the .ai.code.files/architecture/ output directory)."
           ""
           "Use this Org structure:"
           "#+TITLE: Architecture Guardrails"
           ""
           "* Purpose"
           "* Important Modules / Areas"
           "* Dependency Rules"
           "* State and Ownership Rules"
           "* AI Change Rules"
           "* Required Validation"
           "* Notes and Uncertainties"
           ""
           "If the file already exists, refine it instead of rewriting unrelated guidance.")
     "\n")))

;;;###autoload
(defun ai-code-derive-architecture-guardrails ()
  "Ask the current AI backend to derive repository architecture guardrails."
  (interactive)
  (let ((git-root (ai-code--git-root)))
    (unless git-root
      (user-error "Not in a git repository"))
    (ai-code--ensure-architecture-guardrails-file)
    (let* ((base-prompt (ai-code--build-architecture-guardrails-prompt git-root))
           (initial-prompt (ai-code--append-document-language base-prompt)))
      (if-let ((final-prompt
                (ai-code-plain-read-string "Prompt: " initial-prompt)))
          (progn
            (ai-code--insert-prompt final-prompt)
            (message "Requested architecture guardrails for %s" git-root))
        (message "Architecture guardrails request cancelled")))))

;;;###autoload
(defun ai-code-derive-ddd-context ()
  "Ask AI to derive a lightweight DDD context document for the current repo.
The target Org file under `.ai.code.files/architecture/` is created if it does
not already exist, so the backend has a concrete document to create or update."
  (interactive)
  (let* ((git-root (or (ai-code--git-root)
                       (user-error "Not inside a Git repository")))
         (files-dir (ai-code--ensure-files-directory))
         (architecture-dir (expand-file-name "architecture" files-dir))
         (target-file (expand-file-name "domain-context.org" architecture-dir)))
    (make-directory architecture-dir t)
    (unless (file-exists-p target-file)
      (write-region "" nil target-file nil 'silent))
    (let* ((base-prompt
            (concat (ai-code--derive-ddd-context-prompt git-root)
                    (or (ai-code--format-repo-context-info) "")))
           (initial-prompt (ai-code--append-document-language base-prompt))
           (final-prompt (ai-code-plain-read-string "Derive DDD context prompt: "
                                                    initial-prompt)))
      (when final-prompt
        (ai-code--insert-prompt final-prompt)))))

;;;###autoload
(defun ai-code-derive-test-context ()
  "Ask AI to derive a lightweight Test Context document for the current repo.
The target Org file under `.ai.code.files/architecture/` is created if it does
not already exist, so the backend has a concrete document to create or update."
  (interactive)
  (let* ((git-root (or (ai-code--git-root)
                       (user-error "Not inside a Git repository")))
         (files-dir (ai-code--ensure-files-directory))
         (architecture-dir (expand-file-name "architecture" files-dir))
         (target-file (expand-file-name "test-context.org" architecture-dir)))
    (make-directory architecture-dir t)
    (unless (file-exists-p target-file)
      (write-region "" nil target-file nil 'silent))
    (let* ((base-prompt
            (concat (ai-code--derive-test-context-prompt git-root)
                    (or (ai-code--format-repo-context-info) "")))
           (initial-prompt (ai-code--append-document-language base-prompt))
           (final-prompt (ai-code-plain-read-string "Derive Test Context prompt: "
                                                    initial-prompt)))
      (when final-prompt
        (ai-code--insert-prompt final-prompt)))))

(provide 'ai-code-doc)
;;; ai-code-doc.el ends here
