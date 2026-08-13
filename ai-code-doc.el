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
    ("Derive C4 PlantUML Architecture Document" . ai-code-derive-c4-plantuml)
    ("Derive Repository Map" . ai-code-derive-repo-map)
    ("Derive DDD Context for Repo" . ai-code-derive-ddd-context)
    ("Derive Test Context Document" . ai-code-derive-test-context))
  "Choices for `ai-code-derive-architecture-document'.")

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

(defconst ai-code-c4-plantuml-output-relative-path
  ".ai.code.files/architecture/c4-overview.org"
  "Repository-relative path for the derived C4 PlantUML architecture document.")

(defconst ai-code-repo-map-output-relative-path
  ".ai.code.files/architecture/repo-map.org"
  "Repository-relative path for the derived repository map document.")

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

(defun ai-code--ensure-architecture-document-file (file-name)
  "Ensure an architecture document named FILE-NAME exists and return its path."
  (let* ((files-dir (ai-code--ensure-files-directory))
         (architecture-dir (expand-file-name "architecture" files-dir))
         (target-file (expand-file-name file-name architecture-dir)))
    (make-directory architecture-dir t)
    (unless (file-exists-p target-file)
      (write-region "" nil target-file nil 'silent))
    target-file))

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

(defun ai-code--derive-c4-plantuml-prompt (git-root)
  "Build and return a C4 PlantUML architecture document prompt for GIT-ROOT."
  (concat
   "Derive a C4-style architecture overview document for this existing repository.\n"
   "Generate the document as Org mode and embed PlantUML C4 diagrams in Org Babel source blocks.\n"
   "Create or update the document as an architecture reading guide, not just a collection of diagrams.\n"
   "Infer architecture from actual source files, tests, README files, package metadata, scripts, and configuration.\n"
   "Do not invent external systems, deployment topology, runtime dependencies, users, or protocols that are not supported by code or documentation.\n"
   "Mark uncertain boundaries, relationships, and naming choices explicitly.\n"
   "Prefer fewer boxes and clearer relationships over large, noisy diagrams.\n"
   "Use C4 only as an architectural draft for human review.\n"
   "When referencing any code file, folder, module, function, variable, or type, you MUST provide a relative Org-mode link in the format [[file:../../path/to/file::symbol_or_line][description_text]] pointing to its definition in the codebase (relative to the .ai.code.files/architecture/ output directory).\n"
   "For every diagram, include explanatory notes after the PlantUML block that summarize what the diagram shows and what remains uncertain.\n"
   "Use Org Babel blocks like #+begin_src plantuml :file c4-context.svg :exports both and include @startuml / @enduml inside each block.\n"
   "Use PlantUML C4 includes such as !include <C4/C4_Context>, !include <C4/C4_Container>, and !include <C4/C4_Component> when appropriate.\n"
   (format "Repository root: %s\n" git-root)
   (format "Create or update the Org file at %s.\n\n"
            ai-code-c4-plantuml-output-relative-path)
   "Use this Org structure:\n"
   "#+TITLE: C4 Architecture Overview\n\n"
   "* Purpose\n"
   "Explain what this generated architecture guide is for and what it does not prove.\n"
   "* Confidence and Assumptions\n"
   "List confidence level, source inputs, assumptions, and unverified areas.\n"
   "* Repository Summary\n"
   "Summarize the repository responsibilities in a few practical bullets.\n"
   "* Glossary\n"
   "Define terms used in the diagrams.\n"
   "* How to Read These Diagrams\n"
   "Explain the intended reading order: System Context, Container, Component, then runtime flows.\n"
   "* System Context\n"
   "Include a C4 System Context PlantUML Babel block and notes.\n"
   "* Container View\n"
   "Include a C4 Container PlantUML Babel block and notes. Treat containers as major deployable or logical units, not necessarily Docker containers.\n"
   "* Component View\n"
   "Include one focused C4 Component PlantUML Babel block for the most important container or module, and notes.\n"
   "* Important Runtime Flows\n"
   "Describe 1-3 important flows. Include a PlantUML sequence diagram when it helps.\n"
   "* Key Architectural Decisions\n"
   "List practical design choices inferred from the code and docs.\n"
   "* Open Questions\n"
   "List areas that need human confirmation.\n"
   "* Source Evidence\n"
   "Provide a table mapping important claims to Org links pointing at source evidence."))

(defun ai-code--derive-repo-map-prompt (git-root)
  "Build and return a repository map derivation prompt for GIT-ROOT."
  (concat
   "Derive a lightweight Repository Map document for this existing repository.\n"
   "The primary goal is to help a new human contributor or AI coding agent quickly understand how to read and navigate the codebase.\n"
   "This is an onboarding and reading-path document, not a C4 architecture document and not a full design document.\n"
   "Focus on concrete source layout, important files, entry points, reading order, and high-signal versus low-signal areas.\n"
   "Infer from actual source files, tests, README files, package metadata, scripts, and configuration.\n"
   "Do not invent modules, workflows, or dependencies that are not supported by code or documentation.\n"
   "Mark uncertainty explicitly when a file or directory purpose is inferred rather than documented.\n"
   "Prefer practical guidance over abstract architecture theory.\n"
   "Keep the document concise enough to be reused in future AI coding prompts.\n"
   "When referencing any code file, folder, module, function, variable, or type, you MUST provide a relative Org-mode link in the format [[file:../../path/to/file::symbol_or_line][description_text]] pointing to its definition in the codebase (relative to the .ai.code.files/architecture/ output directory).\n"
   "Use text and tables as the main format. Include at most two small PlantUML diagrams only when they improve navigation: one top-level dependency or module graph, and optionally one suggested reading-path graph.\n"
   "Use Org Babel PlantUML blocks with :file when adding diagrams.\n"
   (format "Repository root: %s\n" git-root)
   (format "Create or update the Org file at %s.\n\n"
           ai-code-repo-map-output-relative-path)
   "Use this Org structure:\n"
   "#+TITLE: Repository Map\n\n"
   "* Purpose\n"
   "Explain that this document helps readers navigate a new repository quickly, and that it should be reviewed by humans.\n"
   "* What This Repository Does\n"
   "Summarize the repository responsibilities in 2-5 practical bullets.\n"
   "* Top-Level Directory and File Map\n"
   "Provide a table with Path, Purpose, Importance, and First-read? columns.\n"
   "* Suggested Reading Order\n"
   "Give a short ordered reading path for a new contributor. Explain why each step appears in that order.\n"
   "* Important Entry Points\n"
   "List interactive commands, public APIs, executable scripts, package entry files, hooks, or configuration entry points.\n"
   "* Core Concepts\n"
   "Define repository-specific concepts that a reader must know before editing code.\n"
   "* Module / File Relationship Sketch\n"
   "Include a compact PlantUML dependency sketch only if the relationships are supported by source evidence. Keep it small.\n"
   "* Files Usually Changed Together\n"
   "List files, tests, docs, or configs that appear coupled and should be considered together.\n"
   "* High-Risk or High-Churn Areas\n"
   "Identify files or directories that appear central, risky, unstable, or dependency-heavy. Explain the evidence.\n"
   "* Low-Signal Areas to Ignore Initially\n"
   "Identify generated, vendor, build-output, archived, or repetitive files that a new reader should skip at first.\n"
   "* Common Change Scenarios\n"
   "Map likely user tasks to the files or directories they should inspect first.\n"
   "* Open Questions\n"
   "List areas that need human confirmation.\n"
   "* Source Evidence\n"
   "Provide a table mapping important claims to Org links pointing at source evidence."))

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
The target Org file under `.ai.code.files/architecture/' is created if it does
not already exist, so the backend has a concrete document to create or update."
  (interactive)
  (let* ((git-root (or (ai-code--git-root)
                       (user-error "Not inside a Git repository"))))
    (ai-code--ensure-architecture-document-file "domain-context.org")
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
The target Org file under `.ai.code.files/architecture/' is created if it does
not already exist, so the backend has a concrete document to create or update."
  (interactive)
  (let* ((git-root (or (ai-code--git-root)
                       (user-error "Not inside a Git repository"))))
    (ai-code--ensure-architecture-document-file "test-context.org")
    (let* ((base-prompt
            (concat (ai-code--derive-test-context-prompt git-root)
                    (or (ai-code--format-repo-context-info) "")))
           (initial-prompt (ai-code--append-document-language base-prompt))
           (final-prompt (ai-code-plain-read-string "Derive Test Context prompt: "
                                                    initial-prompt)))
      (when final-prompt
        (ai-code--insert-prompt final-prompt)))))

;;;###autoload
(defun ai-code-derive-c4-plantuml ()
  "Ask AI to derive a C4 PlantUML architecture document for the current repo.
The target Org file under `.ai.code.files/architecture/' is created if it does
not already exist, so the backend has a concrete document to create or update."
  (interactive)
  (let* ((git-root (or (ai-code--git-root)
                       (user-error "Not inside a Git repository"))))
    (ai-code--ensure-architecture-document-file "c4-overview.org")
    (let* ((base-prompt
            (concat (ai-code--derive-c4-plantuml-prompt git-root)
                    (or (ai-code--format-repo-context-info) "")))
           (initial-prompt (ai-code--append-document-language base-prompt))
           (final-prompt (ai-code-plain-read-string "Derive C4 PlantUML prompt: "
                                                    initial-prompt)))
      (when final-prompt
        (ai-code--insert-prompt final-prompt)))))

;;;###autoload
(defun ai-code-derive-repo-map ()
  "Ask AI to derive a repository map document for the current repo.
The target Org file under `.ai.code.files/architecture/' is created if it does
not already exist, so the backend has a concrete document to create or update."
  (interactive)
  (let* ((git-root (or (ai-code--git-root)
                       (user-error "Not inside a Git repository"))))
    (ai-code--ensure-architecture-document-file "repo-map.org")
    (let* ((base-prompt
            (concat (ai-code--derive-repo-map-prompt git-root)
                    (or (ai-code--format-repo-context-info) "")))
           (initial-prompt (ai-code--append-document-language base-prompt))
           (final-prompt (ai-code-plain-read-string "Derive repository map prompt: "
                                                    initial-prompt)))
      (when final-prompt
        (ai-code--insert-prompt final-prompt)))))

(provide 'ai-code-doc)
;;; ai-code-doc.el ends here
