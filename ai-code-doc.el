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
(declare-function ai-code--get-git-web-repo-url "ai-code-github" ())
(declare-function magit-git-string "magit-git" (&rest args))
(declare-function magit-git-success "magit-git" (&rest args))

(defconst ai-code--architecture-document-choices
  '(("Derive Architecture Guardrails" . ai-code-derive-architecture-guardrails)
    ("Derive C4 PlantUML Architecture Document" . ai-code-derive-c4-plantuml)
    ("Derive Repository Map" . ai-code-derive-repo-map)
    ("Derive DDD Context for Repo" . ai-code-derive-ddd-context)
    ("Derive Test Context Document" . ai-code-derive-test-context)
    ("Derive Unit Test to Help Understand the Topic" . ai-code-derive-topic-unit-tests))
  "Choices for `ai-code-derive-architecture-document'.")

(defun ai-code--doc-emit-prompt (title prompt)
  "Insert PROMPT under a TITLE headline at point, or send it to the AI.
In `ai-code-prompt-mode' the user is asked whether to write the prompt
into the current buffer, as an Org section at the level of the
surrounding section, in which case no AI request is made.  When the
answer is no, and everywhere outside `ai-code-prompt-mode', PROMPT is
handed to `ai-code--insert-prompt' as usual."
  (if (and (derived-mode-p 'ai-code-prompt-mode)
           (y-or-n-p "Insert prompt into this buffer instead of sending it to the AI? "))
      (let ((level (or (org-current-level) 1)))
        (unless (bolp)
          (insert "\n"))
        (insert (make-string level ?*) " " title " ")
        (org-insert-time-stamp (current-time) t t)
        (insert "\n" prompt "\n"))
    (ai-code--insert-prompt prompt)))

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

(defun ai-code--topic-at-point ()
  "Return the function or class at point as a topic, or nil.
Only `prog-mode' buffers offer one, formatted as \"Service.run
\(src/Service.java)\" with the file relative to the repository root.
The file part is omitted when the buffer visits no file."
  (when (derived-mode-p 'prog-mode)
    (when-let* ((scope (ai-code--current-qualified-scope-name)))
      (if buffer-file-name
          (format "%s (%s)" scope
                  (file-relative-name buffer-file-name (ai-code--git-root)))
        scope))))

(defun ai-code--read-document-topic ()
  "Ask which topic the document should cover.
Return nil for an empty answer, which means the whole repository.
When `ai-code--topic-at-point' offers a topic, first ask whether to scope
the document to it: declining means the whole repository, and accepting
pre-fills the topic for editing."
  (let ((at-point (ai-code--topic-at-point)))
    (when (or (null at-point)
              (y-or-n-p (format "Scope document to \"%s\"? " at-point)))
      (let ((topic (string-trim
                    (read-string "Document topic (empty for whole repo): "
                                 at-point))))
        (unless (string-empty-p topic) topic)))))

(defun ai-code--topic-file-name (file-name topic)
  "Return FILE-NAME with a slug and stable digest of TOPIC in its base name.
The digest distinguishes topics whose readable slugs are identical.
FILE-NAME is returned unchanged when TOPIC is nil, so whole-repository
documents keep their well-known path and topic documents never overwrite
them."
  (if topic
      (let ((slug (string-trim
                   (downcase (replace-regexp-in-string "[^A-Za-z0-9]+" "-" topic))
                   "-+" "-+")))
        (format "%s-%s-%s.%s"
                (file-name-sans-extension file-name)
                (if (string-empty-p slug) "topic" slug)
                (substring (secure-hash 'sha256
                                        (encode-coding-string topic 'utf-8))
                           0 12)
                (file-name-extension file-name)))
    file-name))

(defun ai-code--append-document-topic (base-prompt topic)
  "Append TOPIC scoping instructions to BASE-PROMPT.
BASE-PROMPT is returned unchanged when TOPIC is nil."
  (if topic
      (concat base-prompt
              (format "\nScope this document to the topic: %s.\n" topic)
              "Only cover the code, tests, and docs relevant to this topic, and state which parts of the repository are out of scope.\n")
    base-prompt))

(defun ai-code--finalize-document-prompt (base-prompt topic)
  "Append the TOPIC scope and the document language to BASE-PROMPT.
The topic is added first so that the language question stays the last one
asked before the prompt is edited."
  (ai-code--append-document-language
   (ai-code--append-document-topic base-prompt topic)))

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

(defun ai-code--doc-github-repo-url ()
  "Return the GitHub web URL of the current repository, or nil.
Only GitHub remotes are recognized, because the generated links use the
GitHub /blob/REVISION/ URL shape.  Any failure to reach Git or to parse the
remote simply yields nil, which keeps documents on local file links."
  (ignore-errors
    (require 'ai-code-github)
    (let ((url (ai-code--get-git-web-repo-url)))
      (when (and url (string-match-p "\\`https://[^/]*github[^/]*/" url))
        url))))

(defun ai-code--doc-github-source-url ()
  "Return a GitHub URL pinned to the analyzed commit, or nil.
Require unchanged tracked files and an origin remote-tracking ref that
contains HEAD.  Unpublished commits and Git failures use local links.
Remote-tracking refs are checked locally; no network request is made."
  (ignore-errors
    (let* ((repo-url (ai-code--doc-github-repo-url))
           (revision (and repo-url
                          (magit-git-string "rev-parse" "--verify" "HEAD"))))
      (when (and revision
                 (magit-git-success "diff" "--quiet" revision "--")
                 (magit-git-string "for-each-ref" "--format=%(refname)"
                                   (concat "--contains=" revision)
                                   "refs/remotes/origin/"))
        (format "%s/blob/%s" repo-url revision)))))

(defun ai-code--read-document-link-style ()
  "Ask whether code references should link to GitHub or to local files.
Return `github' or `local'.  Skip the question unless the analyzed commit
is known on a GitHub origin and tracked files are unchanged."
  (if (and (ai-code--doc-github-source-url)
           (string-prefix-p
            "g"
            (string-trim (read-string "Link code references to (github or local): "
                                      "github"))
            t))
      'github
    'local))

(defun ai-code--org-link-instruction (output-relative-path &optional link-style)
  "Return the Org link rules for a document written to OUTPUT-RELATIVE-PATH.
LINK-STYLE is `github' to prefer browser links, and anything else keeps
the document on local file links.  Local links to files inside the
repository are relative to the document itself, so one \"../\" is needed
per directory level OUTPUT-RELATIVE-PATH sits in below the repository
root; files outside the repository have no such anchor and are linked by
absolute path."
  (let* ((depth (length (split-string
                         (or (file-name-directory output-relative-path) "")
                         "/" t)))
         (prefix (apply #'concat (make-list depth "../")))
         (local-rules
          (concat
           (format "For a file inside this repository, use a relative link: [[file:%spath/to/file::symbol][description]].\n"
                   prefix)
           "For a file outside this repository, use an absolute link: [[file:/absolute/path/to/file::symbol][description]].\n"))
         (source-url (and (eq link-style 'github)
                          (ai-code--doc-github-source-url))))
    (concat
     "When referencing any file, folder, module, function, variable, type, or test case, you MUST turn it into a link so that the reader can jump from the document straight to the code.\n"
     (if source-url
         (concat
          (format "Use a GitHub link, which opens in a browser: [[%s/path/to/file#L42][description]], anchored at the line where the definition starts.\n"
                  source-url)
          "Keep the commit ID in the URL; do not replace it with HEAD or a branch name. Verify that the referenced file and lines match this commit and that it is available on GitHub.\n"
          "For files outside this repository, untracked, ignored, generated, or locally modified files, or when the commit is unavailable on GitHub, fall back to a local link instead:\n"
          local-rules)
       local-rules)
     "Point each link at the definition: in a local link use ::symbol as the search target, and fall back to ::<line-number> only when there is no named symbol to search for.\n"
     "Only link to paths and symbols you have actually confirmed in the repository; when you cannot confirm a definition, say so in plain text instead of guessing a link.\n"
     "Add the link at the first mention in each section, in every table cell that names a file or a symbol, and in the explanatory notes that follow each diagram.\n")))

(defun ai-code--ensure-architecture-document-file (file-name)
  "Ensure an architecture document named FILE-NAME exists and return its path."
  (let* ((files-dir (ai-code--ensure-files-directory))
         (architecture-dir (expand-file-name "architecture" files-dir))
         (target-file (expand-file-name file-name architecture-dir)))
    (make-directory architecture-dir t)
    (unless (file-exists-p target-file)
      (write-region "" nil target-file nil 'silent))
    target-file))

(defun ai-code--derive-ddd-context-prompt (git-root &optional topic link-style)
  "Build and return a formatted DDD context derivation prompt string for GIT-ROOT.
TOPIC narrows the output file name when non-nil, and LINK-STYLE
selects how code references are linked."
  (concat
   "Derive a lightweight Domain-Driven Design (DDD) style context document for this existing repository.\n"
   "Do not assume the repository already follows DDD today.\n"
   "Do not invent an ideal architecture.\n"
   "Infer domain terms, bounded context candidates, core flows, invariants, and testing ideas from the actual code, tests, docs, filenames, and existing behavior.\n"
   "Mark uncertainty explicitly.\n"
   "Keep the output practical, concise, and useful for future AI coding tasks.\n"
   "Do not suggest large refactors unless you list them separately as optional future ideas.\n"
   (ai-code--org-link-instruction ai-code-ddd-context-output-relative-path link-style)
   (format "Repository root: %s\n" git-root)
   (format "Create or update the Org file at %s.\n\n"
            (ai-code--topic-file-name ai-code-ddd-context-output-relative-path topic))
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

(defun ai-code--derive-test-context-prompt (git-root &optional topic link-style)
  "Build and return Test Context prompt for GIT-ROOT.
TOPIC narrows the output file name when non-nil, and LINK-STYLE
selects how code references are linked."
  (concat
   "Derive a lightweight Test Context and Verification Guide document for this existing repository.\n"
   "Analyze the existing tests, test runner configuration, and mocking/verification patterns.\n"
   "Explain how the tests demonstrate and safeguard core business invariants.\n"
   "Keep the output practical, concise, and useful for future AI coding tasks.\n"
   (ai-code--org-link-instruction ai-code-test-context-output-relative-path link-style)
   (format "Repository root: %s\n" git-root)
   (format "Create or update the Org file at %s.\n\n"
            (ai-code--topic-file-name ai-code-test-context-output-relative-path topic))
   "Use this structure:\n"
   "* Test Context and Verification Guide\n\n"
   "** Purpose\n"
   "** Test Runner & Tooling\n"
   "** Folder Structure & Organization\n"
   "** Key Mocking & Fixture Patterns\n"
   "** Business Rules Derived from Tests\n"
   "** Coverage Gaps & Actionable Testing Ideas\n"
   "** Notes and Uncertainties"))

(defun ai-code--derive-c4-plantuml-prompt (git-root &optional topic link-style)
  "Build and return a C4 PlantUML architecture document prompt for GIT-ROOT.
TOPIC narrows the output file name when non-nil, and LINK-STYLE
selects how code references are linked."
  (concat
   "Derive a C4-style architecture overview document for this existing repository.\n"
   "Generate the document as Org mode and embed PlantUML C4 diagrams in Org Babel source blocks.\n"
   "Create or update the document as an architecture reading guide, not just a collection of diagrams.\n"
   "Infer architecture from actual source files, tests, README files, package metadata, scripts, and configuration.\n"
   "Do not invent external systems, deployment topology, runtime dependencies, users, or protocols that are not supported by code or documentation.\n"
   "Mark uncertain boundaries, relationships, and naming choices explicitly.\n"
   "Prefer fewer boxes and clearer relationships over large, noisy diagrams.\n"
   "Use C4 only as an architectural draft for human review.\n"
   (ai-code--org-link-instruction ai-code-c4-plantuml-output-relative-path link-style)
   "For every diagram, include explanatory notes after the PlantUML block that summarize what the diagram shows and what remains uncertain.\n"
   "Use Org Babel blocks like #+begin_src plantuml :file c4-context.svg :exports both and include @startuml / @enduml inside each block.\n"
   "Use PlantUML C4 includes such as !include <C4/C4_Context>, !include <C4/C4_Container>, and !include <C4/C4_Component> when appropriate.\n"
   (format "Repository root: %s\n" git-root)
   (format "Create or update the Org file at %s.\n\n"
            (ai-code--topic-file-name ai-code-c4-plantuml-output-relative-path topic))
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

(defun ai-code--derive-repo-map-prompt (git-root &optional topic link-style)
  "Build and return a repository map derivation prompt for GIT-ROOT.
TOPIC narrows the output file name when non-nil, and LINK-STYLE
selects how code references are linked."
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
   (ai-code--org-link-instruction ai-code-repo-map-output-relative-path link-style)
   "Use text and tables as the main format. Include at most two small PlantUML diagrams only when they improve navigation: one top-level dependency or module graph, and optionally one suggested reading-path graph.\n"
   "Use Org Babel PlantUML blocks with :file when adding diagrams.\n"
   (format "Repository root: %s\n" git-root)
   (format "Create or update the Org file at %s.\n\n"
           (ai-code--topic-file-name ai-code-repo-map-output-relative-path topic))
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

(defun ai-code--read-unit-test-topic ()
  "Read the topic the derived unit tests must explain.
An empty topic leaves the backend nothing to teach, so it is rejected
instead of falling back to the whole repository.  The topic is pre-filled
from `ai-code--topic-at-point'."
  (let ((topic (string-trim (read-string "Unit test topic: "
                                         (ai-code--topic-at-point)))))
    (if (string-empty-p topic)
        (user-error "A topic is required to derive unit tests")
      topic)))

(defun ai-code--derive-topic-unit-tests-prompt (git-root topic)
  "Build a prompt asking AI to write learning unit tests for TOPIC in GIT-ROOT.
The generated tests are ordinary runnable tests of the code that already
exists, so they belong beside the repository's own tests instead of under
`.ai.code.files/', and they carry no Org link instructions."
  (concat
   (format "Write runnable unit tests whose purpose is to teach a reader the code related to this topic: %s.\n"
           topic)
   "These tests are a reading aid first and a safety net second: every assertion must document how the existing code already behaves.\n"
   "Read the relevant code before writing anything, and assert only behavior you have confirmed in this repository. Never assert an invented API.\n"
   "Use the test framework, naming convention, directory layout, fixtures, and build integration this repository already uses, so the tests run with the project's normal test command.\n"
   "Group the tests into one or more test classes or files whose names make clear that they are learning tests for this topic.\n"
   "Order them as a reading path: the most basic entry point first, then the common use cases, then the advanced behavior, then the edge and error cases. State that order in a header comment and keep each file readable from top to bottom.\n"
   "Keep every test small and independent, prefer literal expected values over computed ones, and mock only what the reader does not need to understand.\n"
   "Comment each test with what the reader should learn from it and which source file and symbol it exercises.\n"
   "Do not modify production code. When a behavior cannot be exercised without changing it, explain the obstacle in a comment instead of adding a test that would fail.\n"
   (format "Repository root: %s\n" git-root)
   "Finally, report the files you created and the exact command that runs these tests.\n"))

(defun ai-code--architecture-guardrails-relative-path (&optional topic)
  "Return the repo-relative path for the architecture guardrails file.
TOPIC narrows the file name when non-nil."
  (concat ai-code-files-dir-name "/"
          ai-code-file--architecture-guardrails-directory-name "/"
          (ai-code--topic-file-name
           ai-code-file--architecture-guardrails-file-name topic)))

(defun ai-code--architecture-guardrails-file-path (&optional topic)
  "Return the absolute path for the architecture guardrails file.
TOPIC narrows the file name when non-nil."
  (expand-file-name (ai-code--topic-file-name
                     ai-code-file--architecture-guardrails-file-name topic)
                    (expand-file-name
                     ai-code-file--architecture-guardrails-directory-name
                     (ai-code--ensure-files-directory))))

(defun ai-code--ensure-architecture-guardrails-file (&optional topic)
  "Create the architecture guardrails file with a starter template if missing.
TOPIC narrows the file name when non-nil."
  (let ((target-file (ai-code--architecture-guardrails-file-path topic)))
    (unless (file-directory-p (file-name-directory target-file))
      (make-directory (file-name-directory target-file) t))
    (unless (file-exists-p target-file)
      (with-temp-file target-file
        (insert ai-code-file--architecture-guardrails-template)))
    target-file))

(defun ai-code--build-architecture-guardrails-prompt (git-root &optional topic link-style)
  "Build the default prompt to derive architecture guardrails for GIT-ROOT.
TOPIC narrows the output file name when non-nil, and LINK-STYLE
selects how code references are linked."
  (let ((relative-path (ai-code--architecture-guardrails-relative-path topic)))
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
           (string-trim-right (ai-code--org-link-instruction relative-path link-style))
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
    (let* ((topic (ai-code--read-document-topic))
           (link-style (ai-code--read-document-link-style))
           (base-prompt (ai-code--build-architecture-guardrails-prompt
                         git-root topic link-style))
           (initial-prompt (ai-code--finalize-document-prompt base-prompt topic)))
      (ai-code--ensure-architecture-guardrails-file topic)
      (if-let ((final-prompt
                (ai-code-plain-read-string "Prompt: " initial-prompt)))
          (progn
            (ai-code--doc-emit-prompt "Derive Architecture Guardrails" final-prompt)
            (message "Architecture guardrails prompt ready for %s" git-root))
        (message "Architecture guardrails request cancelled")))))

;;;###autoload
(defun ai-code-derive-ddd-context ()
  "Ask AI to derive a lightweight DDD context document for the current repo.
The target Org file under `.ai.code.files/architecture/' is created if it does
not already exist, so the backend has a concrete document to create or update."
  (interactive)
  (let* ((git-root (or (ai-code--git-root)
                       (user-error "Not inside a Git repository")))
         (topic (ai-code--read-document-topic))
         (link-style (ai-code--read-document-link-style)))
    (ai-code--ensure-architecture-document-file
     (ai-code--topic-file-name "domain-context.org" topic))
    (let* ((base-prompt
            (concat (ai-code--derive-ddd-context-prompt git-root topic link-style)
                    (or (ai-code--format-repo-context-info) "")))
           (initial-prompt (ai-code--finalize-document-prompt base-prompt topic))
           (final-prompt (ai-code-plain-read-string "Derive DDD context prompt: "
                                                    initial-prompt)))
      (when final-prompt
        (ai-code--doc-emit-prompt "Derive DDD Context for Repo" final-prompt)))))

;;;###autoload
(defun ai-code-derive-test-context ()
  "Ask AI to derive a lightweight Test Context document for the current repo.
The target Org file under `.ai.code.files/architecture/' is created if it does
not already exist, so the backend has a concrete document to create or update."
  (interactive)
  (let* ((git-root (or (ai-code--git-root)
                       (user-error "Not inside a Git repository")))
         (topic (ai-code--read-document-topic))
         (link-style (ai-code--read-document-link-style)))
    (ai-code--ensure-architecture-document-file
     (ai-code--topic-file-name "test-context.org" topic))
    (let* ((base-prompt
            (concat (ai-code--derive-test-context-prompt git-root topic link-style)
                    (or (ai-code--format-repo-context-info) "")))
           (initial-prompt (ai-code--finalize-document-prompt base-prompt topic))
           (final-prompt (ai-code-plain-read-string "Derive Test Context prompt: "
                                                    initial-prompt)))
      (when final-prompt
        (ai-code--doc-emit-prompt "Derive Test Context Document" final-prompt)))))

;;;###autoload
(defun ai-code-derive-c4-plantuml ()
  "Ask AI to derive a C4 PlantUML architecture document for the current repo.
The target Org file under `.ai.code.files/architecture/' is created if it does
not already exist, so the backend has a concrete document to create or update."
  (interactive)
  (let* ((git-root (or (ai-code--git-root)
                       (user-error "Not inside a Git repository")))
         (topic (ai-code--read-document-topic))
         (link-style (ai-code--read-document-link-style)))
    (ai-code--ensure-architecture-document-file
     (ai-code--topic-file-name "c4-overview.org" topic))
    (let* ((base-prompt
            (concat (ai-code--derive-c4-plantuml-prompt git-root topic link-style)
                    (or (ai-code--format-repo-context-info) "")))
           (initial-prompt (ai-code--finalize-document-prompt base-prompt topic))
           (final-prompt (ai-code-plain-read-string "Derive C4 PlantUML prompt: "
                                                    initial-prompt)))
      (when final-prompt
        (ai-code--doc-emit-prompt "Derive C4 PlantUML Architecture Document"
                                  final-prompt)))))

;;;###autoload
(defun ai-code-derive-repo-map ()
  "Ask AI to derive a repository map document for the current repo.
The target Org file under `.ai.code.files/architecture/' is created if it does
not already exist, so the backend has a concrete document to create or update."
  (interactive)
  (let* ((git-root (or (ai-code--git-root)
                       (user-error "Not inside a Git repository")))
         (topic (ai-code--read-document-topic))
         (link-style (ai-code--read-document-link-style)))
    (ai-code--ensure-architecture-document-file
     (ai-code--topic-file-name "repo-map.org" topic))
    (let* ((base-prompt
            (concat (ai-code--derive-repo-map-prompt git-root topic link-style)
                    (or (ai-code--format-repo-context-info) "")))
           (initial-prompt (ai-code--finalize-document-prompt base-prompt topic))
           (final-prompt (ai-code-plain-read-string "Derive repository map prompt: "
                                                    initial-prompt)))
      (when final-prompt
        (ai-code--doc-emit-prompt "Derive Repository Map" final-prompt)))))

;;;###autoload
(defun ai-code-derive-topic-unit-tests ()
  "Ask AI to write runnable unit tests that explain a topic in the current repo.
Unlike the other derivation commands this one produces test code beside
the repository's own tests, so a topic is required and no Org document is
created."
  ;; DONE: If the current buffer is a prog-mode derived buffer (eg. java-mode, or java-ts-mode), we could pre-fill the topic with context under cursor, eg. a function, or a class.
  (interactive)
  (let* ((git-root (or (ai-code--git-root)
                       (user-error "Not inside a Git repository")))
         (topic (ai-code--read-unit-test-topic))
         (base-prompt
          (concat (ai-code--derive-topic-unit-tests-prompt git-root topic)
                  (or (ai-code--format-repo-context-info) "")))
         (initial-prompt
          (concat base-prompt
                  (format "\nWrite the test comments and any explanation in %s.\n"
                          (ai-code--read-document-language))))
         (final-prompt (ai-code-plain-read-string "Derive topic unit tests prompt: "
                                                  initial-prompt)))
    (when final-prompt
      (ai-code--doc-emit-prompt "Derive Unit Test to Help Understand the Topic"
                                final-prompt))))

(provide 'ai-code-doc)
;;; ai-code-doc.el ends here
