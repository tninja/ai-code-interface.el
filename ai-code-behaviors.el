;;; ai-code-behaviors.el --- Behavior injection system for AI prompts -*- lexical-binding: t; -*-

;; Author: davidwuchn
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This module provides behavior injection based on prompt intent classification.
;; Behaviors are loaded from the ai-behaviors repository (https://github.com/xificurC/ai-behaviors)
;; and injected into prompts to guide AI responses.
;;
;; Features:
;; - Automatic intent classification (GPTel or keyword-based fallback)
;; - Explicit hashtag control (#=code, #deep, #tdd, etc.)
;; - Backend-agnostic injection
;;
;; Entry Points (in order of priority):
;; 1. `ai-code--insert-prompt-behaviors-advice' - Advice around `ai-code--insert-prompt'
;;    Handles preset-only prompts, session checks, command-specific presets.
;; 2. `ai-code--process-behaviors' - Main behavior processing
;;    Extracts hashtags, merges with presets, builds instruction blocks.
;; 3. `ai-code-behaviors-apply-preset' - Direct preset application
;;    Used by mode-line menu and interactive commands.
;; 4. `ai-code--behaviors-check-preset-only-prompt' - Detects preset-only prompts
;;    Called by advice to handle @preset without message content.
;;
;; Threading Model:
;; This module is designed for Emacs' single-threaded execution model.
;; State is stored in hash tables keyed by project root (git directory).
;; No locking is required as there are no concurrent accesses.
;; Caches use TTL-based expiration rather than explicit invalidation.

;;; Code:

(require 'seq)
(require 'cl-lib)

(require 'gptel nil t)

(declare-function ai-code-call-gptel-sync "ai-code-prompt-mode" (question))
(declare-function ai-code-plain-read-string "ai-code-input" (prompt &optional initial-input candidate-list))
(declare-function ai-code-helm-read-string-with-history "ai-code-input" (prompt history-file-name &optional initial-input candidate-list))

(defgroup ai-code-behaviors nil
  "Behavior injection system for AI prompts."
  :group 'ai-code)

(defcustom ai-code-behaviors-enabled t
  "When non-nil, enable behavior injection based on prompt classification."
  :type 'boolean
  :group 'ai-code-behaviors)

(defcustom ai-code-behaviors-auto-classify t
  "When non-nil, automatically classify prompts to suggest behaviors.
When nil, only explicit #hashtags in prompts are processed."
  :type 'boolean
  :group 'ai-code-behaviors)

(defcustom ai-code-behaviors-repo-path "~/.config/ai-behaviors"
  "Path to cloned ai-behaviors repository.
The repository should be cloned from https://github.com/xificurC/ai-behaviors"
  :type 'directory
  :group 'ai-code-behaviors)

(defcustom ai-code-behaviors-auto-clone nil
  "When non-nil, automatically clone ai-behaviors repo if not found.
The clone happens on first behavior-related operation.
Default is nil to avoid unexpected network access."
  :type 'boolean
  :group 'ai-code-behaviors)

(defcustom ai-code-behaviors-auto-enable nil
  "When non-nil, automatically enable preset application on load.
If nil, call `ai-code-behaviors-enable-auto-presets' to activate.
Default is nil - users must explicitly opt in."
  :type 'boolean
  :group 'ai-code-behaviors)

(defcustom ai-code-behaviors-repo-url "https://github.com/xificurC/ai-behaviors.git"
  "URL for cloning the ai-behaviors repository."
  :type 'string
  :group 'ai-code-behaviors)

(defcustom ai-code-behaviors-detection-patterns nil
  "Custom file patterns for preset detection.
Each entry is (PATTERN . PRESET-NAME) where PATTERN is a regex.
Example: ((\"_spec\\.clj$\" . \"tdd-dev\"))"
  :type '(alist :key-type string :value-type string)
  :group 'ai-code-behaviors)

(defcustom ai-code-behaviors-override-preset nil
  "When non-nil, override all detection with this preset.
Set to a preset name string to force that preset."
  :type '(choice (const nil) string)
  :group 'ai-code-behaviors)

(defcustom ai-code-behaviors-default-preset "quick-fix"
  "Default preset when no signals match.
Set to nil to return nil instead of a default preset."
  :type '(choice (const nil) string)
  :group 'ai-code-behaviors)

(defcustom ai-code-behaviors-detection-enabled-signals
  '(:filename :major-mode :project :git)
  "Which signals to use for preset detection.
:filename - Detect from file name patterns
:major-mode - Detect from major mode
:project - Detect from project structure
:git - Detect from git branch name"
  :type '(set (const :filename) (const :major-mode)
              (const :project) (const :git))
  :group 'ai-code-behaviors)

(defcustom ai-code-behaviors-detection-cache-ttl 300
  "Time-to-live for detection cache in seconds.
Applies to git and project detection results."
  :type 'integer
  :group 'ai-code-behaviors)

(defcustom ai-code-behaviors-reclassify-min-confidence 'medium
  "Minimum confidence level required for auto-classify to override session state.
In gptel-agent context, classifications below this threshold use session state.
One of high, medium, or low."
  :type '(choice (const high) (const medium) (const low))
  :group 'ai-code-behaviors)

(defcustom ai-code-behaviors-gptel-agent-auto-classify t
  "When non-nil, auto-classify prompts in gptel-agent buffers.
When nil, gptel-agent prompts without explicit hashtags use existing
session state instead of auto-classifying.
Default is t to enable automatic behavior detection in agent workflows."
  :type 'boolean
  :group 'ai-code-behaviors)

(defvar ai-code--behaviors-cache (make-hash-table :test #'equal)
  "Cache for loaded behavior prompts.
Each entry is (CONTENT . TIMESTAMP).")

(defcustom ai-code-behaviors-cache-ttl 3600
  "Time-to-live in seconds for behavior prompt cache.
Default is 1 hour. Set to 0 to disable TTL checking."
  :type 'integer
  :group 'ai-code-behaviors)

(defcustom ai-code-behaviors-max-response-size 10240
  "Maximum GPTel response size in bytes for JSON parsing.
Responses larger than this are rejected to prevent DoS.
Default is 10KB. Set to 0 to disable limit."
  :type 'integer
  :group 'ai-code-behaviors)

(defcustom ai-code-behaviors-max-brace-depth 50
  "Maximum nesting depth for JSON brace counting.
Prevents CPU exhaustion on deeply nested malformed responses."
  :type 'integer
  :group 'ai-code-behaviors)

(defun ai-code--git-command-output (&rest args)
  "Run git with ARGS and return trimmed output, or nil on error.
Uses call-process instead of shell to avoid injection vulnerabilities."
  (with-temp-buffer
    (when (zerop (apply #'call-process "git" nil t nil args))
      (string-trim (buffer-string)))))

(defvar ai-code--behaviors-session-states (make-hash-table :test #'equal)
  "Hash table of behaviors per git repository.
Key: git root directory (string)
Value: plist (:state BEHAVIOR-STATE :preset PRESET-NAME)")

(defvar ai-code--behaviors-update-checked nil
  "Non-nil if update check has been performed this session.")

(defvar ai-code--detection-cache (make-hash-table :test #'equal)
  "Unified cache for preset detection.
Key: (SOURCE . ROOT) where SOURCE is :project or :git.
Value: (:result RESULT :timestamp TIME).")

(defvar ai-code--behavior-annotation-cache (make-hash-table :test #'equal)
  "Cache for behavior annotation strings.")

(defvar ai-code--behaviors-pending-presets (make-hash-table :test #'equal)
  "Hash table of pending presets per project root.
Key: project root, Value: preset name string or nil.
Pending presets are shown in mode-line but not committed until first prompt.")

(defvar ai-code--behaviors-last-prompts (make-hash-table :test #'equal)
  "Hash table of last processed prompts per project root.
Key: project root, Value: plist (:original ORIG :processed PROC :behaviors BEH).")

(declare-function ai-code--git-root "ai-code-file" (&optional dir))
(declare-function ai-code--behaviors-extract-project-from-buffer-name
                  "ai-code-behaviors" ())

(defun ai-code--behaviors-project-root (&optional buffer)
  "Return git root for BUFFER, or current buffer if nil.
For gptel-agent buffers, falls back to extracting project from buffer name.
Returns nil if not in a project (prevents state leakage between projects)."
  (if (bufferp buffer)
      (with-current-buffer buffer
        (or (and (fboundp 'ai-code--git-root) (ai-code--git-root))
            (and (fboundp 'ai-code--behaviors-extract-project-from-buffer-name)
                 (ai-code--behaviors-extract-project-from-buffer-name))))
    (or (and (fboundp 'ai-code--git-root) (ai-code--git-root))
        (and (fboundp 'ai-code--behaviors-extract-project-from-buffer-name)
             (ai-code--behaviors-extract-project-from-buffer-name)))))

(defun ai-code--behaviors--get (key &optional root)
  "Get entry KEY from session states for ROOT.
If ROOT is nil, use current project root.
Returns nil if not in a project."
  (let ((r (or root (ai-code--behaviors-project-root))))
    (when r
      (plist-get (or (gethash r ai-code--behaviors-session-states)
                     '(:state nil :preset nil))
                 key))))

(defun ai-code--behaviors--set (key value &optional root)
  "Set entry KEY to VALUE in session states for ROOT.
If ROOT is nil, use current project root.
Does nothing and returns nil if not in a project (prevents state leakage)."
  (let ((r (or root (ai-code--behaviors-project-root))))
    (when r
      (let ((entry (or (gethash r ai-code--behaviors-session-states)
                       '(:state nil :preset nil))))
        (puthash r (plist-put (copy-tree entry) key value)
                 ai-code--behaviors-session-states)
        value))))

(defun ai-code--behaviors-get-state (&optional root)
  "Get behavior state for project ROOT, or current project if nil."
  (ai-code--behaviors--get :state root))

(defun ai-code--behaviors-set-state (state &optional root)
  "Set behavior STATE for project ROOT, or current project if nil."
  (ai-code--behaviors--set :state state root))

(defun ai-code--behaviors-get-preset (&optional root)
  "Get preset name for project ROOT, or current project if nil."
  (ai-code--behaviors--get :preset root))

(defun ai-code--behaviors-set-preset (preset &optional root)
  "Set preset name to PRESET for project ROOT, or current project if nil."
  (ai-code--behaviors--set :preset preset root))

(defun ai-code--behaviors-clear-state (&optional root)
  "Clear behavior state for project ROOT, or current project if nil."
  (remhash (or root (ai-code--behaviors-project-root)) ai-code--behaviors-session-states))

(defun ai-code--behaviors-set-pending-preset (preset &optional root)
  "Set pending PRESET for project ROOT."
  (puthash (or root (ai-code--behaviors-project-root)) preset
           ai-code--behaviors-pending-presets))

(defun ai-code--behaviors-get-pending-preset (&optional root)
  "Get pending preset for project ROOT."
  (gethash (or root (ai-code--behaviors-project-root))
           ai-code--behaviors-pending-presets))

(defun ai-code--behaviors-clear-pending-preset (&optional root)
  "Clear pending preset for project ROOT."
  (remhash (or root (ai-code--behaviors-project-root))
           ai-code--behaviors-pending-presets))

(defun ai-code--behaviors-get-active-bundle (&optional root)
  "Get active constraint bundle for project ROOT, or current project if nil."
  (gethash (or root (ai-code--behaviors-project-root))
           ai-code--active-constraint-bundles))

(defun ai-code--behaviors-set-active-bundle (bundle &optional root)
  "Set active constraint BUNDLE for project ROOT, or current project if nil."
  (puthash (or root (ai-code--behaviors-project-root)) bundle
           ai-code--active-constraint-bundles))

(defun ai-code--behaviors-clear-active-bundle (&optional root)
  "Clear active constraint bundle for project ROOT."
  (remhash (or root (ai-code--behaviors-project-root))
           ai-code--active-constraint-bundles))

(defconst ai-code--behavior-operating-modes
  '("=frame" "=research" "=design" "=spec" "=code" "=debug"
    "=review" "=test" "=mentor" "=drive" "=navigate" "=probe" "=record")
  "Operating mode behaviors. Only one can be active at a time.")

(defconst ai-code--behavior-modifiers
  '("deep" "wide" "ground" "negative-space" "challenge" "steel-man"
    "user-lens" "concise" "first-principles" "creative" "subtract"
    "meta" "simulate" "decompose" "recursive" "fractal" "tdd"
    "io" "contract" "backward" "analogy" "temporal" "name" "checklist" "file")
  "Modifier behaviors. Multiple can be active simultaneously.")

(defconst ai-code--behavior-readonly-modes
  '("=frame" "=research" "=design" "=spec" "=review" "=test"
    "=mentor" "=navigate" "=probe")
  "Operating modes compatible with gptel-plan (read-only phase).
These modes analyze, plan, or guide without modifying files.")

(defconst ai-code--behavior-modify-modes
  '("=code" "=debug" "=drive" "=record")
  "Operating modes that modify files - require gptel-agent.")

(defconst ai-code--constraint-modifiers
  '(;; Language
    ("chinese" . "∀ response: 简体中文. code ∪ comments = English.    -- HARD CONSTRAINT")
    ("english" . "∀ response: English.    -- HARD CONSTRAINT")
    ("japanese" . "∀ response: 日本語. code ∪ comments = English.    -- HARD CONSTRAINT")
    ("korean" . "∀ response: 한국어. code ∪ comments = English.    -- HARD CONSTRAINT")

    ;; Testing
    ("test-after" . "code → test → verify. No untested code ships.    -- HARD CONSTRAINT")
    ("test-unit" . "Unit tests for every function. Isolated, fast, deterministic.    -- HARD CONSTRAINT")
    ("test-integration" . "Integration tests for component boundaries.    -- HARD CONSTRAINT")
    ("test-e2e" . "End-to-end tests for critical user flows.    -- HARD CONSTRAINT")
    ("test-coverage" . "Coverage ≥ 80%. Track and report gaps.    -- HARD CONSTRAINT")

    ;; Code Style
    ("strict-lint" . "lint ∩ errors = ∅. Fix before commit.    -- HARD CONSTRAINT")
    ("strict-types" . "∀ params/returns: explicit types. 'any' ⊆ forbidden.    -- HARD CONSTRAINT")
    ("no-comments" . "code = documentation. Comments only when code cannot speak.    -- HARD CONSTRAINT")
    ("doc-comments" . "∀ public: docstring. Parameters, returns, throws, examples.    -- HARD CONSTRAINT")
    ("no-todos" . "No TODO/FIXME in committed code. Resolve or create issue.    -- HARD CONSTRAINT")

    ;; Paradigm
    ("functional" . "state ∩ mutation = ∅. Pure functions. Immutable data.    -- HARD CONSTRAINT")
    ("immutable" . "∀ data: const/final. No in-place mutation.    -- HARD CONSTRAINT")
    ("oop" . "Encapsulate state. Message passing. Single responsibility.    -- HARD CONSTRAINT")
    ("procedural" . "Step-by-step functions. Explicit state. Clear flow.    -- HARD CONSTRAINT")

    ;; Safety
    ("defensive" . "∀ public input: validate. Fail fast, fail explicitly.    -- HARD CONSTRAINT")
    ("secure" . "∀ input: untrusted. OWASP Top 10 ⊆ review.    -- HARD CONSTRAINT")
    ("no-unsafe" . "unsafe/raw pointers ⊆ forbidden. Bounds checked.    -- HARD CONSTRAINT")
    ("memory-safe" . "No memory leaks. Ownership clear. Resources freed.    -- HARD CONSTRAINT")

    ;; Error Handling
    ("errors-raise" . "Error → throw/raise. Let caller handle.    -- HARD CONSTRAINT")
    ("errors-result" . "Error → Result/Either type. Explicit handling.    -- HARD CONSTRAINT")
    ("errors-checked" . "∀ errors: declared and handled. No silent failures.    -- HARD CONSTRAINT")
    ("errors-typed" . "Typed exceptions. Specific error types per domain.    -- HARD CONSTRAINT")

    ;; Performance
    ("performant" . "O(n) preferred. Allocations minimized. Hot paths identified.    -- HARD CONSTRAINT")
    ("minimal" . "Least code. Built-ins preferred. No over-engineering.    -- HARD CONSTRAINT")
    ("lazy" . "Compute on demand. Defer until needed. Cache results.    -- HARD CONSTRAINT")
    ("batch" . "Batch operations. Minimize round-trips. Chunk large datasets.    -- HARD CONSTRAINT")

    ;; Async
    ("async-await" . "async/await preferred. No callback hell.    -- HARD CONSTRAINT")
    ("sync-only" . "No async. Blocking calls acceptable.    -- HARD CONSTRAINT")
    ("reactive" . "Streams and observables. Push-based data flow.    -- HARD CONSTRAINT")

    ;; API Design
    ("api-rest" . "REST conventions. Resources, verbs, status codes.    -- HARD CONSTRAINT")
    ("api-graphql" . "GraphQL conventions. Schema-first.    -- HARD CONSTRAINT")
    ("api-rpc" . "RPC style. Procedure calls. Named operations.    -- HARD CONSTRAINT")
    ("api-versioned" . "Version all endpoints. Backwards compatible.    -- HARD CONSTRAINT")

    ;; Logging
    ("logging-verbose" . "Log entry/exit, params, timing. Debug-friendly.    -- HARD CONSTRAINT")
    ("logging-minimal" . "Errors only. Production-ready.    -- HARD CONSTRAINT")
    ("no-logging" . "No log statements. Pure functions.    -- HARD CONSTRAINT")
    ("structured-logging" . "JSON logs. Correlation IDs. Searchable.    -- HARD CONSTRAINT")

    ;; State
    ("stateless" . "No internal state. Pure functions. Idempotent.    -- HARD CONSTRAINT")
    ("state-explicit" . "State changes logged. Transitions named.    -- HARD CONSTRAINT")

    ;; Naming
    ("naming-verbose" . "Descriptive names. No abbreviations. Self-documenting.    -- HARD CONSTRAINT")
    ("naming-short" . "Concise names. Common abbreviations OK.    -- HARD CONSTRAINT")

    ;; Dependencies
    ("no-deps" . "No new dependencies. Use built-ins.    -- HARD CONSTRAINT")
    ("minimal-deps" . "Minimize dependencies. Audit each addition.    -- HARD CONSTRAINT"))
  "Built-in constraint modifiers with their template instructions.
These are lighter-weight than repo behaviors and cover common constraints.
Format: terse formal notation with -- HARD CONSTRAINT marker for LLM parsing.")

(defconst ai-code--constraint-bundles
  '(("react-stack" . (:constraints ("strict-types" "functional" "async-await" "test-unit")
                      :description "React + TypeScript stack"))
    ("spring-stack" . (:constraints ("defensive" "doc-comments" "errors-raise" "test-integration")
                       :description "Spring Boot stack"))
    ("clojure-stack" . (:constraints ("functional" "immutable" "errors-result" "test-unit")
                         :description "Clojure/Scheme functional stack"))
    ("rust-stack" . (:constraints ("strict-types" "immutable" "errors-result" "no-unsafe" "memory-safe")
                      :description "Rust safety-first stack"))
    ("python-stack" . (:constraints ("strict-types" "test-after" "doc-comments" "secure")
                        :description "Python production stack"))
    ("node-stack" . (:constraints ("strict-types" "async-await" "test-unit" "minimal")
                      :description "Node.js/TypeScript stack"))
    ("go-stack" . (:constraints ("errors-checked" "minimal" "test-unit" "performant")
                     :description "Go production stack"))
    ("elixir-stack" . (:constraints ("functional" "immutable" "async-await" "test-unit")
                        :description "Elixir/Phoenix stack"))
    ("kotlin-stack" . (:constraints ("strict-types" "defensive" "doc-comments" "test-integration")
                        :description "Kotlin/JVM stack"))
    ("swift-stack" . (:constraints ("strict-types" "memory-safe" "async-await" "test-unit")
                       :description "Swift/iOS stack"))
    ("dotnet-stack" . (:constraints ("strict-types" "defensive" "async-await" "test-unit")
                        :description ".NET/C# stack"))
    ("rails-stack" . (:constraints ("strict-types" "test-after" "secure" "api-rest")
                       :description "Ruby on Rails stack"))
    ("django-stack" . (:constraints ("strict-types" "secure" "test-after" "api-rest")
                        :description "Django stack"))
    ("fastapi-stack" . (:constraints ("strict-types" "async-await" "api-rest" "test-unit")
                         :description "FastAPI stack"))
    ("graphql-stack" . (:constraints ("strict-types" "api-graphql" "test-integration" "secure")
                         :description "GraphQL API stack"))
    ("microservices-stack" . (:constraints ("api-rest" "async-await" "stateless" "secure" "structured-logging")
                               :description "Microservices architecture"))
    ("serverless-stack" . (:constraints ("stateless" "minimal" "async-await" "test-unit")
                            :description "Serverless/Lambda stack"))
    ("embedded-stack" . (:constraints ("minimal" "no-deps" "memory-safe" "performant")
                          :description "Embedded systems stack"))
    ("data-pipeline-stack" . (:constraints ("functional" "lazy" "batch" "test-unit")
                               :description "Data processing pipeline stack"))
    ("cli-tool-stack" . (:constraints ("minimal" "errors-checked" "stateless" "doc-comments")
                          :description "CLI tool stack")))
  "Predefined constraint bundles for common tech stacks.
Each bundle is (NAME . (:constraints (C1 C2 ...) :description DESC)).")

(defconst ai-code--project-config-constraint-map
  '(;; TypeScript/JavaScript
    ("tsconfig.json" . (:patterns (("strict.*true" . "strict-types")
                                   ("noImplicitAny.*true" . "strict-types"))
                       :constraints ("strict-types")))
    (".eslintrc" . (:constraints ("strict-lint")))
    (".eslintrc.js" . (:constraints ("strict-lint")))
    (".eslintrc.json" . (:constraints ("strict-lint")))
    (".eslintrc.yml" . (:constraints ("strict-lint")))
    (".eslintrc.yaml" . (:constraints ("strict-lint")))
    ("eslint.config.js" . (:constraints ("strict-lint")))
    (".prettierrc" . (:constraints ("strict-lint")))
    (".prettierrc.json" . (:constraints ("strict-lint")))

    ;; Python
    ("pyproject.toml" . (:patterns (("\\[tool.mypy\\]" . "strict-types")
                                    ("\\[tool.pytest\\]" . "test-after")
                                    ("pytest" . "test-unit")
                                    ("strict = true" . "strict-types"))))
    ("setup.cfg" . (:patterns (("mypy" . "strict-types")
                               ("pytest" . "test-after"))))
    ("mypy.ini" . (:constraints ("strict-types")))
    ("pytest.ini" . (:constraints ("test-after" "test-unit")))
    ("tox.ini" . (:constraints ("test-after")))
    ("ruff.toml" . (:constraints ("strict-lint")))
    (".ruff.toml" . (:constraints ("strict-lint")))

    ;; Rust
    ("Cargo.toml" . (:constraints ("strict-types")
                      :patterns (("\\[dev-dependencies\\]" . "test-unit"))))

    ;; Go
    ("go.mod" . (:constraints ("errors-checked" "minimal")))

    ;; Java/Kotlin
    ("pom.xml" . (:constraints ("doc-comments" "defensive")))
    ("build.gradle" . (:constraints ("doc-comments" "defensive")))
    ("build.gradle.kts" . (:constraints ("doc-comments" "defensive")))

    ;; Clojure
    ("project.clj" . (:constraints ("functional" "immutable")))
    ("deps.edn" . (:constraints ("functional" "immutable")))
    ("shadow-cljs.edn" . (:constraints ("functional" "immutable")))

    ;; Ruby
    ("Gemfile" . (:constraints ("test-after")))
    (".rubocop.yml" . (:constraints ("strict-lint")))

    ;; Elixir
    ("mix.exs" . (:constraints ("functional" "immutable" "test-unit")))

    ;; Swift
    ("Package.swift" . (:constraints ("memory-safe" "async-await")))
    (".swiftlint.yml" . (:constraints ("strict-lint")))

    ;; .NET
    ("*.csproj" . (:constraints ("strict-types" "async-await")))
    ("Directory.Build.props" . (:constraints ("strict-types")))

    ;; CI/CD - implies security focus
    (".github/workflows" . (:constraints ("secure")))
    (".gitlab-ci.yml" . (:constraints ("secure")))
    ("azure-pipelines.yml" . (:constraints ("secure")))
    ("Jenkinsfile" . (:constraints ("secure")))

    ;; Testing frameworks
    ("jest.config.js" . (:constraints ("test-unit")))
    ("jest.config.ts" . (:constraints ("test-unit")))
    ("vitest.config.ts" . (:constraints ("test-unit")))
    ("karma.conf.js" . (:constraints ("test-unit")))
    ("mocha.opts" . (:constraints ("test-unit")))
    (".mocharc.json" . (:constraints ("test-unit")))

    ;; API definitions
    ("openapi.yaml" . (:constraints ("api-rest")))
    ("openapi.json" . (:constraints ("api-rest")))
    ("swagger.yaml" . (:constraints ("api-rest")))
    ("schema.graphql" . (:constraints ("api-graphql")))
    ("schema.gql" . (:constraints ("api-graphql")))

    ;; Docker/Container
    ("Dockerfile" . (:constraints ("minimal" "secure")))
    ("docker-compose.yml" . (:constraints ("secure")))
    ("docker-compose.yaml" . (:constraints ("secure")))

    ;; Kubernetes
    ("k8s" . (:constraints ("secure" "stateless")))
    ("kubernetes" . (:constraints ("secure" "stateless")))
    ("helm" . (:constraints ("secure")))
    ("Chart.yaml" . (:constraints ("secure")))

    ;; Config management
    ("terraform" . (:constraints ("immutable" "state-explicit")))
    ("ansible" . (:constraints ("defensive" "state-explicit")))
    ("puppet" . (:constraints ("defensive"))))
  "Map project files/patterns to auto-detected constraints.
Each entry is (FILENAME . (:constraints (C1 C2 ...) :patterns ((REGEX . CONSTRAINT) ...))).
Patterns are matched against file content for conditional constraint activation.")

(defcustom ai-code-constraints-auto-detect-enabled t
  "When non-nil, automatically detect constraints from project configuration files."
  :type 'boolean
  :group 'ai-code-behaviors)

(defcustom ai-code-constraints-persistence-file ".ai-behaviors/constraints"
  "Relative path for project-level constraint persistence.
Stored in the project root directory."
  :type 'string
  :group 'ai-code-behaviors)

(defvar ai-code--constraints-cache (make-hash-table :test #'equal)
  "Cache for auto-detected constraints per project root.
Key: project root, Value: (:constraints (C1 C2 ...) :timestamp TIME).")

(defvar ai-code--active-constraint-bundles (make-hash-table :test #'equal)
  "Hash table of active constraint bundles per project.
Key: project root, Value: bundle name string or nil.")

(defconst ai-code-behaviors--synced-commit "8633aa9"
  "The upstream ai-behaviors commit this source code is synced with.
Update this when syncing with upstream behavior changes.")

(defun ai-code--behaviors-mode-readonly-p (mode)
  "Return non-nil if MODE is compatible with gptel-plan (read-only)."
  (member mode ai-code--behavior-readonly-modes))

(defun ai-code--behaviors-preset-readonly-p (preset-name)
  "Return non-nil if PRESET-NAME is compatible with gptel-plan (read-only).
Checks the preset's operating mode against readonly modes."
  (when-let ((data (assoc preset-name ai-code--behavior-presets)))
    (let ((mode (plist-get (cdr data) :mode)))
      (or (null mode)
          (ai-code--behaviors-mode-readonly-p mode)))))

(defun ai-code--behaviors-get-repo-behavior-names ()
  "Get list of behavior names from upstream repository.
Returns (MODES . MODIFIERS) where MODES are operating modes and MODIFIERS are modifiers."
  (when (ai-code--behaviors-repo-available-p)
    (let* ((behaviors-dir (expand-file-name "behaviors" ai-code-behaviors-repo-path))
           (entries (directory-files behaviors-dir nil "^[^.]"))
           (modes nil)
           (modifiers nil))
      (dolist (entry entries)
        (let ((prompt-file (expand-file-name (format "%s/prompt.md" entry) behaviors-dir)))
          (when (file-exists-p prompt-file)
            (if (string-match-p "^=" entry)
                (push entry modes)
              (push entry modifiers)))))
      (cons (sort modes #'string<) (sort modifiers #'string<)))))

(defun ai-code--behaviors-check-sync ()
  "Check if source code is synced with upstream repository.
Returns t if synced, nil if mismatch, 'no-repo if repo not available."
  (let ((repo-commit (ai-code--behaviors-get-current-commit)))
    (cond
     ((not repo-commit) 'no-repo)
     ((string= repo-commit ai-code-behaviors--synced-commit) t)
     (t
      (let ((repo-behaviors (ai-code--behaviors-get-repo-behavior-names)))
        (if (not repo-behaviors)
            'no-repo
          (let ((repo-modes (car repo-behaviors))
                (repo-modifiers (cdr repo-behaviors))
                (source-modes (sort (copy-sequence ai-code--behavior-operating-modes) #'string<))
                (source-modifiers (sort (copy-sequence ai-code--behavior-modifiers) #'string<)))
            (and (equal repo-modes source-modes)
                 (equal repo-modifiers source-modifiers)))))))))

(defun ai-code--behaviors-get-current-commit ()
  "Get current commit hash of ai-behaviors repository.
Returns short commit hash or nil if repo not available."
  (when (ai-code--behaviors-repo-available-p)
    (let ((default-directory (expand-file-name ai-code-behaviors-repo-path)))
      (ai-code--git-command-output "rev-parse" "--short" "HEAD"))))

(defconst ai-code--behavior-presets
  '(("frame-problem" . (:mode "=frame" :modifiers ("subtract" "challenge")
                       :description "Problem framing with critical analysis"))
     ("design-options" . (:mode "=design" :modifiers ("deep" "wide")
                        :description "Solution design exploration"))
     ("tdd-dev" . (:mode "=code" :modifiers ("tdd" "deep")
                    :description "Test-driven development"))
     ("thorough-debug" . (:mode "=debug" :modifiers ("deep" "challenge")
                          :description "Deep debugging with critical analysis"))
     ("quick-review" . (:mode "=review" :modifiers ("concise")
                        :description "Fast code review"))
     ("deep-review" . (:mode "=review" :modifiers ("deep" "challenge")
                       :description "Thorough code review"))
     ("research-deep" . (:mode "=research" :modifiers ("deep" "wide")
                         :description "Comprehensive research"))
     ("mentor-learn" . (:mode "=mentor" :modifiers ("first-principles")
                        :description "Learning/explanation mode"))
     ("spec-planning" . (:mode "=spec" :modifiers ("decompose" "wide")
                         :description "Architecture/planning mode"))
     ("quick-fix" . (:mode "=code" :modifiers ("concise")
                     :description "Simple code changes")))
   "Preset behavior combinations.
Each preset is (NAME . (:mode MODE :modifiers (MOD1 MOD2) :description DESC)).")

;;; Context detection constants

(defconst ai-code--major-mode-preset-map
  '((org-mode . "mentor-learn")
    (markdown-mode . "mentor-learn")
    (gfm-mode . "mentor-learn")
    (rst-mode . "mentor-learn")
    (yaml-mode . "quick-review")
    (yaml-ts-mode . "quick-review")
    (json-mode . "quick-review")
    (json-ts-mode . "quick-review")
    (toml-mode . "quick-review")
    (dockerfile-mode . "quick-review")
    (sh-mode . "quick-fix")
    (bash-ts-mode . "quick-fix")
    (makefile-mode . "quick-fix")
    (protobuf-mode . "spec-planning")
    (graphql-mode . "spec-planning"))
  "Map major modes to presets.")

(defconst ai-code--file-pattern-preset-map
  '(("_test\\.py$" . (:preset "tdd-dev" :confidence :high))
    ("_spec\\.rb$" . (:preset "tdd-dev" :confidence :high))
    ("\\.test\\.js$" . (:preset "tdd-dev" :confidence :high))
    ("\\.test\\.ts$" . (:preset "tdd-dev" :confidence :high))
    ("\\.spec\\.ts$" . (:preset "tdd-dev" :confidence :high))
    ("_test\\.go$" . (:preset "tdd-dev" :confidence :high))
    ("Tests\\.swift$" . (:preset "tdd-dev" :confidence :high))
    ("_test\\.rs$" . (:preset "tdd-dev" :confidence :high))
    ("Test\\.java$" . (:preset "tdd-dev" :confidence :high))
    ("_test\\.clj$" . (:preset "tdd-dev" :confidence :high))
    ("README" . (:preset "mentor-learn" :confidence :high))
    ("CHANGELOG" . (:preset "mentor-learn" :confidence :medium))
    ("CONTRIBUTING" . (:preset "mentor-learn" :confidence :medium))
    ("\\.md$" . (:preset "mentor-learn" :confidence :medium))
    ("\\.org$" . (:preset "mentor-learn" :confidence :medium))
    ("\\.rst$" . (:preset "mentor-learn" :confidence :medium))
    ("docs/" . (:preset "mentor-learn" :confidence :medium))
    ("\\.ya?ml$" . (:preset "quick-review" :confidence :low))
    ("\\.json$" . (:preset "quick-review" :confidence :low))
    ("\\.toml$" . (:preset "quick-review" :confidence :low))
    ("Dockerfile" . (:preset "quick-review" :confidence :medium))
    ("Makefile" . (:preset "quick-fix" :confidence :low))
    ("\\.sh$" . (:preset "quick-fix" :confidence :low))
    ("\\.log$" . (:preset "thorough-debug" :confidence :medium))
    ("\\.proto$" . (:preset "spec-planning" :confidence :medium))
    ("\\.graphql$" . (:preset "spec-planning" :confidence :medium)))
  "Map file patterns to preset with confidence level.")

(defconst ai-code--project-structure-signals
  '(("package.json" . (("jest.config.js" . "tdd-dev")
                       ("vitest.config.js" . "tdd-dev")
                       ("mocha.opts" . "tdd-dev")))
    ("Cargo.toml" . (("tests/" . "tdd-dev")))
    ("pyproject.toml" . (("pytest.ini" . "tdd-dev")
                         ("tox.ini" . "tdd-dev")))
    ("Gemfile" . (("spec/" . "tdd-dev"))))
  "Project files that signal test framework usage.
Note: Go projects are detected via filename patterns (_test.go), not project structure.")

(defconst ai-code--git-branch-patterns
  '(("^feature/" . "spec-planning")
    ("^feat/" . "spec-planning")
    ("^bugfix/" . "thorough-debug")
    ("^fix/" . "thorough-debug")
    ("^hotfix/" . "thorough-debug")
    ("^debug/" . "thorough-debug")
    ("^investigate/" . "thorough-debug")
    ("^test/" . "tdd-dev")
    ("^testing/" . "tdd-dev")
    ("^docs/" . "mentor-learn")
    ("^documentation/" . "mentor-learn")
    ("^refactor/" . "deep-review")
    ("^cleanup/" . "quick-review"))
  "Map git branch patterns to presets.")

;;; Mode-line faces for different operating modes

(defface ai-code-behaviors-mode-line-code
  '((t (:foreground "#228B22" :weight bold)))
  "Face for code mode in mode-line."
  :group 'ai-code-behaviors)

(defface ai-code-behaviors-mode-line-debug
  '((t (:foreground "#CD5C5C" :weight bold)))
  "Face for debug mode in mode-line."
  :group 'ai-code-behaviors)

(defface ai-code-behaviors-mode-line-review
  '((t (:foreground "#4682B4" :weight bold)))
  "Face for review mode in mode-line."
  :group 'ai-code-behaviors)

(defface ai-code-behaviors-mode-line-mentor
  '((t (:foreground "#DAA520" :weight bold)))
  "Face for mentor mode in mode-line."
  :group 'ai-code-behaviors)

(defface ai-code-behaviors-mode-line-research
  '((t (:foreground "#9370DB" :weight bold)))
  "Face for research mode in mode-line."
  :group 'ai-code-behaviors)

(defface ai-code-behaviors-mode-line-spec
  '((t (:foreground "#20B2AA" :weight bold)))
  "Face for spec mode in mode-line."
  :group 'ai-code-behaviors)

(defface ai-code-behaviors-mode-line-default
  '((t (:foreground "#808080" :weight bold)))
  "Face for unknown mode in mode-line."
  :group 'ai-code-behaviors)

(defconst ai-code--intent-classification-keywords
  '((=code . ("implement" "refactor" "fix" "add" "update" "change"
              "edit" "modify" "create" "write" "build" "remove"))
    (=debug . ("error" "bug" "exception" "failing" "broken" "crash"
               "debug" "not working" "doesn't work" "fix this"))
    (=research . ("what" "how does" "explain" "understand" "investigate"
                  "explore" "research" "find out" "tell me about"))
    (=review . ("review" "check" "audit" "analyze" "inspect" "look at"
                "feedback" "opinion" "thoughts on"))
    (=spec . ("plan" "design" "propose" "architecture" "spec" "specify"
              "outline" "structure" "approach for"))
    (=test . ("test" "verify" "assert" "coverage" "unit test" "testing"))
    (=mentor . ("teach" "learn" "explain in detail" "how do I"
                "guide me" "show me how" "walk me through"))
    (=assess . ("evaluate" "compare" "pros and cons" "better" "vs"
                "which is" "should I use"))
    (=record . ("document" "write docs" "readme" "record" "documentation"
                "write up")))
  "Keywords for intent classification when GPTel is unavailable.")

(defconst ai-code--modifier-trigger-keywords
  '((deep . ("thoroughly" "in detail" "comprehensive" "deeply"
             "carefully" "exhaustive"))
    (tdd . ("test-driven" "tdd" "write tests first" "red green"))
    (challenge . ("critically" "find flaws" "what's wrong"))
    (concise . ("briefly" "short" "summary" "tldr" "quickly")))
  "Keywords that trigger automatic modifier suggestions.")

(defun ai-code--behaviors-repo-available-p ()
  "Return non-nil if ai-behaviors repository exists."
  (let ((path (expand-file-name ai-code-behaviors-repo-path)))
    (and (file-directory-p path)
         (file-directory-p (expand-file-name "behaviors" path)))))

(defun ai-code--ensure-behaviors-repo ()
  "Ensure ai-behaviors repository is available.
Clone it if missing and `ai-code-behaviors-auto-clone' is non-nil.
Return non-nil if repo is available after this call."
  (when (and (not (ai-code--behaviors-repo-available-p))
             ai-code-behaviors-auto-clone)
    (let* ((repo-path (directory-file-name (expand-file-name ai-code-behaviors-repo-path)))
           (parent-dir (file-name-directory repo-path))
           (repo-name (file-name-nondirectory repo-path)))
      (unless (file-directory-p parent-dir)
        (make-directory parent-dir t))
      (message "Cloning ai-behaviors repository to %s..." repo-path)
      (let ((default-directory parent-dir)
            (result (call-process "git" nil nil nil
                                  "clone" ai-code-behaviors-repo-url repo-name)))
        (if (eq result 0)
            (message "Successfully cloned ai-behaviors repository")
          (message "Failed to clone ai-behaviors repository")))))
  (ai-code--behaviors-repo-available-p))

(defun ai-code--behaviors-check-for-updates ()
  "Check if ai-behaviors repo has updates available.
Fetches from remote first (with 5s timeout), then compares.
Return one of: `up-to-date', `updates-available', `no-remote', `no-repo', or `error'.
Note: This performs network I/O; use sparingly."
  (cond
   ((not (ai-code--behaviors-repo-available-p)) 'no-repo)
   (t
    (let ((default-directory (expand-file-name ai-code-behaviors-repo-path)))
      (condition-case nil
          (progn
            (call-process "git" nil nil nil "fetch" "--quiet")
            (let* ((remote-head (ai-code--git-command-output "rev-parse" "@{u}"))
                   (local-head (ai-code--git-command-output "rev-parse" "HEAD")))
              (cond
               ((or (null remote-head) (string-empty-p remote-head)) 'no-remote)
               ((string= local-head remote-head) 'up-to-date)
               (t 'updates-available))))
        (error 'error))))))

(defun ai-code--behaviors-maybe-check-updates ()
  "Check for updates once per session and message if available."
  (unless ai-code--behaviors-update-checked
    (setq ai-code--behaviors-update-checked t)
    (when (eq (ai-code--behaviors-check-for-updates) 'updates-available)
      (message "ai-behaviors has updates available. Run M-x ai-code-behaviors-install to update."))))

(defun ai-code--behaviors-commit-info ()
  "Return plist with current commit info for ai-behaviors repo.
Returns nil if repo not available."
  (when (ai-code--behaviors-repo-available-p)
    (let ((default-directory (expand-file-name ai-code-behaviors-repo-path)))
      (let ((commit (ai-code--git-command-output "rev-parse" "--short" "HEAD"))
            (date (ai-code--git-command-output "log" "-1" "--format=%ci" "HEAD")))
        (when (and commit date)
          (list :commit commit :date date))))))

(defun ai-code--behavior-file-path (behavior-name)
  "Return path to prompt.md for BEHAVIOR-NAME."
  (expand-file-name
   (format "behaviors/%s/prompt.md" behavior-name)
   (expand-file-name ai-code-behaviors-repo-path)))

(defun ai-code--behaviors-cache-get (key)
  "Get cached value for KEY with TTL check.
Returns content string or nil if expired/missing."
  (when-let ((entry (gethash key ai-code--behaviors-cache)))
    (let ((content (car entry))
          (timestamp (cdr entry)))
      (if (or (<= ai-code-behaviors-cache-ttl 0)
              (< (- (float-time) timestamp) ai-code-behaviors-cache-ttl))
          content
        (remhash key ai-code--behaviors-cache)
        nil))))

(defun ai-code--behaviors-cache-put (key value)
  "Cache VALUE for KEY with current timestamp."
  (puthash key (cons value (float-time)) ai-code--behaviors-cache)
  value)

(defvar ai-code--behaviors-cleanup-timer nil
  "Idle timer for periodic cache cleanup.")

(defun ai-code--behaviors-cleanup-expired-caches ()
  "Remove expired entries from all TTL-based caches.
Called periodically by idle timer to prevent memory growth."
  (let ((now (float-time)))
    (when (> ai-code-behaviors-cache-ttl 0)
      (maphash
       (lambda (k v)
         (when (> (- now (cdr v)) ai-code-behaviors-cache-ttl)
           (remhash k ai-code--behaviors-cache)))
       ai-code--behaviors-cache))
    (when (> ai-code-behaviors-detection-cache-ttl 0)
      (maphash
       (lambda (k v)
         (let ((timestamp (plist-get v :timestamp)))
           (when (and timestamp (> (- now timestamp) ai-code-behaviors-detection-cache-ttl))
             (remhash k ai-code--detection-cache))))
       ai-code--detection-cache))))

(defun ai-code--behaviors-start-cleanup-timer ()
  "Start the idle timer for cache cleanup.
Timer runs every 5 minutes when Emacs is idle."
  (unless ai-code--behaviors-cleanup-timer
    (setq ai-code--behaviors-cleanup-timer
          (run-with-idle-timer 300 t #'ai-code--behaviors-cleanup-expired-caches))))

(defun ai-code--behaviors-stop-cleanup-timer ()
  "Stop the idle timer for cache cleanup."
  (when ai-code--behaviors-cleanup-timer
    (cancel-timer ai-code--behaviors-cleanup-timer)
    (setq ai-code--behaviors-cleanup-timer nil)))

(defun ai-code--load-behavior-prompt (behavior-name)
  "Load and cache the prompt content for BEHAVIOR-NAME.
Return the prompt content string, or nil if not found."
  (let ((cached (ai-code--behaviors-cache-get behavior-name)))
    (if cached
        cached
      (when (ai-code--ensure-behaviors-repo)
        (ai-code--behaviors-maybe-check-updates)
        (let* ((file-path (ai-code--behavior-file-path behavior-name))
               (content (when (file-exists-p file-path)
                          (with-temp-buffer
                            (insert-file-contents file-path)
                            (buffer-string)))))
          (when content
            (ai-code--behaviors-cache-put behavior-name content)))))))

(defun ai-code--all-behavior-names ()
  "Return list of all available behavior names including presets, constraints, and bundles."
  (append (ai-code--behavior-preset-names)
          (mapcar (lambda (m) (concat "#" m)) ai-code--behavior-operating-modes)
          (mapcar (lambda (m) (concat "#" m)) ai-code--behavior-modifiers)
          (mapcar (lambda (c) (concat "#" (car c))) ai-code--constraint-modifiers)
          (ai-code--constraint-bundle-names)))

(defun ai-code--behavior-preset-names ()
  "Return list of all preset names with @ prefix for completion."
  (mapcar (lambda (p) (concat "@" (car p))) ai-code--behavior-presets))

(defun ai-code--constraint-bundle-names ()
  "Return list of constraint bundle names with @ prefix for completion."
  (mapcar (lambda (b) (concat "@" (car b))) ai-code--constraint-bundles))

(defun ai-code--behavior-preset-and-bundle-names ()
  "Return list of all preset and bundle names with @ prefix for completion."
  (append (ai-code--behavior-preset-names)
          (ai-code--constraint-bundle-names)))

(defun ai-code--behavior-preset-capf ()
  "Completion-at-point function for @preset and @bundle names.
Shows * annotation for modify presets in gptel modes."
  (when (and (boundp 'major-mode)
             (eq major-mode 'ai-code-prompt-mode)
             (save-excursion
               (skip-chars-backward "a-zA-Z0-9_-")
               (eq (char-before) ?@)))
    (let* ((start (1- (point)))
           (end (point))
           (gptel-mode-p (when (boundp 'gptel--preset)
                           (memq gptel--preset '(gptel-plan gptel-agent))))
           (candidates (ai-code--behavior-preset-and-bundle-names))
           (annotation-fn
            (lambda (cand)
              (let ((name (string-trim (substring cand 1))))
                (cond
                 ((and gptel-mode-p
                       (assoc name ai-code--behavior-presets)
                       (not (ai-code--behaviors-preset-readonly-p name)))
                  (let ((data (cdr (assoc name ai-code--behavior-presets))))
                    (format "* %s" (plist-get data :description))))
                 ((assoc name ai-code--constraint-bundles)
                  (let ((data (cdr (assoc name ai-code--constraint-bundles))))
                    (format " %s" (plist-get data :description))))
                 ((assoc name ai-code--behavior-presets)
                  (let ((data (cdr (assoc name ai-code--behavior-presets))))
                    (format " %s" (plist-get data :description))))
                 (t ""))))))
      (list start end candidates
            :annotation-function annotation-fn
            :exclusive 'no))))

(defun ai-code--behavior-setup-preset-completion ()
  "Add preset completion and mode-line to prompt mode buffers."
  (add-hook 'completion-at-point-functions #'ai-code--behavior-preset-capf nil t)
  (ai-code-behaviors-mode-line-enable))

(defun ai-code--behavior-teardown-preset-completion ()
  "Remove preset completion from prompt mode buffers."
  (remove-hook 'completion-at-point-functions #'ai-code--behavior-preset-capf t))

(defun ai-code--behavior-merge-preset-candidates (candidates)
  "Append preset and bundle names to CANDIDATES for @ completion.
This allows preset and bundle names to appear alongside file paths in the
auto-triggered completion from `ai-code--prompt-auto-trigger-filepath-completion'."
  (append candidates (ai-code--behavior-preset-and-bundle-names)))

(defun ai-code--behavior-enable-preset-in-file-completion ()
  "Enable preset names in @ file completion via advice."
  (advice-add 'ai-code--prompt-filepath-candidates :filter-return
              #'ai-code--behavior-merge-preset-candidates))

(defun ai-code--behavior-disable-preset-in-file-completion ()
  "Disable preset names in @ file completion."
  (advice-remove 'ai-code--prompt-filepath-candidates
                 #'ai-code--behavior-merge-preset-candidates))

(defun ai-code--behavior-minibuffer-setup-hook ()
  "Setup behavior completion in minibuffer."
  (local-set-key (kbd "TAB") #'ai-code--behavior-minibuffer-complete))

(defun ai-code--behavior-minibuffer-complete ()
  "Complete behavior hashtag at point in minibuffer."
  (interactive)
  (let* ((end (point))
         (hash-pos (save-excursion
                     (skip-chars-backward "A-Za-z0-9_=-")
                     (when (eq (char-before) ?#)
                       (1- (point))))))
    (if (and hash-pos (> end hash-pos))
        (let* ((prefix (buffer-substring-no-properties hash-pos end))
               (candidates (ai-code--all-behavior-names))
               (matches (seq-filter (lambda (c) (string-prefix-p prefix c)) candidates)))
          (if (= (length matches) 1)
              (progn
                (delete-region hash-pos end)
                (insert (car matches)))
            (when matches
              (let ((choice (completing-read "Behavior: " matches nil nil prefix)))
                (when (and choice (not (string-empty-p choice)))
                  (delete-region hash-pos end)
                  (insert choice))))))
      (minibuffer-complete))))

(defun ai-code--behavior-plain-read-string-advice (orig-fun prompt &optional initial-input candidate-list)
  "Advice for `ai-code-plain-read-string' to inject behavior candidates.
ORIG-FUN is the original function."
  (let* ((behavior-candidates (ai-code--all-behavior-names))
         (completion-candidates
          (delete-dups (append candidate-list
                               behavior-candidates
                               (when (boundp 'ai-code-read-string-history)
                                 ai-code-read-string-history)))))
    (add-hook 'minibuffer-setup-hook #'ai-code--behavior-minibuffer-setup-hook)
    (unwind-protect
        (funcall orig-fun prompt initial-input completion-candidates)
      (remove-hook 'minibuffer-setup-hook #'ai-code--behavior-minibuffer-setup-hook))))

(defun ai-code--behavior-helm-read-string-advice (orig-fun prompt history-file-name &optional initial-input candidate-list)
  "Advice for `ai-code-helm-read-string-with-history' to inject behavior candidates.
ORIG-FUN is the original function."
  (let* ((behavior-candidates (ai-code--all-behavior-names))
         (result (funcall orig-fun prompt history-file-name initial-input
                          (append (or candidate-list '()) behavior-candidates))))
    result))

(defun ai-code--behavior-prompt-auto-trigger-advice (orig-fun)
  "Advice for `ai-code--prompt-auto-trigger-filepath-completion' to handle # behavior.
ORIG-FUN is the original function. When # is typed at start of line or after
whitespace, offer behavior completion instead of symbol completion."
  (when (not (minibufferp))
    (pcase (char-before)
      (?#
       (let ((behavior-candidates (ai-code--all-behavior-names)))
         (if (and behavior-candidates
                  (save-excursion
                    (forward-char -1)
                    (or (bolp)
                        (memq (char-before) '(?\s ?\t ?\n)))))
             (let ((choice (completing-read "Behavior: " behavior-candidates nil nil)))
               (when (and choice (not (string-empty-p choice)))
                 (delete-char -1)
                 (insert choice)))
           (funcall orig-fun))))
      (_ (funcall orig-fun)))))

(defun ai-code--behavior-p (name)
  "Return non-nil if NAME is a valid behavior or constraint."
  (or (member name ai-code--behavior-operating-modes)
      (member name ai-code--behavior-modifiers)
      (assoc name ai-code--constraint-modifiers)))

(defun ai-code--operating-mode-p (name)
  "Return non-nil if NAME is an operating mode behavior."
  (member name ai-code--behavior-operating-modes))

(defun ai-code--constraint-bundle-p (name)
  "Return non-nil if NAME is a constraint bundle."
  (assoc name ai-code--constraint-bundles))

(defun ai-code--expand-constraint-bundle (bundle-name)
  "Expand BUNDLE-NAME to its constraint list.
Returns list of constraint names from the bundle."
  (when-let ((bundle-data (assoc bundle-name ai-code--constraint-bundles)))
    (plist-get (cdr bundle-data) :constraints)))

(defun ai-code--extract-and-remove-hashtags (prompt-text &optional context-preset)
  "Extract behaviors and remove hashtags from PROMPT-TEXT in single pass.
CONTEXT-PRESET is 'gptel-plan or 'gptel-agent for context-aware validation.
Return list (BEHAVIORS CLEANED-PROMPT SWITCH-NEEDED BUNDLE-NAME) where:
  BEHAVIORS is plist (:mode MODE :modifiers MODS :constraint-modifiers CONSTRAINTS :preset PRESET) or nil
  CLEANED-PROMPT is the prompt with tags removed
  SWITCH-NEEDED is t when in gptel-plan and a modify mode/preset is used
  BUNDLE-NAME is the detected constraint bundle name, or nil.

Callers should set the bundle using the correct project root via
`ai-code--behaviors-set-active-bundle'."
  (let ((mode nil)
        (modifiers nil)
        (constraints nil)
        (preset nil)
        (constraint-bundle nil)
        (unknown nil)
        (unknown-presets nil)
        (switch-needed nil)
        (valid-tags (append ai-code--behavior-operating-modes
                            ai-code--behavior-modifiers
                            (mapcar #'car ai-code--constraint-modifiers)))
        (result prompt-text))
    (save-match-data
      (with-temp-buffer
        (insert prompt-text)
        (goto-char (point-min))
        (while (re-search-forward "@\\([a-zA-Z0-9_-]+\\)" nil t)
          (let ((at-name (match-string 1)))
            (cond
             ((assoc at-name ai-code--behavior-presets)
              (if preset
                  (message "Warning: Multiple presets, keeping @%s" preset)
                (setq preset at-name)))
             ((ai-code--constraint-bundle-p at-name)
              (if constraint-bundle
                  (message "Warning: Multiple constraint bundles, keeping @%s" constraint-bundle)
                (setq constraint-bundle at-name)
                (let ((bundle-constraints (ai-code--expand-constraint-bundle at-name)))
                  (dolist (c bundle-constraints)
                    (cl-pushnew c constraints :test #'equal)))))
             (t (cl-pushnew at-name unknown-presets :test #'equal)))))
        (goto-char (point-min))
        (while (re-search-forward "#\\([=a-zA-Z0-9_-]+\\)" nil t)
          (let ((tag (match-string 1)))
            (cond
             ((member tag ai-code--behavior-operating-modes)
              (if mode
                  (message "Warning: Multiple operating modes, keeping #%s (ignoring #%s)" mode tag)
                (setq mode tag)))
             ((member tag ai-code--behavior-modifiers)
              (cl-pushnew tag modifiers :test #'equal))
             ((assoc tag ai-code--constraint-modifiers)
              (cl-pushnew tag constraints :test #'equal))
             (t (cl-pushnew tag unknown :test #'equal)))))
        (when unknown
          (message "Warning: Unknown behaviors preserved in prompt: #%s"
                   (mapconcat #'identity unknown " #")))
        (when unknown-presets
          (message "Warning: Unknown presets preserved in prompt: @%s"
                   (mapconcat #'identity unknown-presets " @")))
        (when (eq context-preset 'gptel-plan)
          (when (and mode (not (ai-code--behaviors-mode-readonly-p mode)))
            (message "Switching to agent mode for #%s..." mode)
            (setq switch-needed t))
          (when (and preset (not (ai-code--behaviors-preset-readonly-p preset)))
            (message "Switching to agent mode for @%s..." preset)
            (setq switch-needed t)))
        (goto-char (point-min))
        (while (re-search-forward "@\\([a-zA-Z0-9_-]+\\)\\s-*" nil t)
          (let ((name (match-string 1)))
            (when (or (assoc name ai-code--behavior-presets)
                      (ai-code--constraint-bundle-p name))
              (replace-match ""))))
        (goto-char (point-min))
        (dolist (tag valid-tags)
          (goto-char (point-min))
          (while (re-search-forward (concat "#" (regexp-quote tag) "\\s-*") nil t)
            (replace-match "")))
        (setq result (string-trim (buffer-string)))))
    (list (when (or mode modifiers constraints preset)
            (list :mode mode
                  :modifiers (nreverse modifiers)
                  :constraint-modifiers (nreverse constraints)
                  :preset preset))
          result
          switch-needed
          constraint-bundle)))

(defun ai-code--classify-prompt-intent-gptel (prompt-text)
  "Classify PROMPT-TEXT intent using GPTel.
Return list suitable for behavior injection."
  (condition-case err
      (when (featurep 'gptel)
        (let* ((modes-string (mapconcat #'identity
                                        (mapcar (lambda (m) (substring m 1))
                                                ai-code--behavior-operating-modes)
                                        ", "))
               (prompt (format
                        "Classify this user prompt's intent for an AI coding assistant.

Reply with a JSON object: {\"mode\": \"MODE\", \"modifiers\": [\"MOD1\", ...]}

Valid modes (pick exactly one): %s

Valid modifiers (pick 0-3): %s

Guidelines:
- If the user wants to implement/fix/change code: mode=code
- If debugging an error/bug: mode=debug
- If asking to understand/explain something: mode=research
- If reviewing existing code: mode=review
- If planning/designing: mode=spec
- If writing tests: mode=test
- If learning/guidance: mode=mentor
- If comparing options: mode=assess
- If documenting: mode=record

Add modifiers:
- deep: for complex/thorough analysis needed
- tdd: if test-driven development context
- challenge: if critical review needed

Prompt:
%s"
                        modes-string
                        (mapconcat #'identity ai-code--behavior-modifiers ", ")
                        prompt-text))
               (response (ai-code-call-gptel-sync prompt))
               (json-object-type 'plist)
               (json-key-type 'keyword)
               (data (when (stringp response)
                       (ai-code--extract-json-from-response response)))
               (mode (when data (plist-get data :mode)))
               (modifiers (when data (plist-get data :modifiers))))
          (when mode
            (let ((mode-name (concat "=" mode)))
              (when (member mode-name ai-code--behavior-operating-modes)
                (list :mode mode-name
                      :modifiers (seq-filter
                                  (lambda (m) (member m ai-code--behavior-modifiers))
                                  (when (listp modifiers) modifiers))))))))
    (error
     (display-warning 'ai-code-behaviors
       (format "GPTel classification failed: %s\nFalling back to keyword matching."
               (error-message-string err))
       :warning)
     nil)))

(defun ai-code--extract-json-from-response (response)
  "Extract first JSON object from RESPONSE string.
Tries markdown code blocks first (```json...```), then balanced braces.
Returns parsed plist or nil if no valid JSON found or response exceeds size limit."
  (when (and response
             (or (<= ai-code-behaviors-max-response-size 0)
                 (< (length response) ai-code-behaviors-max-response-size)))
    (save-match-data
      (let ((trimmed (string-trim response)))
        (or (ai-code--extract-json-from-code-block trimmed)
            (ai-code--extract-json-balanced trimmed))))))

(defun ai-code--extract-json-from-code-block (text)
  "Extract JSON from markdown code block in TEXT.
Returns parsed plist or nil if no valid JSON code block found."
  (when (string-match "```\\(?:json\\)?[[:space:]]*\n\\([[:s:][:print:]]*?\\)[[:space:]]*```" text)
    (condition-case nil
        (json-read-from-string (match-string 1 text))
      (error nil))))

(defun ai-code--extract-json-balanced (text)
  "Extract JSON using balanced brace detection from TEXT.
Returns parsed plist or nil if no valid JSON found or depth limit exceeded."
  (cond
   ((string-match-p "\\`[[:space:]]*{" text)
    (condition-case nil
        (json-read-from-string text)
      (error nil)))
   ((string-match "{" text)
    (let ((start (match-beginning 0))
          (depth 0)
          (max-depth ai-code-behaviors-max-brace-depth)
          (i (match-beginning 0))
          (len (length text))
          (in-string nil)
          (escape-next nil))
      (while (and (< i len) (>= depth 0) (<= depth max-depth))
        (let ((ch (aref text i)))
          (cond
           (escape-next (setq escape-next nil))
           ((eq ch ?\\) (setq escape-next t))
           (in-string (when (eq ch ?\") (setq in-string nil)))
           ((eq ch ?\") (setq in-string t))
           ((not in-string)
            (cond ((eq ch ?{) (setq depth (1+ depth)))
                  ((eq ch ?}) (setq depth (1- depth)))))))
        (setq i (1+ i)))
      (when (and (= depth 0) (<= depth max-depth))
        (condition-case nil
            (json-read-from-string (substring text start i))
          (error nil)))))
   (t nil)))

(defun ai-code--classify-prompt-intent-keywords (prompt-text)
  "Classify PROMPT-TEXT intent using keyword matching.
Return plist with :mode, :modifiers, and :confidence.
Confidence is high (2+ matches), medium (1 match), or low."
  (let* ((lower-prompt (downcase prompt-text))
         (mode-order (mapcar #'car ai-code--intent-classification-keywords))
         (mode-scores
          (delq nil
                (mapcar
                 (lambda (entry)
                   (let ((score (cl-count-if
                                 (lambda (kw) (string-match-p (regexp-quote kw) lower-prompt))
                                 (cdr entry))))
                     (when (> score 0)
                       (cons (car entry) score))))
                 ai-code--intent-classification-keywords)))
         (best-entry (car (sort mode-scores
                                (lambda (a b)
                                  (or (> (cdr a) (cdr b))
                                      (and (= (cdr a) (cdr b))
                                           (< (cl-position (car a) mode-order)
                                              (cl-position (car b) mode-order))))))))
         (modifiers nil))
    (when best-entry
      (dolist (entry ai-code--modifier-trigger-keywords)
        (let ((mod (car entry))
              (keywords (cdr entry)))
          (dolist (kw keywords)
            (when (string-match-p (regexp-quote kw) lower-prompt)
              (push (symbol-name mod) modifiers)))))
      (let ((confidence (if (>= (cdr best-entry) 2) 'high 'medium)))
        (list :mode (symbol-name (car best-entry))
              :modifiers (delete-dups modifiers)
              :confidence confidence)))))

(defun ai-code--extract-clean-user-prompt (text)
  "Extract clean user prompt from TEXT for classification.
Strips behavior injection blocks and extracts content within <user-prompt> tags.
Returns TEXT unchanged if no special structure found."
  (let ((result text))
    (when (stringp result)
      (when (string-match "<user-prompt>\\s-*\\(\\(?:.\\|\n\\)*?\\)\\s-*</user-prompt>" result)
        (setq result (match-string 1 result)))
      (when (string-match "^AdditionalContext:" result)
        (setq result (replace-regexp-in-string
                      "^AdditionalContext:\\(?:.\\|\n\\)*?\\(<user-prompt>\\|\\'\\)"
                      "" result)))
      (setq result (string-trim result)))
    result))

(defun ai-code--classify-prompt-intent (prompt-text)
  "Classify PROMPT-TEXT intent for behavior injection.
Uses GPTel if available, falls back to keyword matching.
Only classifies the clean user prompt, ignoring behavior injection blocks.
Return list of (:mode MODE :modifiers MODIFIERS)."
  (let ((clean-prompt (ai-code--extract-clean-user-prompt prompt-text)))
    (or (and (bound-and-true-p ai-code-use-gptel-classify-prompt)
             (ai-code--classify-prompt-intent-gptel clean-prompt))
        (ai-code--classify-prompt-intent-keywords clean-prompt))))

(declare-function ai-code--get-clipboard-text "ai-code" ())

(defvar ai-code-prompt-suffix nil)
(defvar ai-code-use-prompt-suffix t)
(defvar ai-code-auto-test-type nil)
(defvar ai-code-auto-test-suffix nil)

(defun ai-code--get-effective-custom-suffix ()
  "Get combined custom suffix from prompt-suffix and auto-test-suffix.
Returns nil if ai-code-use-prompt-suffix is nil."
  (when ai-code-use-prompt-suffix
    (let ((parts (delq nil (list ai-code-prompt-suffix
                                 (when ai-code-auto-test-type
                                   ai-code-auto-test-suffix)))))
      (when parts
        (mapconcat #'identity parts "\n")))))

(defun ai-code--merge-preset-with-modifiers (preset-name explicit-behaviors)
  "Merge PRESET-NAME with EXPLICIT-BEHAVIORS.
Returns final behaviors plist with custom-suffix applied, or nil if both
PRESET-NAME and EXPLICIT-BEHAVIORS are nil."
  (let ((preset-data (when preset-name
                         (cdr (assoc preset-name ai-code--behavior-presets))))
        (custom-suffix (ai-code--get-effective-custom-suffix)))
    (cond
     (preset-data
      (list :mode (plist-get preset-data :mode)
            :modifiers (delete-dups
                        (append (plist-get preset-data :modifiers)
                                (plist-get explicit-behaviors :modifiers)))
            :constraint-modifiers (copy-sequence (plist-get explicit-behaviors :constraint-modifiers))
            :custom-suffix custom-suffix))
     (explicit-behaviors
      (plist-put (copy-tree explicit-behaviors) :custom-suffix custom-suffix))
     (t nil))))

(defun ai-code--build-behavior-instruction (behaviors)
  "Build instruction block from BEHAVIORS list.
BEHAVIORS is (:mode MODE :modifiers MODIFIERS :constraint-modifiers CONSTRAINTS
:custom-suffix SUFFIX).  Return formatted string for injection."
  (let ((mode (plist-get behaviors :mode))
        (modifiers (plist-get behaviors :modifiers))
        (constraints (plist-get behaviors :constraint-modifiers))
        (custom-suffix (plist-get behaviors :custom-suffix))
        (blocks nil))
    (when mode
      (let ((content (ai-code--load-behavior-prompt mode)))
        (when content
          (push (format "AdditionalContext: <operating-mode>\n%s\n</operating-mode>" content) blocks))))
    (when modifiers
      (let ((mod-contents
             (delq nil
                   (mapcar (lambda (mod)
                             (ai-code--load-behavior-prompt mod))
                           modifiers))))
        (when mod-contents
          (push (format "AdditionalContext: <behavior-modifiers>\n%s\n</behavior-modifiers>"
                        (mapconcat #'identity mod-contents "\n\n"))
                blocks))))
    (when constraints
      (let ((constraint-texts
             (delq nil
                   (mapcar (lambda (c) (cdr (assoc c ai-code--constraint-modifiers)))
                           constraints))))
        (when constraint-texts
          (push (format "AdditionalContext: <constraints>\n%s\n</constraints>"
                        (mapconcat #'identity constraint-texts "\n"))
                blocks))))
    (when (and custom-suffix (not (string-empty-p custom-suffix)))
      (push (format "AdditionalContext: <custom-constraints>\n%s\n</custom-constraints>" custom-suffix) blocks))
    (when blocks
      (concat (mapconcat #'identity (nreverse blocks) "\n\n")
              "\n\nThese behaviors apply until superseded by new hashtags. During compaction, preserve the most recent <operating-mode> and <behavior-modifiers> blocks."))))

(defun ai-code--behaviors-wrap-with-instruction (behaviors prompt-text)
  "Wrap PROMPT-TEXT with instruction from BEHAVIORS.
Returns formatted string with instruction block, or PROMPT-TEXT if no instruction.
If PROMPT-TEXT already contains <user-prompt> tags, extracts content first."
  (let ((instruction (ai-code--build-behavior-instruction behaviors)))
    (if instruction
        (let* ((clean-text (string-trim prompt-text))
               ;; Extract content if already wrapped
               (content (if (string-match "<user-prompt>\\s-*\\(\\(?:.\\|\n\\)*?\\)\\s-*</user-prompt>" clean-text)
                            (match-string 1 clean-text)
                          clean-text)))
          (format "%s\n\n<user-prompt>\n%s\n</user-prompt>"
                  instruction (string-trim content)))
      prompt-text)))

(defun ai-code--behaviors-meets-confidence-threshold-p (confidence)
  "Check if CONFIDENCE meets `ai-code-behaviors-reclassify-min-confidence'.
CONFIDENCE should be 'high, 'medium, or 'low."
  (let ((levels '(high medium low))
        (min-level ai-code-behaviors-reclassify-min-confidence))
    (and confidence
         min-level
         (<= (or (cl-position confidence levels) 0)
             (or (cl-position min-level levels) 1)))))

(defun ai-code--behaviors-apply-and-format (preset-name behaviors project-root &optional message-text)
  "Apply PRESET-NAME and BEHAVIORS for PROJECT-ROOT, return formatted prompt.
MESSAGE-TEXT is optional message to display after applying.
Returns the wrapped prompt text."
  (ai-code--behaviors-set-preset preset-name project-root)
  (ai-code--behaviors-set-state behaviors project-root)
  (ai-code--behaviors-update-mode-line project-root)
  (when message-text
    (message "%s" message-text))
  behaviors)

(defun ai-code--process-behaviors (prompt-text &optional project-root)
  "Process behaviors for PROMPT-TEXT and return modified prompt.
This is the main entry point for behavior injection.
PROJECT-ROOT specifies the project for state lookup/storage; uses current
project if nil.
Priority order (regular context):
1. Explicit #hashtags/@preset - always wins, clears pending
2. Pending preset - committed on first non-empty prompt
3. Session state - reused if no pending
4. Auto-classify - if enabled and no session state
Returns the modified prompt with behaviors injected, or the original
PROMPT-TEXT if no behaviors apply.
Note: Preset-only prompts (empty after tag removal) are handled by
`ai-code--behaviors-check-preset-only-prompt' in the advice layer."
  (if (not ai-code-behaviors-enabled)
      prompt-text
    (let* ((extracted (ai-code--extract-and-remove-hashtags prompt-text))
           (explicit-behaviors (nth 0 extracted))
           (cleaned-prompt (nth 1 extracted))
           (bundle-name (nth 3 extracted))
           (session-state (ai-code--behaviors-get-state project-root))
           (pending-preset (ai-code--behaviors-get-pending-preset project-root)))
      (when bundle-name
        (ai-code--behaviors-set-active-bundle bundle-name project-root))
      (cond
       (explicit-behaviors
        (ai-code--behaviors-clear-pending-preset project-root)
        (let* ((preset-name (plist-get explicit-behaviors :preset))
               (final-behaviors (ai-code--merge-preset-with-modifiers preset-name explicit-behaviors)))
          (ai-code--behaviors-apply-and-format preset-name final-behaviors project-root)
          (ai-code--behaviors-wrap-with-instruction final-behaviors cleaned-prompt)))
       ((and pending-preset (not (string-empty-p (string-trim cleaned-prompt))))
        (ai-code--behaviors-clear-pending-preset project-root)
        (let ((final-behaviors (ai-code--merge-preset-with-modifiers pending-preset nil)))
          (ai-code--behaviors-apply-and-format pending-preset final-behaviors project-root
                                                (format "Activated preset: @%s" pending-preset))
          (ai-code--behaviors-wrap-with-instruction final-behaviors cleaned-prompt)))
       (session-state
        (ai-code--behaviors-wrap-with-instruction session-state prompt-text))
       ((when-let ((classified (and ai-code-behaviors-auto-classify
                                      (ai-code--classify-prompt-intent prompt-text))))
          (let* ((suggested-preset (ai-code--suggest-preset-for-classification classified))
                 (final-behaviors (if suggested-preset
                                       (ai-code--merge-preset-with-modifiers suggested-preset nil)
                                     (ai-code--merge-preset-with-modifiers nil classified))))
            (ai-code--behaviors-apply-and-format suggested-preset final-behaviors project-root
                                                  (format "Auto-classified: @%s (%s)"
                                                          (or suggested-preset "custom")
                                                          (or (plist-get final-behaviors :mode) "unknown")))
            (ai-code--behaviors-wrap-with-instruction final-behaviors prompt-text))))
       (t prompt-text)))))

(defun ai-code-behaviors-status ()
  "Show current active behaviors."
  (interactive)
  (let ((state (ai-code--behaviors-get-state)))
    (if state
        (let ((mode (plist-get state :mode))
              (modifiers (plist-get state :modifiers))
              (constraints (plist-get state :constraint-modifiers)))
          (message "Active behaviors: Mode=%s Modifiers=%s Constraints=%s"
                   (or mode "none")
                   (if modifiers (mapconcat (lambda (m) (concat "#" m)) modifiers " ") "none")
                   (if constraints (mapconcat (lambda (c) (concat "#" c)) constraints " ") "none")))
      (message "No active behaviors"))))

(defun ai-code-behaviors-clear ()
  "Clear active behaviors and constraint bundle for current project."
  (interactive)
  (ai-code--behaviors-clear-state)
  (ai-code--behaviors-clear-active-bundle)
  (ai-code--behaviors-update-mode-line)
  (message "Behaviors cleared for current project"))

(defun ai-code-behaviors-clear-all ()
  "Clear behaviors for all projects.
Clears session states, active bundles, pending presets, and last prompts."
  (interactive)
  (clrhash ai-code--behaviors-session-states)
  (clrhash ai-code--active-constraint-bundles)
  (clrhash ai-code--behaviors-pending-presets)
  (clrhash ai-code--behaviors-last-prompts)
  (ai-code--behaviors-update-mode-line)
  (message "All behaviors cleared"))

(defun ai-code--behaviors-clear-all-caches ()
  "Clear all behavior-related caches.
Call this after updating the ai-behaviors repository."
  (clrhash ai-code--behaviors-cache)
  (clrhash ai-code--detection-cache)
  (clrhash ai-code--behavior-annotation-cache)
  (setq ai-code--behaviors-update-checked nil))

(defun ai-code-behaviors-install ()
  "Clone or update the ai-behaviors repository.
Returns t on success, nil on failure."
  (interactive)
  (if (ai-code--behaviors-repo-available-p)
      (let* ((default-directory (expand-file-name ai-code-behaviors-repo-path))
             (before-info (ai-code--behaviors-commit-info))
             (before-commit (plist-get before-info :commit))
             (update-status (ai-code--behaviors-check-for-updates)))
        (cond
         ((eq update-status 'up-to-date)
          (message "ai-behaviors already up to date (commit %s)" before-commit)
          t)
         ((eq update-status 'updates-available)
          (message "Updating ai-behaviors from commit %s..." before-commit)
          (let ((result (call-process "git" nil nil nil "pull")))
            (if (eq result 0)
                (progn
                  (ai-code--behaviors-clear-all-caches)
                  (let ((after-info (ai-code--behaviors-commit-info)))
                    (message "ai-behaviors updated to commit %s"
                             (plist-get after-info :commit)))
                  t)
              (message "Failed to update ai-behaviors (git pull exited %s)" result)
              nil)))
         (t
          (message "Updating ai-behaviors repository...")
          (let ((result (call-process "git" nil nil nil "pull")))
            (if (eq result 0)
                (progn
                  (ai-code--behaviors-clear-all-caches)
                  (message "ai-behaviors repository updated")
                  t)
              (message "Failed to update ai-behaviors (git pull exited %s)" result)
              nil)))))
    (if (ai-code--ensure-behaviors-repo)
        (progn
          (message "ai-behaviors repository installed at %s" ai-code-behaviors-repo-path)
          t)
      (message "Failed to clone ai-behaviors repository")
      nil)))

(defun ai-code-behaviors-version-info ()
  "Display version info for ai-behaviors repository."
  (interactive)
  (if (not (ai-code--behaviors-repo-available-p))
      (message "ai-behaviors repository not installed. Run M-x ai-code-behaviors-install")
    (let* ((info (ai-code--behaviors-commit-info))
           (commit (plist-get info :commit))
           (date (plist-get info :date))
           (update-status (ai-code--behaviors-check-for-updates)))
      (message "ai-behaviors: commit %s (%s) - %s"
               commit
               date
               (pcase update-status
                 ('up-to-date "up to date")
                 ('updates-available "UPDATES AVAILABLE")
                 ('no-remote "no remote")
                 ('error "error checking")
(_ "unknown"))))))

(defun ai-code--behavior-readme-path (behavior-name)
  "Return path to README.md for BEHAVIOR-NAME."
  (expand-file-name
   (format "behaviors/%s/README.md" behavior-name)
   (expand-file-name ai-code-behaviors-repo-path)))

(defun ai-code--load-behavior-readme (behavior-name)
  "Load README.md content for BEHAVIOR-NAME.
Return content string or nil if not found."
  (let ((file-path (ai-code--behavior-readme-path behavior-name)))
    (when (file-exists-p file-path)
      (with-temp-buffer
        (insert-file-contents file-path)
        (buffer-string)))))

(defun ai-code--extract-behavior-annotation (behavior-name)
  "Extract one-line annotation for BEHAVIOR-NAME from its README.md.
Return short description string or nil if not found."
  (let ((cached (gethash behavior-name ai-code--behavior-annotation-cache)))
    (if (eq cached :not-found)
        nil
      (if cached
          cached
        (let ((content (ai-code--load-behavior-readme behavior-name))
              (annotation nil))
          (when content
            (with-temp-buffer
              (insert content)
              (goto-char (point-min))
              (when (re-search-forward "^# .+$" nil t)
                (forward-line 1)
                (while (and (not (eobp)) (string-empty-p (string-trim (thing-at-point 'line t))))
                  (forward-line 1))
                (let ((line (string-trim (thing-at-point 'line t))))
                  (when (and line (not (string-empty-p line))
                             (not (string-match-p "^#" line)))
                    (setq annotation line))))
              (when (and (not annotation)
                         (re-search-forward "\\*\\*Role\\*\\*" nil t))
                (let ((line (string-trim (thing-at-point 'line t))))
                  (setq annotation (replace-regexp-in-string "^[|* ]+" "" line))
                  (setq annotation (replace-regexp-in-string "[|]+$" "" annotation))))
              (when annotation
                (setq annotation (truncate-string-to-width annotation 50 nil nil t)))))
          (puthash behavior-name (or annotation :not-found) ai-code--behavior-annotation-cache)
          annotation)))))

;;; Mode-line helper functions

(defun ai-code--behaviors-get-mode-face (mode)
  "Get face for MODE."
  (pcase mode
    ("=code" 'ai-code-behaviors-mode-line-code)
    ("=debug" 'ai-code-behaviors-mode-line-debug)
    ("=review" 'ai-code-behaviors-mode-line-review)
    ("=mentor" 'ai-code-behaviors-mode-line-mentor)
    ("=research" 'ai-code-behaviors-mode-line-research)
    ("=spec" 'ai-code-behaviors-mode-line-spec)
    (_ 'ai-code-behaviors-mode-line-default)))

(defun ai-code--behaviors-build-tooltip (preset state)
  "Build tooltip text for PRESET and STATE."
  (if (not (or preset state))
      "No behaviors active\n\nmouse-1: Select preset/bundle\nmouse-3: Actions"
    (let* ((mode (plist-get state :mode))
           (modifiers (plist-get state :modifiers))
           (constraints (plist-get state :constraint-modifiers))
           (custom-suffix (plist-get state :custom-suffix))
           (active-bundle (ai-code--behaviors-get-active-bundle))
           (preset-desc (when preset
                          (plist-get (cdr (assoc preset ai-code--behavior-presets))
                                     :description)))
           (bundle-desc (when active-bundle
                          (plist-get (cdr (assoc active-bundle ai-code--constraint-bundles))
                                     :description)))
           (lines nil))
      (push "" lines)
      (push "mouse-3: Actions" lines)
      (push "mouse-1: Select preset/bundle" lines)
      (when custom-suffix
        (push "+custom-suffix" lines))
      (when (and constraints (not active-bundle))
        (push (format "Constraints: %s"
                      (mapconcat (lambda (c) (concat "#" c)) constraints " "))
              lines))
      (when modifiers
        (push (format "Modifiers: %s"
                      (mapconcat (lambda (m) (concat "#" m)) modifiers " "))
              lines))
      (when mode
        (push (format "Mode: #%s" mode) lines))
      (when active-bundle
        (push "" lines)
        (when bundle-desc
          (push bundle-desc lines))
        (push (format "Bundle: @%s" active-bundle) lines))
      (when preset
        (push "" lines)
        (when preset-desc
          (push preset-desc lines))
        (push (format "@%s" preset) lines))
      (mapconcat #'identity (nreverse lines) "\n"))))

;;; Multi-signal preset detection

(defun ai-code--detect-from-filename (file)
  "Detect preset from FILE name.
Returns plist with :preset, :confidence, :source, or nil."
  (when (and file (memq :filename ai-code-behaviors-detection-enabled-signals))
    (let (result)
      (dolist (pattern ai-code-behaviors-detection-patterns)
        (when (and (not result) (string-match-p (car pattern) file))
          (setq result (list :preset (cdr pattern)
                             :confidence :high
                             :source :custom-pattern))))
      (unless result
        (dolist (entry ai-code--file-pattern-preset-map)
          (when (and (not result) (string-match-p (car entry) file))
            (setq result (append (cdr entry) (list :source :filename))))))
      result)))

(defun ai-code--detect-from-major-mode ()
  "Detect preset from current major mode.
Returns plist with :preset, :confidence, :source, or nil."
  (when (memq :major-mode ai-code-behaviors-detection-enabled-signals)
    (when-let ((preset (cdr (assq major-mode ai-code--major-mode-preset-map))))
      (list :preset preset
            :confidence :medium
            :source :major-mode))))

(defun ai-code--detect-project-structure (root)
  "Detect preset from project at ROOT.
Returns plist with :preset, :confidence or nil."
  (let ((default-directory root))
    (catch 'found
      (dolist (entry ai-code--project-structure-signals)
        (when (file-exists-p (car entry))
          (let ((signals (cdr entry)))
            (dolist (signal signals)
              (when (or (file-exists-p (car signal))
                        (file-directory-p (car signal)))
                (throw 'found (list :preset (cdr signal)
                                    :confidence :medium))))))))))

(defun ai-code--with-detection-cache (source detect-fn)
  "Get cached detection result for SOURCE using DETECT-FN.
SOURCE is a keyword like :project or :git.
DETECT-FN is a function that returns the detection result.
Returns plist with :preset, :confidence, or nil.
Caches both positive and negative results.
Note: Caller already knows SOURCE, so it's not included in return value."
  (let* ((root (ai-code--behaviors-project-root))
         (cache-key (cons source root))
         (cached (gethash cache-key ai-code--detection-cache)))
    (if (and cached
             (< (- (float-time) (plist-get cached :timestamp))
                ai-code-behaviors-detection-cache-ttl))
        (let ((result (plist-get cached :result)))
          (when (not (eq result :not-found))
            result))
      (let ((result (funcall detect-fn)))
        (puthash cache-key
                 (list :result (or result :not-found)
                       :timestamp (float-time))
                 ai-code--detection-cache)
        result))))

(defun ai-code--detect-from-project ()
  "Detect preset from project structure.
Returns plist with :preset, :confidence, :source, or nil.
Uses cache with TTL."
  (when (memq :project ai-code-behaviors-detection-enabled-signals)
    (ai-code--with-detection-cache :project
      (lambda () (ai-code--detect-project-structure (ai-code--behaviors-project-root))))))

(declare-function magit-get-current-branch "magit-git" ())

(defun ai-code--detect-git-branch ()
  "Detect preset from current git branch.
Returns plist with :preset, :confidence or nil.
Uses magit if available, falls back to git rev-parse."
  (when-let ((branch (cond
                      ((fboundp 'magit-get-current-branch)
                       (magit-get-current-branch))
                      ((executable-find "git")
                       (ai-code--git-command-output "rev-parse" "--abbrev-ref" "HEAD")))))
    (unless (string-empty-p branch)
      (catch 'found
        (dolist (entry ai-code--git-branch-patterns)
          (when (string-match-p (car entry) branch)
            (throw 'found (list :preset (cdr entry)
                                :confidence :low))))))))

(defun ai-code--detect-from-git ()
  "Detect preset from git context.
Returns plist with :preset, :confidence, :source, or nil.
Uses cache with TTL."
  (when (memq :git ai-code-behaviors-detection-enabled-signals)
    (ai-code--with-detection-cache :git #'ai-code--detect-git-branch)))

(defun ai-code--select-best-preset (signals)
  "Select the best preset from SIGNALS list.
Priority: :high > :medium > :low."
  (when signals
    (let* ((rank '((:high . 3) (:medium . 2) (:low . 1)))
           (ranked (sort signals
                         (lambda (a b)
                           (> (cdr (assq (plist-get a :confidence) rank))
                              (cdr (assq (plist-get b :confidence) rank)))))))
      (plist-get (car ranked) :preset))))

(defun ai-code--behaviors-clear-detection-cache ()
  "Clear all detection caches."
  (interactive)
  (clrhash ai-code--detection-cache)
  (message "Behavior detection cache cleared"))

(defun ai-code--behaviors-detect-context-preset ()
  "Detect appropriate preset from multiple signals.
Returns preset name string, or `ai-code-behaviors-default-preset' if no signals match."
  (or ai-code-behaviors-override-preset
      (let ((signals
             (delq nil
                   (list (ai-code--detect-from-filename (or buffer-file-name ""))
                         (ai-code--detect-from-major-mode)
                         (ai-code--detect-from-project)
                         (ai-code--detect-from-git)))))
        (or (ai-code--select-best-preset signals)
            ai-code-behaviors-default-preset))))

;;; Mode-line popup menus

(defvar ai-code--behaviors-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      'ai-code-behaviors-mode-line-select-preset)
    (define-key map [mode-line mouse-3]
      'ai-code-behaviors-mode-line-actions)
    (define-key map [header-line mouse-1]
      'ai-code-behaviors-mode-line-select-preset)
    (define-key map [header-line mouse-3]
      'ai-code-behaviors-mode-line-actions)
    map)
  "Keymap for behavior mode-line indicator.")

(defun ai-code-behaviors-mode-line-select-preset (&optional event)
  "Show preset and bundle selection popup menu.
EVENT is the mouse event.
Shows all presets with * annotation for modify presets in gptel modes.
Auto-switches to agent mode when modify preset is selected in plan mode."
  (interactive)
  (let* ((menu (make-sparse-keymap "Select Preset or Bundle"))
         (current-preset (when (boundp 'gptel--preset) gptel--preset))
         (gptel-mode-p (memq current-preset '(gptel-plan gptel-agent)))
         (readonly-presets '("frame-problem" "research-deep" "design-options"
                             "spec-planning" "quick-review" "deep-review" "mentor-learn"))
         (modify-presets '("tdd-dev" "quick-fix" "thorough-debug")))
    (define-key menu [clear]
      '(menu-item "Clear behaviors" ai-code-behaviors-clear))
    (define-key menu [sep-bundles] '(menu-item "--"))
    (dolist (b (reverse ai-code--constraint-bundles))
      (define-key menu (vector (intern (concat "bundle-" (car b))))
        `(menu-item ,(format "@%s - %s" (car b)
                             (plist-get (cdr b) :description))
                    (lambda () (interactive)
                      (ai-code-constraints-apply-bundle ,(car b))))))
    (define-key menu [sep-modify] '(menu-item "--"))
    (dolist (name (reverse modify-presets))
      (let* ((preset (assoc name ai-code--behavior-presets))
             (label (format "@%s%s - %s"
                            name
                            (if gptel-mode-p "*" "")
                            (plist-get (cdr preset) :description))))
        (define-key menu (vector (intern (concat "mod-" name)))
          `(menu-item ,label
                      (lambda () (interactive)
                        (when (and (boundp 'gptel--preset)
                                   (eq gptel--preset 'gptel-plan))
                          (let ((state (ai-code--behaviors-get-state)))
                            (when state
                              (let ((mode (plist-get state :mode)))
                                (when (member mode ai-code--behavior-readonly-modes)
                                  (setq state (plist-put (copy-tree state) :mode nil))
                                  (ai-code--behaviors-set-state state)))))
                          (gptel--apply-preset 'gptel-agent
                            (lambda (sym val) (set (make-local-variable sym) val)))
                          (message "Switched to agent mode for @%s" ,name))
                        (ai-code-behaviors-apply-preset ,name))))))
    (define-key menu [sep-readonly] '(menu-item "--"))
    (dolist (name (reverse readonly-presets))
      (let* ((preset (assoc name ai-code--behavior-presets))
             (label (format "@%s - %s"
                            name
                            (plist-get (cdr preset) :description))))
        (define-key menu (vector (intern (concat "ro-" name)))
          `(menu-item ,label
                      (lambda () (interactive)
                        (ai-code-behaviors-apply-preset ,name))))))
    (if event
        (popup-menu menu event)
      (popup-menu menu))))

(defun ai-code-behaviors-mode-line-actions (&optional event)
  "Show behavior actions popup menu.
EVENT is the mouse event."
  (interactive)
  (let ((menu (make-sparse-keymap "Actions"))
        (preset (ai-code--behaviors-get-preset))
        (active-bundle (ai-code--behaviors-get-active-bundle)))
    (define-key menu [disable]
      '(menu-item "Disable mode-line indicator"
                  ai-code-behaviors-mode-line-disable))
    (define-key menu [sep2] '(menu-item "--"))
    (define-key menu [clear-all]
      '(menu-item "Clear all projects" ai-code-behaviors-clear-all))
    (define-key menu [clear-constraints]
      '(menu-item "Clear constraints" ai-code-constraints-clear))
    (define-key menu [update]
      '(menu-item "Update behavior repo" ai-code-behaviors-install))
    (define-key menu [sep1] '(menu-item "--"))
    (define-key menu [list-constraints]
      '(menu-item "List all constraints" ai-code-constraints-list))
    (define-key menu [auto-detect]
      '(menu-item "Auto-detect constraints" ai-code-constraints-auto-detect-and-apply))
    (define-key menu [add-constraint]
      '(menu-item "Add constraint..." ai-code-constraints-select))
    (when (or preset active-bundle)
      (define-key menu [describe]
        `(menu-item "Describe current behavior"
                    (lambda () (interactive)
                      (ai-code-describe-behavior ,(or preset active-bundle))))))
    (define-key menu [status]
      '(menu-item "Show status" ai-code-behaviors-status))
    (if event
        (popup-menu menu event)
      (popup-menu menu))))

(defun ai-code--behaviors-mode-line-string ()
  "Return propertized mode-line string for behaviors."
  (when ai-code-behaviors-enabled
    (let* ((state (ai-code--behaviors-get-state))
           (preset (ai-code--behaviors-get-preset))
           (active-bundle (ai-code--behaviors-get-active-bundle))
           (mode (and state (plist-get state :mode)))
           (modifiers (and state (plist-get state :modifiers)))
           (constraints (and state (plist-get state :constraint-modifiers)))
           (has-custom (and state (plist-get state :custom-suffix)))
           (constraint-count (+ (length constraints) (if has-custom 1 0)))
           (face (ai-code--behaviors-get-mode-face mode))
           (text (cond
                  ((and preset active-bundle)
                   (format "[@%s @%s]" preset active-bundle))
                  ((and preset (> constraint-count 0))
                   (format "[@%s +%d]" preset constraint-count))
                  (preset (format "[@%s]" preset))
                  (active-bundle
                   (format "[@%s +%d]" active-bundle constraint-count))
                  ((or mode modifiers constraints has-custom)
                   (concat "["
                           (or mode "")
                           (when (and mode modifiers) " ")
                           (when modifiers (mapconcat #'identity modifiers " "))
                           (when (> constraint-count 0)
                             (format " +%d" constraint-count))
                           "]"))
                  (t "[○]")))
           (tooltip (ai-code--behaviors-build-tooltip preset state)))
      (propertize text
                  'face face
                  'mouse-face 'mode-line-highlight
                  'help-echo tooltip
                  'local-map ai-code--behaviors-mode-line-map))))

(defun ai-code--behaviors-update-mode-line (&optional project-root)
  "Update mode-line with current behavior indicator.
If PROJECT-ROOT is specified, update all buffers for that project.
Otherwise, update current buffer only."
  (if project-root
      (save-current-buffer
        (dolist (buf (buffer-list))
          (when (buffer-live-p buf)
            (set-buffer buf)
            (when (equal (ai-code--behaviors-project-root) project-root)
              (force-mode-line-update t)))))
    (force-mode-line-update t)))

(defun ai-code-describe-behavior (behavior-name)
  "Display documentation for BEHAVIOR-NAME.
Shows the behavior's README.md in a help buffer, or constraint/bundle description.
BEHAVIOR-NAME should not include the # or @ prefix."
  (interactive
   (let* ((presets (mapcar (lambda (p) (concat "@" (car p))) ai-code--behavior-presets))
          (bundles (mapcar (lambda (b) (concat "@" (car b))) ai-code--constraint-bundles))
          (modes (mapcar (lambda (m) (concat "#" m)) ai-code--behavior-operating-modes))
          (modifiers (mapcar (lambda (m) (concat "#" m)) ai-code--behavior-modifiers))
          (constraints (mapcar (lambda (c) (concat "#" (car c))) ai-code--constraint-modifiers))
          (all-behaviors (append presets bundles modes modifiers constraints))
          (input (completing-read "Describe behavior: " all-behaviors nil t)))
     (list (when (string-match "[#@]\\(.+\\)" input) (match-string 1 input)))))
  (if (not behavior-name)
      (message "No behavior selected")
    (cond
     ((assoc behavior-name ai-code--constraint-bundles)
      (let* ((bundle (assoc behavior-name ai-code--constraint-bundles))
             (desc (plist-get (cdr bundle) :description))
             (constraints (plist-get (cdr bundle) :constraints)))
        (with-help-window (help-buffer)
          (princ (format "@%s - Constraint Bundle\n\n" behavior-name))
          (when desc (princ (format "Description: %s\n\n" desc)))
          (princ "Constraints:\n")
          (dolist (c constraints)
            (let ((c-desc (cdr (assoc c ai-code--constraint-modifiers))))
              (princ (format "  #%s\n" c))
              (when c-desc
                (princ (format "    %s\n" c-desc))))))))
     ((assoc behavior-name ai-code--constraint-modifiers)
      (let ((constraint-desc (cdr (assoc behavior-name ai-code--constraint-modifiers))))
        (with-help-window (help-buffer)
          (princ (format "#%s\n\n" behavior-name))
          (princ constraint-desc))))
     (t
      (let ((content (ai-code--load-behavior-readme behavior-name)))
        (if (not content)
            (message "No documentation found for %s" behavior-name)
          (with-help-window (help-buffer)
            (princ (format "#%s\n\n" behavior-name))
            (princ content))))))))

(defun ai-code--behavior-annotated-candidates ()
  "Return completion candidates with annotations.
Returns list of (DISPLAY . VALUE) pairs where DISPLAY includes annotation.
Includes presets, constraint bundles, and behaviors."
  (let ((candidates nil))
    (when ai-code--behavior-presets
      (dolist (preset ai-code--behavior-presets)
        (let* ((name (concat "@" (car preset)))
               (desc (plist-get (cdr preset) :description))
               (display (format "%-15s %s" name (or desc ""))))
          (push (cons display (cons 'preset (car preset))) candidates))))
    (when ai-code--constraint-bundles
      (dolist (bundle ai-code--constraint-bundles)
        (let* ((name (concat "@" (car bundle)))
               (desc (plist-get (cdr bundle) :description))
               (display (format "%-15s %s" name (or desc ""))))
          (push (cons display (cons 'bundle (car bundle))) candidates))))
    (when ai-code--constraint-modifiers
      (dolist (constraint ai-code--constraint-modifiers)
        (let* ((name (concat "#" (car constraint)))
               (desc (cdr constraint))
               (display (format "%-15s %s" name (truncate-string-to-width desc 40 nil nil t))))
          (push (cons display (cons 'constraint (car constraint))) candidates))))
    (when (ai-code--behaviors-repo-available-p)
      (dolist (mod ai-code--behavior-modifiers)
        (let* ((name (concat "#" mod))
               (annotation (ai-code--extract-behavior-annotation mod)))
          (push (cons (if annotation (format "%-15s %s" name annotation) name)
                      (cons 'behavior name)) candidates)))
      (dolist (mode ai-code--behavior-operating-modes)
        (let* ((name (concat "#" mode))
               (annotation (ai-code--extract-behavior-annotation mode)))
          (push (cons (if annotation (format "%-15s %s" name annotation) name)
                      (cons 'behavior name)) candidates))))
    (nreverse candidates)))

(defun ai-code-behaviors-apply-preset (preset-name)
  "Apply preset named PRESET-NAME.
Preserves existing constraint-modifiers from current state."
  (let ((preset (assoc preset-name ai-code--behavior-presets)))
    (when preset
      (let* ((data (cdr preset))
             (existing-state (ai-code--behaviors-get-state))
             (existing-constraints (plist-get existing-state :constraint-modifiers)))
        (ai-code--behaviors-set-state
         (list :mode (plist-get data :mode)
               :modifiers (copy-sequence (plist-get data :modifiers))
               :constraint-modifiers existing-constraints
               :custom-suffix (ai-code--get-effective-custom-suffix)))
        (ai-code--behaviors-set-preset preset-name)
        (ai-code--behaviors-update-mode-line)
        (message "Preset applied: %s (%s %s)%s"
                 preset-name
                 (plist-get data :mode)
                 (mapconcat #'identity (plist-get data :modifiers) " ")
                 (if existing-constraints
                     (format " +%d constraint(s)" (length existing-constraints))
                   ""))))))

(defun ai-code-behaviors-preset ()
  "Select and apply a behavior preset.
In gptel modes, shows all presets with * annotation for modify presets.
Auto-switches to agent mode when modify preset is selected in plan mode."
  (interactive)
  (let* ((current-preset (when (boundp 'gptel--preset) gptel--preset))
         (gptel-mode-p (memq current-preset '(gptel-plan gptel-agent)))
         (readonly-presets '("frame-problem" "research-deep" "design-options"
                             "spec-planning" "quick-review" "deep-review" "mentor-learn"))
         (modify-presets '("tdd-dev" "quick-fix" "thorough-debug"))
         (presets
          (append
           (mapcar
            (lambda (name)
              (let* ((preset (assoc name ai-code--behavior-presets)))
                (cons (format "%-15s %s" name (plist-get (cdr preset) :description))
                      name)))
            readonly-presets)
           (mapcar
            (lambda (name)
              (let* ((preset (assoc name ai-code--behavior-presets))
                     (display-name (if gptel-mode-p (concat name "*") name)))
                (cons (format "%-15s %s" display-name (plist-get (cdr preset) :description))
                      name)))
            modify-presets)))
         (choice (completing-read "Select preset: " presets nil t)))
    (when (and choice (not (string-empty-p choice)))
      (let* ((preset-name (cdr (assoc choice presets)))
             (readonly (ai-code--behaviors-preset-readonly-p preset-name)))
        (when preset-name
          (when (and (boundp 'gptel--preset)
                     (eq gptel--preset 'gptel-plan)
                     (not readonly))
            (let ((state (ai-code--behaviors-get-state)))
              (when state
                (let ((mode (plist-get state :mode)))
                  (when (member mode ai-code--behavior-readonly-modes)
                    (setq state (plist-put (copy-tree state) :mode nil))
                    (ai-code--behaviors-set-state state)))))
            (gptel--apply-preset 'gptel-agent
              (lambda (sym val) (set (make-local-variable sym) val)))
            (message "Switched to agent mode for @%s" preset-name))
          (ai-code-behaviors-apply-preset preset-name))))))

(defun ai-code-behaviors-select ()
  "Interactively select and apply behaviors or presets.
Sets session state based on selection."
  (interactive)
  (let* ((candidates (ai-code--behavior-annotated-candidates))
         (selection (completing-read "Set behavior: " candidates nil t)))
    (when (and selection (not (string-empty-p selection)))
      (let ((value (cdr (assoc selection candidates))))
        (when (and value (consp value))
          (pcase (car value)
            ('preset (ai-code-behaviors-apply-preset (cdr value)))
            ('behavior
             (let* ((extracted (nth 0 (ai-code--extract-and-remove-hashtags (cdr value))))
                    (behaviors (ai-code--merge-preset-with-modifiers nil extracted)))
               (when behaviors
                 (ai-code--behaviors-set-preset nil)
                 (ai-code--behaviors-set-state behaviors)
                 (ai-code--behaviors-update-mode-line)
                 (message "Behavior set: %s" (cdr value)))))
            ('constraint
             (let* ((existing (ai-code--behaviors-get-state))
                    (behaviors (or existing '(:mode nil :modifiers nil :constraint-modifiers nil)))
                    (current-constraints (plist-get behaviors :constraint-modifiers))
                    (new-constraints (delete-dups (cons (cdr value) current-constraints)))
                    (updated (plist-put (copy-tree behaviors) :constraint-modifiers new-constraints)))
               (ai-code--behaviors-set-preset nil)
               (ai-code--behaviors-set-state updated)
               (ai-code--behaviors-update-mode-line)
               (message "Constraint added: %s" (cdr value))))
            ('bundle (ai-code-constraints-apply-bundle (cdr value)))
            (_ nil)))))))

(defun ai-code-behaviors-mode-line-enable ()
  "Enable mode-line display of active behaviors for current buffer.
Only shows in gptel-mode or ai-code-prompt-mode buffers.
For gptel-agent buffers, extracts project from buffer name.
Installs global advice for gptel preset changes (once only).
Starts idle timer for periodic cache cleanup."
  (interactive)
  (ai-code--behaviors-install-gptel-advice)
  (ai-code--behaviors-start-cleanup-timer)
  (when (or (bound-and-true-p gptel-mode)
             (eq major-mode 'ai-code-prompt-mode))
    (make-local-variable 'mode-line-misc-info)
    (unless (member '(:eval (ai-code--behaviors-mode-line-string)) mode-line-misc-info)
      (setq mode-line-misc-info
            (append mode-line-misc-info
                    (list '(:eval (ai-code--behaviors-mode-line-string))))))
    (ai-code--behaviors-update-mode-line)))

(defun ai-code-behaviors-mode-line-disable ()
  "Disable mode-line display of active behaviors for current buffer.
Note: Global gptel advice remains installed for other buffers."
  (interactive)
  (when (local-variable-p 'mode-line-misc-info)
    (setq mode-line-misc-info
          (delete '(:eval (ai-code--behaviors-mode-line-string)) mode-line-misc-info))
    (force-mode-line-update t)))

(defun ai-code--behaviors-install-gptel-advice ()
  "Install global advice for gptel preset changes.
Uses `advice-member-p' (Emacs 27+) to ensure advice is installed only once.
On older Emacs versions, advice is always installed (may have duplicates)."
  (when (fboundp 'gptel--apply-preset)
    (when (or (not (fboundp 'advice-member-p))
               (not (advice-member-p #'ai-code--behaviors-gptel-preset-change-advice
                                      'gptel--apply-preset)))
      (advice-add 'gptel--apply-preset :around
                  #'ai-code--behaviors-gptel-preset-change-advice))))

(defun ai-code--behaviors-uninstall-gptel-advice ()
  "Remove global advice for gptel preset changes.
Call this when completely disabling ai-code-behaviors."
  (when (and (fboundp 'advice-member-p)
              (advice-member-p #'ai-code--behaviors-gptel-preset-change-advice
                                'gptel--apply-preset))
    (advice-remove 'gptel--apply-preset
                   #'ai-code--behaviors-gptel-preset-change-advice)))

(defconst ai-code--backend-session-prefixes
  '((opencode . "opencode")
    (claude-code . "claude")
    (gemini . "gemini")
    (github-copilot-cli . "copilot")
    (codex . "codex")
    (cursor . "cursor")
    (aider . "aider")
    (grok . "grok")
    (kiro . "kiro")
    (codebuddy . "codebuddy"))
  "Map CLI backend names to their session buffer prefixes.
Only includes terminal-based backends. ECA and agent-shell use different detection.")

(declare-function ai-code-backends-infra--session-working-directory
                  "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra--find-session-buffers
                  "ai-code-backends-infra" (prefix directory))

(defun ai-code--get-session-prefix ()
  "Get session prefix for current CLI backend.
Returns nil for non-CLI backends (ECA, agent-shell)."
  (and (boundp 'ai-code-selected-backend)
       (alist-get ai-code-selected-backend
                  ai-code--backend-session-prefixes)))

(defun ai-code--session-exists-p ()
  "Return non-nil if an AI session exists for current project."
  (cond
   ;; ECA backend - use eca-session
   ((and (boundp 'ai-code-selected-backend)
         (eq ai-code-selected-backend 'eca))
    (and (fboundp 'eca-session)
         (condition-case nil
             (eca-session)
           (error nil))))

   ;; agent-shell backend - use agent-shell--shell-buffer
   ((and (boundp 'ai-code-selected-backend)
         (eq ai-code-selected-backend 'agent-shell))
    (and (fboundp 'agent-shell--shell-buffer)
         (condition-case nil
             (agent-shell--shell-buffer :no-create t :no-error t)
           (error nil))))

   ;; gptel-agent backend - use ai-code-gptel-agent--get-buffer
   ((and (boundp 'ai-code-selected-backend)
         (eq ai-code-selected-backend 'gptel-agent))
    (and (fboundp 'ai-code-gptel-agent--get-buffer)
         (condition-case nil
             (ai-code-gptel-agent--get-buffer)
           (error nil))))

   ;; CLI backends - use terminal buffer detection
   ((ai-code--get-session-prefix)
    (when-let* ((prefix (ai-code--get-session-prefix))
                (working-dir (and (fboundp 'ai-code-backends-infra--session-working-directory)
                                  (ai-code-backends-infra--session-working-directory))))
      (and (fboundp 'ai-code-backends-infra--find-session-buffers)
           (ai-code-backends-infra--find-session-buffers prefix working-dir)
           t)))

   ;; Unknown backend - require explicit session start
   (t nil)))

(defconst ai-code--command-preset-map
  '((ai-code-code-change . "quick-fix")
    (ai-code-implement-todo . "tdd-dev")
    (ai-code-ask-question . "mentor-learn")
    (ai-code-explain . "mentor-learn")
    (ai-code-refactor-book-method . "quick-fix")
    (ai-code-tdd-cycle . "tdd-dev")
    (ai-code-pull-or-review-diff-file . "deep-review")
    (ai-code-investigate-exception . "thorough-debug")
    (ai-code-flycheck-fix-errors-in-scope . "quick-fix")
    (ai-code-send-command . nil))
  "Map commands to their default behavior presets.
When these commands execute, the associated preset is automatically applied.
A nil value means session check only, no preset.")

(defun ai-code--apply-preset-for-command (command)
  "Apply preset for COMMAND if defined.
Always applies, overriding any existing preset."
  (when-let ((preset-name (alist-get command ai-code--command-preset-map)))
    (ai-code-behaviors-apply-preset preset-name)
    (message "[ai-code] Applied preset: @%s" preset-name)))

(defun ai-code--behaviors-check-preset-only-prompt (prompt-text)
  "Check if PROMPT-TEXT is only behavior tags with no content.
If so, apply the behaviors and return t to signal abort.
Otherwise return nil to continue normal processing."
  (when (and ai-code-behaviors-enabled
             (stringp prompt-text))
    (let* ((extracted (ai-code--extract-and-remove-hashtags prompt-text))
           (explicit-behaviors (nth 0 extracted))
           (cleaned-prompt (nth 1 extracted))
           (bundle-name (nth 3 extracted)))
      (when (and explicit-behaviors
                 (string-empty-p (string-trim cleaned-prompt)))
        (let* ((preset-name (plist-get explicit-behaviors :preset))
               (final-behaviors (ai-code--merge-preset-with-modifiers preset-name explicit-behaviors)))
          (ai-code--behaviors-set-preset preset-name)
          (ai-code--behaviors-set-state final-behaviors)
          (when bundle-name
            (ai-code--behaviors-set-active-bundle bundle-name))
          (ai-code--behaviors-update-mode-line)
          (message "Preset applied: %s%s"
                   (if preset-name (concat "@" preset-name) "")
                   (if-let ((mode (plist-get final-behaviors :mode)))
                       (format " (%s)" mode)
                     ""))
          t)))))

(defun ai-code--insert-prompt-behaviors-advice (orig-fun prompt-text)
  "Advice for ai-code--insert-prompt.
ORIG-FUN is the original function.
PROMPT-TEXT is the prompt being processed.
Handles preset-only detection, session checks, and preset application.
Only applies command-specific behavior when called interactively.
Signals `user-error' for preset-only prompts to abort the send cleanly."
  (let ((preset-only-result (ai-code--behaviors-check-preset-only-prompt prompt-text)))
    (if preset-only-result
        (user-error "Preset-only prompt: behavior applied, no message sent")
      (when (and this-command (assq this-command ai-code--command-preset-map))
        (unless (ai-code--session-exists-p)
          (if (y-or-n-p "No AI session for this project. Start one? ")
              (progn
                (ai-code-cli-start)
                (user-error "Session started. Please run the command again."))
            (user-error "Cancelled")))
        (ai-code--apply-preset-for-command this-command))
      (funcall orig-fun prompt-text))))

;;; Auto-enable functions

(defun ai-code-behaviors-enable-auto-presets ()
  "Enable automatic preset application for ai-code commands.
This adds advice to apply context-appropriate presets when running
commands like `ai-code-tdd-cycle' or `ai-code-code-change'.
Clears detection cache on enable.
Idempotent - safe to call multiple times.
Returns t if enabled, nil if `ai-code--insert-prompt' is not defined."
  (interactive)
  (unless (fboundp 'ai-code--insert-prompt)
    (message "Cannot enable: ai-code--insert-prompt not defined (load ai-code first)")
    (cl-return-from ai-code-behaviors-enable-auto-presets nil))
  (ai-code--behaviors-clear-detection-cache)
  (advice-remove 'ai-code--insert-prompt #'ai-code--insert-prompt-behaviors-advice)
  (advice-add 'ai-code--insert-prompt :around
              #'ai-code--insert-prompt-behaviors-advice)
  (add-hook 'ai-code-prompt-mode-hook #'ai-code--behavior-setup-preset-completion)
  (ai-code--behavior-enable-preset-in-file-completion)
  (advice-add 'ai-code-plain-read-string :around
              #'ai-code--behavior-plain-read-string-advice)
  (advice-add 'ai-code-helm-read-string-with-history :around
              #'ai-code--behavior-helm-read-string-advice)
  (advice-add 'ai-code--prompt-auto-trigger-filepath-completion :around
              #'ai-code--behavior-prompt-auto-trigger-advice)
  (when-let ((preset (ai-code--behaviors-detect-context-preset)))
    (ai-code-behaviors-apply-preset preset))
  (message "ai-code-behaviors auto-presets enabled")
  t)

(defun ai-code-behaviors-disable-auto-presets ()
  "Disable automatic preset application."
  (interactive)
  (advice-remove 'ai-code--insert-prompt
                 #'ai-code--insert-prompt-behaviors-advice)
  (remove-hook 'ai-code-prompt-mode-hook #'ai-code--behavior-setup-preset-completion)
  (ai-code--behavior-disable-preset-in-file-completion)
  (advice-remove 'ai-code-plain-read-string
                 #'ai-code--behavior-plain-read-string-advice)
  (advice-remove 'ai-code-helm-read-string-with-history
                 #'ai-code--behavior-helm-read-string-advice)
  (advice-remove 'ai-code--prompt-auto-trigger-filepath-completion
                 #'ai-code--behavior-prompt-auto-trigger-advice)
  (message "ai-code-behaviors auto-presets disabled"))

;; Auto-enable based on defcustom - defer until ai-code is loaded
;; This avoids adding advice prematurely if ai-code is not yet loaded
(when ai-code-behaviors-auto-enable
  (if (featurep 'ai-code)
      (ai-code-behaviors-enable-auto-presets)
    (eval-after-load 'ai-code
      #'ai-code-behaviors-enable-auto-presets)))

;;; GPTel-Agent Integration

(defvar gptel-prompt-transform-functions)
(defvar gptel-fsm-info)
(defvar gptel--preset)
(defvar gptel--fsm-last)
(declare-function gptel-fsm-info "gptel-request" (fsm))

(defun ai-code--gptel-agent-process-behaviors (prompt-text project-root &optional context-preset)
  "Process behaviors for PROMPT-TEXT in gptel-agent context.
PROJECT-ROOT specifies the project for state lookup.
CONTEXT-PRESET is 'gptel-plan or 'gptel-agent for mode validation.
Respects `ai-code-behaviors-gptel-agent-auto-classify'.
Returns list (BEHAVIORS-APPLIED RESULT-TEXT SWITCH-NEEDED).
BEHAVIORS-APPLIED is t if behaviors were applied (or preset-only).
RESULT-TEXT is the processed text or nil for preset-only prompts.
SWITCH-NEEDED is t when in gptel-plan and modify mode/preset is used.

Priority order (gptel-agent context):
1. Explicit #hashtags/@preset - always wins
2. Auto-classify (if enabled and meets confidence threshold)
3. Pending preset (committed on first prompt)
4. Session state (fallback)
5. No changes"
  (let* ((extracted (ai-code--extract-and-remove-hashtags prompt-text context-preset))
         (explicit-behaviors (nth 0 extracted))
         (cleaned-prompt (nth 1 extracted))
         (switch-needed (nth 2 extracted))
         (bundle-name (nth 3 extracted))
         (session-state (ai-code--behaviors-get-state project-root))
         (pending-preset (ai-code--behaviors-get-pending-preset project-root))
         (classified (and ai-code-behaviors-gptel-agent-auto-classify
                          ai-code-behaviors-auto-classify
                          (ai-code--classify-prompt-intent prompt-text)))
         (confidence (and classified
                          (or (plist-get classified :confidence)
                              'high)))
         (meets-threshold (and confidence
                               (ai-code--behaviors-meets-confidence-threshold-p confidence))))
    (when bundle-name
      (ai-code--behaviors-set-active-bundle bundle-name project-root))
    (cond
     (explicit-behaviors
      (ai-code--behaviors-clear-pending-preset project-root)
      (let* ((preset-name (plist-get explicit-behaviors :preset))
             (final-behaviors (ai-code--merge-preset-with-modifiers preset-name explicit-behaviors)))
        (ai-code--behaviors-apply-and-format preset-name final-behaviors project-root)
        (if (string-empty-p (string-trim cleaned-prompt))
            (progn
              (message "Preset applied: %s%s"
                       (if preset-name (concat "@" preset-name) "")
                       (if-let ((mode (plist-get final-behaviors :mode)))
                           (format " (%s)" mode) ""))
              (list t nil switch-needed))
          (list t (ai-code--behaviors-wrap-with-instruction final-behaviors cleaned-prompt) switch-needed))))
     (meets-threshold
      (ai-code--behaviors-clear-pending-preset project-root)
      (let* ((suggested-preset (ai-code--suggest-preset-for-classification classified))
             (final-behaviors (if suggested-preset
                                   (ai-code--merge-preset-with-modifiers suggested-preset nil)
                                 (ai-code--merge-preset-with-modifiers nil classified))))
        (ai-code--behaviors-apply-and-format suggested-preset final-behaviors project-root
                                              (format "Auto-classified: @%s (%s)"
                                                      (or suggested-preset "custom")
                                                      (or (plist-get final-behaviors :mode) "unknown")))
        (list t (ai-code--behaviors-wrap-with-instruction final-behaviors prompt-text) nil)))
     ((and pending-preset (not (string-empty-p (string-trim cleaned-prompt))))
      (ai-code--behaviors-clear-pending-preset project-root)
      (let ((final-behaviors (ai-code--merge-preset-with-modifiers pending-preset nil)))
        (ai-code--behaviors-apply-and-format pending-preset final-behaviors project-root
                                              (format "Activated preset: @%s" pending-preset))
        (list t (ai-code--behaviors-wrap-with-instruction final-behaviors cleaned-prompt) nil)))
     (session-state
      (list t (ai-code--behaviors-wrap-with-instruction session-state prompt-text) nil))
     (t (list nil prompt-text nil)))))

(defun ai-code--gptel-agent-transform-inject-behaviors (next-or-fsm &optional fsm)
  "Transform function for gptel-agent to inject behaviors.
Only injects when `gptel--preset' is `gptel-plan' or `gptel-agent'.
Handles preset-only prompts by applying state without sending.
Operates on current buffer (gptel request buffer).

Supports both calling conventions:
- (fsm) - legacy single-arg, returns t if modified
- (callback fsm) - gptel chained transform, calls callback when done"
  (let* ((next (and fsm next-or-fsm))
         (fsm (or fsm next-or-fsm))
         modified)
    (condition-case err
        (let* ((info (and fsm (gptel-fsm-info fsm)))
               (source-buffer (and info (plist-get info :buffer)))
               (preset (when (buffer-live-p source-buffer)
                         (buffer-local-value 'gptel--preset source-buffer)))
               (prompt-start (save-excursion
                               (goto-char (point-max))
                               (let ((heading-match (re-search-backward "^### " nil t)))
                                 (if heading-match
                                     (point)
                                   (let ((prop-match (text-property-search-backward 'gptel nil t)))
                                     (if prop-match
                                         (prop-match-beginning prop-match)
                                       (point-min)))))))
               (prompt-text (string-trim
                             (buffer-substring-no-properties prompt-start (point-max))))
               (original-prompt prompt-text))
          (if (or (not ai-code-behaviors-enabled)
                  (not (memq preset '(gptel-plan gptel-agent)))
                  (string-empty-p (string-trim prompt-text)))
              nil
            (if (not (ai-code--behaviors-repo-available-p))
                (progn
                  (message "ai-code-behaviors: Repository not available, skipping behavior injection")
                  nil)
              (let* ((project-root (ai-code--behaviors-project-root source-buffer))
                     (result (ai-code--gptel-agent-process-behaviors prompt-text project-root preset))
                     (behaviors-applied (nth 0 result))
                     (processed-text (nth 1 result))
                     (switch-needed (nth 2 result))
                     (behaviors-state (ai-code--behaviors-get-state project-root)))
                (when (and switch-needed (buffer-live-p source-buffer))
                  (with-current-buffer source-buffer
                    (gptel--apply-preset 'gptel-agent
                      (lambda (sym val) (set (make-local-variable sym) val)))))
                (puthash project-root
                         (list :original original-prompt
                               :processed processed-text
                               :behaviors behaviors-state)
                         ai-code--behaviors-last-prompts)
                (cond
                 ((and behaviors-applied (null processed-text))
                  (delete-region prompt-start (point-max))
                  (setq modified t))
                 ((and behaviors-applied processed-text
                       (not (string= processed-text prompt-text)))
                  (delete-region prompt-start (point-max))
                  (goto-char prompt-start)
                  (insert processed-text)
                  (setq modified t))
                 (t nil))))))
      (error
       (message "ai-code-behaviors transform error: %s" (error-message-string err))
       (setq modified nil)))
    (if next
        (or modified (funcall next fsm))
      modified)))

(defun ai-code--gptel-agent-setup-transform ()
  "Set up gptel-agent behavior integration.
Adds transform and completion to gptel buffers.
Mode-line is enabled via `gptel-mode-hook' in `ai-code--behavior-setup-hashtag-completion'.

NOTE: We do NOT register behavior presets with gptel--known-presets.
If we did, gptel's gptel--transform-apply-preset would remove @preset
from prompts before our transform could see it. We handle @preset
in our own transform and provide completion via ai-code--behavior-preset-gptel-capf."
  (add-hook 'gptel-mode-hook #'ai-code--behavior-setup-hashtag-completion)
  (unless (memq 'ai-code--gptel-agent-transform-inject-behaviors
                (default-value 'gptel-prompt-transform-functions))
    (add-hook 'gptel-prompt-transform-functions
              #'ai-code--gptel-agent-transform-inject-behaviors))
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (bound-and-true-p gptel-mode)
          (ai-code--behavior-setup-hashtag-completion)))))
  (message "ai-code-behaviors gptel-agent integration enabled"))

(defun ai-code--behaviors-gptel-preset-change-advice (orig-fun preset &rest args)
  "Handle mode switching between gptel-plan and gptel-agent.
ORIG-FUN is the original gptel--apply-preset function.
PRESET is the new preset being applied.
ARGS are additional arguments passed to gptel--apply-preset.

Updates mode-line and switches to appropriate default preset:
- Plan->Agent: switch to @quick-fix if current preset is readonly
- Agent->Plan: switch to @quick-review if current preset is modify"
  (let ((prev-preset (and (boundp 'gptel--preset) gptel--preset)))
    (apply orig-fun preset args)
    (when ai-code-behaviors-enabled
      (cond
       ((and (eq preset 'gptel-agent)
             (eq prev-preset 'gptel-plan))
        (let* ((current-preset-name (ai-code--behaviors-get-preset))
               (is-readonly (and current-preset-name
                                 (ai-code--behaviors-preset-readonly-p current-preset-name))))
          (when (and current-preset-name is-readonly)
            (ai-code-behaviors-apply-preset "quick-fix")
            (message "Switched to agent mode with @quick-fix"))))
       ((and (eq preset 'gptel-plan)
             (eq prev-preset 'gptel-agent))
        (let* ((current-preset-name (ai-code--behaviors-get-preset))
               (is-modify (and current-preset-name
                               (not (ai-code--behaviors-preset-readonly-p current-preset-name)))))
          (when (and current-preset-name is-modify)
            (ai-code-behaviors-apply-preset "quick-review")
            (message "Switched to plan mode with @quick-review")))))
      (force-mode-line-update t))))

(defun ai-code--behavior-setup-hashtag-completion ()
  "Add behavior hashtag and preset completion to current buffer.
Intended for `gptel-mode-hook'.
Also adds font-lock for behavior hashtags, keybinding, and mode-line."
  (add-hook 'completion-at-point-functions #'ai-code--behavior-hashtag-capf nil t)
  (add-hook 'completion-at-point-functions #'ai-code--behavior-preset-gptel-capf nil t)
  (local-set-key (kbd "C-c P") #'ai-code-behaviors-show-last-prompt)
  ;; Set up local transform list if needed
  ;; Don't add 't' if our transform is already in the default - avoids double execution
  (when (boundp 'gptel-prompt-transform-functions)
    (make-local-variable 'gptel-prompt-transform-functions)
    (unless (or (memq t gptel-prompt-transform-functions)
                (memq 'ai-code--gptel-agent-transform-inject-behaviors
                      (default-value 'gptel-prompt-transform-functions)))
      (setq gptel-prompt-transform-functions
            (cons t gptel-prompt-transform-functions))))
  (font-lock-add-keywords
   nil
   `((ai-code--fontify-behavior-keyword
      0 ',(list :box -1 :inherit 'font-lock-keyword-face)
      prepend))
   t)
  (ai-code-behaviors-mode-line-enable))

(defun ai-code--fontify-behavior-keyword (end)
  "Font-lock function for behavior hashtags in chat buffers.
Return fontification info for text up to END."
  (and (re-search-forward "#\\([=a-zA-Z0-9_-]+\\)\\_>" end t)
       (or (= (match-beginning 0) (point-min))
           (memq (char-syntax (char-before (match-beginning 0))) '(32 62)))
       (let* ((matched (match-string 1))
              (with-eq (if (string-prefix-p "=" matched)
                           matched
                         (concat "=" matched))))
         (or (member matched ai-code--behavior-modifiers)
             (member matched (mapcar #'car ai-code--constraint-modifiers))
             (member with-eq ai-code--behavior-operating-modes)))))

(defun ai-code--behavior-hashtag-capf ()
  "Completion-at-point function for #behavior hashtags.
Works in gptel-agent buffers and ai-code-prompt-mode.
Requires #= prefix for operating modes, # prefix for modifiers/constraints.
In gptel-plan mode, only shows readonly operating modes."
  (when (and ai-code-behaviors-enabled
             (or (bound-and-true-p gptel-mode)
                 (and (boundp 'major-mode) (eq major-mode 'ai-code-prompt-mode))))
    (let* ((pos (save-excursion
                  (skip-chars-backward "=a-zA-Z0-9_-")
                  (point)))
           (current-preset (when (boundp 'gptel--preset) gptel--preset))
           (has-equals (and (< pos (point))
                            (eq (char-after pos) ?=))))
      (when (and (> pos (point-min))
                 (eq (char-before pos) ?#)
                 (or (= pos (1+ (point-min)))
                     (memq (char-syntax (char-before (1- pos))) '(?\s ?\t ?\n))))
        (cond
         (has-equals
          (let ((start-pos (1+ pos)))
            (list start-pos (point)
                  (ai-code--behavior-modes-completion-table current-preset)
                  :exclusive 'no
                  :annotation-function #'ai-code--behavior-mode-annotation
                  :exit-function
                  (lambda (_str _status)
                    (when (looking-at "\\>")
                      (insert " "))))))
         (t
          (list pos (point)
                (ai-code--behavior-modifiers-completion-table)
                :exclusive 'no
                :annotation-function #'ai-code--behavior-hashtag-annotation
                :exit-function
                (lambda (_str _status)
                  (when (looking-at "\\>")
                    (insert " "))))))))))

(defun ai-code--behavior-modes-completion-table (&optional context-preset)
  "Return completion table for operating modes.
CONTEXT-PRESET filters to readonly modes when 'gptel-plan."
  (let ((modes (if (eq context-preset 'gptel-plan)
                   ai-code--behavior-readonly-modes
                 ai-code--behavior-operating-modes)))
    (mapcar (lambda (m) (substring m 1)) modes)))

(defun ai-code--behavior-modifiers-completion-table ()
  "Return completion table for modifiers and constraints."
  (append ai-code--behavior-modifiers
          (mapcar #'car ai-code--constraint-modifiers)))

(defun ai-code--behavior-mode-annotation (name)
  "Return annotation for operating mode NAME."
  (let ((annotation (ai-code--extract-behavior-annotation (concat "=" name))))
    (if annotation (format "  %s" annotation) "  (operating mode)")))

(defun ai-code--behavior-hashtag-completion-table ()
  "Return completion table for behavior hashtags.
Deprecated: Use ai-code--behavior-modes-completion-table and
ai-code--behavior-modifiers-completion-table instead."
  (append
   (mapcar (lambda (m) (substring m 1)) ai-code--behavior-operating-modes)
   (mapcar (lambda (m) m) ai-code--behavior-modifiers)
   (mapcar (lambda (c) (car c)) ai-code--constraint-modifiers)))

(defun ai-code--behavior-hashtag-annotation (name)
  "Return annotation for behavior NAME."
  (cond
   ((member (concat "=" name) ai-code--behavior-operating-modes)
    (let ((annotation (ai-code--extract-behavior-annotation (concat "=" name))))
      (if annotation (format "  %s" annotation) "  (operating mode)")))
   ((member name ai-code--behavior-modifiers)
    (let ((annotation (ai-code--extract-behavior-annotation name)))
      (if annotation (format "  %s" annotation) "  (modifier)")))
   ((assoc name ai-code--constraint-modifiers)
    (let ((desc (cdr (assoc name ai-code--constraint-modifiers))))
      (format "  %s" (truncate-string-to-width desc 40 nil nil t))))
   (t "")))

(defun ai-code--behavior-preset-gptel-capf ()
  "Completion at point for behavior presets and constraint bundles in gptel-mode.
Shows behavior presets like @tdd-dev and constraint bundles like @rust-stack.
In gptel modes, shows * annotation for modify presets.
Works alongside gptel's built-in preset completion."
  (when (and ai-code-behaviors-enabled
             (bound-and-true-p gptel-mode))
    (let* ((pos (save-excursion
                  (skip-chars-backward "a-zA-Z0-9_-")
                  (point)))
           (current-preset (when (boundp 'gptel--preset) gptel--preset))
           (gptel-mode-p (memq current-preset '(gptel-plan gptel-agent)))
           (all-candidates
            (append (mapcar #'car ai-code--behavior-presets)
                    (mapcar #'car ai-code--constraint-bundles)))
           (annotation-fn
            (lambda (name)
              (cond
               ((and gptel-mode-p
                     (assoc name ai-code--behavior-presets)
                     (not (ai-code--behaviors-preset-readonly-p name)))
                (let* ((preset (assoc name ai-code--behavior-presets))
                       (desc (plist-get (cdr preset) :description)))
                  (format "* %s" (or desc ""))))
               ((assoc name ai-code--behavior-presets)
                (let* ((preset (assoc name ai-code--behavior-presets))
                       (desc (plist-get (cdr preset) :description)))
                  (format " %s" (or desc ""))))
               ((assoc name ai-code--constraint-bundles)
                (let* ((bundle (assoc name ai-code--constraint-bundles))
                       (desc (plist-get (cdr bundle) :description)))
                  (format " %s" (or desc ""))))
               (t "")))))
      (when (and (> pos (point-min))
                 (eq (char-before pos) ?@)
                 (or (= pos (1+ (point-min)))
                     (memq (char-syntax (char-before (1- pos))) '(?\s ?\t ?\n))))
        (list pos (point)
              all-candidates
              :exclusive 'no
              :annotation-function annotation-fn
              :exit-function
              (lambda (_str _status)
                (when (looking-at "\\>")
                  (insert " "))))))))

(defun ai-code--behavior-preset-or-bundle-annotation (name)
  "Return annotation for preset or bundle NAME."
  (cond
   ((assoc name ai-code--behavior-presets)
    (let* ((preset (assoc name ai-code--behavior-presets))
           (desc (plist-get (cdr preset) :description))
           (mode (plist-get (cdr preset) :mode))
           (modifiers (plist-get (cdr preset) :modifiers)))
      (format "    %s [%s %s]"
              (or desc "")
              (or mode "")
              (mapconcat #'identity (or modifiers '()) " "))))
   ((assoc name ai-code--constraint-bundles)
    (let* ((bundle (assoc name ai-code--constraint-bundles))
           (desc (plist-get (cdr bundle) :description))
           (constraints (plist-get (cdr bundle) :constraints)))
      (format "    [bundle] %s (%s)"
              (or desc "")
              (mapconcat #'identity constraints ", "))))
   (t "")))

(defun ai-code--behavior-preset-gptel-annotation (name)
  "Return annotation for preset NAME."
  (let ((preset (assoc name ai-code--behavior-presets)))
    (if preset
        (let ((desc (plist-get (cdr preset) :description))
              (mode (plist-get (cdr preset) :mode))
              (modifiers (plist-get (cdr preset) :modifiers)))
          (format "    %s [%s %s]"
                  (or desc "")
                  (or mode "")
                  (mapconcat #'identity (or modifiers '()) " ")))
      "")))

(defun ai-code--suggest-preset-for-classification (classification)
  "Suggest a preset name based on CLASSIFICATION.
CLASSIFICATION is a plist like (:mode \"=code\" :modifiers (\"deep\" \"tdd\")).
Returns preset name string or nil."
  (when classification
    (let ((mode (plist-get classification :mode))
          (modifiers (or (plist-get classification :modifiers) '())))
      (cond
       ((and (equal mode "=code")
             (member "tdd" modifiers))
        "tdd-dev")
       ((and (equal mode "=code")
             (member "concise" modifiers))
        "quick-fix")
       ((equal mode "=code")
        "quick-fix")
       ((and (equal mode "=debug")
             (or (member "deep" modifiers) (member "challenge" modifiers)))
        "thorough-debug")
       ((equal mode "=debug")
        "thorough-debug")
       ((and (equal mode "=review")
             (member "concise" modifiers))
        "quick-review")
       ((and (equal mode "=review")
             (or (member "deep" modifiers) (member "challenge" modifiers)))
        "deep-review")
       ((equal mode "=review")
        "quick-review")
       ((and (equal mode "=research")
             (or (member "deep" modifiers) (member "wide" modifiers)))
        "research-deep")
       ((equal mode "=research")
        "research-deep")
       ((equal mode "=mentor")
        "mentor-learn")
       ((and (equal mode "=spec")
             (or (member "decompose" modifiers) (member "wide" modifiers)))
        "spec-planning")
       ((equal mode "=spec")
        "spec-planning")
       ((equal mode "=test")
        "tdd-dev")
       (t nil)))))

(defun ai-code--behaviors-extract-project-from-buffer-name ()
  "Extract project path from gptel-agent buffer name.
For gptel-agent buffers, returns default-directory which is set correctly.
Returns nil if not a gptel-agent buffer."
  (when (string-match "\\*gptel-agent:\\([^*]+\\)\\*" (buffer-name))
    default-directory))

(defun ai-code-behaviors-show-last-prompt ()
  "Show the last prompt processed by behavior injection.
Displays the original prompt, processed prompt, and applied behaviors.
Useful for debugging what was actually sent to the LLM.
In gptel-agent buffers, tries multiple sources to find the project root."
  (interactive)
  (let* ((source-buffer (when (bound-and-true-p gptel-mode)
                          (when-let* ((fsm (bound-and-true-p gptel--fsm-last))
                                      (info (and fsm (gptel-fsm-info fsm))))
                            (plist-get info :buffer))))
         (candidate-roots (delq nil
                                (list (when (buffer-live-p source-buffer)
                                        (ai-code--behaviors-project-root source-buffer))
                                      (ai-code--behaviors-extract-project-from-buffer-name)
                                      (ai-code--behaviors-project-root))))
         (last-prompt nil)
         (found-root nil))
    (dolist (root candidate-roots)
      (when (and root (not last-prompt))
        (when-let ((data (gethash root ai-code--behaviors-last-prompts)))
          (setq last-prompt data)
          (setq found-root root))))
    (unless last-prompt
      (let (all-roots)
        (maphash (lambda (k _v) (push k all-roots)) ai-code--behaviors-last-prompts)
        (when (= (length all-roots) 1)
          (setq found-root (car all-roots))
          (setq last-prompt (gethash found-root ai-code--behaviors-last-prompts)))))
    (if (not last-prompt)
        (let (all-roots)
          (maphash (lambda (k _v) (push k all-roots)) ai-code--behaviors-last-prompts)
          (if all-roots
              (message "No prompt for %s. Available: %s"
                       (or (car candidate-roots) "unknown")
                       (mapconcat #'identity all-roots ", "))
            (message "No prompts processed yet")))
      (let* ((original (plist-get last-prompt :original))
             (processed (plist-get last-prompt :processed))
             (behaviors (plist-get last-prompt :behaviors))
             (buf (get-buffer-create "*ai-code-behaviors-last-prompt*")))
        (with-current-buffer buf
          (erase-buffer)
          (insert (format "Project Root: %s\n\n" found-root))
          (insert "=== ORIGINAL PROMPT ===\n\n")
          (insert (or original "(none)"))
          (insert "\n\n=== PROCESSED PROMPT (sent to LLM) ===\n\n")
          (insert (or processed "(no changes)"))
          (insert "\n\n=== APPLIED BEHAVIORS ===\n\n")
          (if behaviors
              (let ((mode (plist-get behaviors :mode))
                    (modifiers (plist-get behaviors :modifiers))
                    (constraints (plist-get behaviors :constraint-modifiers)))
                (insert (format "Mode: %s\n" (or mode "none")))
                (insert (format "Modifiers: %s\n" (if modifiers (mapconcat #'identity modifiers " ") "none")))
                (insert (format "Constraints: %s\n" (if constraints (mapconcat #'identity constraints " ") "none"))))
            (insert "No behaviors applied"))
          (goto-char (point-min)))
        (pop-to-buffer buf)))))

(defcustom ai-code-behaviors-gptel-agent-integration t
  "When non-nil, inject behaviors into gptel-agent prompts.
Adds a transform function to `gptel-prompt-transform-functions' that
processes behavior hashtags (#=code, @preset, etc.) and injects
corresponding instructions into prompts sent via gptel-agent.
Only injects when `gptel--preset' is `gptel-plan' or `gptel-agent'."
  :type 'boolean
  :group 'ai-code-behaviors)

(when ai-code-behaviors-gptel-agent-integration
  (if (featurep 'gptel)
      (ai-code--gptel-agent-setup-transform)
    (eval-after-load 'gptel
      #'ai-code--gptel-agent-setup-transform)))

;;; Constraint Bundle and Persistence Functions

(defun ai-code--constraints-get-persistence-path ()
  "Get the path to the constraints persistence file for current project."
  (let ((root (ai-code--behaviors-project-root)))
    (when root
      (expand-file-name ai-code-constraints-persistence-file root))))

(defun ai-code--constraints-load-from-project ()
  "Load constraints from project persistence file.
Returns list of constraint names, or nil if no file exists."
  (let ((path (ai-code--constraints-get-persistence-path)))
    (when (and path (file-exists-p path))
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (let ((constraints nil)
              (bundle nil))
          (while (not (eobp))
            (let ((line (string-trim (thing-at-point 'line t))))
              (when (string-match-p "^#" line)
                (let ((name (string-trim (substring line 1))))
                  (cond
                   ((string-prefix-p "Bundle:" name)
                    (setq bundle (string-trim (substring name 7))))
                   ((assoc name ai-code--constraint-modifiers)
                    (push name constraints))))))
            (forward-line 1))
          (when bundle
            (ai-code--behaviors-set-active-bundle bundle))
          (nreverse constraints))))))

(defun ai-code--constraints-save-to-project (constraints)
  "Save CONSTRAINTS to project persistence file.
CONSTRAINTS is a list of constraint names."
  (let ((path (ai-code--constraints-get-persistence-path)))
    (when path
      (let ((dir (file-name-directory path)))
        (unless (file-directory-p dir)
          (make-directory dir t)))
      (with-temp-buffer
        (insert "# Auto-detected and user-set constraints\n")
        (insert "# Lines starting with # are constraints\n")
        (insert "# Bundle: <name> applies a predefined bundle\n\n")
        (dolist (c constraints)
          (insert (concat "#" c "\n")))
        (when-let ((bundle (ai-code--behaviors-get-active-bundle)))
          (insert (concat "\n# Bundle: " bundle "\n")))
        (write-region (point-min) (point-max) path nil 'silent)))))

(defun ai-code--glob-to-regexp (glob)
  "Convert GLOB pattern to regexp.
Handles * (matches anything) and ? (matches single char)."
  (let ((result "")
        (i 0)
        (len (length glob)))
    (while (< i len)
      (let ((char (aref glob i)))
        (cond
         ((eq char ?*)
          (setq result (concat result ".*")))
         ((eq char ??)
          (setq result (concat result ".")))
         ((memq char '(?. ?^ ?$ ?+ ?\\ ?\[ ?\] ?\( ?\)))
          (setq result (concat result "\\" (string char))))
         (t
          (setq result (concat result (string char))))))
      (setq i (1+ i)))
    result))

(defun ai-code--constraints-detect-from-file (file-path &optional project-root)
  "Detect constraints from a single project config FILE-PATH.
PROJECT-ROOT is used to compute relative paths for directory patterns.
Returns list of detected constraint names."
  (let* ((file-name (file-name-nondirectory file-path))
         (relative-path (when project-root
                          (file-relative-name file-path project-root)))
         (entry (cl-find-if (lambda (e)
                              (let ((pattern (car e)))
                                (or (string= pattern file-name)
                                    (and relative-path (string= pattern relative-path))
                                    (string-match-p (concat (ai-code--glob-to-regexp pattern) "$") file-name)
                                    (and relative-path
                                         (string-match-p (concat (ai-code--glob-to-regexp pattern) "$") relative-path)))))
                           ai-code--project-config-constraint-map)))
    (when entry
      (let ((base-constraints (plist-get (cdr entry) :constraints))
            (patterns (plist-get (cdr entry) :patterns))
            (detected nil))
        (setq detected (or base-constraints '()))
        (when (and patterns (file-exists-p file-path))
          (with-temp-buffer
            (insert-file-contents file-path)
            (dolist (pattern-entry patterns)
              (let ((pattern (car pattern-entry))
                    (constraint (cdr pattern-entry)))
                (goto-char (point-min))
                (when (re-search-forward pattern nil t)
                  (cl-pushnew constraint detected :test #'equal))))))
        detected))))

(defun ai-code--glob-pattern-p (pattern)
  "Return non-nil if PATTERN contains glob wildcards (* or ?)."
  (string-match-p "[*?]" pattern))

(defun ai-code--expand-glob-in-dir (pattern dir)
  "Expand glob PATTERN in directory DIR.
Returns list of matching file paths."
  (let ((regex (concat (ai-code--glob-to-regexp pattern) "$")))
    (delq nil
          (mapcar (lambda (file)
                    (when (string-match-p regex file)
                      (expand-file-name file dir)))
                  (directory-files dir nil)))))

(defun ai-code--constraints-auto-detect ()
  "Auto-detect constraints from project configuration files.
Scans project root for known config files and extracts constraints.
Returns list of detected constraint names."
  (let ((root (ai-code--behaviors-project-root)))
    (let ((cached (gethash root ai-code--constraints-cache)))
      (if (and cached
               (< (- (float-time) (plist-get cached :timestamp))
                  ai-code-behaviors-detection-cache-ttl))
          (plist-get cached :constraints)
        (let ((all-constraints nil))
          (dolist (entry ai-code--project-config-constraint-map)
            (let* ((pattern (car entry))
                   (matched-files nil))
              (if (ai-code--glob-pattern-p pattern)
                  (setq matched-files (ai-code--expand-glob-in-dir pattern root))
                (let ((full-path (expand-file-name pattern root)))
                  (when (or (file-exists-p full-path)
                            (file-directory-p full-path))
                    (setq matched-files (list full-path)))))
              (dolist (file-path matched-files)
                (let ((detected (ai-code--constraints-detect-from-file file-path root)))
                  (setq all-constraints (append all-constraints detected))))))
          (setq all-constraints (delete-dups all-constraints))
          (puthash root (list :constraints all-constraints
                              :timestamp (float-time))
                   ai-code--constraints-cache)
          all-constraints)))))

(defun ai-code-constraints-apply-bundle (bundle-name)
  "Apply constraint bundle BUNDLE-NAME to current session.
Fetches constraints from `ai-code--constraint-bundles' and merges
with existing session state, preserving other keys like :custom-suffix."
  (interactive
   (list (completing-read "Apply constraint bundle: "
                          (mapcar #'car ai-code--constraint-bundles)
                          nil t)))
  (let ((bundle-data (assoc bundle-name ai-code--constraint-bundles)))
    (unless bundle-data
      (user-error "Unknown constraint bundle: %s" bundle-name))
    (let* ((constraints (plist-get (cdr bundle-data) :constraints))
           (existing-state (ai-code--behaviors-get-state))
           (new-state (plist-put (copy-sequence existing-state)
                                  :constraint-modifiers constraints)))
      (ai-code--behaviors-set-state new-state)
      (ai-code--behaviors-set-active-bundle bundle-name)
      (ai-code--constraints-save-to-project constraints)
      (ai-code--behaviors-update-mode-line)
      (message "Applied constraint bundle: %s (%s)"
               bundle-name
               (mapconcat #'identity constraints ", ")))))

(defun ai-code-constraints-auto-detect-and-apply ()
  "Auto-detect constraints from project and apply to session.
Useful after cloning a project or switching contexts.
Preserves other session state like :custom-suffix.
Clears any active constraint bundle since auto-detect takes precedence."
  (interactive)
  (let ((detected (ai-code--constraints-auto-detect)))
    (if detected
        (let* ((existing-state (ai-code--behaviors-get-state))
               (new-state (plist-put (copy-sequence existing-state)
                                      :constraint-modifiers detected)))
          (ai-code--behaviors-clear-active-bundle)
          (ai-code--behaviors-set-state new-state)
          (ai-code--constraints-save-to-project detected)
          (ai-code--behaviors-update-mode-line)
          (message "Auto-detected constraints: %s"
                   (mapconcat #'identity detected ", ")))
      (message "No constraints detected from project config"))))

(defun ai-code-constraints-list ()
  "List all available constraints with descriptions.
Shows both individual constraints and bundles."
  (interactive)
  (let ((buf (get-buffer-create "*ai-code-constraints*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== CONSTRAINT MODIFIERS ===\n\n")
      (dolist (entry ai-code--constraint-modifiers)
        (insert (format "#%-20s %s\n" (car entry) (cdr entry))))
      (insert "\n=== CONSTRAINT BUNDLES ===\n\n")
      (dolist (entry ai-code--constraint-bundles)
        (let* ((name (car entry))
               (data (cdr entry))
               (constraints (plist-get data :constraints))
               (desc (plist-get data :description)))
          (insert (format "@%-20s %s\n" name desc))
          (insert (format "  Constraints: %s\n\n"
                          (mapconcat #'identity constraints ", ")))))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun ai-code-constraints-clear ()
  "Clear all constraints from current session.
Preserves other session state like :mode, :modifiers, and :custom-suffix."
  (interactive)
  (let* ((existing-state (ai-code--behaviors-get-state))
         (new-state (plist-put (copy-sequence existing-state)
                               :constraint-modifiers nil)))
    (ai-code--behaviors-set-state new-state)
    (ai-code--behaviors-clear-active-bundle)
    (let ((path (ai-code--constraints-get-persistence-path)))
      (when (and path (file-exists-p path))
        (delete-file path)))
    (ai-code--behaviors-update-mode-line)
    (message "Cleared all constraints")))

(defun ai-code-constraints-select ()
  "Interactively select and apply a constraint or constraint bundle.
Shows only constraints and bundles, not presets or behaviors."
  (interactive)
  (let ((candidates nil))
    (dolist (bundle ai-code--constraint-bundles)
      (let* ((name (concat "@" (car bundle)))
             (desc (plist-get (cdr bundle) :description))
             (display (format "%-15s %s" name (or desc ""))))
        (push (cons display (cons 'bundle (car bundle))) candidates)))
    (dolist (constraint ai-code--constraint-modifiers)
      (let* ((name (concat "#" (car constraint)))
             (desc (cdr constraint))
             (display (format "%-15s %s" name (truncate-string-to-width desc 40 nil nil t))))
        (push (cons display (cons 'constraint (car constraint))) candidates)))
    (setq candidates (nreverse candidates))
    (let ((selection (completing-read "Add constraint: " candidates nil t)))
      (when (and selection (not (string-empty-p selection)))
        (let ((value (cdr (assoc selection candidates))))
          (when (and value (consp value))
            (pcase (car value)
              ('constraint
               (let* ((existing (ai-code--behaviors-get-state))
                      (behaviors (or existing '(:mode nil :modifiers nil :constraint-modifiers nil)))
                      (current-constraints (plist-get behaviors :constraint-modifiers))
                      (new-constraints (delete-dups (cons (cdr value) current-constraints)))
                      (updated (plist-put (copy-tree behaviors) :constraint-modifiers new-constraints)))
                 (ai-code--behaviors-set-state updated)
                 (ai-code--behaviors-update-mode-line)
                 (message "Constraint added: %s" (cdr value))))
              ('bundle (ai-code-constraints-apply-bundle (cdr value)))
              (_ nil))))))))

(defun ai-code--all-constraint-names ()
  "Return all constraint names including bundles for completion."
  (append (mapcar (lambda (c) (concat "#" (car c))) ai-code--constraint-modifiers)
          (ai-code--constraint-bundle-names)))

(provide 'ai-code-behaviors)

;;; ai-code-behaviors.el ends here
