;;; ai-code-behaviors.el --- Behavior injection system for AI prompts -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
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

(defvar ai-code--behaviors-cache (make-hash-table :test #'equal)
  "Cache for loaded behavior prompts.")

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

(declare-function ai-code--git-root "ai-code-file" (&optional dir))

(defun ai-code--behaviors-project-root ()
  "Return git root for current project, or default-directory if not in a repo."
  (or (and (fboundp 'ai-code--git-root) (ai-code--git-root))
      default-directory))

(defun ai-code--behaviors--get (key)
  "Get entry KEY from session states."
  (plist-get (or (gethash (ai-code--behaviors-project-root) 
                          ai-code--behaviors-session-states)
                  '(:state nil :preset nil))
             key))

(defun ai-code--behaviors--set (key value)
  "Set entry KEY to VALUE in session states."
  (let* ((root (ai-code--behaviors-project-root))
         (entry (or (gethash root ai-code--behaviors-session-states)
                    '(:state nil :preset nil))))
    (puthash root (plist-put (copy-tree entry) key value)
             ai-code--behaviors-session-states)
    value))

(defun ai-code--behaviors-get-state ()
  "Get current behavior state for this project."
  (ai-code--behaviors--get :state))

(defun ai-code--behaviors-set-state (state)
  "Set behavior STATE for this project."
  (ai-code--behaviors--set :state state))

(defun ai-code--behaviors-get-preset ()
  "Get current preset name for this project."
  (ai-code--behaviors--get :preset))

(defun ai-code--behaviors-set-preset (preset)
  "Set preset name to PRESET for this project."
  (ai-code--behaviors--set :preset preset))

(defun ai-code--behaviors-clear-state ()
  "Clear behavior state for current project."
  (remhash (ai-code--behaviors-project-root) ai-code--behaviors-session-states))

(defconst ai-code--behavior-operating-modes
  '("=code" "=debug" "=research" "=review" "=spec" "=test"
    "=mentor" "=assess" "=record" "=drive" "=navigate" "=probe")
  "Operating mode behaviors. Only one can be active at a time.")

(defconst ai-code--behavior-modifiers
  '("deep" "wide" "ground" "negative-space" "challenge" "steel-man"
    "user-lens" "concise" "first-principles" "creative" "subtract"
    "meta" "simulate" "decompose" "recursive" "fractal" "tdd"
    "io" "contract" "backward" "analogy" "temporal" "name")
  "Modifier behaviors. Multiple can be active simultaneously.")

(defconst ai-code--constraint-modifiers
  '(("chinese" . "Reply in Simplified Chinese, use English in code files and comments")
    ("english" . "Reply in English")
    ("test-after" . "Run unit-tests after code changes and follow up on test results")
    ("strict-lint" . "Run linter before considering code complete, fix all lint errors")
    ("no-comments" . "Do not add comments to code unless explicitly requested")
    ("doc-comments" . "Add docstrings/documentation comments to all public functions")
    ("strict-types" . "Use strict type annotations, avoid 'any' or dynamic types")
    ("functional" . "Prefer functional programming style: pure functions, no mutations, immutability")
    ("defensive" . "Add input validation and error handling to all public functions")
    ("secure" . "Review for security vulnerabilities: sanitize inputs, avoid injection risks")
    ("performant" . "Optimize for performance: avoid unnecessary allocations, use efficient algorithms")
    ("minimal" . "Write minimal code: prefer built-in functions, no over-engineering, keep it simple"))
  "Built-in constraint modifiers with their template instructions.
These are lighter-weight than repo behaviors and cover common constraints.")

(defconst ai-code--behavior-presets
  '(("tdd-dev" . (:mode "=code" :modifiers ("tdd" "deep") 
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
            (let* ((remote-head (string-trim 
                                 (shell-command-to-string 
                                  "git rev-parse '@{u}' 2>/dev/null")))
                   (local-head (string-trim 
                                (shell-command-to-string 
                                 "git rev-parse HEAD 2>/dev/null"))))
              (cond
               ((string-empty-p remote-head) 'no-remote)
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
      (condition-case nil
          (list :commit (string-trim 
                         (shell-command-to-string "git rev-parse --short HEAD 2>/dev/null"))
                :date (string-trim 
                       (shell-command-to-string "git log -1 --format=%ci HEAD 2>/dev/null")))
        (error nil)))))

(defun ai-code--behavior-file-path (behavior-name)
  "Return path to prompt.md for BEHAVIOR-NAME."
  (expand-file-name
   (format "behaviors/%s/prompt.md" behavior-name)
   (expand-file-name ai-code-behaviors-repo-path)))

(defun ai-code--load-behavior-prompt (behavior-name)
  "Load and cache the prompt content for BEHAVIOR-NAME.
Return the prompt content string, or nil if not found."
  (let ((cached (gethash behavior-name ai-code--behaviors-cache)))
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
            (puthash behavior-name content ai-code--behaviors-cache))
          content)))))

(defun ai-code--all-behavior-names ()
  "Return list of all available behavior names including presets and constraints."
  (append (ai-code--behavior-preset-names)
          (mapcar (lambda (m) (concat "#" m)) ai-code--behavior-operating-modes)
          (mapcar (lambda (m) (concat "#" m)) ai-code--behavior-modifiers)
          (mapcar (lambda (c) (concat "#" (car c))) ai-code--constraint-modifiers)))

(defun ai-code--behavior-preset-names ()
  "Return list of all preset names with @ prefix for completion."
  (mapcar (lambda (p) (concat "@" (car p))) ai-code--behavior-presets))

(defun ai-code--behavior-preset-capf ()
  "Completion-at-point function for @preset names.
Add to `completion-at-point-functions' in prompt buffers."
  (when (and (boundp 'major-mode)
             (eq major-mode 'ai-code-prompt-mode)
             (save-excursion
               (skip-chars-backward "a-zA-Z0-9_-")
               (eq (char-before) ?@)))
    (let ((start (1- (point)))
          (end (point)))
      (list start end (ai-code--behavior-preset-names) :exclusive 'no))))

(defun ai-code--behavior-setup-preset-completion ()
  "Add preset completion to prompt mode buffers."
  (add-hook 'completion-at-point-functions #'ai-code--behavior-preset-capf nil t))

(defun ai-code--behavior-teardown-preset-completion ()
  "Remove preset completion from prompt mode buffers."
  (remove-hook 'completion-at-point-functions #'ai-code--behavior-preset-capf t))

(defun ai-code--behavior-merge-preset-candidates (candidates)
  "Append preset names to CANDIDATES for @ completion.
This allows preset names to appear alongside file paths in the
auto-triggered completion from `ai-code--prompt-auto-trigger-filepath-completion'."
  (append candidates (ai-code--behavior-preset-names)))

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

(defun ai-code--extract-and-remove-hashtags (prompt-text)
  "Extract behaviors and remove hashtags from PROMPT-TEXT in single pass.
Return cons cell (BEHAVIORS . CLEANED-PROMPT) where BEHAVIORS is plist
(:mode MODE :modifiers MODS :constraint-modifiers CONSTRAINTS :preset PRESET) or nil.
PRESET is the name of a preset detected via @preset-name syntax."
  (let ((mode nil)
        (modifiers nil)
        (constraints nil)
        (preset nil)
        (unknown nil)
        (unknown-presets nil)
        (valid-tags (append ai-code--behavior-operating-modes
                            ai-code--behavior-modifiers
                            (mapcar #'car ai-code--constraint-modifiers)))
        (result prompt-text))
    (save-match-data
      (with-temp-buffer
        (insert prompt-text)
        (goto-char (point-min))
        (while (re-search-forward "@\\([a-zA-Z0-9_-]+\\)" nil t)
          (let ((preset-name (match-string 1)))
            (if (assoc preset-name ai-code--behavior-presets)
                (if preset
                    (message "Warning: Multiple presets, keeping @%s" preset)
                  (setq preset preset-name))
              (cl-pushnew preset-name unknown-presets :test #'equal))))
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
        (goto-char (point-min))
        (while (re-search-forward "@\\([a-zA-Z0-9_-]+\\)\\s-*" nil t)
          (let ((name (match-string 1)))
            (when (assoc name ai-code--behavior-presets)
              (replace-match ""))))
        (goto-char (point-min))
        (dolist (tag valid-tags)
          (goto-char (point-min))
          (while (re-search-forward (concat "#" (regexp-quote tag) "\\s-*") nil t)
            (replace-match "")))
        (setq result (string-trim (buffer-string)))))
    (cons (when (or mode modifiers constraints preset)
            (list :mode mode
                  :modifiers (nreverse modifiers)
                  :constraint-modifiers (nreverse constraints)
                  :preset preset))
          result)))

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
     (message "GPTel classification failed: %s" (error-message-string err))
     nil)))

(defun ai-code--extract-json-from-response (response)
  "Extract first balanced JSON object from RESPONSE string.
Returns parsed plist or nil if no valid JSON found."
  (save-match-data
    (let ((trimmed (string-trim response)))
      (cond
       ((string-match-p "\\`[[:space:]]*{" trimmed)
        (condition-case nil
            (json-read-from-string trimmed)
          (error nil)))
       ((string-match "{" trimmed)
        (let ((start (match-beginning 0))
              (depth 0)
              (i (match-beginning 0))
              (len (length trimmed))
              (in-string nil)
              (escape-next nil))
          (while (and (< i len) (>= depth 0))
            (let ((ch (aref trimmed i)))
              (cond
               (escape-next (setq escape-next nil))
               ((eq ch ?\\) (setq escape-next t))
               (in-string (when (eq ch ?\") (setq in-string nil)))
               ((eq ch ?\") (setq in-string t))
               ((not in-string)
                (cond ((eq ch ?{) (setq depth (1+ depth)))
                      ((eq ch ?}) (setq depth (1- depth)))))))
            (setq i (1+ i)))
          (when (= depth 0)
            (condition-case nil
                (json-read-from-string (substring trimmed start i))
              (error nil)))))
       (t nil)))))

(defun ai-code--classify-prompt-intent-keywords (prompt-text)
  "Classify PROMPT-TEXT intent using keyword matching.
Return list suitable for behavior injection."
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
      (list :mode (symbol-name (car best-entry))
            :modifiers (delete-dups modifiers)))))

(defun ai-code--classify-prompt-intent (prompt-text)
  "Classify PROMPT-TEXT intent for behavior injection.
Uses GPTel if available, falls back to keyword matching.
Return list of (:mode MODE :modifiers MODIFIERS)."
  (or (and (bound-and-true-p ai-code-use-gptel-classify-prompt)
           (ai-code--classify-prompt-intent-gptel prompt-text))
      (ai-code--classify-prompt-intent-keywords prompt-text)))

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

(defun ai-code--process-behaviors (prompt-text)
  "Process behaviors for PROMPT-TEXT and return modified prompt.
This is the main entry point for behavior injection.
1. Extract explicit #hashtags and @preset from prompt
2. If preset found, apply preset (merged with any additional modifiers)
3. If no preset but hashtags, use explicit behaviors
4. If no hashtags, check session state for persisted behaviors
5. If no session state and auto-classify is enabled, classify intent
Returns the modified prompt with behaviors injected, or the original
PROMPT-TEXT if no behaviors apply.
Note: Preset-only prompts (empty after tag removal) are handled by
`ai-code--behaviors-check-preset-only-prompt' in the advice layer."
  (if (not ai-code-behaviors-enabled)
      prompt-text
    (let* ((extracted (ai-code--extract-and-remove-hashtags prompt-text))
           (explicit-behaviors (car extracted))
           (cleaned-prompt (cdr extracted))
           (session-state (ai-code--behaviors-get-state)))
      (cond
       (explicit-behaviors
        (let* ((preset-name (plist-get explicit-behaviors :preset))
               (final-behaviors (ai-code--merge-preset-with-modifiers preset-name explicit-behaviors)))
          (ai-code--behaviors-set-preset preset-name)
          (ai-code--behaviors-set-state final-behaviors)
          (ai-code--behaviors-update-mode-line)
          (let ((instruction (ai-code--build-behavior-instruction final-behaviors)))
            (if instruction
                (format "%s\n\n<user-prompt>\n%s\n</user-prompt>"
                        instruction cleaned-prompt)
              cleaned-prompt))))
       (session-state
        (let ((instruction (ai-code--build-behavior-instruction session-state)))
          (if instruction
              (format "%s\n\n<user-prompt>\n%s\n</user-prompt>"
                      instruction (string-trim prompt-text))
            prompt-text)))
((when-let ((classified (and ai-code-behaviors-auto-classify
                                    (ai-code--classify-prompt-intent prompt-text))))
           (let ((final-behaviors (ai-code--merge-preset-with-modifiers nil classified)))
             (ai-code--behaviors-set-preset nil)
             (ai-code--behaviors-set-state final-behaviors)
             (ai-code--behaviors-update-mode-line)
             (message "Auto-classified: #%s" (or (plist-get final-behaviors :mode) "unknown"))
             (let ((instruction (ai-code--build-behavior-instruction final-behaviors)))
              (if instruction
                  (format "%s\n\n<user-prompt>\n%s\n</user-prompt>"
                          instruction (string-trim prompt-text))
                prompt-text)))))
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
  "Clear active behaviors for current project."
  (interactive)
  (ai-code--behaviors-clear-state)
  (ai-code--behaviors-update-mode-line)
  (message "Behaviors cleared for current project"))

(defun ai-code-behaviors-clear-all ()
  "Clear behaviors for all projects."
  (interactive)
  (clrhash ai-code--behaviors-session-states)
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
      "No behaviors active\n\nmouse-1: Select preset\nmouse-3: Actions"
    (let* ((mode (plist-get state :mode))
           (modifiers (plist-get state :modifiers))
           (constraints (plist-get state :constraint-modifiers))
           (custom-suffix (plist-get state :custom-suffix))
           (preset-desc (when preset 
                          (plist-get (cdr (assoc preset ai-code--behavior-presets)) 
                                     :description)))
           (lines nil))
      (push "" lines)
      (push "mouse-3: Actions" lines)
      (push "mouse-1: Select preset" lines)
      (when custom-suffix
        (push "+custom-suffix" lines))
      (when constraints
        (push (format "Constraints: %s" 
                      (mapconcat (lambda (c) (concat "#" c)) constraints " "))
              lines))
      (when modifiers
        (push (format "Modifiers: %s" 
                      (mapconcat (lambda (m) (concat "#" m)) modifiers " "))
              lines))
      (when mode
        (push (format "Mode: #%s" mode) lines))
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
                       (string-trim
                        (shell-command-to-string
                         "git rev-parse --abbrev-ref HEAD 2>/dev/null"))))))
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
  "Show preset selection popup menu.
EVENT is the mouse event."
  (interactive)
  (let ((menu (make-sparse-keymap "Select Preset")))
    (define-key menu [clear]
      '(menu-item "Clear behaviors" ai-code-behaviors-clear))
    (define-key menu [sep] '(menu-item "--"))
    (dolist (p (reverse ai-code--behavior-presets))
      (define-key menu (vector (intern (car p)))
        `(menu-item ,(format "@%s - %s" (car p)
                             (plist-get (cdr p) :description))
                    (lambda () (interactive)
                      (ai-code-behaviors-apply-preset ,(car p))))))
    (if event
        (popup-menu menu event)
      (popup-menu menu))))

(defun ai-code-behaviors-mode-line-actions (&optional event)
  "Show behavior actions popup menu.
EVENT is the mouse event."
  (interactive)
  (let ((menu (make-sparse-keymap "Actions"))
        (preset (ai-code--behaviors-get-preset)))
    (define-key menu [disable]
      '(menu-item "Disable mode-line indicator" 
                  ai-code-behaviors-mode-line-disable))
    (define-key menu [sep2] '(menu-item "--"))
    (define-key menu [clear-all]
      '(menu-item "Clear all projects" ai-code-behaviors-clear-all))
    (define-key menu [update]
      '(menu-item "Update behavior repo" ai-code-behaviors-install))
    (define-key menu [sep1] '(menu-item "--"))
    (define-key menu [add-constraint]
      '(menu-item "Add constraint..." ai-code-behaviors-select))
    (when preset
      (define-key menu [describe]
        `(menu-item "Describe current behavior"
                    (lambda () (interactive)
                      (ai-code-describe-behavior ,preset)))))
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
           (mode (and state (plist-get state :mode)))
           (modifiers (and state (plist-get state :modifiers)))
           (constraints (and state (plist-get state :constraint-modifiers)))
           (has-custom (and state (plist-get state :custom-suffix)))
           (constraint-count (+ (length constraints) (if has-custom 1 0)))
           (face (ai-code--behaviors-get-mode-face mode))
           (text (cond
                  ((and preset (> constraint-count 0))
                   (format "[@%s +%d]" preset constraint-count))
                  (preset (format "[@%s]" preset))
                  ((or mode modifiers constraints has-custom)
                   (concat "["
                           (or mode "")
                           (when (and mode modifiers) " ")
                           (when modifiers (mapconcat #'identity modifiers " "))
                           (when (> constraint-count 0)
                             (format " +%d" constraint-count))
                           "]"))))
           (tooltip (ai-code--behaviors-build-tooltip preset state)))
      (when text
        (propertize text
                    'face face
                    'mouse-face 'mode-line-highlight
                    'help-echo tooltip
                    'local-map ai-code--behaviors-mode-line-map)))))

(defun ai-code--behaviors-update-mode-line ()
  "Update mode-line with current behavior indicator."
  (force-mode-line-update t))

(defun ai-code-describe-behavior (behavior-name)
  "Display documentation for BEHAVIOR-NAME.
Shows the behavior's README.md in a help buffer, or constraint description.
BEHAVIOR-NAME should not include the # or @ prefix."
  (interactive
   (let* ((presets (mapcar (lambda (p) (concat "@" (car p))) ai-code--behavior-presets))
          (modes (mapcar (lambda (m) (concat "#" m)) ai-code--behavior-operating-modes))
          (modifiers (mapcar (lambda (m) (concat "#" m)) ai-code--behavior-modifiers))
          (constraints (mapcar (lambda (c) (concat "#" (car c))) ai-code--constraint-modifiers))
          (all-behaviors (append presets modes modifiers constraints))
          (input (completing-read "Describe behavior: " all-behaviors nil t)))
     (list (when (string-match "[#@]\\(.+\\)" input) (match-string 1 input)))))
  (if (not behavior-name)
      (message "No behavior selected")
    (let ((constraint-desc (cdr (assoc behavior-name ai-code--constraint-modifiers))))
      (if constraint-desc
          (with-help-window (help-buffer)
            (princ (format "#%s\n\n" behavior-name))
            (princ constraint-desc))
        (let ((content (ai-code--load-behavior-readme behavior-name)))
          (if (not content)
              (message "No documentation found for %s" behavior-name)
            (with-help-window (help-buffer)
              (princ (format "#%s\n\n" behavior-name))
              (princ content))))))))

(defun ai-code--behavior-annotated-candidates ()
  "Return completion candidates with annotations.
Returns list of (DISPLAY . VALUE) pairs where DISPLAY includes annotation.
Includes presets, operating modes, modifiers, and constraint modifiers."
  (let ((candidates nil))
    (when ai-code--behavior-presets
      (dolist (preset ai-code--behavior-presets)
        (let* ((name (concat "@" (car preset)))
               (desc (plist-get (cdr preset) :description))
               (display (format "%-15s %s" name (or desc ""))))
          (push (cons display (cons 'preset (car preset))) candidates)))
      (push (cons "─── Presets ───" "") candidates))
    (when (ai-code--behaviors-repo-available-p)
      (dolist (mode ai-code--behavior-operating-modes)
        (let* ((name (concat "#" mode))
               (annotation (ai-code--extract-behavior-annotation mode)))
          (push (cons (if annotation (format "%-15s %s" name annotation) name) 
                      (cons 'behavior name)) candidates)))
      (push (cons "─── Modifiers ───" "") candidates)
      (dolist (mod ai-code--behavior-modifiers)
        (let* ((name (concat "#" mod))
               (annotation (ai-code--extract-behavior-annotation mod)))
          (push (cons (if annotation (format "%-15s %s" name annotation) name) 
                      (cons 'behavior name)) candidates))))
    (when ai-code--constraint-modifiers
      (push (cons "─── Constraints ───" "") candidates)
      (dolist (constraint ai-code--constraint-modifiers)
        (let* ((name (concat "#" (car constraint)))
               (desc (cdr constraint))
               (display (format "%-15s %s" name (truncate-string-to-width desc 40 nil nil t))))
          (push (cons display (cons 'constraint (car constraint))) candidates))))
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
  "Select and apply a behavior preset."
  (interactive)
  (let* ((presets (mapcar (lambda (p) 
                            (cons (format "%-15s %s" 
                                         (car p) 
                                         (plist-get (cdr p) :description))
                                  (car p)))
                          ai-code--behavior-presets))
         (choice (completing-read "Select preset: " presets nil t)))
    (when (and choice (not (string-empty-p choice)))
      (let ((preset-name (cdr (assoc choice presets))))
        (when preset-name
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
             (let* ((extracted (car (ai-code--extract-and-remove-hashtags (cdr value))))
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
            (_ nil)))))))

(defun ai-code-behaviors-mode-line-enable ()
  "Enable mode-line display of active behaviors."
  (interactive)
  (unless (member '(:eval (ai-code--behaviors-mode-line-string)) mode-line-misc-info)
    (setq mode-line-misc-info
          (append mode-line-misc-info
                  (list '(:eval (ai-code--behaviors-mode-line-string))))))
  (ai-code--behaviors-update-mode-line))

(defun ai-code-behaviors-mode-line-disable ()
  "Disable mode-line display of active behaviors."
  (interactive)
  (setq mode-line-misc-info
        (delete '(:eval (ai-code--behaviors-mode-line-string)) mode-line-misc-info))
  (force-mode-line-update t))

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
         (eca-session)))
   
   ;; agent-shell backend - use agent-shell--shell-buffer
   ((and (boundp 'ai-code-selected-backend)
         (eq ai-code-selected-backend 'agent-shell))
    (and (fboundp 'agent-shell--shell-buffer)
         (agent-shell--shell-buffer :no-create t :no-error t)))
   
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
           (explicit-behaviors (car extracted))
           (cleaned-prompt (cdr extracted)))
      (when (and explicit-behaviors
                 (string-empty-p (string-trim cleaned-prompt)))
        (let* ((preset-name (plist-get explicit-behaviors :preset))
               (final-behaviors (ai-code--merge-preset-with-modifiers preset-name explicit-behaviors)))
          (ai-code--behaviors-set-preset preset-name)
          (ai-code--behaviors-set-state final-behaviors)
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
  (ai-code-behaviors-mode-line-enable)
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
  (ai-code-behaviors-mode-line-disable)
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

(provide 'ai-code-behaviors)

;;; ai-code-behaviors.el ends here
