;;; ai-code-git.el --- Git operations for AI Code -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides git operation functionality for the AI Code package.

;;; Code:

(require 'magit)

(require 'ai-code-input)
(require 'ai-code-prompt-mode)

(declare-function helm-gtags-create-tags "helm-gtags" (dir &optional label))

(defcustom ai-code-init-project-gtags-label "pygments"
  "Default label passed to Helm-Gtags when initializing a project.
Candidate values:
- \\='default
- \\='native
- \\='ctags
- \\='new-ctags
- \\='pygments"
  :type 'string
  :group 'ai-code)

(declare-function ai-code--insert-prompt "ai-code-prompt-mode" (prompt-text))

;;;###autoload
(defun ai-code-pull-or-review-diff-file ()
  "Review a diff file with AI Code or generate one if not viewing a diff.
If current buffer is a .diff file, ask AI Code to review it.
Otherwise, generate the diff."
  (interactive)
  (if (and buffer-file-name (string-match-p "\\.diff$" buffer-file-name))
      (let* ((file-name (file-name-nondirectory buffer-file-name))
             (init-prompt (format "Code review for %s. Use relevant file in repository as context.

**Focus**: Quality, security, performance, patterns
**Format**: Location, issue, solution, priority (High/Medium/Low)

Provide overall assessment." file-name))
             (prompt (ai-code-read-string "Enter diff review prompt: " init-prompt)))
        (ai-code--insert-prompt prompt))
    (ai-code--magit-generate-feature-branch-diff-file)))

(defun ai-code--validate-git-repository ()
  "Validate that current directory is in a git repository.
Return the git root directory if valid, otherwise signal an error."
  (let ((git-root (magit-toplevel)))
    (if git-root
        git-root
      (user-error "Not in a git repository"))))

(defun ai-code--validate-buffer-file ()
  "Validate that current buffer is visiting a file.
Signal an error if not visiting a file."
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file")))

(defun ai-code--get-full-branch-ref (branch)
  "Get full reference for BRANCH, handling remote branches properly.
Prefer remote branch (origin/BRANCH) if it exists.
Otherwise, use local branch or ref.
Git can diff remote branches directly without checking them out locally."
  (cond
   ;; Check if it exists as a remote branch first, unless 'branch' already starts with 'origin/'
   ((and (not (string-prefix-p "origin/" branch)) (magit-branch-p (concat "origin/" branch)))
    (concat "origin/" branch))
   ;; Then check if it's a valid local branch or ref (this will also handle cases like "origin/main" directly)
   ((or (magit-branch-p branch) (magit-rev-verify branch))
    branch)
   ;; Return as is (might be a commit hash or special ref if not caught above)
   (t branch)))

(defun ai-code--verify-branches (base-branch feature-branch)
  "Verify that BASE-BRANCH and FEATURE-BRANCH exist.
Signal an error if either branch doesn't exist."
  ;; Verify base branch exists
  (unless (or (magit-branch-p base-branch)
              (magit-branch-p (concat "origin/" base-branch))
              (magit-rev-verify base-branch))
    (user-error "Base branch '%s' not found locally or in remotes" base-branch))
  ;; Verify feature branch exists (if not HEAD)
  (when (and (not (string= feature-branch "HEAD"))
             (not (magit-branch-p feature-branch))
             (not (magit-branch-p (concat "origin/" feature-branch)))
             (not (magit-rev-verify feature-branch)))
    (user-error "Feature branch '%s' not found locally or in remotes" feature-branch)))

(defun ai-code--generate-staged-diff (diff-file)
  "Generate diff for staged (staged) change and save to DIFF-FILE."
  (message "Generating diff for staged (staged) changes...")
  (magit-run-git "diff" "--cached" (concat "--output=" diff-file)))

(defun ai-code--resolve-diff-branches (type input-base-branch input-feature-branch &optional branch-scope)
  "Resolve base and feature branches for diff generation.
TYPE is `'commit`, `'base-vs-head`, or `'branch-range`.
INPUT-BASE-BRANCH and INPUT-FEATURE-BRANCH are user-provided names.
BRANCH-SCOPE is `'local` or `'remote`, used for `'branch-range`.
Returns a cons cell (RESOLVED-BASE . RESOLVED-FEATURE)."
  (let (resolved-base-branch resolved-feature-branch)
    (pcase type
      ('commit
       ;; Input is already commit^ and commit
       (setq resolved-base-branch input-base-branch)
       (setq resolved-feature-branch input-feature-branch))
      ('base-vs-head
       ;; Base branch can be local or remote, feature is HEAD
       (setq resolved-base-branch (ai-code--get-full-branch-ref input-base-branch))
       (setq resolved-feature-branch "HEAD")) ; HEAD is always resolved correctly by git
      ('branch-range
       (pcase branch-scope
         ('local
          ;; User asserts branches are local
          (setq resolved-base-branch input-base-branch)
          (setq resolved-feature-branch input-feature-branch))
         ('remote
          ;; For remote scope, explicitly try to use origin/ prefixed branches
          ;; Add origin/ prefix if not already present
          (setq resolved-base-branch
                (if (string-prefix-p "origin/" input-base-branch)
                    input-base-branch
                  (concat "origin/" input-base-branch)))
          (setq resolved-feature-branch
                (if (string-prefix-p "origin/" input-feature-branch)
                    input-feature-branch
                  (concat "origin/" input-feature-branch))))
         (_ ; Default or unknown scope, fallback to smart resolution (should not happen with prompt)
          (setq resolved-base-branch (ai-code--get-full-branch-ref input-base-branch))
          (setq resolved-feature-branch (ai-code--get-full-branch-ref input-feature-branch))))))
    (cons resolved-base-branch resolved-feature-branch)))

(defun ai-code--generate-branch-or-commit-diff (diff-params diff-file)
  "Generate diff based on DIFF-PARAMS and save to DIFF-FILE.
DIFF-PARAMS is a plist with :type (`'commit`, `'base-vs-head`, `'branch-range`),
:base-branch, :feature-branch, :diff-file-name-part, and optionally
:branch-scope."
  (let* ((type (plist-get diff-params :type))
         (input-base-branch (plist-get diff-params :base-branch))
         (input-feature-branch (plist-get diff-params :feature-branch))
         (branch-scope (plist-get diff-params :branch-scope)) ; Might be nil for 'commit' or 'base-vs-head'
         (diff-file-name-part (plist-get diff-params :diff-file-name-part))

         (resolved-branches (ai-code--resolve-diff-branches type input-base-branch input-feature-branch branch-scope))
         (resolved-base-branch (car resolved-branches))
         (resolved-feature-branch (cdr resolved-branches)))

    (message "Fetching from all remotes to ensure latest branches...")
    (magit-run-git "fetch" "--all")
    ;; Verify input branches for relevant types
    (when (memq type '(base-vs-head branch-range))
      (ai-code--verify-branches input-base-branch input-feature-branch))
    ;; Display message about what we're doing
    (pcase type
      ('commit
       (message "Generating diff for single commit: %s" diff-file-name-part))
      ('base-vs-head
       (message "Generating diff between %s and HEAD" resolved-base-branch))
      ('branch-range
       (message "Generating diff between branches: %s..%s (%s)"
                resolved-base-branch resolved-feature-branch (or branch-scope "unknown-scope"))))
    (when (magit-anything-modified-p)
      (message "Repository has uncommitted changes. You might want to commit or stash them first.")
      (sleep-for 1))
    (message "Generating diff file: %s" diff-file)
    (magit-run-git "diff" (concat resolved-base-branch ".." resolved-feature-branch)
                   (concat "--output=" diff-file))))

(defun ai-code--open-diff-file (diff-file)
  "Open the generated DIFF-FILE."
  (find-file diff-file)
  (message "Generated diff file: %s" diff-file))

(defun ai-code--handle-staged-diff-generation (git-root)
  "Handle generation of diff for staged change.
GIT-ROOT is the root directory of the Git repository."
  (let* ((diff-file-name-part "staged")
         (diff-file (expand-file-name (concat diff-file-name-part ".diff") git-root)))
    (ai-code--generate-staged-diff diff-file)
    diff-file))

(defun ai-code--handle-base-vs-head-diff-generation (git-root
                                                      &optional
                                                      open-in-browser)
  "Handle generation of diff between a base branch and HEAD.
GIT-ROOT is the root directory of the Git repository.
If OPEN-IN-BROWSER is non-nil, only open the diff in GitHub web
interface without generating file."
  (let* ((base-branch (ai-code-read-string "Base branch name: "))
         (current-branch (magit-get-current-branch)))
    (if open-in-browser
        (progn
          (ai-code--open-git-web-compare base-branch (or current-branch "HEAD"))
          nil)  ; Return nil since no file was generated
      (let* ((diff-file-name-part (concat (replace-regexp-in-string "/" "-" base-branch) ".HEAD"))
             (diff-file (expand-file-name (concat diff-file-name-part ".diff") git-root))
             (diff-params (list :type 'base-vs-head
                                :base-branch base-branch
                                :feature-branch "HEAD"
                                :diff-file-name-part diff-file-name-part)))
        (ai-code--generate-branch-or-commit-diff diff-params diff-file)
        diff-file))))

(defun ai-code--handle-branch-range-diff-generation (git-root
                                                      &optional
                                                      open-in-browser)
  "Handle generation of diff between a base and a feature branch.
GIT-ROOT is the root directory of the Git repository.
If OPEN-IN-BROWSER is non-nil, only open the diff in GitHub web
interface without generating file."
  (let* ((base-branch (ai-code-read-string "Base branch name: "))
         (feature-branch (ai-code-read-string "Feature branch name: ")))
    (if open-in-browser
        (progn
          (ai-code--open-git-web-compare base-branch feature-branch)
          nil)  ; Return nil since no file was generated
      (let* ((branch-scope)
             (scope-alist '(("Local" . local)
                            ("Remote (will prefix with 'origin/')" . remote)))
             (raw-scope-choice (completing-read "Are branches local or remote? "
                                                scope-alist
                                                nil t nil nil "Local")))
        (setq branch-scope
              (if (consp raw-scope-choice)
                  (cdr raw-scope-choice)
                (cdr (assoc raw-scope-choice scope-alist))))
        (let* ((diff-file-name-part (concat (replace-regexp-in-string "/" "-" base-branch)
                                           "."
                                           (replace-regexp-in-string "/" "-" feature-branch)))
               (diff-file (expand-file-name (concat diff-file-name-part ".diff") git-root))
               (diff-params (list :type 'branch-range
                                   :base-branch base-branch
                                   :feature-branch feature-branch
                                   :branch-scope branch-scope
                                   :diff-file-name-part diff-file-name-part)))
          (ai-code--generate-branch-or-commit-diff diff-params diff-file)
          diff-file)))))

(defun ai-code--handle-commit-diff-generation (git-root
                                                &optional open-in-browser)
  "Handle generation of diff for a single commit.
GIT-ROOT is the root directory of the Git repository.
If OPEN-IN-BROWSER is non-nil, only open the commit in GitHub web
interface without generating file."
  (let* ((commit-hash (ai-code-read-string "Commit hash: ")))
    (if open-in-browser
        (progn
          (ai-code--open-git-web-commit commit-hash)
          nil)  ; Return nil since no file was generated
      (let* ((base-branch (concat commit-hash "^"))  ; Diff against parent
             (feature-branch commit-hash)
             (diff-file-name-part commit-hash)
             (diff-file (expand-file-name (concat diff-file-name-part ".diff") git-root))
             (diff-params (list :type 'commit
                                :base-branch base-branch
                                :feature-branch feature-branch
                                :diff-file-name-part diff-file-name-part)))
        (ai-code--generate-branch-or-commit-diff diff-params diff-file)
        diff-file))))

(defun ai-code--get-diff-type-choice ()
  "Get user's choice for diff type and return the corresponding value."
  (let* ((diff-type-alist '(("Staged changes" . staged)
                            ("Base branch vs HEAD" . base-vs-head)
                            ("Branch range (e.g., base..feature)" . branch-range)
                            ("Single commit" . commit)
                            ("Commit range (e.g., commitA..commitB)" . commit-range)))
         (raw-diff-type-choice
          (completing-read "Select diff type: "
                           diff-type-alist
                           nil t nil nil "Staged changes")))
    (if (consp raw-diff-type-choice)
        (cdr raw-diff-type-choice)
      ;; If raw-diff-type-choice is a string, look up its corresponding value
      (cdr (assoc raw-diff-type-choice diff-type-alist)))))

;;; New helper for commit ranges
(defun ai-code--handle-commit-range-diff-generation (git-root &optional open-in-browser)
  "Handle generation of diff between two commits (commit range).
GIT-ROOT is the repository root directory.
If OPEN-IN-BROWSER is non-nil, only open the diff in GitHub web interface
without generating file."
  (let* ((raw-start (ai-code-read-string "Start commit or branch: "))
         (raw-end   (ai-code-read-string "End commit or branch: "))
         ;; try to resolve remote branches or commits
         (start     (ai-code--get-full-branch-ref raw-start))
         (end       (ai-code--get-full-branch-ref raw-end)))
    (if open-in-browser
        (progn
          (ai-code--open-git-web-compare start end)
          nil)  ; Return nil since no file was generated
      (let* ((name      (format "%s..%s" start end))
             (file      (expand-file-name (concat name ".diff") git-root))
             ;; reuse branch-range plumbing (it will fetch and verify)
             (params    (list :type 'branch-range
                              :base-branch start
                              :feature-branch end
                              :diff-file-name-part name)))
        (ai-code--generate-branch-or-commit-diff params file)
        file))))

(defun ai-code--get-git-web-repo-url ()
  "Get Git repository web URL from git remote.
Returns the HTTPS URL of the repository for web browsing.
Supports GitHub, GitLab, Bitbucket, and other Git hosting services.
Returns nil if unable to construct a web URL."
  (let* ((remote-url (magit-git-string "config" "--get" "remote.origin.url")))
    (when (and remote-url (stringp remote-url) (not (string-empty-p remote-url)))
      (cond
       ;; HTTPS URL: https://host.com/user/repo.git or https://host.com/user/repo
       ((string-match "https://\\([^/]+\\)/\\(.+\\)" remote-url)
        (let ((host (match-string 1 remote-url))
              (path (match-string 2 remote-url)))
          (format "https://%s/%s" host (replace-regexp-in-string "\\.git$" "" path))))
       ;; SSH URL: git@host.com:user/repo.git or git@host.com:user/repo
       ((string-match "git@\\([^:]+\\):\\(.+\\)" remote-url)
        (let ((host (match-string 1 remote-url))
              (path (match-string 2 remote-url)))
          (format "https://%s/%s" host (replace-regexp-in-string "\\.git$" "" path))))
       (t nil)))))

(defun ai-code--open-git-web-compare (start end)
  "Open Git web compare page for START..END in browser.
Works with GitHub, GitLab, Bitbucket, and other Git hosting services."
  (let ((repo-url (ai-code--get-git-web-repo-url)))
    (if repo-url
        (let ((compare-url (format "%s/compare/%s...%s" repo-url start end)))
          (browse-url compare-url)
          (message "Opened web compare: %s" compare-url))
      (message "Unable to determine repository web URL"))))

(defun ai-code--open-git-web-commit (commit)
  "Open Git web commit page for COMMIT in browser.
Works with GitHub, GitLab, Bitbucket, and other Git hosting services."
  (let ((repo-url (ai-code--get-git-web-repo-url)))
    (if repo-url
        (let ((commit-url (format "%s/commit/%s" repo-url commit)))
          (browse-url commit-url)
          (message "Opened web commit: %s" commit-url))
      (message "Unable to determine repository web URL"))))

(defun ai-code--magit-generate-feature-branch-diff-file ()
  "Generate a diff file based on user-selected type (staged, branches, commit).
For non-staged diffs, user is prompted whether to open in browser."
  (when-let ((git-root (ai-code--validate-git-repository)))
    (let* ((selected-diff-type-value (ai-code--get-diff-type-choice))
           (open-in-browser (and (not (eq selected-diff-type-value 'staged))
                                 (y-or-n-p "Open diff in browser? ")))
           (diff-file (pcase selected-diff-type-value
                        ('staged       (ai-code--handle-staged-diff-generation git-root))
                        ('base-vs-head (ai-code--handle-base-vs-head-diff-generation git-root open-in-browser))
                        ('branch-range (ai-code--handle-branch-range-diff-generation git-root open-in-browser))
                        ('commit       (ai-code--handle-commit-diff-generation git-root open-in-browser))
                        ('commit-range (ai-code--handle-commit-range-diff-generation git-root open-in-browser))
                        (_ (user-error "Invalid diff type selected")))))
      (when diff-file
        (ai-code--open-diff-file diff-file)
        (when (y-or-n-p "Review this change? ")
          (ai-code-pull-or-review-diff-file))))))

;;;###autoload
(defun ai-code-magit-blame-analyze ()
  "Analyze current file or region Git history with AI for deeper insights.
If region is active, analyze just that region.  Otherwise analyze entire file.
Combines `magit-blame' history tracking with AI analysis to help understand
code evolution and the reasoning behind changes."
  (interactive)
  (when (ai-code--validate-buffer-file)
  (let* ((file-path (buffer-file-name))
         (has-region (use-region-p))
         (line-start (if has-region
                         (line-number-at-pos (region-beginning))
                       1))
         (line-end (if has-region
                       (line-number-at-pos (region-end))
                     (line-number-at-pos (point-max))))
         (region-text (if has-region
                          (buffer-substring-no-properties
                           (region-beginning) (region-end))
                        nil))
         (blame-args (list "blame" "-l"
                           (format "-L%d,%d" line-start line-end)
                           file-path))
         (blame-output (with-temp-buffer
                         (apply #'process-file "git" nil t nil blame-args)
                         (buffer-string)))
         (context (format "File: %s\nLines: %d-%d\n\n"
                          file-path line-start line-end))
         (files-context-string (ai-code--get-context-files-string))
         (code-sample (if has-region
                          (concat "Selected code:\n```\n" region-text "\n```\n\n")
                        ""))
         (default-analysis "Please provide the following analysis:\n1. Code evolution patterns and timeline\n2. Key changes and their purpose\n3. Potential design decisions and thought processes\n4. Possible refactoring or improvement opportunities\n5. Insights about code architecture or design")
         (analysis-instructions (ai-code-read-string "Analysis instructions: " default-analysis))
         (prompt (format "Analyze the Git commit history for this code:\n\n%s%s%sCommit history information:\n```\n%s\n```\n\n%s"
                         context files-context-string code-sample blame-output analysis-instructions)))
    (ai-code--insert-prompt prompt))))

(defun ai-code--ensure-git-log (git-root repo-name keyword)
  "Fetch commits from the last X months under GIT-ROOT for REPO-NAME.
KEYWORD is used to filter commits.
Returns the path to the git.log file."
  (let* ((project-log-file-path (expand-file-name "git.log" git-root))
         (date-str (ai-code-read-string (format "Start date for history of %s (YYYY-MM-DD, e.g. 2025-01-01): " repo-name)))
         (since-arg (unless (string-empty-p date-str)
                      (format "--since=%s" date-str)))
         ;; compute defaults, then let user review & edit
         (magit-args-default
          (let ((args (list "log" "--pretty=medium" "--stat")))
            (when since-arg
              (setq args (append args (list since-arg))))
            (unless (string-empty-p keyword)
              (setq args (append args (list "-S" keyword))))
            args))
         (magit-args-str (mapconcat #'identity magit-args-default " "))
         (magit-args-input
          (ai-code-read-string (format "Git log args (edit if needed): ") magit-args-str))
         (magit-args (split-string magit-args-input nil t))
         (log-output (apply #'magit-git-output magit-args)))
    (message "Saving Git log to %s" project-log-file-path)
    (with-temp-file project-log-file-path
      (insert log-output))
    (find-file project-log-file-path)
    project-log-file-path))

(defun ai-code--default-log-analysis-instructions (keyword)
  "Return the default analysis prompt for KEYWORD (may be empty)."
  (if (not (string-empty-p keyword))
      (format "Analyze the commits filtered by keyword '%s'. Provide insights on:\n\
1. Overall '%s' related feature evolution and major development phases, with author name in each phase.\n\
2. Frequency and patterns of '%s' related commits.\n\
3. Files or areas most impacted by '%s' changes.\n\
4. Main contributors and their roles in '%s' work.\n\
5. Trends or hotspots in '%s' related development.\n\
6. Suggestions for improving or refactoring '%s' implementation.\n"
              keyword keyword keyword keyword keyword keyword keyword)
    (concat "Please analyze the following Git log for the entire repository. Provide insights on:\n"
            "1. Overall project evolution and major development phases, with author name in each phase.\n"
            "2. Identification of key features, refactorings, or architectural changes and their timeline, with author name for each one.\n"
            "3. Patterns in development activity (e.g., periods of rapid development, bug fixing, etc.), with author name.\n"
            "4. Significant contributors or shifts in contribution patterns (if discernible from commit messages).\n"
            "5. Potential areas of technical debt or architectural concerns suggested by the commit history.\n"
            "6. General trends in the project's direction or focus over time.")))

(defun ai-code--build-log-prompt (repo-name analysis-instructions)
  "Build the final AI prompt for REPO-NAME using ANALYSIS-INSTRUCTIONS."
  (let ((context (format "Repository: %s\n\n" repo-name)))
    (format "Analyze the Git commit history for the entire repository '%s'.\n\n%sThe detailed Git log content is in the 'git.log' file (which has been added to the chat).\nPlease use its content for your analysis, following these instructions:\n%s"
            repo-name context analysis-instructions)))

(defun ai-code-magit-log-analyze ()
  "Analyze Git log with AI.
If current buffer is visiting a file named \\='git.log\\=', analyze its
content.  Otherwise, prompt for number of commits (default 100) and
optionally a keyword, generate the log, save it to
\\='PROJECT_ROOT/git.log\\=', open this file, and then analyze its content."
  (interactive)
  (let* ((git-root (ai-code--validate-git-repository))
         (repo-name (file-name-nondirectory (directory-file-name git-root)))
         (keyword (ai-code-read-string "Optional: Keyword to filter commits (leave empty for no filter): "))
         (default-analysis (ai-code--default-log-analysis-instructions keyword))
         (analysis-instructions (ai-code-read-string "Analysis instructions for repository log: " default-analysis))
         (prompt (ai-code--build-log-prompt repo-name analysis-instructions)))
    (ai-code--insert-prompt prompt)))

;;;###autoload
(defun ai-code-magit-blame-or-log-analyze (&optional arg)
  "If current buffer is git.log, run log analysis.
Otherwise if prefix ARG, run log analysis; else run blame analysis.
ARG is the prefix argument."
  (interactive "P")
  (cond ((and buffer-file-name
              (string-equal (file-name-nondirectory buffer-file-name) "git.log"))
         (ai-code-magit-log-analyze))
        (arg (ai-code-magit-log-analyze))
        (t (ai-code-magit-blame-analyze))))

;;;###autoload
(defun ai-code-magit-setup-transients ()
  "Configure AI Code's transient menu entries in Magit.
This function uses `with-eval-after-load` to ensure that the
Magit transients are modified only after Magit itself has been loaded.
Call this function to register the AI Code commands with Magit."
  (interactive)
  ;; Integration with magit: This with-eval-after-load is intentional
  ;; to provide optional Magit integration without breaking when Magit is absent
  (with-eval-after-load 'magit
    ;; For magit-diff-popup (usually 'd' in status buffer)
    (transient-append-suffix 'magit-diff "r" ; "Extra" group
      '("a" "AI Code: Review/generate diff" ai-code-pull-or-review-diff-file))
    ;; For magit-blame-popup (usually 'B' in status buffer or log)
    (transient-append-suffix 'magit-blame "b" ; "Extra" group
      '("a" "AI Code: Analyze blame" ai-code-magit-blame-analyze))
    ;; For magit-log-popup (usually 'l' in status buffer)
    (transient-append-suffix 'magit-log "b" ; "Extra" group
      '("a" "AI Code: Analyze log" ai-code-magit-log-analyze))))

;; Ensure the Magit transients are set up when this file is loaded.
;; (ai-code-magit-setup-transients)

;;;###autoload
(defun ai-code-init-project (prefix)
  "Initialize project helpers for Projectile and Helm-Gtags.
If either package is available, prompt for a project directory
defaulting to the Magit repository root, initialize the project in
Projectile, and configure Helm-Gtags with a pygments label.  Show a
summary message of performed actions.
PREFIX is the prefix argument."
  (interactive "P")
  (let* ((projectile-available (or (featurep 'projectile)
                                   (require 'projectile nil t)))
         (helm-gtags-available (or (featurep 'helm-gtags)
                                   (require 'helm-gtags nil t))))
    (cond
     ((not (or projectile-available helm-gtags-available))
      (message "Projectile and Helm-Gtags are not available; skipping project initialization."))
     ((not (require 'magit nil t))
      (message "Magit is not available; cannot determine project root."))
     (t
      (let* ((git-root (condition-case nil
                           (magit-toplevel)
                         (error nil)))
             (initial-dir (or git-root default-directory))
             (dir (expand-file-name
                   (read-directory-name "Initialize project at: "
                                        initial-dir nil t initial-dir))))
        (let ((gtags-label (when (and helm-gtags-available
                                      (fboundp 'helm-gtags--read-gtagslabel))
                             (if prefix
                                 (helm-gtags--read-gtagslabel)
                               ai-code-init-project-gtags-label)))
              actions)
          (when (and projectile-available
                     (fboundp 'projectile-init-project))
            (projectile-init-project dir)
            (push (format "initialized Projectile project at %s" dir) actions))
          (cond
           ((and helm-gtags-available
                 (fboundp 'helm-gtags--read-gtagslabel)
                 gtags-label)
            (helm-gtags-create-tags dir gtags-label)
            (push (format "set Helm-Gtags label to %s for %s" gtags-label dir)
                  actions))
           ((and helm-gtags-available prefix)
            (push (format "skipped Helm-Gtags label for %s" dir) actions)))
          (if actions
              (message "ai-code-init-project: %s"
                       (mapconcat #'identity (nreverse actions) "; "))
            (message "ai-code-init-project: no actions performed for %s" dir))
          (ai-code-update-git-ignore)))))))

;;;###autoload
(defun ai-code-update-git-ignore ()
  "Ensure repository .gitignore contains AI Code related entries.
If not inside a Git repository, do nothing."
  (interactive)
  (let ((git-root (magit-toplevel)))
    (if (not git-root)
        (message "ai-code-update-git-ignore: not in a git repository, skipped")
      (let* ((gitignore-path (expand-file-name ".gitignore" git-root))
             (required-entries (list ai-code-prompt-file-name
                                     ai-code-notes-file-name
                                     ".projectile"
                                     "GTAGS"
                                     "GRTAGS"
                                     "GPATH"))
             (gitignore-content (when (file-exists-p gitignore-path)
                                  (with-temp-buffer
                                    (insert-file-contents gitignore-path)
                                    (buffer-string))))
             (missing-entries nil))
        (dolist (entry required-entries)
          (unless (and gitignore-content
                       (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                              (regexp-quote entry)
                                              "\\s-*\\(?:\n\\|$\\)")
                                       gitignore-content))
            (unless (member entry missing-entries)
              (push entry missing-entries))))
        (if missing-entries
            (let ((added (nreverse missing-entries)))
              (with-temp-buffer
                (when gitignore-content
                  (insert gitignore-content)
                  (unless (or (zerop (buffer-size))
                              (eq (char-before) ?\n))
                    (insert "\n")))
                (dolist (entry added)
                  (insert entry "\n"))
                (write-region (point-min) (point-max) gitignore-path))
              (message "ai-code-update-git-ignore: added %s"
                       (mapconcat #'identity added ", ")))
          (message "ai-code-update-git-ignore: no updates needed"))))))

(provide 'ai-code-git)

;;; ai-code-git.el ends here
