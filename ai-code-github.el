;;; ai-code-github.el --- GitHub review and PR workflow for AI Code -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides GitHub-specific PR and issue review workflows for the AI Code package.

;;; Code:

(require 'magit)
(require 'ai-code-input)
(require 'ai-code-prompt-mode)

(declare-function ai-code-read-string "ai-code-input")
(declare-function ai-code--insert-prompt "ai-code-prompt-mode" (prompt-text))
(declare-function magit-git-string "magit-git" (&rest args))
(declare-function difftastic-magit-diff "difftastic" ())
(declare-function ai-code--validate-git-repository "ai-code-git" ())
(declare-function ai-code--require-current-branch "ai-code-git" ())
(declare-function ai-code--default-pr-target-branch "ai-code-git" (current-branch))
(declare-function ai-code--magit-generate-feature-branch-diff-file "ai-code-git" ())
(declare-function ai-code--generate-staged-diff "ai-code-git" (diff-file))
(declare-function ai-code--generate-branch-or-commit-diff "ai-code-git" (diff-params diff-file))
(declare-function ai-code--open-diff-file "ai-code-git" (diff-file))
(declare-function ai-code--explain-code-change "ai-code-discussion" (&optional review-source))

(defcustom ai-code-default-review-source nil
  "Default review source for pull request and issue analysis.
When set, `ai-code--pull-or-review-action-choice' returns this value
directly without prompting.  Valid values are `github-mcp', `gh-cli',
or nil (prompt the user)."
  :type '(choice (const :tag "Prompt user" nil)
                 (const :tag "GitHub MCP server" github-mcp)
                 (const :tag "gh CLI tool" gh-cli))
  :group 'ai-code)

(defvar ai-code-pr-title-history nil
  "Minibuffer history for optional PR titles.")

(defun ai-code--extract-url-from-region ()
  "Return the first URL found in the active region, or nil."
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      (when (string-match "https?://[^ \t\n]+" text)
        (match-string 0 text)))))

(defun ai-code--pull-or-review-action-choice ()
  "Prompt user for action in `ai-code-pull-or-review-diff-file'.
When `ai-code-default-review-source' is set, return it directly."
  (or ai-code-default-review-source
      (let* ((action-alist '(("Use GitHub MCP server" . github-mcp)
                             ("Use gh CLI tool" . gh-cli)))
             (choice (completing-read "Select review source: "
                                      action-alist
                                      nil t nil nil "Use GitHub MCP server")))
        (alist-get choice action-alist nil nil #'string=))))

(defun ai-code--message-review-source-config-hint ()
  "Message a minibuffer hint about configuring `ai-code-default-review-source'."
  (message
   "Tip: set ai-code-default-review-source to github-mcp or gh-cli to skip this review-source prompt in future C-c a v runs."))

(defun ai-code--pull-or-review-source-instruction (review-source &optional review-mode)
  "Return source instruction string for REVIEW-SOURCE and REVIEW-MODE."
  (pcase review-mode
    ('investigate-issue
     (pcase review-source
       ('github-mcp
        "Use GitHub MCP server to inspect the GitHub issue and relevant repository context.")
       ('gh-cli
        "Use gh CLI tool to inspect the GitHub issue and relevant repository context.")
       (_ "Investigate this GitHub issue using the repository as context.")))
    ('review-ci-checks
     (pcase review-source
       ('github-mcp
        "Use GitHub MCP server to fetch pull request details and CI checks.")
       ('gh-cli
        "Use gh CLI tool to fetch pull request details and CI checks.")
       (_ "Review the GitHub CI checks for this pull request.")))
    ('resolve-merge-conflict
     (pcase review-source
       ('github-mcp
        "Use GitHub MCP server to fetch pull request branch details and merge status.")
       ('gh-cli
        "Use gh CLI tool to fetch pull request branch details and merge status.")
       (_ "Resolve merge conflicts for this pull request.")))
    ('explain-code-change
     (pcase review-source
       ('github-mcp
        "Use GitHub MCP server to inspect the pull request diff, changed files, commits, and relevant metadata.")
       ('gh-cli
        "Use gh CLI tool to inspect the pull request diff, changed files, commits, and relevant metadata.")
       (_ "Inspect the pull request diff and relevant metadata to understand the change.")))
    (_
     (pcase review-source
       ('github-mcp
        "Use GitHub MCP server to fetch pull request details and review comments.")
       ('gh-cli
        "Use gh CLI tool to fetch pull request details and review comments.")
       (_ "Review this pull request.")))))

(defun ai-code--build-pr-review-init-prompt (review-source pr-url)
  "Build PR review initial prompt for REVIEW-SOURCE with PR-URL."
  (let ((source-instruction
         (ai-code--pull-or-review-source-instruction review-source)))
    (format "Review pull request: %s

%s

Review Steps:
1. Requirement Fit: Verify the PR implementation against requirements.
2. Code Quality: Check code quality, security, and performance concerns.
3. Findings: For each issue include location, issue, fix suggestion, and priority.

Provide an overall assessment at the end."
            pr-url source-instruction)))

(defun ai-code--build-pr-feedback-check-init-prompt (review-source pr-url)
  "Build unresolved feedback check prompt for REVIEW-SOURCE with PR-URL."
  (let ((source-instruction
         (ai-code--pull-or-review-source-instruction review-source)))
    (format "Check unresolved feedback for pull request: %s

%s

Feedback Check Steps:
1. Find unresolved feedback or unresolved review comments in this PR.
2. For each unresolved feedback, explain whether it makes sense and why.
3. If a feedback does not make sense, explain why it may not be necessary.
4. No need to make code change. Provide analysis only."
            pr-url source-instruction)))

(defun ai-code--build-issue-investigation-init-prompt (review-source issue-url)
  "Build issue investigation prompt for REVIEW-SOURCE with ISSUE-URL."
  (let ((source-instruction
         (ai-code--pull-or-review-source-instruction review-source 'investigate-issue)))
    (format "Investigate issue: %s

%s

Issue Investigation Steps:
1. Understand the issue description, reproduction details, and expected behavior.
2. Analyze relevant code in this repository as context and identify likely root causes.
3. Provide concrete insights on how to fix it, including likely files or areas to change.
4. No need to make code change. Provide analysis only."
            issue-url source-instruction)))

(defun ai-code--build-pr-description-init-prompt (review-source pr-url)
  "Build PR description prompt for REVIEW-SOURCE with PR-URL."
  (let ((source-instruction
         (ai-code--pull-or-review-source-instruction review-source)))
    (format "Prepare a pull request description for: %s

%s

PR Description Steps:
1. Add a summary of the problem and the approach taken in the PR.
2. Highlight the most important code changes and user-visible impact.
3. Add a testing section with relevant verification details.
4. Format the result as a concise PR description ready to share with reviewers,
   written in the voice of the author or maintainer."
            pr-url source-instruction)))

(defun ai-code--build-pr-ci-check-init-prompt (review-source pr-url)
  "Build a CI check review prompt for REVIEW-SOURCE with PR-URL."
  (let ((source-instruction
         (ai-code--pull-or-review-source-instruction review-source 'review-ci-checks)))
    (format "Review GitHub CI checks for pull request: %s

%s

CI Checks Review Steps:
1. Review the GitHub CI checks for this pull request.
2. If there is a failing or error state, inspect the failing checks and relevant details.
3. Analyze the likely root cause of each failure.
4. No need to make code change. Provide analysis only."
            pr-url source-instruction)))

(defun ai-code--build-resolve-merge-conflict-init-prompt (review-source pr-url)
  "Build merge conflict resolution prompt for REVIEW-SOURCE with PR-URL."
  (let ((source-instruction
         (ai-code--pull-or-review-source-instruction review-source 'resolve-merge-conflict)))
    (format "Resolve merge conflict for pull request: %s

%s

Merge Conflict Resolution Steps:
1. Identify the current working branch and the target branch from the PR.
2. Verify the current working branch matches the PR source branch.
3. Update the local target branch to the latest remote version.
4. Attempt to merge the target branch into the current working branch.
5. If there are merge conflicts, analyze each conflict and provide detailed instructions on how to resolve them, including which files to change and how.
6. Do not make code changes. Provide analysis and resolution suggestions only.
7. If there are no merge conflicts, report that the merge would succeed and return a success message."
            pr-url source-instruction)))

(defun ai-code--pull-or-review-url-prompt (review-mode)
  "Return the URL prompt string for REVIEW-MODE."
  (if (eq review-mode 'investigate-issue)
      "GitHub issue URL: "
    "Pull request URL: "))

(defun ai-code--pull-or-review-pr-mode-choice ()
  "Prompt user to choose analysis mode for a pull request or issue."
  (let* ((review-mode-alist '(("Review the PR" . review-pr)
                              ("Check unresolved feedback" . check-feedback)
                              ("Prepare PR description" . prepare-pr-description)
                              ("Send out PR for current branch" . send-current-branch-pr)
                              ("Review current branch with difftastic"
                               . review-current-branch-with-difftastic)
                              ("Investigate issue" . investigate-issue)
                              ("Review GitHub CI checks" . review-ci-checks)
                              ("Explain code change" . explain-code-change)
                              ("Resolve merge conflict" . resolve-merge-conflict)
                              ("Generate diff file" . generate-diff-file)))
         (review-mode (completing-read "Select analysis mode (PR or issue): "
                                       review-mode-alist
                                       nil t nil nil "Review the PR")))
    (or (alist-get review-mode review-mode-alist nil nil #'string=)
        'review-pr)))

(defun ai-code--review-current-branch-with-difftastic ()
  "Review the current branch with `difftastic-magit-diff'.
Signal a helpful error when difftastic is unavailable."
  (require 'difftastic nil t)
  (unless (fboundp 'difftastic-magit-diff)
    (user-error
     (concat
      "The package difftastic is not installed. "
      "Install it from MELPA or https://github.com/pkryger/difftastic.el")))
  (call-interactively #'difftastic-magit-diff))

(defun ai-code--send-current-branch-pr-source-instruction (review-source)
  "Return PR creation instructions for REVIEW-SOURCE."
  (pcase review-source
    ('gh-cli
     (concat
      "Use GitHub CLI to create the pull request. "
      "Run `gh pr create` with the current branch as the head branch, "
      "target the requested base branch, and include the final title and body."))
    ('github-mcp
     (concat
      "Use GitHub MCP tools to create the pull request directly. "
      "Do not fetch review comments before the PR exists; "
      "create the PR first, then return the resulting PR URL."))
    (_
     (concat
      "Create the pull request using the backend's PR creation capability. "
      "Do not treat this as a PR review flow before the PR exists."))))

(defun ai-code--build-send-current-branch-pr-init-prompt
    (review-source current-branch target-branch &optional pr-title)
  "Build a PR creation prompt.
REVIEW-SOURCE, CURRENT-BRANCH, TARGET-BRANCH, and PR-TITLE
define the PR request."
  (let ((source-instruction
         (ai-code--send-current-branch-pr-source-instruction review-source))
        (title-instruction
         (if (string-empty-p (or pr-title ""))
             "2. Generate a concise PR title based on the code change.\n"
           (format "2. Use this PR title exactly: %s\n" pr-title))))
    (format "Create a pull request from branch %s into %s.

%s

PR Creation Steps:
1. Inspect the current branch changes and open or send out a pull request into %s.
%s3. Write a concise PR description that sounds like it was written by the author, but do not make it too short.
4. Keep the description focused on the problem, the approach, and the most important verification, with enough detail for reviewers to understand the change quickly.
5. Aim for a compact but complete description, roughly a short summary plus 2 to 3 brief supporting paragraphs or bullet points.
6. Return the final PR URL, the exact PR title, and the exact description that were used."
            current-branch
            target-branch
            source-instruction
            target-branch
            title-instruction)))

(defun ai-code--build-pr-init-prompt (review-source target-url review-mode)
  "Build initial prompt for REVIEW-SOURCE, TARGET-URL and REVIEW-MODE."
  (pcase review-mode
    ('investigate-issue
     (ai-code--build-issue-investigation-init-prompt review-source target-url))
    ('check-feedback
     (ai-code--build-pr-feedback-check-init-prompt review-source target-url))
    ('review-ci-checks
     (ai-code--build-pr-ci-check-init-prompt review-source target-url))
    ('prepare-pr-description
     (ai-code--build-pr-description-init-prompt review-source target-url))
    ('resolve-merge-conflict
     (ai-code--build-resolve-merge-conflict-init-prompt review-source target-url))
    (_
     (ai-code--build-pr-review-init-prompt review-source target-url))))

;;;###autoload
(defun ai-code--pull-or-review-pr-with-source (review-source)
  "Prompt for a mode and send a prompt for REVIEW-SOURCE to AI."
  (require 'ai-code-git nil t)
  (let* ((review-mode (ai-code--pull-or-review-pr-mode-choice)))
    (cond
     ((eq review-mode 'generate-diff-file)
      (ai-code--magit-generate-feature-branch-diff-file))
     ((eq review-mode 'review-current-branch-with-difftastic)
      (ai-code--review-current-branch-with-difftastic))
      ((eq review-mode 'explain-code-change)
       (unless (fboundp 'ai-code--explain-code-change)
         (require 'ai-code-discussion nil t))
       (unless (fboundp 'ai-code--explain-code-change)
         (user-error "Code change explanation support is not available"))
       (ai-code--explain-code-change review-source))
     (t
      (let* ((init-prompt
              (if (eq review-mode 'send-current-branch-pr)
                  (progn
                    (unless (magit-toplevel)
                      (user-error "Not inside a Git repository"))
                    (let* ((current-branch (ai-code--require-current-branch))
                           (default-target-branch
                            (ai-code--default-pr-target-branch current-branch))
                           (target-branch
                            (ai-code-read-string "Target branch to merge into: "
                                                 default-target-branch))
                           (pr-title
                            (read-string
                             "PR title (optional, leave empty for AI to generate): "
                             nil
                             'ai-code-pr-title-history)))
                      (ai-code--build-send-current-branch-pr-init-prompt
                       review-source current-branch target-branch pr-title)))
                (let* ((url-prompt (ai-code--pull-or-review-url-prompt review-mode))
                       (region-url (ai-code--extract-url-from-region))
                       (target-url (ai-code-read-string url-prompt region-url)))
                  (ai-code--build-pr-init-prompt review-source target-url review-mode))))
             (prompt-label (if (eq review-mode 'send-current-branch-pr)
                               "Enter PR creation prompt: "
                             "Enter review prompt: "))
             (prompt (ai-code-read-string prompt-label init-prompt)))
        (ai-code--insert-prompt prompt))))))

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

(defun ai-code-pull-or-review-diff-file ()
  "Review a diff file with AI Code or choose a PR review workflow.
If current buffer is a .diff file, ask AI Code to review it.
Otherwise, prompt for a review source and analysis mode."
  (interactive)
  (if (and buffer-file-name (string-match-p "\\.diff$" buffer-file-name))
      (let* ((file-name (file-name-nondirectory buffer-file-name))
             (init-prompt (format "Code review for %s. Use relevant file in repository as context.

**Review Steps**:
1. **Requirement Fit** (Top Priority): Verify if the code change fulfills the requirement below. Identify gaps or missing implementations.
2. **Code Quality**: Check for quality, security, performance, and coding patterns.
3. **Issues Found**: For each issue: Location, Issue, Solution, Priority (High/Medium/Low)

Provide overall assessment.

**Requirement**: " file-name))
             (prompt (ai-code-read-string "Enter review prompt (type requirement at end): " init-prompt)))
        (ai-code--insert-prompt prompt))
    ;; For non-diff files, let user choose PR review via MCP/gh CLI.
    (unless ai-code-default-review-source
      (ai-code--message-review-source-config-hint))
    (let ((review-source (ai-code--pull-or-review-action-choice)))
      (ai-code--pull-or-review-pr-with-source review-source))))

(provide 'ai-code-github)

;;; ai-code-github.el ends here
