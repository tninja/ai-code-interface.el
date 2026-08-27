;;; test_ai-code-github.el --- Tests for ai-code-github.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-github module, especially review-source selection
;; and prompt generation flows.

;;; Code:

(require 'ert)
(require 'ai-code-github)
(require 'ai-code-git)
(require 'ai-code-harness)
(require 'ai-code-prompt-mode)

(declare-function difftastic-magit-diff "difftastic" ())

(defun ai-code-test--run-pull-or-review-diff-file (choice pr-url &optional review-mode-choice)
  "Run `ai-code-pull-or-review-diff-file' with CHOICE and optional PR-URL.
REVIEW-MODE-CHOICE is used for review mode selection when prompted.
Return (CAPTURED-PROMPT DIFF-CALLED)."
  (let* ((captured-prompt nil)
         (diff-called nil)
         (completing-read-results (delq nil (list choice review-mode-choice))))
    (with-temp-buffer
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _args)
                   (let ((selected (car completing-read-results)))
                     (setq completing-read-results (cdr completing-read-results))
                     selected)))
                ((symbol-function 'ai-code-read-string)
                 (lambda (prompt &optional initial-input &rest _args)
                   (if (string-match-p "URL:" prompt)
                       pr-url
                     initial-input)))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq captured-prompt prompt)))
                ((symbol-function 'ai-code--magit-generate-feature-branch-diff-file)
                 (lambda () (setq diff-called t))))
        (ai-code-pull-or-review-diff-file)))
    (list captured-prompt diff-called)))

(defun ai-code-test--sent-prompt-for-review-mode (review-mode-choice)
  "Return the sent prompt and Grill questions for REVIEW-MODE-CHOICE."
  (let ((ai-code-default-review-source 'github-mcp)
        (ai-code-grill-me-enabled t)
        (ai-code-prompt-preprocess-filepaths nil)
        (ai-code-prompt-suffix-functions
         '(ai-code--grill-me-suffix-provider))
        (this-command 'ai-code-pull-or-review-diff-file)
        captured-prompt
        grill-questions)
    (with-temp-buffer
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _args) review-mode-choice))
                ((symbol-function 'ai-code-read-string)
                 (lambda (prompt &optional initial-input &rest _args)
                   (if (string-match-p "URL:" prompt)
                       "https://github.com/acme/demo/pull/123"
                     initial-input)))
                ((symbol-function 'read-string)
                 (lambda (_prompt &optional initial-input &rest _args)
                   initial-input))
                ((symbol-function 'y-or-n-p)
                 (lambda (prompt)
                   (push prompt grill-questions)
                   t))
                ((symbol-function 'ai-code--get-ai-code-prompt-file-path)
                 (lambda () nil))
                ((symbol-function 'ai-code--send-prompt)
                 (lambda (prompt) (setq captured-prompt prompt))))
        (ai-code-pull-or-review-diff-file)))
    (list captured-prompt (nreverse grill-questions))))

(ert-deftest ai-code-test-pull-or-review-diff-file-grills-only-selected-review-modes ()
  "Only selected GitHub review modes should offer the Grill harness."
  (dolist (review-mode-choice '("Investigate issue"
                                "Review the PR"
                                "Resolve merge conflict"))
    (pcase-let ((`(,sent-prompt ,grill-questions)
                 (ai-code-test--sent-prompt-for-review-mode
                  review-mode-choice)))
      (should (equal grill-questions '("Grill me before acting? ")))
      (should (string-match-p "prompt/grilling\\.v1\\.md" sent-prompt))))
  (pcase-let ((`(,sent-prompt ,grill-questions)
               (ai-code-test--sent-prompt-for-review-mode
                "Review GitHub CI checks")))
    (should-not grill-questions)
    (should-not (string-match-p "prompt/grilling\\.v1\\.md" sent-prompt))))

(ert-deftest ai-code-test-action-choice-returns-github-mcp-when-default-set ()
  "When `ai-code-default-review-source' is `github-mcp', return it directly."
  (let ((ai-code-default-review-source 'github-mcp)
        (completing-read-called nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (setq completing-read-called t)
                 "Use GitHub MCP server")))
      (should (eq (ai-code--pull-or-review-action-choice) 'github-mcp))
      (should-not completing-read-called))))

(ert-deftest ai-code-test-action-choice-returns-gh-cli-when-default-set ()
  "When `ai-code-default-review-source' is `gh-cli', return it directly."
  (let ((ai-code-default-review-source 'gh-cli)
        (completing-read-called nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (setq completing-read-called t)
                 "Use gh CLI tool")))
      (should (eq (ai-code--pull-or-review-action-choice) 'gh-cli))
      (should-not completing-read-called))))

(ert-deftest ai-code-test-action-choice-prompts-when-default-nil ()
  "When `ai-code-default-review-source' is nil, use `completing-read'."
  (let ((ai-code-default-review-source nil)
        (completing-read-called nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (setq completing-read-called t)
                 "Use GitHub MCP server")))
      (should (eq (ai-code--pull-or-review-action-choice) 'github-mcp))
      (should completing-read-called))))

(ert-deftest ai-code-test-pull-or-review-message-displays-config-hint ()
  "The review-source guidance should be messaged in the minibuffer."
  (let (captured-message)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq captured-message (apply #'format format-string args)))))
      (ai-code--message-review-source-config-hint)
      (should (string-match-p "ai-code-default-review-source" captured-message))
      (should (string-match-p "C-c a v" captured-message)))))

(ert-deftest ai-code-test-pull-or-review-diff-file-messages-config-hint-when-default-nil ()
  "When default review source is nil, `ai-code-pull-or-review-diff-file' should show guidance."
  (let ((ai-code-default-review-source nil)
        (captured-message nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq captured-message (apply #'format format-string args))))
              ((symbol-function 'ai-code--pull-or-review-action-choice)
               (lambda () 'github-mcp))
              ((symbol-function 'ai-code--pull-or-review-pr-with-source)
               (lambda (_review-source &rest _args)
                 nil)))
      (with-temp-buffer
        (ai-code-pull-or-review-diff-file))
      (should (string-match-p "ai-code-default-review-source" captured-message)))))

(ert-deftest ai-code-test-pull-or-review-diff-file-skips-config-hint-when-default-set ()
  "When default review source is set, `ai-code-pull-or-review-diff-file' should skip guidance."
  (let ((ai-code-default-review-source 'github-mcp)
        (message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args)
                 (setq message-called t)))
              ((symbol-function 'ai-code--pull-or-review-pr-with-source)
               (lambda (_review-source &rest _args)
                 nil)))
      (with-temp-buffer
        (ai-code-pull-or-review-diff-file))
      (should-not message-called))))

(ert-deftest ai-code-test-pull-or-review-pr-mode-choice-prepare-pr-description ()
  "Choosing PR description mode should return `prepare-pr-description'."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "Prepare PR description")))
    (should (eq (ai-code--pull-or-review-pr-mode-choice)
                'prepare-pr-description))))

(ert-deftest ai-code-test-pull-or-review-pr-mode-choice-review-ci-checks ()
  "Choosing CI checks mode should return `review-ci-checks'."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "Review GitHub CI checks")))
    (should (eq (ai-code--pull-or-review-pr-mode-choice)
                'review-ci-checks))))

(ert-deftest ai-code-test-pull-or-review-pr-mode-choice-explain-code-change ()
  "Choosing explain code change mode should return `explain-code-change'."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "Explain code change")))
    (should (eq (ai-code--pull-or-review-pr-mode-choice)
                'explain-code-change))))

(ert-deftest ai-code-test-pull-or-review-pr-mode-choice-send-current-branch-pr ()
  "Choosing current branch PR mode should return `send-current-branch-pr'."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "Send out PR for current branch")))
    (should (eq (ai-code--pull-or-review-pr-mode-choice)
                'send-current-branch-pr))))

(ert-deftest ai-code-test-pull-or-review-pr-mode-choice-resolve-merge-conflict ()
  "Choosing resolve merge conflict mode should return `resolve-merge-conflict'."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "Resolve merge conflict")))
    (should (eq (ai-code--pull-or-review-pr-mode-choice)
                'resolve-merge-conflict))))

(ert-deftest ai-code-test-pull-or-review-pr-mode-choice-review-current-branch-with-difftastic ()
  "Choosing the difftastic mode should return `review-current-branch-with-difftastic'."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "Review current branch with difftastic")))
    (should (eq (ai-code--pull-or-review-pr-mode-choice)
                'review-current-branch-with-difftastic))))

(ert-deftest ai-code-test-pull-or-review-pr-mode-choice-generate-diff-file ()
  "Choosing generate diff file mode should return `generate-diff-file'."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "Generate diff file")))
    (should (eq (ai-code--pull-or-review-pr-mode-choice)
                'generate-diff-file))))

(ert-deftest ai-code-test-pull-or-review-source-instruction-explain-code-change ()
  "Explain-code-change mode should inspect the diff, not review comments."
  (let ((instruction
         (ai-code--pull-or-review-source-instruction 'github-mcp
                                                     'explain-code-change)))
    (should (string-match-p "GitHub MCP server" instruction))
    (should (string-match-p "diff" (downcase instruction)))
    (should-not (string-match-p "review comments" (downcase instruction)))))

(ert-deftest ai-code-test-build-send-current-branch-pr-init-prompt-draft ()
  "Build a draft PR creation prompt for the current branch."
  (cl-letf (((symbol-function 'y-or-n-p)
             (lambda (_prompt) t)))
    (let ((prompt (ai-code--build-send-current-branch-pr-init-prompt
                   'gh-cli
                   "feature/improve-pr-flow"
                   "main")))
      (let ((case-fold-search nil))
        (should (string-match-p "Use GitHub CLI to create the pull request" prompt)))
      (should (string-match-p "feature/improve-pr-flow" prompt))
      (should (string-match-p "main" prompt))
      (should (string-match-p "create a draft pull request" (downcase prompt)))
      (should (string-match-p "short" (downcase prompt)))
      (should (string-match-p "author" (downcase prompt)))
      (should-not (string-match-p "review comments" (downcase prompt))))))

(ert-deftest ai-code-test-build-send-current-branch-pr-init-prompt-ready-for-review ()
  "Build a normal PR creation prompt when draft mode is declined."
  (cl-letf (((symbol-function 'y-or-n-p)
             (lambda (_prompt) nil)))
    (let ((prompt (ai-code--build-send-current-branch-pr-init-prompt
                   'gh-cli
                   "feature/improve-pr-flow"
                   "main")))
      (should (string-match-p "create a normal pull request" (downcase prompt)))
      (should-not (string-match-p "draft pull request" (downcase prompt))))))

(ert-deftest ai-code-test-default-pr-target-branch-uses-origin-head-when-main-and-master-absent ()
  "Fallback target branch should use origin HEAD when available."
  (cl-letf (((symbol-function 'magit-git-string)
             (lambda (&rest args)
               (pcase args
                 (`("rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{upstream}") nil)
                 (`("symbolic-ref" "--quiet" "--short" "refs/remotes/origin/HEAD")
                  "origin/develop")
                 (_ nil))))
            ((symbol-function 'magit-branch-p)
             (lambda (_branch) nil)))
    (should (equal (ai-code--default-pr-target-branch "feature/improve-pr-flow")
                   "develop"))))

(defun ai-code-test--with-reflog-entries (entries resolved-refs thunk &optional remotes)
  "Call THUNK with reflog ENTRIES and ref resolution stubbed.
ENTRIES is the list of `git reflog show --format=%gs' output lines, newest
first.  RESOLVED-REFS is an alist mapping a raw ref to the full ref name that
`git rev-parse --symbolic-full-name' would return, or nil when the ref does not
resolve.  REMOTES overrides the configured remote names, defaulting to origin."
  (cl-letf (((symbol-function 'magit-git-lines)
             (lambda (&rest args)
               (pcase args
                 (`("reflog" "show" "--format=%gs" ,_branch) entries)
                 (`("remote") (or remotes '("origin")))
                 (_ nil))))
            ((symbol-function 'magit-git-string)
             (lambda (&rest args)
               (pcase args
                 (`("rev-parse" "--symbolic-full-name" ,ref)
                  (alist-get ref resolved-refs nil nil #'string=))
                 (_ nil)))))
    (funcall thunk)))

(ert-deftest ai-code-test-branch-parent-branch-reads-reflog-creation-entry ()
  "Parent branch detection should read the reflog creation entry."
  (ai-code-test--with-reflog-entries
   '("commit: work in progress" "branch: Created from 1.88")
   '(("1.88" . "refs/heads/1.88"))
   (lambda ()
     (should (equal (ai-code--branch-parent-branch "rdar_183143740_1.88")
                    "1.88")))))

(ert-deftest ai-code-test-branch-parent-branch-normalizes-remote-tracking-ref ()
  "A remote-tracking creation ref should be normalized to a bare branch name."
  (ai-code-test--with-reflog-entries
   '("branch: Created from refs/remotes/origin/develop")
   '(("refs/remotes/origin/develop" . "refs/remotes/origin/develop"))
   (lambda ()
     (should (equal (ai-code--branch-parent-branch "feature/improve-pr-flow")
                    "develop")))))

(ert-deftest ai-code-test-branch-parent-branch-strips-non-origin-remote-prefix ()
  "A non-origin remote parent should return the bare branch name.
Regression test: `gh pr create --base' takes a branch name, so a
remote-qualified name such as upstream/develop is not a usable target."
  ;; Short form, as recorded by `git switch -c feature upstream/develop'.
  (ai-code-test--with-reflog-entries
   '("branch: Created from upstream/develop")
   '(("upstream/develop" . "refs/remotes/upstream/develop"))
   (lambda ()
     (should (equal (ai-code--branch-parent-branch "feature/improve-pr-flow")
                    "develop")))
   '("origin" "upstream"))
  ;; Full ref form must resolve even when the branch exists only on a fork
  ;; remote, which a local-or-origin-only probe would have rejected.
  (ai-code-test--with-reflog-entries
   '("branch: Created from refs/remotes/upstream/develop")
   '(("refs/remotes/upstream/develop" . "refs/remotes/upstream/develop"))
   (lambda ()
     (should (equal (ai-code--branch-parent-branch "feature/improve-pr-flow")
                    "develop")))
   '("origin" "upstream")))

(ert-deftest ai-code-test-branch-parent-branch-handles-remote-name-with-slash ()
  "A remote name containing a slash should be stripped as a whole.
Git allows remote names such as grp/sub, so splitting on the first path
segment would yield the wrong branch name."
  (ai-code-test--with-reflog-entries
   '("branch: Created from grp/sub/develop")
   '(("grp/sub/develop" . "refs/remotes/grp/sub/develop"))
   (lambda ()
     (should (equal (ai-code--branch-parent-branch "feature/improve-pr-flow")
                    "develop")))
   '("origin" "grp/sub")))

(ert-deftest ai-code-test-branch-parent-branch-dereferences-remote-head ()
  "A remote symbolic HEAD parent should resolve to the branch it points at.
Regression test: returning the literal name HEAD would name a nonexistent
branch as the PR base."
  (ai-code-test--with-reflog-entries
   '("branch: Created from origin/HEAD")
   ;; git rev-parse dereferences origin/HEAD to the branch it points at.
   '(("origin/HEAD" . "refs/remotes/origin/develop"))
   (lambda ()
     (should (equal (ai-code--branch-parent-branch "feature/improve-pr-flow")
                    "develop")))))

(ert-deftest ai-code-test-branch-parent-branch-uses-oldest-creation-entry ()
  "When several creation entries exist, use the oldest (last) one."
  (ai-code-test--with-reflog-entries
   '("branch: Created from stale-topic" "commit: work" "branch: Created from main")
   '(("main" . "refs/heads/main") ("stale-topic" . "refs/heads/stale-topic"))
   (lambda ()
     (should (equal (ai-code--branch-parent-branch "feature/improve-pr-flow")
                    "main")))))

(ert-deftest ai-code-test-branch-parent-branch-rejects-head-and-unknown-refs ()
  "Non-branch creation sources should not be treated as a parent branch."
  ;; A bare HEAD carries no parent information: it resolves to whatever branch
  ;; is checked out at lookup time, which need not be the branch under query.
  (ai-code-test--with-reflog-entries
   '("branch: Created from HEAD")
   '(("HEAD" . "refs/heads/some-other-checked-out-branch"))
   (lambda ()
     (should-not (ai-code--branch-parent-branch "feature/improve-pr-flow"))))
  ;; A raw commit does not resolve to a symbolic ref at all.
  (ai-code-test--with-reflog-entries
   '("branch: Created from 11465c9") nil
   (lambda ()
     (should-not (ai-code--branch-parent-branch "feature/improve-pr-flow"))))
  ;; A tag resolves, but refs/tags is not a branch namespace.
  (ai-code-test--with-reflog-entries
   '("branch: Created from v1.0") '(("v1.0" . "refs/tags/v1.0"))
   (lambda ()
     (should-not (ai-code--branch-parent-branch "feature/improve-pr-flow"))))
  ;; A deleted parent branch no longer resolves.
  (ai-code-test--with-reflog-entries
   '("branch: Created from merged-and-deleted") nil
   (lambda ()
     (should-not (ai-code--branch-parent-branch "feature/improve-pr-flow"))))
  ;; An expired or missing reflog yields no creation entry at all.
  (ai-code-test--with-reflog-entries
   '("commit: work in progress") nil
   (lambda ()
     (should-not (ai-code--branch-parent-branch "feature/improve-pr-flow")))))

(ert-deftest ai-code-test-branch-parent-branch-rejects-self-reference ()
  "A creation entry naming the branch itself should be ignored."
  (ai-code-test--with-reflog-entries
   '("branch: Created from refs/remotes/origin/feature/improve-pr-flow")
   '(("refs/remotes/origin/feature/improve-pr-flow"
      . "refs/remotes/origin/feature/improve-pr-flow"))
   (lambda ()
     (should-not (ai-code--branch-parent-branch "feature/improve-pr-flow")))))

(ert-deftest ai-code-test-branch-parent-branch-keeps-nested-branch-name ()
  "Slashes inside the branch name itself must be preserved."
  (ai-code-test--with-reflog-entries
   '("branch: Created from origin/release/2.0")
   '(("origin/release/2.0" . "refs/remotes/origin/release/2.0"))
   (lambda ()
     (should (equal (ai-code--branch-parent-branch "feature/improve-pr-flow")
                    "release/2.0")))))

(ert-deftest ai-code-test-default-pr-target-branch-prefers-reflog-parent-branch ()
  "The reflog parent branch should outrank same-name upstream and origin HEAD."
  (cl-letf (((symbol-function 'magit-git-lines)
             (lambda (&rest args)
               (pcase args
                 (`("reflog" "show" "--format=%gs" ,_branch)
                  '("commit: work in progress" "branch: Created from 1.88"))
                 (`("remote") '("origin"))
                 (_ nil))))
            ((symbol-function 'magit-git-string)
             (lambda (&rest args)
               (pcase args
                 (`("rev-parse" "--symbolic-full-name" "1.88") "refs/heads/1.88")
                 ;; Typical workflow: upstream is the same-name remote branch.
                 (`("rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{upstream}")
                  "origin/rdar_183143740_1.88")
                 (`("symbolic-ref" "--quiet" "--short" "refs/remotes/origin/HEAD")
                  "origin/main")
                 (_ nil))))
            ((symbol-function 'magit-branch-p)
             (lambda (branch) (member branch '("1.88" "main" "origin/main")))))
    (should (equal (ai-code--default-pr-target-branch "rdar_183143740_1.88")
                   "1.88"))))

(ert-deftest ai-code-test-default-pr-target-branch-falls-back-without-reflog-parent ()
  "Without a usable reflog parent, existing upstream fallbacks still apply."
  (cl-letf (((symbol-function 'magit-git-lines)
             (lambda (&rest args)
               (pcase args
                 (`("reflog" "show" "--format=%gs" ,_branch)
                  '("branch: Created from HEAD"))
                 (`("remote") '("origin"))
                 (_ nil))))
            ((symbol-function 'magit-git-string)
             (lambda (&rest args)
               (pcase args
                 ;; A bare HEAD is rejected before any ref resolution.
                 (`("rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{upstream}")
                  "origin/rdar_183143740_1.88")
                 (`("symbolic-ref" "--quiet" "--short" "refs/remotes/origin/HEAD")
                  "origin/main")
                 (_ nil))))
            ((symbol-function 'magit-branch-p)
             (lambda (branch) (member branch '("main" "origin/main")))))
    (should (equal (ai-code--default-pr-target-branch "rdar_183143740_1.88")
                   "main"))))

(ert-deftest ai-code-test-pull-or-review-pr-with-source-send-current-branch-pr-uses-neutral-prompt ()
  "Current branch PR flow should validate repo and use a PR creation prompt label."
  (let (captured-read-prompts captured-read-string-prompts captured-inserted-prompt)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "Send out PR for current branch"))
              ((symbol-function 'magit-toplevel)
               (lambda (&optional _dir) "/tmp/repo/"))
              ((symbol-function 'magit-get-current-branch)
               (lambda () "feature/improve-pr-flow"))
              ((symbol-function 'magit-git-string)
               (lambda (&rest _args) "origin/main"))
              ((symbol-function 'ai-code-read-string)
               (lambda (prompt &optional initial-input _candidate-list)
                 (push prompt captured-read-prompts)
                 (cond
                  ((string= prompt "Target branch to merge into: ")
                   (or initial-input "main"))
                  ((string= prompt "Enter PR creation prompt: ")
                   initial-input)
                  (t initial-input))))
              ((symbol-function 'read-string)
               (lambda (prompt &optional initial-input _history _default-value &rest _args)
                 (push prompt captured-read-string-prompts)
                 (if (string= prompt "PR title (optional, leave empty for AI to generate): ")
                     ""
                   (ai-code-read-string prompt initial-input))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil))
              ((symbol-function 'ai-code--insert-prompt)
               (lambda (prompt)
                 (setq captured-inserted-prompt prompt))))
      (ai-code--pull-or-review-pr-with-source 'gh-cli)
      (should (member "Target branch to merge into: " captured-read-prompts))
      (should (member "PR title (optional, leave empty for AI to generate): "
                      captured-read-string-prompts))
      (should (member "Enter PR creation prompt: " captured-read-prompts))
      (should-not (member "Enter review prompt: " captured-read-prompts))
      (should (string-match-p "feature/improve-pr-flow" captured-inserted-prompt))
      (should (string-match-p "generate a concise pr title" (downcase captured-inserted-prompt)))
      (should (string-match-p "create a normal pull request"
                              (downcase captured-inserted-prompt))))))

(ert-deftest ai-code-test-pull-or-review-pr-with-source-explain-code-change-shares-flow ()
  "Explain code change mode should dispatch to the shared explanation flow."
  (let (captured-review-source)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "Explain code change"))
              ((symbol-function 'fboundp)
               (lambda (fn)
                 (eq fn 'ai-code--explain-code-change)))
              ((symbol-function 'ai-code--explain-code-change)
               (lambda (&optional review-source)
                 (setq captured-review-source review-source))))
      (ai-code--pull-or-review-pr-with-source 'github-mcp)
      (should (eq captured-review-source 'github-mcp)))))

(ert-deftest ai-code-test-pull-or-review-pr-with-source-generate-diff-file-calls-diff-generation ()
  "When mode is generate-diff-file, call diff generation instead of building a prompt."
  (let (diff-called)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "Generate diff file"))
              ((symbol-function 'ai-code--magit-generate-feature-branch-diff-file)
               (lambda () (setq diff-called t))))
       (ai-code--pull-or-review-pr-with-source 'github-mcp)
       (should diff-called))))

(ert-deftest ai-code-test-pull-or-review-pr-with-source-review-current-branch-with-difftastic ()
  "Difftastic mode should dispatch to the dedicated review helper."
  (let (difftastic-called)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "Review current branch with difftastic"))
              ((symbol-function 'ai-code--review-current-branch-with-difftastic)
               (lambda ()
                 (setq difftastic-called t))))
      (ai-code--pull-or-review-pr-with-source 'github-mcp)
      (should difftastic-called))))

(ert-deftest ai-code-test-review-current-branch-with-difftastic-calls-command ()
  "When difftastic is available, run its Magit diff command interactively."
  (let (captured-command)
    (cl-letf (((symbol-function 'fboundp)
               (lambda (fn)
                 (eq fn 'difftastic-magit-diff)))
              ((symbol-function 'call-interactively)
               (lambda (fn &optional _record-flag _keys)
                 (setq captured-command fn))))
      (ai-code--review-current-branch-with-difftastic)
      (should (eq captured-command #'difftastic-magit-diff)))))

(ert-deftest ai-code-test-review-current-branch-with-difftastic-signals-install-guidance ()
  "When difftastic is unavailable, show installation guidance."
  (cl-letf (((symbol-function 'fboundp)
              (lambda (_fn) nil)))
    (let ((error-message
           (cadr (should-error (ai-code--review-current-branch-with-difftastic)
                               :type 'user-error))))
      (should (string-match-p "MELPA" error-message))
      (should (string-match-p "pkryger/difftastic\\.el" error-message)))))

(ert-deftest ai-code-test-pull-or-review-diff-file-use-github-mcp ()
  "When user chooses GitHub MCP in non-diff buffer, insert a PR review prompt."
  (pcase-let ((`(,captured-prompt ,diff-called)
                (ai-code-test--run-pull-or-review-diff-file "Use GitHub MCP server"
                                                            "https://github.com/acme/demo/pull/123")))
    (let ((case-fold-search nil))
      (should (string-match-p "Use GitHub MCP server" captured-prompt)))
    (should (string-match-p "https://github.com/acme/demo/pull/123" captured-prompt))
    (should-not diff-called)))

(ert-deftest ai-code-test-pull-or-review-diff-file-use-gh-cli ()
  "When user chooses gh CLI in non-diff buffer, insert a PR review prompt."
  (pcase-let ((`(,captured-prompt ,diff-called)
               (ai-code-test--run-pull-or-review-diff-file "Use gh CLI tool"
                                                           "https://github.com/acme/demo/pull/456")))
    (let ((case-fold-search nil))
      (should (string-match-p "Use gh CLI tool" captured-prompt)))
    (should (string-match-p "https://github.com/acme/demo/pull/456" captured-prompt))
    (should-not diff-called)))

(ert-deftest ai-code-test-pull-or-review-diff-file-generate-diff-option ()
  "When user chooses diff generation in non-diff buffer, keep existing logic."
  (pcase-let ((`(,captured-prompt ,diff-called)
               (ai-code-test--run-pull-or-review-diff-file "Use GitHub MCP server" nil "Generate diff file")))
    (should diff-called)
    (should-not captured-prompt)))

(ert-deftest ai-code-test-pull-or-review-diff-file-check-feedback-github-mcp ()
  "When choosing feedback mode with GitHub MCP, prompt should target unresolved feedback."
  (pcase-let ((`(,captured-prompt ,diff-called)
               (ai-code-test--run-pull-or-review-diff-file "Use GitHub MCP server"
                                                           "https://github.com/acme/demo/pull/789"
                                                           "Check unresolved feedback")))
    (let ((case-fold-search nil))
      (should (string-match-p "Use GitHub MCP server" captured-prompt)))
    (should (string-match-p "unresolved feedback" (downcase captured-prompt)))
    (should (string-match-p "no need to make code change" (downcase captured-prompt)))
    (should-not diff-called)))

(ert-deftest ai-code-test-pull-or-review-diff-file-check-feedback-gh-cli ()
  "When choosing feedback mode with gh CLI, prompt should target unresolved feedback."
  (pcase-let ((`(,captured-prompt ,diff-called)
               (ai-code-test--run-pull-or-review-diff-file "Use gh CLI tool"
                                                           "https://github.com/acme/demo/pull/790"
                                                           "Check unresolved feedback")))
    (let ((case-fold-search nil))
      (should (string-match-p "Use gh CLI tool" captured-prompt)))
    (should (string-match-p "unresolved feedback" (downcase captured-prompt)))
    (should (string-match-p "no need to make code change" (downcase captured-prompt)))
    (should-not diff-called)))

(ert-deftest ai-code-test-pull-or-review-diff-file-investigate-issue-github-mcp ()
  "When choosing issue investigation mode, prompt should analyze an issue without code changes."
  (pcase-let ((`(,captured-prompt ,diff-called)
               (ai-code-test--run-pull-or-review-diff-file "Use GitHub MCP server"
                                                           "https://github.com/acme/demo/issues/42"
                                                           "Investigate issue")))
    (let ((case-fold-search nil))
      (should (string-match-p "Use GitHub MCP server" captured-prompt)))
    (should (string-match-p "https://github.com/acme/demo/issues/42" captured-prompt))
    (should (string-match-p "investigate issue" (downcase captured-prompt)))
    (should (string-match-p "repository as context" (downcase captured-prompt)))
    (should (string-match-p "no need to make code change" (downcase captured-prompt)))
    (should-not diff-called)))

(ert-deftest ai-code-test-pull-or-review-diff-file-prepare-pr-description-github-mcp ()
  "When choosing PR description mode, prompt should ask AI to draft a PR description."
  (pcase-let ((`(,captured-prompt ,diff-called)
               (ai-code-test--run-pull-or-review-diff-file "Use GitHub MCP server"
                                                           "https://github.com/acme/demo/pull/791"
                                                           "Prepare PR description")))
    (let ((case-fold-search nil))
      (should (string-match-p "Use GitHub MCP server" captured-prompt)))
    (should (string-match-p "https://github.com/acme/demo/pull/791" captured-prompt))
    (should (string-match-p "prepare a pull request description" (downcase captured-prompt)))
    (should (string-match-p "summary" (downcase captured-prompt)))
    (should (string-match-p "author" (downcase captured-prompt)))
    (should (string-match-p "maintainer" (downcase captured-prompt)))
    (should (string-match-p "testing" (downcase captured-prompt)))
    (should-not diff-called)))

(ert-deftest ai-code-test-pull-or-review-diff-file-review-ci-checks-github-mcp ()
  "When choosing CI checks mode, prompt should ask for root-cause analysis only."
  (pcase-let ((`(,captured-prompt ,diff-called)
               (ai-code-test--run-pull-or-review-diff-file "Use GitHub MCP server"
                                                           "https://github.com/acme/demo/pull/792"
                                                           "Review GitHub CI checks")))
    (let ((case-fold-search nil))
      (should (string-match-p "Use GitHub MCP server" captured-prompt)))
    (should (string-match-p "https://github.com/acme/demo/pull/792" captured-prompt))
    (should (string-match-p "review github ci checks" (downcase captured-prompt)))
    (should (string-match-p "root cause" (downcase captured-prompt)))
    (should (string-match-p "no need to make code change" (downcase captured-prompt)))
    (should-not diff-called)))

(ert-deftest ai-code-test-pull-or-review-diff-file-resolve-merge-conflict-github-mcp ()
  "When choosing resolve merge conflict mode with GitHub MCP, prompt should target merge conflicts."
  (pcase-let ((`(,captured-prompt ,diff-called)
               (ai-code-test--run-pull-or-review-diff-file "Use GitHub MCP server"
                                                           "https://github.com/acme/demo/pull/999"
                                                           "Resolve merge conflict")))
    (let ((case-fold-search nil))
      (should (string-match-p "Use GitHub MCP server" captured-prompt)))
    (should (string-match-p "https://github.com/acme/demo/pull/999" captured-prompt))
    (should (string-match-p "merge" (downcase captured-prompt)))
    (should (string-match-p "conflict" (downcase captured-prompt)))
    (should-not diff-called)))

(ert-deftest ai-code-test-build-issue-investigation-prompt-with-semantic-context ()
  "Include the current semantic scope in an issue investigation prompt."
  (let (captured-prompt)
    (with-temp-buffer
      (setq buffer-file-name "/path/to/my-source-file.el")
      (insert "defun hello-world ()")
      (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'ai-code--pull-or-review-source-instruction)
                 (lambda (&rest _) "Use GitHub MCP server."))
                ((symbol-function 'ai-code--get-context-files-string) (lambda () ""))
                ((symbol-function 'ai-code--format-repo-context-info) (lambda () nil))
                ((symbol-function 'ai-code--current-line-scope-context)
                 (lambda ()
                   (list :function-name "hello-world"
                         :class-name "GreetingService"
                         :class-header "class GreetingService:"
                         :function-header "defun hello-world ()"
                         :range (cons (point-min) (point-max)))))
                ((symbol-function 'ai-code--format-scope-context)
                 (lambda (context)
                   (should (equal (plist-get context :class-name)
                                  "GreetingService"))
                   (concat "Enclosing class: GreetingService\n"
                           "Class definition: class GreetingService:\n"
                           "Function: hello-world\n"
                           "Function definition: defun hello-world ()"))))
        (setq captured-prompt
              (ai-code--build-issue-investigation-init-prompt
               'github-mcp "https://github.com/acme/demo/issues/42" t))))
    (should (string-match-p "Investigate issue: https://github.com/acme/demo/issues/42" captured-prompt))
    (should (string-match-p "Local Context:" captured-prompt))
    (should (string-match-p "Current file: /path/to/my-source-file.el" captured-prompt))
    (should (string-match-p "Class definition: class GreetingService:" captured-prompt))
    (should (string-match-p "Function definition: defun hello-world ()" captured-prompt))))

(provide 'test_ai-code-github)

;;; test_ai-code-github.el ends here
