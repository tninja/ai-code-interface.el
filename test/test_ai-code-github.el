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
                 (lambda (prompt &optional initial-input _candidate-list)
                   (if (string-match-p "URL:" prompt)
                       pr-url
                     initial-input)))
                ((symbol-function 'ai-code--insert-prompt)
                 (lambda (prompt) (setq captured-prompt prompt)))
                ((symbol-function 'ai-code--magit-generate-feature-branch-diff-file)
                 (lambda () (setq diff-called t))))
        (ai-code-pull-or-review-diff-file)))
    (list captured-prompt diff-called)))

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
               (lambda (_review-source)
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
               (lambda (_review-source)
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

(ert-deftest ai-code-test-build-send-current-branch-pr-init-prompt ()
  "Build a concise PR creation prompt for the current branch."
  (let ((prompt (ai-code--build-send-current-branch-pr-init-prompt
                 'gh-cli
                 "feature/improve-pr-flow"
                 "main")))
    (let ((case-fold-search nil))
      (should (string-match-p "Use GitHub CLI to create the pull request" prompt)))
    (should (string-match-p "feature/improve-pr-flow" prompt))
    (should (string-match-p "main" prompt))
    (should (string-match-p "create a pull request" (downcase prompt)))
    (should (string-match-p "short" (downcase prompt)))
    (should (string-match-p "author" (downcase prompt)))
    (should-not (string-match-p "review comments" (downcase prompt)))))

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
                   initial-input)))
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
      (should (string-match-p "generate a concise pr title" (downcase captured-inserted-prompt))))))

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

(provide 'test_ai-code-github)

;;; test_ai-code-github.el ends here
