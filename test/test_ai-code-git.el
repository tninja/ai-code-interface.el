;;; test_ai-code-git.el --- Tests for ai-code-git.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the ai-code-git module, specifically testing
;; the .gitignore update logic.

;;; Code:

(require 'ert)
(require 'ai-code-git)
(require 'ai-code-prompt-mode)
(require 'ai-code-discussion)

(defun ai-code-test--gitignore-required-entries ()
  "Return the default ignore entries expected from `ai-code-update-git-ignore'."
  (list (concat ai-code-files-dir-name "/")
        ".projectile"
        "GTAGS"
        "GRTAGS"
        "GPATH"
        "__pycache__/"
        "*.elc"
        "flycheck_*"))

(ert-deftest ai-code-test-ai-code-gitignore-regex-pattern ()
  "Test that the regex pattern correctly matches entries in .gitignore.
This is a unit test for the regex pattern used in ai-code-update-git-ignore."
  (let ((gitignore-content "# Test .gitignore file
.ai.code.prompt.org
.ai.code.notes.org
.projectile
GTAGS
GRTAGS
GPATH
# End of file
"))
    ;; Test that existing entries are found
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote ".ai.code.prompt.org")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote ".ai.code.notes.org")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote ".projectile")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote "GTAGS")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote "GRTAGS")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                   (regexp-quote "GPATH")
                                   "\\s-*\\(?:\n\\|$\\)")
                           gitignore-content))
    
    ;; Test that a missing entry is not found
    (should-not (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                       (regexp-quote "MISSING_ENTRY")
                                       "\\s-*\\(?:\n\\|$\\)")
                               gitignore-content))
    
    ;; Test entries with whitespace
    (let ((gitignore-with-whitespace "  .projectile  
GTAGS
"))
      (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                     (regexp-quote ".projectile")
                                     "\\s-*\\(?:\n\\|$\\)")
                             gitignore-with-whitespace))
      (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                     (regexp-quote "GTAGS")
                                     "\\s-*\\(?:\n\\|$\\)")
                             gitignore-with-whitespace)))
    
    ;; Test entry at beginning of file (no leading newline)
    (let ((gitignore-start ".ai.code.prompt.org
other-file"))
      (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                     (regexp-quote ".ai.code.prompt.org")
                                     "\\s-*\\(?:\n\\|$\\)")
                             gitignore-start)))
    
    ;; Test entry at end of file (no trailing newline)
    (let ((gitignore-end "other-file
.ai.code.prompt.org"))
      (should (string-match-p (concat "\\(?:^\\|\n\\)\\s-*"
                                     (regexp-quote ".ai.code.prompt.org")
                                     "\\s-*\\(?:\n\\|$\\)")
                             gitignore-end)))))


(ert-deftest ai-code-test-ai-code-update-git-ignore-no-duplicates ()
  "Test that ai-code-update-git-ignore does not add duplicate entries.
When .gitignore already contains the required entries, they should
not be added again."
  (let* ((temp-dir (file-truename (make-temp-file "ai-code-test-" t)))
         (gitignore-path (expand-file-name ".gitignore" temp-dir))
         (required-entries (ai-code-test--gitignore-required-entries)))
    (unwind-protect
        (progn
          ;; Initialize git repository
          (let ((default-directory temp-dir))
            (shell-command "git init"))

          ;; Create .gitignore with entries already present
          (with-temp-file gitignore-path
            (insert "# Existing entries\n")
            (dolist (entry required-entries)
              (insert entry "\n"))
            (insert "# End of file\n"))

          ;; Store original content
          (let ((original-content (with-temp-buffer
                                    (insert-file-contents gitignore-path)
                                    (buffer-string))))

            ;; Mock ai-code--git-root to return temp-dir
            (cl-letf (((symbol-function 'ai-code--git-root)
                       (lambda (&optional dir) temp-dir)))
              ;; Call the function
              (ai-code-update-git-ignore))

            ;; Read the updated content
            (let ((updated-content (with-temp-buffer
                                     (insert-file-contents gitignore-path)
                                     (buffer-string))))
              ;; Content should be the same (no duplicates added)
              (should (string= original-content updated-content))

              ;; Each entry should appear exactly once
              (dolist (entry required-entries)
                (let ((count 0))
                  (with-temp-buffer
                    (insert updated-content)
                    (goto-char (point-min))
                    (while (re-search-forward (concat "^\\s-*" (regexp-quote entry) "\\s-*$") nil t)
                      (setq count (1+ count))))
                  (should (= count 1)))))))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest ai-code-test-ai-code-update-git-ignore-adds-missing ()
  "Test that ai-code-update-git-ignore adds missing entries.
When .gitignore is missing some entries, they should be added."
  (let* ((temp-dir (file-truename (make-temp-file "ai-code-test-" t)))
         (gitignore-path (expand-file-name ".gitignore" temp-dir)))
    (unwind-protect
        (progn
          ;; Initialize git repository
          (let ((default-directory temp-dir))
            (shell-command "git init"))

          ;; Create .gitignore with only some entries
          (with-temp-file gitignore-path
            (insert "# Existing entries\n")
            (insert ".projectile\n")
            (insert "GTAGS\n"))

          ;; Mock ai-code--git-root to return temp-dir
          (cl-letf (((symbol-function 'ai-code--git-root)
                     (lambda (&optional dir) temp-dir)))
            ;; Call the function
            (ai-code-update-git-ignore))

          ;; Read the updated content
          (let ((updated-content (with-temp-buffer
                                   (insert-file-contents gitignore-path)
                                   (buffer-string))))
            ;; All required entries should be present
            (dolist (entry (ai-code-test--gitignore-required-entries))
              (should (string-match-p (regexp-quote entry) updated-content)))))
      ;; Cleanup
      (delete-directory temp-dir t))))

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
               (ai-code-test--run-pull-or-review-diff-file "Generate diff file" nil)))
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

(ert-deftest ai-code-test-build-pr-review-init-prompt-uses-fallback-for-unknown-source ()
  "Unknown review source should use the fallback instruction."
  (let ((prompt (ai-code--build-pr-review-init-prompt
                 'unknown-source
                 "https://github.com/acme/demo/pull/999")))
    (should (string-match-p "Review this pull request\\." prompt))))

(ert-deftest ai-code-test-git-worktree-branch-creates-repo-directory-and-adds-worktree ()
  "Create repo worktree directory and invoke git worktree add with expected path."
  (let* ((temp-worktree-root (make-temp-file "ai-code-worktree-root-" t))
         (ai-code-git-worktree-root temp-worktree-root)
         (git-root "/tmp/sample-repo/")
         (branch "feature/new-branch")
         (start-point "main")
         (repo-dir (expand-file-name "sample-repo" temp-worktree-root))
         (worktree-path (expand-file-name branch repo-dir))
         (worktree-parent-dir (file-name-directory worktree-path))
         captured-git-args
         captured-visited-path)
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code--validate-git-repository)
                   (lambda () git-root))
                  ((symbol-function 'magit-run-git)
                   (lambda (&rest _args)
                     (ert-fail "`magit-run-git' should not be used for worktree add status check")))
                  ((symbol-function 'magit-call-git)
                   (lambda (&rest args)
                     (setq captured-git-args args)
                     0))
                  ((symbol-function 'magit-diff-visit-directory)
                   (lambda (path)
                     (setq captured-visited-path path))))
          (should-not (file-directory-p repo-dir))
          (ai-code-git-worktree-branch branch start-point)
          (should (file-directory-p repo-dir))
          (should (file-directory-p worktree-parent-dir))
          (should (equal captured-git-args
                         (list "worktree"
                               "add"
                               "-b"
                               branch
                               (file-truename worktree-path)
                               start-point)))
          (should (equal captured-visited-path worktree-path)))
      (delete-directory temp-worktree-root t))))

(ert-deftest ai-code-test-git-worktree-action-without-prefix-calls-worktree-branch ()
  "Without prefix arg, dispatch to `ai-code-git-worktree-branch'."
  (let (captured-fn)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (fn &optional _record-flag _keys)
                 (setq captured-fn fn))))
      (ai-code-git-worktree-action nil)
      (should (eq captured-fn #'ai-code-git-worktree-branch)))))

(ert-deftest ai-code-test-git-worktree-action-with-prefix-calls-magit-worktree-status ()
  "With prefix arg, dispatch to `magit-worktree-status'."
  (let (captured-fn)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (fn &optional _record-flag _keys)
                 (setq captured-fn fn))))
      (ai-code-git-worktree-action '(4))
      (should (eq captured-fn #'magit-worktree-status)))))

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

(provide 'test_ai-code-git)

;;; test_ai-code-git.el ends here
