;;; ai-code-discussion.el --- AI code discussion operations -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides code discussion functionality for the AI Code Interface package.

;;; Code:

(require 'which-func)
(require 'savehist)

(require 'ai-code-input)
(require 'ai-code-prompt-mode)
(require 'ai-code-change)

(declare-function ai-code--insert-prompt "ai-code-prompt-mode")
(declare-function ai-code--get-clipboard-text "ai-code")
(declare-function ai-code-call-gptel-sync "ai-code-prompt-mode")
(declare-function ai-code--ensure-files-directory "ai-code-prompt-mode")
(declare-function ai-code--git-root "ai-code-file" (&optional dir))
(declare-function ai-code--format-repo-context-info "ai-code-file")
(declare-function ai-code--pull-or-review-action-choice "ai-code-github")
(declare-function ai-code--pull-or-review-source-instruction "ai-code-github"
                  (review-source &optional review-mode))
(declare-function org-current-level "org")
(declare-function org-roam-db-update-file "org-roam-db" (&optional file-path no-require))
(declare-function org-roam-db-sync "org-roam-db" (&optional force))
(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))
(declare-function dired-get-marked-files "dired"
                  (&optional localp arg filter distinguish-one-marked error-if-none-p))

(defvar ai-code--repo-context-info)
(defvar org-roam-directory)
(defvar ai-code-note-request-history nil
  "Minibuffer history for note requests.")

(defun ai-code--ensure-note-request-history ()
  "Register persistent minibuffer history for note requests.
Only adds the variable to savehist tracking; does not enable savehist-mode.
Users should enable savehist-mode in their Emacs configuration to persist history."
  (add-to-list 'savehist-additional-variables 'ai-code-note-request-history))

(ai-code--ensure-note-request-history)

(defconst ai-code-discussion--question-only-note
  "Note: This is a question only - please do not modify the code."
  "Prompt note for question-only requests.")

(defconst ai-code-discussion--selected-region-note
  "Note: This is a question about the selected region - please do not modify the code."
  "Prompt note for question-only requests about the selected region.")

(defconst ai-code-discussion--explain-selected-files-prefix
  "Please explain the selected files or directories."
  "Prompt prefix for explaining selected files or directories.")

(defconst ai-code-discussion--explain-file-at-path-prefix
  "Please explain the file at path @"
  "Prompt prefix for explaining a file selected from Dired.")

(defconst ai-code-discussion--explain-directory-at-path-prefix
  "Please explain the directory at path @"
  "Prompt prefix for explaining a directory selected from Dired.")

(defconst ai-code-discussion--explain-code-prefix
  "Please explain the following code:"
  "Prompt prefix for explaining a selected region.")

(defconst ai-code-discussion--explain-symbol-prefix
  "Please explain the symbol '"
  "Prompt prefix for explaining a symbol.")

(defconst ai-code-discussion--explain-line-prefix
  "Please explain the following line of code:"
  "Prompt prefix for explaining the current line.")

(defconst ai-code-discussion--explain-function-prefix
  "Please explain the function '"
  "Prompt prefix for explaining the current function.")

(defconst ai-code-discussion--explain-file-prefix
  "Please explain the following file:"
  "Prompt prefix for explaining the current file.")

(defconst ai-code-discussion--explain-files-prefix
  "Please explain the following files:"
  "Prompt prefix for explaining visible files.")

(defconst ai-code-discussion--explain-git-repo-prefix
  "Please explain the current git repository:"
  "Prompt prefix for explaining the current repository.")

(defconst ai-code-discussion--explain-code-change-pr-prefix
  "Please explain the code change in pull request:"
  "Prompt prefix for explaining a pull request code change.")

(defconst ai-code-discussion--explain-code-change-branch-range-prefix
  "Please explain the code change between branches:"
  "Prompt prefix for explaining a branch range code change.")

(defconst ai-code-discussion--explain-code-change-commit-prefix
  "Please explain the code change introduced by commit:"
  "Prompt prefix for explaining a commit code change.")

(defconst ai-code-discussion--explain-code-change-focus-note
  "4. Focus on understanding the change. Do not make code changes."
  "Shared final instruction for code change explanation prompts.")

(defconst ai-code-discussion--explain-prompt-prefixes
  (list ai-code-discussion--explain-selected-files-prefix
        ai-code-discussion--explain-file-at-path-prefix
        ai-code-discussion--explain-directory-at-path-prefix
        ai-code-discussion--explain-code-prefix
        ai-code-discussion--explain-symbol-prefix
        ai-code-discussion--explain-line-prefix
        ai-code-discussion--explain-function-prefix
        ai-code-discussion--explain-file-prefix
        ai-code-discussion--explain-files-prefix
        ai-code-discussion--explain-git-repo-prefix
        ai-code-discussion--explain-code-change-pr-prefix
        ai-code-discussion--explain-code-change-branch-range-prefix
        ai-code-discussion--explain-code-change-commit-prefix)
  "Known explain prompt prefixes generated by discussion commands.")

;;;###autoload
(defun ai-code-ask-question (arg)
  "Generate prompt to ask questions about specific code.
With a prefix argument \[universal-argument], append the clipboard
contents as context.  If current buffer is a file, keep existing logic.
If current buffer is a Dired buffer:
  - If there are files or directories marked, use them as context
    \(use git repo relative path, start with @ character)
  - If there are no files or dirs marked, but under cursor there is
    file or dir, use it as context of prompt
If a region is selected, ask about that specific region.
If cursor is in a function, ask about that function.
Otherwise, ask a general question about the file.
Inserts the prompt into the AI prompt file and optionally sends to AI.

Argument ARG is the prefix argument."
  (interactive "P")
  ;; DONE: similar to ai-code-code-change, when todo-info is available, call ai-code-implement-todo
  (cond
   ((derived-mode-p 'dired-mode)
    (let ((clipboard-context (when arg (ai-code--get-clipboard-text))))
      (ai-code--ask-question-dired clipboard-context)))
   (t
    (let ((todo-info (when buffer-file-name
                       (ai-code--detect-todo-info (region-active-p)))))
      (if todo-info
          (ai-code-implement-todo arg "Ask question")
        (let ((clipboard-context (when arg (ai-code--get-clipboard-text))))
          (ai-code--ask-question-file clipboard-context)))))))

(defun ai-code--ask-question-dired (clipboard-context)
  "Handle ask question for Dired buffer.
CLIPBOARD-CONTEXT is optional clipboard text to append as context."
  (let* ((all-marked (dired-get-marked-files))
         (file-at-point (dired-get-filename nil t))
         (truly-marked (remove file-at-point all-marked))
         (has-marks (> (length truly-marked) 0))
         (context-files (cond
                         (has-marks truly-marked)
                         (file-at-point (list file-at-point))
                         (t nil)))
         (git-relative-files (when context-files
                              (ai-code--get-git-relative-paths context-files)))
         (files-context-string (when git-relative-files
                                (concat "\nFiles:\n"
                                       (mapconcat (lambda (f) (concat "@" f))
                                                 git-relative-files "\n"))))
         (prompt-label (cond
                        ((and clipboard-context
                              (string-match-p "\\S-" clipboard-context))
                         (if has-marks
                             "Question about marked files/directories (clipboard context): "
                           (if file-at-point
                               (format "Question about %s (clipboard context): " (file-name-nondirectory file-at-point))
                             "General question about directory (clipboard context): ")))
                        (has-marks "Question about marked files/directories: ")
                        (file-at-point (format "Question about %s: " (file-name-nondirectory file-at-point)))
                        (t "General question about directory: ")))
         (question (ai-code-read-string prompt-label ""))
         (repo-context-string (ai-code--format-repo-context-info))
         (final-prompt (concat question
                                files-context-string
                                repo-context-string
                                (when (and clipboard-context
                                           (string-match-p "\\S-" clipboard-context))
                                  (concat "\n\nClipboard context:\n" clipboard-context))
                                (concat "\n" ai-code-discussion--question-only-note))))
    (ai-code--insert-prompt final-prompt)))

(defun ai-code--ask-question-file (clipboard-context)
  "Handle ask question for regular file buffer.
CLIPBOARD-CONTEXT is optional clipboard text to append as context."
  (let* ((file-extension (when buffer-file-name
                          (file-name-extension buffer-file-name)))
         (is-diff-or-patch (and file-extension
                               (member file-extension '("diff" "patch"))))
         (function-name (unless is-diff-or-patch
                         (which-function)))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (region-location-info (when region-active
                                 (ai-code--get-region-location-info (region-beginning) (region-end))))
         (prompt-label
          (cond
           ((and clipboard-context
                 (string-match-p "\\S-" clipboard-context))
            (cond
             (region-active
              (if function-name
                  (format "Question about selected code in function %s (clipboard context): " function-name)
                "Question about selected code (clipboard context): "))
             (function-name
              (format "Question about function %s (clipboard context): " function-name))
             (buffer-file-name
              (format "General question about %s (clipboard context): " (file-name-nondirectory buffer-file-name)))
             (t "General question (clipboard context): ")))
           (region-active
            (if function-name
                (format "Question about selected code in function %s: " function-name)
              "Question about selected code: "))
           (function-name
            (format "Question about function %s: " function-name))
           (buffer-file-name
            (format "General question about %s: " (file-name-nondirectory buffer-file-name)))
           (t "General question: ")))
         (question (ai-code-read-string prompt-label ""))
         (files-context-string (ai-code--get-context-files-string))
         (repo-context-string (ai-code--format-repo-context-info))
         (final-prompt
          (concat question
                  (when region-text
                    (concat "\nSelected region:\n"
                            (when region-location-info
                              (concat region-location-info "\n"))
                            region-text))
                  (when function-name
                    (format "\nFunction: %s" function-name))
                   files-context-string
                   repo-context-string
                   (when (and clipboard-context
                              (string-match-p "\\S-" clipboard-context))
                     (concat "\n\nClipboard context:\n" clipboard-context))
                   (if region-text
                       (concat "\n" ai-code-discussion--selected-region-note)
                     (concat "\n" ai-code-discussion--question-only-note)))))
    (ai-code--insert-prompt final-prompt)))

(defun ai-code--get-git-relative-paths (file-paths)
  "Convert absolute FILE-PATHS to git repository relative paths.
Returns a list of relative paths from the git repository root."
  (when file-paths
    (let ((git-root (ai-code--git-root)))
      (when git-root
        (mapcar (lambda (file-path)
                  (file-relative-name file-path git-root))
                file-paths)))))

(defun ai-code--get-region-location-info (region-beginning region-end)
  "Compute region location information for the active region.
Returns region-location-info
REGION-BEGINNING and REGION-END are the region boundaries.
Returns nil if region is not active or required information is unavailable."
  (when (and region-beginning region-end buffer-file-name)
    (let* ((region-end-line (line-number-at-pos region-end))
           (region-start-line (line-number-at-pos region-beginning))
           (git-relative-path (car (ai-code--get-git-relative-paths (list buffer-file-name))))
           (region-location-info (when (and git-relative-path region-start-line region-end-line)
                                   (format "%s#L%d-L%d" git-relative-path region-start-line region-end-line))))
      region-location-info)))

;;;###autoload
(defun ai-code-investigate-exception (arg)
  "Generate prompt to investigate exceptions or errors in code.
With a prefix argument \[universal-argument], use context from clipboard
as the error to investigate.  If a *compilation* buffer is visible in
the current window, use its full content as context.  If a region is
selected, investigate that specific error or exception.  If cursor is
in a function, investigate exceptions in that function.  Otherwise,
investigate general exception handling in the file.  Inserts the prompt
into the AI prompt file and optionally sends to AI.
Argument ARG is the prefix argument."
  (interactive "P")
  (let* ((clipboard-content (when arg
                             (condition-case nil
                               (current-kill 0)
                               (error nil))))
         (compilation-buffer (get-buffer "*compilation*"))
         (compilation-content (when (and compilation-buffer
                                        (get-buffer-window compilation-buffer)
                                        (not arg))
                               (with-current-buffer compilation-buffer
                                 (buffer-substring-no-properties (point-min) (point-max)))))
         (region-text (when (region-active-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (buffer-file buffer-file-name)
         (full-buffer-context (when (and (not buffer-file) (not region-text))
                                (buffer-substring-no-properties (point-min) (point-max))))
         (function-name (which-function))
         (files-context-string (ai-code--get-context-files-string))
         (repo-context-string (ai-code--format-repo-context-info))
         (context-section
          (if full-buffer-context
              (concat "\n\nContext:\n" full-buffer-context)
            (let ((context-blocks nil))
              (when clipboard-content
                (push (concat "Clipboard context (error/exception):\n" clipboard-content)
                      context-blocks))
              (when compilation-content
                (push (concat "Compilation output:\n" compilation-content)
                      context-blocks))
              (when region-text
                (push (concat "Selected code:\n" region-text)
                      context-blocks))
              (when context-blocks
                (concat "\n\nContext:\n"
                        (mapconcat #'identity (nreverse context-blocks) "\n\n"))))))
         (default-question "How to fix the error in this code? Please analyze the error, explain the root cause, and provide the corrected code to resolve the issue: ")
         (prompt-label
          (cond
           (clipboard-content
            "Investigate error from clipboard: ")
           (compilation-content
            "Investigate compilation error: ")
           (full-buffer-context
            "Investigate exception in current buffer: ")
           (region-text
            (if function-name
                (format "Investigate exception in function %s: " function-name)
              "Investigate selected exception: "))
           (function-name
            (format "Investigate exceptions in function %s: " function-name))
           (t "Investigate exceptions in code: ")))
         (initial-prompt (ai-code-read-string prompt-label default-question))
         (final-prompt
          (concat initial-prompt
                  context-section
                  (when function-name (format "\nFunction: %s" function-name))
                  files-context-string
                  repo-context-string
                  (concat "\n\nNote: Please focus on how to fix the error. Your response should include:\n"
                          "1. A brief explanation of the root cause of the error.\n"
                          "2. A code snippet with the fix.\n"
                          "3. An explanation of how the fix addresses the error."))))
         (ai-code--insert-prompt final-prompt)))

;;;###autoload
(defun ai-code-explain ()
  "Generate prompt to explain code at different levels.
If current buffer is a Dired buffer and under cursor is a directory or
file, explain that directory or file using relative path as context
\(start with @ character).  If a region is selected, explain that
specific region using function/file as context.  Otherwise, prompt user
to select scope: symbol, line, function, file, repository, or code
change.  Inserts the prompt into the AI prompt file and optionally
sends to AI."
  (interactive)
  (cond
   ;; Handle dired buffer
   ((derived-mode-p 'dired-mode)
    (ai-code--explain-dired))
   ;; Handle region selection
   ((region-active-p)
    (ai-code--explain-region))
   ;; Handle regular file buffer
   (t (ai-code--explain-with-scope-selection))))

(defun ai-code--explain-dired ()
  "Handle explain for Dired buffer."
  (let* ((file-at-point (dired-get-filename nil t))
         (all-marked (dired-get-marked-files))
         (has-marked-files (> (length all-marked) 1))
         (context-files (if has-marked-files
                            all-marked
                          (when file-at-point
                            (list file-at-point))))
         (git-relative-paths (when context-files
                               (ai-code--get-git-relative-paths context-files)))
         (files-context-string (when git-relative-paths
                                (concat "\nFiles:\n"
                                        (mapconcat (lambda (path) (concat "@" path))
                                                   git-relative-paths
                                                   "\n"))))
          (file-type (if (and file-at-point (file-directory-p file-at-point))
                         "directory"
                       "file"))
          (path-prefix (if (string-equal file-type "directory")
                           ai-code-discussion--explain-directory-at-path-prefix
                         ai-code-discussion--explain-file-at-path-prefix))
          (initial-prompt (cond
                           (has-marked-files
                            (format "%s\n\nProvide a clear explanation of what these files or directories contain, their purpose, and their role in the project structure.%s"
                                    ai-code-discussion--explain-selected-files-prefix
                                    (or files-context-string "")))
                           ((car git-relative-paths)
                            (format "%s%s.\n\nProvide a clear explanation of what this %s contains, its purpose, and its role in the project structure.%s"
                                    path-prefix
                                    (car git-relative-paths)
                                    file-type
                                    (or files-context-string "")))
                           (t "No file or directory found at cursor point.")))
         (final-prompt (if git-relative-paths
                           (ai-code-read-string "Prompt: " initial-prompt)
                         initial-prompt)))
    (when final-prompt
      (ai-code--insert-prompt final-prompt))))

(defun ai-code--explain-region ()
  "Explain the selected region with function/file context."
  (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
         (function-name (which-function))
         (context-info (if function-name
                          (format "Function: %s" function-name)
                        ""))
         (files-context-string (ai-code--get-context-files-string))
         (initial-prompt (format "%s\n\n%s\n\n%s%s%s\n\nProvide a clear explanation of what this code does, how it works, and its purpose within the context."
                         ai-code-discussion--explain-code-prefix
                         region-text
                         context-info
                         (if function-name "\n" "")
                        files-context-string))
         (final-prompt (ai-code-read-string "Prompt: " initial-prompt)))
    (when final-prompt
      (ai-code--insert-prompt final-prompt))))

(defun ai-code--explain-with-scope-selection ()
  "Prompt user to select explanation scope and explain accordingly."
  (let* ((choices '("symbol" "line" "function" "file" "files visible"
                    "git repository" "code change"))
         (scope (completing-read "Select scope to explain: " choices nil t)))
    (pcase scope
      ("symbol" (ai-code--explain-symbol))
      ("line" (ai-code--explain-line))
      ("function" (ai-code--explain-function))
      ("file" (ai-code--explain-file))
      ("files visible" (ai-code--explain-files-visible))
      ("git repository" (ai-code--explain-git-repo))
      ("code change" (ai-code--explain-code-change)))))

(defun ai-code--explain-code-change-source-instruction (review-source)
  "Return a PR explanation source instruction for REVIEW-SOURCE."
  (if (fboundp 'ai-code--pull-or-review-source-instruction)
      (ai-code--pull-or-review-source-instruction review-source 'explain-code-change)
    "Inspect the pull request diff and relevant metadata to understand the change."))

(defun ai-code--ensure-explain-code-change-review-source (review-source)
  "Return REVIEW-SOURCE or prompt for one when needed."
  (or review-source
      (progn
        (require 'ai-code-git nil t)
        (if (fboundp 'ai-code--pull-or-review-action-choice)
            (ai-code--pull-or-review-action-choice)
          (let* ((action-alist '(("Use GitHub MCP server" . github-mcp)
                                 ("Use gh CLI tool" . gh-cli)))
                 (choice (completing-read "Select review source: "
                                          action-alist
                                          nil t nil nil
                                          "Use GitHub MCP server")))
            (alist-get choice action-alist nil nil #'string=))))))

(defun ai-code--explain-code-change-insert-prompt (initial-prompt)
  "Read and insert an explanation prompt starting from INITIAL-PROMPT."
  (let ((final-prompt (ai-code-read-string "Prompt: " initial-prompt)))
    (when final-prompt
      (ai-code--insert-prompt final-prompt))))

(defun ai-code--format-code-change-explanation-outline (step-1 step-2 step-3)
  "Return shared code-change explanation outline using STEP-1, STEP-2, and STEP-3."
  (mapconcat #'identity
             (list "Explanation Steps:"
                   (concat "1. " step-1)
                   (concat "2. " step-2)
                   (concat "3. " step-3)
                   ai-code-discussion--explain-code-change-focus-note)
             "\n"))

(defun ai-code--explain-code-change (&optional review-source)
  "Explain a code change from a PR, branch range, or commit.
When REVIEW-SOURCE is non-nil, use it for the GitHub PR flow."
  (let* ((choices '(("GitHub PR" . github-pr)
                    ("base..branch" . branch-range)
                    ("commit" . commit)))
         (selection (completing-read "Select code change source: " choices nil t))
         (code-change-source (alist-get selection choices nil nil #'string=)))
    (pcase code-change-source
      ('github-pr
       (ai-code--explain-code-change-from-github-pr
        (ai-code--ensure-explain-code-change-review-source review-source)))
      ('branch-range
       (ai-code--explain-code-change-from-branch-range))
      ('commit
       (ai-code--explain-code-change-from-commit))
      (_
       (user-error "Unknown code change source: %s" selection)))))

(defun ai-code--explain-code-change-from-github-pr (review-source)
  "Build a prompt to explain a code change from a GitHub PR using REVIEW-SOURCE."
  (let* ((pr-url (ai-code-read-string "Pull request URL: "))
         (source-instruction
          (ai-code--explain-code-change-source-instruction review-source))
         (repo-context-string (ai-code--format-repo-context-info))
         (initial-prompt
          (format "%s %s

%s

%s%s"
                  ai-code-discussion--explain-code-change-pr-prefix
                  pr-url
                  source-instruction
                  (ai-code--format-code-change-explanation-outline
                   "Summarize the overall goal of the code change."
                   "Explain the main files, functions, and behavior changes in the PR."
                   "Highlight important design decisions, risks, and follow-up considerations.")
                  repo-context-string)))
    (ai-code--explain-code-change-insert-prompt initial-prompt)))

(defun ai-code-explain-code-change (&optional review-source)
  "Explain a code change using shared discussion helpers.
When REVIEW-SOURCE is non-nil, use it for the GitHub PR flow."
  (interactive)
  (ai-code--explain-code-change review-source))

(defun ai-code--explain-code-change-from-branch-range ()
  "Build a prompt to explain a code change from BASE..BRANCH."
  (let ((git-root (ai-code--git-root)))
    (unless git-root
      (user-error "Not in a git repository"))
    (let* ((base-branch (ai-code-read-string "Base branch: "))
           (branch-name (ai-code-read-string "Branch to explain: "))
           (repo-context-string (ai-code--format-repo-context-info))
           (explanation-outline
            (ai-code--format-code-change-explanation-outline
             "The overall purpose of this change set."
             "The most important files, functions, and logic changes."
             "The expected behavior impact, migration notes, and risks."))
           (initial-prompt
            (format "%s
Change range: %s..%s
Path: %s

In the current repository, inspect `git diff %s..%s` and explain:
%s%s"
                    ai-code-discussion--explain-code-change-branch-range-prefix
                    base-branch
                    branch-name
                    git-root
                    base-branch
                    branch-name
                    explanation-outline
                    repo-context-string)))
      (ai-code--explain-code-change-insert-prompt initial-prompt))))

(defun ai-code--explain-code-change-from-commit ()
  "Build a prompt to explain a code change from a specific commit."
  (let ((git-root (ai-code--git-root)))
    (unless git-root
      (user-error "Not in a git repository"))
    (let* ((commit-hash (ai-code-read-string "Commit hash: "))
           (repo-context-string (ai-code--format-repo-context-info))
           (explanation-outline
            (ai-code--format-code-change-explanation-outline
             "The problem this commit appears to address."
             "The key code paths and behavior changes introduced by the commit."
             "Any noteworthy implementation details, risks, or trade-offs."))
           (initial-prompt
            (format "%s %s
Path: %s

In the current repository, inspect `git show %s` and explain:
%s%s"
                    ai-code-discussion--explain-code-change-commit-prefix
                    commit-hash
                    git-root
                    commit-hash
                    explanation-outline
                    repo-context-string)))
      (ai-code--explain-code-change-insert-prompt initial-prompt))))

(defun ai-code--explain-symbol ()
  "Explain the symbol at point."
  (let* ((symbol (thing-at-point 'symbol t))
         (function-name (which-function)))
    (unless symbol
      (user-error "No symbol at point"))
    (let* ((initial-prompt (format "%s%s' in the context of:%s\nFile: %s\n\nExplain what this symbol represents, its type, purpose, and how it's used in this context."
                                   ai-code-discussion--explain-symbol-prefix
                                   symbol
                                   (if function-name
                                       (format "\nFunction: %s" function-name)
                                     "")
                                  (or buffer-file-name "current buffer")))
           (final-prompt (ai-code-read-string "Prompt: " initial-prompt)))
      (when final-prompt
        (ai-code--insert-prompt final-prompt)))))

(defun ai-code--explain-line ()
  "Explain the current line."
  (let* ((line-text (string-trim (thing-at-point 'line t)))
         (line-number (line-number-at-pos))
         (function-name (which-function)))
    (let* ((initial-prompt (format "%s\n\nLine %d: %s\n\n%sFile: %s\n\nExplain what this line does, its purpose, and how it fits into the surrounding code."
                                   ai-code-discussion--explain-line-prefix
                                   line-number
                                   line-text
                                   (if function-name
                                       (format "Function: %s\n" function-name)
                                    "")
                                  (or buffer-file-name "current buffer")))
           (final-prompt (ai-code-read-string "Prompt: " initial-prompt)))
      (when final-prompt
        (ai-code--insert-prompt final-prompt)))))

(defun ai-code--explain-function ()
  "Explain the current function."
  (let ((function-name (which-function)))
    (unless function-name
      (user-error "Not inside a function"))
    (let* ((initial-prompt (format "%s%s':
File: %s
Explain what this function does, its parameters, return value, algorithm, and its role in the overall codebase."
                                   ai-code-discussion--explain-function-prefix
                                   function-name
                                   (or buffer-file-name "current buffer")))
            (final-prompt (ai-code-read-string "Prompt: " initial-prompt)))
      (when final-prompt
        (ai-code--insert-prompt final-prompt)))))

(defun ai-code--explain-file ()
  "Explain the current file."
  (let ((file-name (or buffer-file-name "current buffer")))
    (let* ((initial-prompt (format "%s\nFile: %s\nProvide an overview of this file's purpose, its main components, key functions, and how it fits into the larger codebase architecture."
                                  ai-code-discussion--explain-file-prefix
                                  file-name))
            (final-prompt (ai-code-read-string "Prompt: " initial-prompt)))
      (when final-prompt
        (ai-code--insert-prompt final-prompt)))))

(defun ai-code--explain-files-visible ()
  "Explain all files visible in the current window."
  (let ((files-context (ai-code--get-context-files-string)))
    (if (string-empty-p files-context)
        (user-error "No visible files with names found")
      (let* ((initial-prompt (format "%s%s\n\nProvide an overview of these files, their relationships, and how they collectively contribute to the project's functionality."
                                    ai-code-discussion--explain-files-prefix
                                    files-context))
              (final-prompt (ai-code-read-string "Prompt: " initial-prompt)))
        (when final-prompt
          (ai-code--insert-prompt final-prompt))))))

(defun ai-code--explain-git-repo ()
  "Explain the current git repository."
  (let ((git-root (ai-code--git-root)))
    (if (not git-root)
        (user-error "Not in a git repository")
      (let* ((repo-name (file-name-nondirectory (directory-file-name git-root)))
             (initial-prompt (format "%s %s\nPath: %s\n\nProvide a comprehensive overview of this repository, its architecture, main technologies used, key modules, and how the different parts of the system interact."
                                    ai-code-discussion--explain-git-repo-prefix
                                    repo-name git-root))
             (final-prompt (ai-code-read-string "Prompt: " initial-prompt)))
        (when final-prompt
          (ai-code--insert-prompt final-prompt))))))

;;;###autoload
(defcustom ai-code-notes-file-name ".ai.code.notes.org"
  "Default note file name relative to the project root.
This value is used by `ai-code-take-notes' when suggesting where to store notes."
  :type 'string
  :group 'ai-code)

(defconst ai-code-discussion--default-note-request
  "Content of the most recent AI output"
  "Default request text for `ai-code-take-notes'.")

(defun ai-code--get-note-candidates (default-note-file)
  "Get a list of candidate note files.
DEFAULT-NOTE-FILE is included in the list.  Visible org buffers are prioritized."
  (let* ((default-note-file-truename (file-truename default-note-file))
         ;; Get all org-mode buffers with associated files
         (org-buffers (seq-filter
                       (lambda (buf)
                         (with-current-buffer buf
                           (and (derived-mode-p 'org-mode)
                                (buffer-file-name))))
                       (buffer-list)))
         (org-buffer-files (mapcar #'buffer-file-name org-buffers))
         ;; Get org buffers visible in the current frame
         (visible-org-buffers (seq-filter (lambda (buf) (get-buffer-window buf 'visible))
                                         org-buffers))
         (visible-org-files (mapcar #'buffer-file-name visible-org-buffers)))
    (delete-dups
     (mapcar #'file-truename
             (append visible-org-files
                     (list default-note-file-truename)
                     org-buffer-files)))))

(defun ai-code--append-org-note (file title content)
  "Append a note with TITLE and CONTENT to FILE."
  (let ((note-dir (file-name-directory file)))
    (unless (file-exists-p note-dir)
      (make-directory note-dir t)))
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-max))
      (unless (bobp)
        (insert "\n\n"))
      (insert "* " title "\n")
      (org-insert-time-stamp (current-time) t nil)
      (insert "\n\n")
      (insert content)
      (insert "\n"))
    (save-buffer)))

(defun ai-code--build-note-insert-prompt (file-path line-number note-request)
  "Build an AI prompt to insert a note into FILE-PATH.
LINE-NUMBER and NOTE-REQUEST are included in the prompt context."
  (format (concat
           "Insert the note into the current Org file.\n"
           "Target file: %s\n"
           "Insert location: around line %d (current cursor position)\n\n"
           "Note request:\n%s\n\n"
           "Only update the requested insertion location. Do not change unrelated sections. Go ahead and start do the work.")
          file-path
          line-number
          note-request))

(defun ai-code--target-directory-under-org-roam-p (target-dir)
  "Return non-nil when TARGET-DIR is under `org-roam-directory'.
Normalizes paths for robustness against symlinks and case sensitivity."
  (and (ai-code--org-roam-ready-p)
       (let ((target-expanded (expand-file-name target-dir))
             (roam-expanded (expand-file-name org-roam-directory)))
         (string-prefix-p (file-name-as-directory roam-expanded)
                         (file-name-as-directory target-expanded)))))

(defun ai-code--build-note-create-prompt (target-dir note-request)
  "Build an AI prompt to create a note under TARGET-DIR.
NOTE-REQUEST is included in the prompt body."
  (format (concat
           "Create a new Org note file under directory: %s\n"
           "Automatically determine a concise filename from the note title/content you identified. "
           "Use lowercase letters, numbers, and underscores for the filename, with .org extension.\n\n"
           "Note request:\n%s\n\n"
           "%s"
           "Do not modify unrelated files. Go ahead and start the work.")
          target-dir
          note-request
          (if (ai-code--target-directory-under-org-roam-p target-dir)
              "After creating the note file, run org-roam-db-sync so it is discoverable by org-roam commands.\n\n"
            "")))

(defun ai-code--org-roam-ready-p ()
  "Return non-nil when org-roam is available and configured."
  (and (or (featurep 'org-roam) (require 'org-roam nil t))
       (boundp 'org-roam-directory)
       (stringp org-roam-directory)
       (not (string-empty-p org-roam-directory))))

(defun ai-code--select-note-target-directory (default-note-directory)
  "Select note target directory using DEFAULT-NOTE-DIRECTORY as fallback."
  (let ((fallback-dir (file-name-as-directory default-note-directory)))
    (if (and (ai-code--org-roam-ready-p)
             (y-or-n-p (format "Create note under org-roam-directory (%s)? "
                               org-roam-directory)))
        (file-name-as-directory (expand-file-name org-roam-directory))
      (file-name-as-directory
       (read-directory-name "Directory for new note: "
                            fallback-dir
                            fallback-dir
                            t)))))

(defun ai-code--insert-org-note-at-point (title content)
  "Insert a note with TITLE and CONTENT at point in current Org buffer."
  (let ((level (or (org-current-level) 1)))
    (unless (bolp)
      (insert "\n"))
    (insert (make-string level ?*) " " title "\n")
    (org-insert-time-stamp (current-time) t nil)
    (insert "\n\n" content "\n")))

(defun ai-code--sync-org-roam-note-if-needed (note-file)
  "Sync NOTE-FILE into org-roam DB when NOTE-FILE is under `org-roam-directory'."
  (when (and (ai-code--org-roam-ready-p)
             (file-in-directory-p (file-truename note-file)
                                 (file-truename (expand-file-name org-roam-directory))))
    (condition-case err
        (progn
          (when (fboundp 'org-roam-db-update-file)
            (org-roam-db-update-file note-file t))
          (when (fboundp 'org-roam-db-sync)
            (org-roam-db-sync)))
      (error
       (message "Failed to sync org-roam note: %s" (error-message-string err))))))

(defun ai-code--read-note-request ()
  "Prompt user for the note request and return a non-empty string."
  (let ((note-request (string-trim
                       (or (read-string "Specification for the note? "
                                        ai-code-discussion--default-note-request
                                        'ai-code-note-request-history)
                           ""))))
    (when (string-empty-p note-request)
      (user-error "Note request cannot be empty"))
    note-request))

;;;###autoload
(defun ai-code-take-notes (&optional arg)
  "Take notes by AI request and send prompt to the AI session.
When in an Org buffer with a saved file, generates a prompt to insert note content.
Otherwise, generates a prompt to create a new note file.
With prefix ARG, open the default note file in other window instead."
  (interactive "P")
  (let ((files-dir (ai-code--ensure-files-directory)))
    (if arg
        (let ((note-file (expand-file-name ai-code-notes-file-name files-dir)))
          (find-file-other-window note-file))
      (let* ((default-note-dir (file-name-as-directory files-dir))
             (note-request (ai-code--read-note-request)))
        (if (derived-mode-p 'org-mode)
            (if (not buffer-file-name)
                (user-error "Org buffer must be saved to a file before taking notes")
              (let* ((target-file buffer-file-name)
                     (line-number (line-number-at-pos))
                     (default-prompt (ai-code--build-note-insert-prompt
                                      target-file
                                      line-number
                                      note-request)))
                (when-let ((final-prompt (ai-code-read-string "Prompt: " default-prompt)))
                  (ai-code--insert-prompt final-prompt)
                  (message "Generated AI prompt for note insertion in %s" target-file))))
          (let* ((target-dir (ai-code--select-note-target-directory default-note-dir))
                 (default-prompt (ai-code--build-note-create-prompt
                                  target-dir
                                  note-request)))
            (when-let ((final-prompt (ai-code-read-string "Prompt: " default-prompt)))
              (ai-code--insert-prompt final-prompt)
              (when (ai-code--target-directory-under-org-roam-p target-dir)
                (when (require 'org-roam nil t)
                  (org-roam-db-sync)))
              (message "Generated AI prompt for note creation under %s" target-dir))))))))


(provide 'ai-code-discussion)

;;; ai-code-discussion.el ends here
