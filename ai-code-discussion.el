;;; ai-code-discussion.el --- AI code discussion operations -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides code discussion functionality for the AI Code Interface package.

;;; Code:

(require 'which-func)

(require 'ai-code-input)
(require 'ai-code-prompt-mode)

(declare-function ai-code-read-string "ai-code-input")
(declare-function ai-code--insert-prompt "ai-code-prompt-mode")

;;;###autoload
(defun ai-code-ask-question (arg)
  "Generate prompt to ask questions about specific code.
With a prefix argument (\M-x), prompt for a question without adding any context.
If current buffer is a file, keep existing logic.
If current buffer is a dired buffer:
  - If there are files or directories marked, use them as context (use git repo relative path, start with @ character)
  - If there are no files or dirs marked, but under cursor there is file or dir, use it as context of prompt
If a region is selected, ask about that specific region.
If cursor is in a function, ask about that function.
Otherwise, ask a general question about the file.
Inserts the prompt into the AI prompt file and optionally sends to AI.

Argument ARG is the prefix argument."
  (interactive "P")
  (if arg
      (let ((question (ai-code-read-string "Ask question (no context): " "")))
        (ai-code--insert-prompt question))
    (cond
     ;; Handle dired buffer
     ((eq major-mode 'dired-mode)
      (ai-code--ask-question-dired))
     ;; Handle regular file buffer
     (t (ai-code--ask-question-file)))))

(defun ai-code--ask-question-dired ()
  "Handle ask question for dired buffer."
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
                        (has-marks "Question about marked files/directories: ")
                        (file-at-point (format "Question about %s: " (file-name-nondirectory file-at-point)))
                        (t "General question about directory: ")))
         (question (ai-code-read-string prompt-label ""))
         (final-prompt (concat question files-context-string
                              "\nNote: This is a question only - please do not modify the code.")))
    (ai-code--insert-prompt final-prompt)))

(defun ai-code--ask-question-file ()
  "Handle ask question for regular file buffer."
  (let* ((file-extension (when buffer-file-name
                          (file-name-extension buffer-file-name)))
         (is-diff-or-patch (and file-extension
                               (member file-extension '("diff" "patch"))))
         (function-name (unless is-diff-or-patch
                         (which-function)))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (prompt-label
          (cond
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
         (final-prompt
          (concat question
                  (when region-text
                    (concat "\n" region-text))
                  (when function-name
                    (format "\nFunction: %s" function-name))
                  files-context-string
                  "\nNote: This is a question only - please do not modify the code.")))
    (ai-code--insert-prompt final-prompt)))

(defun ai-code--get-git-relative-paths (file-paths)
  "Convert absolute file paths to git repository relative paths.
Returns a list of relative paths from the git repository root."
  (when file-paths
    (let ((git-root (magit-toplevel)))
      (when git-root
        (mapcar (lambda (file-path)
                  (file-relative-name file-path git-root))
                file-paths)))))

;;;###autoload
(defun ai-code-investigate-exception (arg)
  "Generate prompt to investigate exceptions or errors in code.
With a prefix argument (C-u), or if not in a programming mode buffer,
prompt for investigation without adding any context.
If a *compilation* buffer is visible in the current window, use its full content as context.
If a region is selected, investigate that specific error or exception.
If cursor is in a function, investigate exceptions in that function.
Otherwise, investigate general exception handling in the file.
Inserts the prompt into the AI prompt file and optionally sends to AI.
Argument ARG is the prefix argument."
  (interactive "P")
  (let* ((compilation-buffer (get-buffer "*compilation*"))
         (compilation-content (when (and compilation-buffer
                                        (get-buffer-window compilation-buffer))
                               (with-current-buffer compilation-buffer
                                 (buffer-substring-no-properties (point-min) (point-max)))))
         (region-text (when (region-active-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (buffer-file buffer-file-name)
         (full-buffer-context (when (and (not buffer-file) (not region-text))
                                (buffer-substring-no-properties (point-min) (point-max))))
         (function-name (which-function))
         (files-context-string (ai-code--get-context-files-string))
         (context-section
          (if full-buffer-context
              (concat "\n\nContext:\n" full-buffer-context)
            (concat
             (when compilation-content
               (concat "\n\nCompilation output:\n" compilation-content))
             (when (and region-text (not compilation-content))
               (concat "\n\nSelected code:\n" region-text)))))
         (default-question "How to fix the error in this code? Please analyze the error, explain the root cause, and provide the corrected code to resolve the issue: ")
         (prompt-label
          (cond
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
                  (concat "\n\nNote: Please focus on how to fix the error. Your response should include:\n"
                          "1. A brief explanation of the root cause of the error.\n"
                          "2. A code snippet with the fix.\n"
                          "3. An explanation of how the fix addresses the error.")))
         (ai-code--insert-prompt final-prompt))))

;;;###autoload
(defun ai-code-explain ()
  "Generate prompt to explain code at different levels.
If current buffer is a dired buffer and under cursor is a directory or file, explain that directory or file using relative path as context (start with @ character).
If a region is selected, explain that specific region using function/file as context.
Otherwise, prompt user to select scope: symbol, line, function, or file.
Inserts the prompt into the AI prompt file and optionally sends to AI."
  (interactive)
  (cond
   ;; Handle dired buffer
   ((eq major-mode 'dired-mode)
    (ai-code--explain-dired))
   ;; Handle region selection
   ((region-active-p)
    (ai-code--explain-region))
   ;; Handle regular file buffer
   (t (ai-code--explain-with-scope-selection))))

(defun ai-code--explain-dired ()
  "Handle explain for dired buffer."
  (let* ((file-at-point (dired-get-filename nil t))
         (git-relative-path (when file-at-point
                             (car (ai-code--get-git-relative-paths (list file-at-point)))))
         (files-context-string (when git-relative-path
                                (concat "\nFiles:\n@" git-relative-path)))
         (file-type (if (and file-at-point (file-directory-p file-at-point))
                       "directory"
                     "file"))
         (initial-prompt (if git-relative-path
                            (format "Please explain the %s at path @%s.\n\nProvide a clear explanation of what this %s contains, its purpose, and its role in the project structure.%s"
                                   file-type 
                                   git-relative-path 
                                   file-type
                                   (or files-context-string ""))
                          "No file or directory found at cursor point."))
         (final-prompt (if git-relative-path
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
         (initial-prompt (format "Please explain the following code:\n\n%s\n\n%s%s%s\n\nProvide a clear explanation of what this code does, how it works, and its purpose within the context."
                        region-text
                        context-info
                        (if function-name "\n" "")
                        files-context-string))
         (final-prompt (ai-code-read-string "Prompt: " initial-prompt)))
    (when final-prompt
      (ai-code--insert-prompt final-prompt))))

(defun ai-code--explain-with-scope-selection ()
  "Prompt user to select explanation scope and explain accordingly."
  (let* ((choices '("symbol" "line" "function" "file"))
         (scope (completing-read "Select scope to explain: " choices nil t)))
    (pcase scope
      ("symbol" (ai-code--explain-symbol))
      ("line" (ai-code--explain-line))
      ("function" (ai-code--explain-function))
      ("file" (ai-code--explain-file)))))

(defun ai-code--explain-symbol ()
  "Explain the symbol at point."
  (let* ((symbol (thing-at-point 'symbol t))
         (function-name (which-function)))
    (unless symbol
      (user-error "No symbol at point"))
    (let* ((initial-prompt (format "Please explain the symbol '%s' in the context of:%s\nFile: %s\n\nExplain what this symbol represents, its type, purpose, and how it's used in this context."
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
    (let* ((initial-prompt (format "Please explain the following line of code:\n\nLine %d: %s\n\n%sFile: %s\n\nExplain what this line does, its purpose, and how it fits into the surrounding code."
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
    (let* ((initial-prompt (format "Please explain the function '%s':
File: %s
Explain what this function does, its parameters, return value, algorithm, and its role in the overall codebase."
                                  function-name
                                  (or buffer-file-name "current buffer")))
           (final-prompt (ai-code-read-string "Prompt: " initial-prompt)))
      (when final-prompt
        (ai-code--insert-prompt final-prompt)))))


(defun ai-code--explain-file ()
  "Explain the current file."
  (let ((file-name (or buffer-file-name "current buffer")))
    (let* ((initial-prompt (format "Please explain the following file:\nFile: %s\nProvide an overview of this file's purpose, its main components, key functions, and how it fits into the larger codebase architecture."
                                 file-name))
           (final-prompt (ai-code-read-string "Prompt: " initial-prompt)))
      (when final-prompt
        (ai-code--insert-prompt final-prompt)))))

(provide 'ai-code-discussion)

;;; ai-code-discussion.el ends here
