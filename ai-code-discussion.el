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
(declare-function ai-code--get-clipboard-text "ai-code-interface")
(declare-function ai-code-call-gptel-sync "ai-code-prompt-mode")

;;;###autoload
(defun ai-code-ask-question (arg)
  "Generate prompt to ask questions about specific code.
With a prefix argument (C-u), append the clipboard contents as context.
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
  (let ((clipboard-context (when arg (ai-code--get-clipboard-text))))
    (cond
     ;; Handle dired buffer
     ((eq major-mode 'dired-mode)
      (ai-code--ask-question-dired clipboard-context))
     ;; Handle regular file buffer
     (t (ai-code--ask-question-file clipboard-context)))))

(defun ai-code--ask-question-dired (clipboard-context)
  "Handle ask question for dired buffer.
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
         (final-prompt (concat question
                              files-context-string
                              (when (and clipboard-context
                                        (string-match-p "\\S-" clipboard-context))
                                (concat "\n\nClipboard context:\n" clipboard-context))
                              "\nNote: This is a question only - please do not modify the code.")))
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
                  (when (and clipboard-context
                            (string-match-p "\\S-" clipboard-context))
                    (concat "\n\nClipboard context:\n" clipboard-context))
                  (if region-text
                      "\nNote: This is a question about the selected region - please do not modify the code."
                    "\nNote: This is a question only - please do not modify the code."))))
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
With a prefix argument (C-u), use context from clipboard as the error to investigate.
If a *compilation* buffer is visible in the current window, use its full content as context.
If a region is selected, investigate that specific error or exception.
If cursor is in a function, investigate exceptions in that function.
Otherwise, investigate general exception handling in the file.
Inserts the prompt into the AI prompt file and optionally sends to AI.
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
                  (concat "\n\nNote: Please focus on how to fix the error. Your response should include:\n"
                          "1. A brief explanation of the root cause of the error.\n"
                          "2. A code snippet with the fix.\n"
                          "3. An explanation of how the fix addresses the error."))))
         (ai-code--insert-prompt final-prompt)))

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

;;;###autoload
(defcustom ai-code-notes-file-name ".ai.code.notes.org"
  "Default note file name relative to the project root.
This value is used by `ai-code-take-notes' when suggesting where to store notes."
  :type 'string
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-notes-use-gptel-headline nil
  "Whether to use GPTel to generate headline for notes.
If non-nil, call `ai-code-call-gptel-sync` to generate a smart default
headline based on the selected content. Otherwise, prompt with empty default."
  :type 'boolean
  :group 'ai-code)

;;;###autoload
(defun ai-code-take-notes ()
  "Take notes from selected region and save to a note file.
When there is a selected region, ask for note file path (default is
.ai.code.notes.org in the git root) and section title.  Add the section
title as a headline at the end of the note file, and put the selected
region as content of that section."
  (interactive)
  (unless (region-active-p)
    (user-error "No region selected. Please select the text you want to save as notes"))
  (let* ((region-text (filter-buffer-substring (region-beginning) (region-end) nil))
         (git-root (condition-case nil
                       (magit-toplevel)
                     (error nil)))
         (default-note-file (if git-root
                                (expand-file-name ai-code-notes-file-name git-root)
                              (expand-file-name ai-code-notes-file-name default-directory)))
         (note-file (read-file-name
                     "Note file: "
                     (file-name-directory default-note-file)
                     default-note-file
                     nil
                     (file-name-nondirectory default-note-file)))
         (default-title (when ai-code-notes-use-gptel-headline
                          (condition-case err
                              (string-trim
                               (ai-code-call-gptel-sync
                                (format "Generate a concise headline (max 10 words) for this note content. Only return the headline text without quotes or extra formatting:\n\n%s"
                                        (if (> (length region-text) 500)
                                            (substring region-text 0 500)
                                          region-text))))
                            (error
                             (message "GPTel headline generation failed: %s" (error-message-string err))
                             ""))))
         (section-title (ai-code-read-string "Section title: " (or default-title ""))))
    (when (string-empty-p section-title)
      (user-error "Section title cannot be empty"))
    ;; Create note file directory if it doesn't exist
    (let ((note-dir (file-name-directory note-file)))
      (unless (file-exists-p note-dir)
        (make-directory note-dir t)))
    ;; Append section to note file
    (with-current-buffer (find-file-noselect note-file)
      (save-excursion
        (goto-char (point-max))
        ;; Add newline before new section if file is not empty
        (unless (bobp)
          (insert "\n\n"))
        ;; Insert headline
        (insert "* " section-title "\n")
        ;; Insert timestamp
        (org-insert-time-stamp (current-time) t nil)
        (insert "\n\n")
        ;; Insert region content
        (insert region-text)
        (insert "\n"))
      (save-buffer))
    ;; Open note file in other window and scroll to bottom
    (let ((note-buffer (find-file-other-window note-file)))
      (with-selected-window (get-buffer-window note-buffer)
        (goto-char (point-max))
        (recenter -1)))
    (message "Notes added to %s under section: %s" note-file section-title)))

(provide 'ai-code-discussion)

;;; ai-code-discussion.el ends here
