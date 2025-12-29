;;; ai-code-change.el --- AI code change operations -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides code change functionality for the AI Code Interface package.

;;; Code:

(require 'which-func)
(require 'cl-lib)
(require 'magit)
(require 'flycheck nil t)

(require 'ai-code-input)
(require 'ai-code-prompt-mode)

(declare-function ai-code-read-string "ai-code-input")
(declare-function ai-code--insert-prompt "ai-code-prompt-mode")
(declare-function ai-code--get-clipboard-text "ai-code-interface")
(declare-function ai-code--get-git-relative-paths "ai-code-discussion")
(declare-function ai-code--get-region-location-info "ai-code-discussion")
(declare-function ai-code--format-repo-context-info "ai-code-file")
(declare-function flycheck-error-pos "flycheck")
(declare-function flycheck-error-line "flycheck")
(declare-function flycheck-error-column "flycheck")
(declare-function flycheck-error-message "flycheck")

(defvar flycheck-current-errors)
(defvar region-start-line)

(defun ai-code--is-comment-line (line)
  "Check if LINE is a comment line based on current buffer's comment syntax.
Returns non-nil if LINE starts with one or more comment characters,
ignoring leading whitespace. Returns nil when the comment content
begins with a DONE: prefix."
  (when comment-start
    (let* ((comment-str (string-trim-right comment-start))
           (trimmed-line (string-trim-left line))
           (comment-re (concat "^[ 	]*"
                               (regexp-quote comment-str)
                               "+[ 	]*")))
      (when (string-match comment-re trimmed-line)
        (let ((content (string-trim-left (substring trimmed-line (match-end 0)))))
          (unless (string-prefix-p "DONE:" content)
            t))))))

(defun ai-code--is-comment-block (text)
  "Check if TEXT is a block of comments (ignoring blank lines)."
  (let ((lines (split-string text "\n")))
    (cl-every (lambda (line)
                (or (string-blank-p line)
                    (ai-code--is-comment-line line)))
              lines)))

(defun ai-code--get-function-name-for-comment ()
  "Get the appropriate function name when cursor is on a comment line.
If the comment precedes a function definition or is inside a function body,
returns that function's name. Otherwise returns the result of `which-function`."
  (interactive)
  (let* ((current-func (which-function))
         (resolved-func
          (save-excursion
            (cl-labels ((line-text ()
                          (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
              (forward-line 1)
              (cl-block resolve
                (let ((text (line-text)))
                  ;; Stop immediately if the next line is blank or buffer ended.
                  (when (or (eobp) (string-blank-p text))
                    (cl-return-from resolve nil))
                  ;; Skip leading comment lines, aborting on blank lines.
                  (while (ai-code--is-comment-line text)
                    (forward-line 1)
                    (setq text (line-text))
                    (when (or (eobp) (string-blank-p text))
                      (cl-return-from resolve nil)))
                  ;; Resolve with a short lookahead; stop on blank lines.
                  (let ((next-func (which-function)))
                    (cl-loop with lookahead = 5
                             while (and (> lookahead 0)
                                        (or (null next-func)
                                            (string= next-func current-func)))
                             do (forward-line 1)
                                (setq lookahead (1- lookahead))
                                (setq text (line-text))
                                (when (string-blank-p text)
                                  (cl-return-from resolve nil))
                                (unless (ai-code--is-comment-line text)
                                  (setq next-func (which-function)))
                             finally return (cond
                                             ((not current-func) next-func)
                                             ((not next-func) current-func)
                                             ((not (string= next-func current-func)) next-func)
                                             (t current-func))))))))))
    ;; (when resolved-func
    ;;   (message "Identified function: %s" resolved-func))
    resolved-func))

(defun ai-code--detect-todo-info (region-active)
  "Detect TODO comment information at cursor or in selected region.
REGION-ACTIVE indicates whether a region is selected.
Returns (TEXT START-POS END-POS) if TODO found, nil otherwise."
  (let ((text (if region-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'line t))))
    (when (and text comment-start)
      (let* ((first-line (car (split-string text "\n")))
             (comment-prefix-re (concat "^[ \t]*" (regexp-quote (string-trim-right comment-start)) "+[ \t]*")))
        (when (string-match comment-prefix-re first-line)
          (let ((rest (string-trim-left (substring first-line (match-end 0)))))
            (when (string-prefix-p "TODO" rest)
              (list text
                    (if region-active (region-beginning) (line-beginning-position))
                    (if region-active (region-end) (line-end-position))))))))))

(defun ai-code--generate-prompt-label (clipboard-context region-active function-name)
  "Generate appropriate prompt label based on context.
CLIPBOARD-CONTEXT is text from clipboard if any.
REGION-ACTIVE indicates if a region is selected.
FUNCTION-NAME is the name of the function at point if any."
  (cond
   ((and clipboard-context
         (string-match-p "\\S-" clipboard-context))
    (cond
     (region-active
      (if function-name
          (format "Change code in function %s (clipboard context): " function-name)
        "Change selected code (clipboard context): "))
     (function-name
      (format "Change function %s (clipboard context): " function-name))
     (t "Change code (clipboard context): ")))
   (region-active
    (if function-name
        (format "Change code in function %s: " function-name)
      "Change selected code: "))
   (function-name
    (format "Change function %s: " function-name))
   (t "Change code: ")))

(defun ai-code--handle-regular-code-change (arg region-active)
  "Handle regular code change operation.
ARG is the prefix argument.
REGION-ACTIVE indicates whether a region is selected."
  (let* ((clipboard-context (when arg (ai-code--get-clipboard-text)))
         (function-name (which-function))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (region-location-info (when region-active
                                 (ai-code--get-region-location-info (region-beginning) (region-end))))
         (prompt-label (ai-code--generate-prompt-label clipboard-context region-active function-name))
         (initial-prompt (ai-code-read-string prompt-label ""))
         (files-context-string (ai-code--get-context-files-string))
         (repo-context-string (ai-code--format-repo-context-info))
         (final-prompt
          (concat initial-prompt
                  (when region-text
                    (concat "\nSelected region:\n"
                            (cond
                             (region-location-info
                              (concat region-location-info "\n"))
                             (region-start-line
                              (format "Start line: %d\n" region-start-line)))
                            region-text))
                  (when function-name (format "\nFunction: %s" function-name))
                  files-context-string
                  repo-context-string
                  (when (and clipboard-context
                            (string-match-p "\\S-" clipboard-context))
                    (concat "\n\nClipboard context:\n" clipboard-context))
                  (if region-text
                      "\nNote: Please apply the code change to the selected region specified above."
                    "\nNote: Please make the code change described above."))))
    (ai-code--insert-prompt final-prompt)))

(defun ai-code--handle-dired-code-change (arg)
  "Handle code change operation in Dired mode.
ARG is the prefix argument."
  (let* ((clipboard-context (when arg (ai-code--get-clipboard-text)))
         (files (dired-get-marked-files))
         (files-str (mapconcat #'identity files "\n"))
         (single-file (when (= (length files) 1) (car files)))
         (prompt-prefix (if single-file
                            (format "Change code in %s" (file-name-nondirectory single-file))
                          "Change code in files"))
         (prompt-label (if (and clipboard-context (string-match-p "\\S-" clipboard-context))
                           (format "%s (clipboard context): " prompt-prefix)
                         (format "%s: " prompt-prefix)))
         (initial-prompt (ai-code-read-string prompt-label ""))
         (repo-context-string (ai-code--format-repo-context-info))
         (final-prompt
          (concat initial-prompt
                  "\nSelected files/directories:\n" files-str
                  repo-context-string
                  (when (and clipboard-context
                             (string-match-p "\\S-" clipboard-context))
                    (concat "\n\nClipboard context:\n" clipboard-context))
                  "\nNote: Please make the code change described above for the selected files/directories.")))
    (ai-code--insert-prompt final-prompt)))

;;;###autoload
(defun ai-code-code-change (arg)
  "Generate prompt to change code under cursor or in selected region.
If the cursor is on a TODO comment or a region with a TODO comment is
selected, it will generate a prompt to implement the TODO in-place.
With a prefix argument \[universal-argument], append the clipboard
contents as context.  If a region is selected, change that specific
region.  Otherwise, change the function under cursor.  If nothing is
selected and no function context, prompts for general code change.
Inserts the prompt into the AI prompt file and optionally sends to AI.
Argument ARG is the prefix argument."
  (interactive "P")
  ;; DONE: when current file is dired buffer: 1. when there is marked
  ;; file / dir, use them as context; 2. otherwise, use file / dir
  ;; under cursor as context. After that, build up full prompt using
  ;; above context and other context such as repository context, and
  ;; send the full prompt to LLM. Otherwise, using current code path.
  (cond
   ((derived-mode-p 'dired-mode)
    (ai-code--handle-dired-code-change arg))
   (t
    (unless buffer-file-name
      (user-error "Error: buffer-file-name must be available"))
    (let* ((region-active (region-active-p))
           (todo-info (ai-code--detect-todo-info region-active)))
      (if todo-info
          (ai-code-implement-todo arg)
        (ai-code--handle-regular-code-change arg region-active))))))

;;;###autoload
(defun ai-code-implement-todo (arg)
  "Generate prompt to implement TODO comments in current context.
Implements code after TODO comments instead of replacing them in-place.
With a prefix argument \\[universal-argument], append the clipboard
contents as context.  If region is selected, implement that specific
region.  If cursor is on a comment line, implement that specific comment.
If the current line is blank, ask user to input TODO comment.
The input string will be prefixed with TODO: and insert to the current
line, with proper indentation.  If cursor is inside a function, implement
comments for that function.
Otherwise implement comments for the entire current file.
Argument ARG is the prefix argument."
  ;; DONE: if the current line under cursor is a comment prefix with DONE: ,
  ;; ask the user if they want to toggle it to TODO, or delete the comment line
  ;; DONE: this function is too long. break it into three major branch
  ;; with sub functions
  (interactive "P")
  (if (not buffer-file-name)
      (user-error "Error: buffer-file-name must be available")
    (cl-block finalize
      (when (ai-code--implement-todo--handle-done-line)
        (cl-return-from finalize nil))
      (when (ai-code--implement-todo--handle-blank-line)
        (cl-return-from finalize nil))
      (ai-code--implement-todo--build-and-send-prompt arg))))

(defun ai-code--implement-todo--handle-done-line ()
  "Handle actions when current line is a DONE comment.
Returns non-nil if the action is handled and the caller should exit."
  (let* ((line-str (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (comment-prefix (and comment-start (string-trim-right comment-start)))
         (done-re (when comment-prefix
                    (concat "^\\([ \t]*" (regexp-quote comment-prefix) "+[ \t]*\\)DONE:"))))
    (when (and line-str done-re (string-match done-re line-str) (not (region-active-p)))
      (let* ((action (completing-read
                      "Current line starts with DONE:. Action: "
                      '("Toggle to TODO" "Delete comment line" "Keep as DONE")
                      nil t nil nil "Toggle to TODO"))
             (line-beg (line-beginning-position))
             (line-end (line-end-position)))
        (pcase action
          ("Toggle to TODO"
           (save-excursion
             (goto-char line-beg)
             (when (search-forward "DONE:" line-end t)
               (replace-match "TODO:" nil nil)))
           (message "Toggled DONE to TODO on current comment line"))
          ("Delete comment line"
           (let ((line-next
                  (save-excursion
                    (goto-char line-beg)
                    (forward-line 1)
                    (min (point) (point-max)))))
             (delete-region line-beg line-next))
           (message "Deleted DONE comment line"))
          (_ (message "Keeping DONE comment line as is")))
        t))))

(defun ai-code--implement-todo--handle-blank-line ()
  "Handle insertion of a TODO comment when on a blank line.
Returns non-nil if handled and the caller should exit."
  ;; (interactive)
  (when (and (not (region-active-p))
             (or (not (thing-at-point 'line t)) (string-blank-p (thing-at-point 'line t)))
             comment-start)
    (let ((todo-text (ai-code-read-string "Enter TODO comment: "))
          (comment-prefix (if (derived-mode-p 'emacs-lisp-mode)
                              (let* ((trimmed (string-trim-right comment-start)))
                                (if (= (length trimmed) 1)
                                    (make-string 2 (string-to-char trimmed))
                                  trimmed))
                            (string-trim-right comment-start))))
      (unless (string-blank-p todo-text)
        (delete-region (line-beginning-position) (line-end-position))
        (indent-according-to-mode)
        (insert comment-prefix
                " TODO: "
                todo-text
                (if (and comment-end (not (string-blank-p comment-end)))
                    (concat " " (string-trim-left comment-end))
                  ""))
        (indent-according-to-mode)))
    t))

(defun ai-code--implement-todo--build-and-send-prompt (arg)
  "Build the TODO implementation prompt and insert it.
ARG is the prefix argument for clipboard context."
  (let* ((clipboard-context (when arg (ai-code--get-clipboard-text)))
         (current-line (string-trim (thing-at-point 'line t)))
         (current-line-number (line-number-at-pos (point)))
         (is-comment (ai-code--is-comment-line current-line))
         (function-name (if is-comment
                            (ai-code--get-function-name-for-comment)
                          (which-function)))
         (function-context (if function-name
                               (format "\nFunction: %s" function-name)
                             ""))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties
                         (region-beginning)
                         (region-end))))
         (region-start-line (when region-active
                              (line-number-at-pos (region-beginning))))
         (region-location-info (when region-active
                                 (ai-code--get-region-location-info
                                  (region-beginning)
                                  (region-end))))
         (region-location-line (when region-text
                                 (or (and region-location-info
                                          (format "Selected region: %s"
                                                  region-location-info))
                                     (when region-start-line
                                       (format "Selected region starting on line %d"
                                               region-start-line)))))
         (files-context-string (ai-code--get-context-files-string))
         (repo-context-string (ai-code--format-repo-context-info))
         (prompt-label
          (cond
           ((and clipboard-context
                 (string-match-p "\\S-" clipboard-context))
            (cond
             (region-text "TODO implementation instruction (clipboard context): ")
             (is-comment "TODO implementation instruction (clipboard context): ")
             (function-name (format "TODO implementation instruction for function %s (clipboard context): " function-name))
             (t "TODO implementation instruction (clipboard context): ")))
           (region-text "TODO implementation instruction: ")
           (is-comment "TODO implementation instruction: ")
           (function-name (format "TODO implementation instruction for function %s: " function-name))
           (t "TODO implementation instruction: ")))
         (initial-input
          (cond
           (region-text
            (unless (ai-code--is-comment-block region-text)
              (user-error "Selected region must be a comment block"))
            (format
             "Please implement code for this requirement comment block in the selected region first. After implementing, keep the comment in place and ensure it begins with a DONE prefix (change TODO to DONE or prepend DONE if no prefix). If this is a pure new code block, place it after the comment; otherwise keep the existing structure and make corresponding change for the context.\n%s\n%s%s%s"
             region-location-line region-text function-context
                    files-context-string))
           (is-comment
            (format "Please implement code for this requirement comment on line %d: '%s' first. After implementing, keep the comment in place and ensure it begins with a DONE prefix (change TODO to DONE or prepend DONE if needed). If this is a pure new code block, place it after the comment; otherwise keep the existing structure and make corresponding change for the context.%s%s"
                    current-line-number current-line function-context files-context-string))
           ;; (function-name
           ;;  (format "Please implement code after all TODO comments in function '%s'. The TODOs are TODO comments. Keep each comment in place and ensure each begins with a DONE prefix (change TODO to DONE or prepend DONE if needed) before adding implementation code after it. Keep the existing code structure and only add code after these marked items.%s"
           ;;          function-name files-context-string))
           ;; (t
           ;;  (format "Please implement code after all TODO comments in file '%s'. The TODOs are TODO comments. Keep each comment in place and ensure each begins with a DONE prefix (change TODO to DONE or prepend DONE if needed) before adding implementation code after it. Keep the existing code structure and only add code after these marked items.%s"
           ;;          (file-name-nondirectory buffer-file-name) files-context-string))
           ;; DONE: otherwise, let user know the current line is not a comment and cannot proceed
           (t
            (user-error "Current line is not a TODO comment and cannot proceed with `ai-code-implement-todo'.  Please select a TODO comment (not DONE), a region of comments, or activate on a blank line"))))
         (prompt (ai-code-read-string prompt-label initial-input))
         (final-prompt
          (concat prompt
                  (when (and clipboard-context
                             (string-match-p "\\S-" clipboard-context))
                    (concat "\n\nClipboard context:\n" clipboard-context))
                  repo-context-string)))
    (ai-code--insert-prompt final-prompt)))

;;; Flycheck integration
(defun ai-code-flycheck--get-errors-in-scope (start end)
  "Return Flycheck errors within given START and END buffer positions."
  (when (and (bound-and-true-p flycheck-mode) flycheck-current-errors)
    (cl-remove-if-not
     (lambda (err)
       (let ((pos (flycheck-error-pos err)))
         (and (integerp pos) (>= pos start) (< pos end))))
     flycheck-current-errors)))

(defun ai-code-flycheck--format-error-list (errors file-path-for-error-reporting)
  "Formats a list string for multiple Flycheck ERRORS.
FILE-PATH-FOR-ERROR-REPORTING is the relative file path
to include in each error report."
  (let ((error-reports '()))
    (dolist (err errors)
      (let* ((line (flycheck-error-line err))
             (col (flycheck-error-column err))
             (msg (flycheck-error-message err)))
        (if (and (integerp line) (integerp col))
            (let* ((error-line-text
                    (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- line))
                      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
              (push (format "File: %s:%d:%d\nError: %s\nContext line:\n%s"
                            file-path-for-error-reporting line col msg error-line-text)
                    error-reports))
          (progn
            (message "AI-Code: Flycheck error for %s. Line: %S, Col: %S. Full location/context not available. Sending general error info."
                     file-path-for-error-reporting line col)
            (push (format "File: %s (Location: Line %s, Column %s)\nError: %s"
                          file-path-for-error-reporting
                          (if (integerp line) (format "%d" line) "N/A")
                          (if (integerp col) (format "%d" col) "N/A")
                          msg)
                  error-reports)))))
    (mapconcat #'identity (nreverse error-reports) "\n\n")))

(defun ai-code--choose-flycheck-scope ()
  "Return a list (START END DESCRIPTION) for Flycheck fixing scope."
  (let* ((scope (if (region-active-p) 'region
                  (intern
                   (completing-read
                    "Select Flycheck fixing scope: "
                    (delq nil
                          `("current-line"
                            ,(when (which-function) "current-function")
                            "whole-file"))
                    nil t))))
         start end description)
    (pcase scope
      ('region
       (setq start (region-beginning)
             end   (region-end)
             description
             (format "the selected region (lines %d–%d)"
                     (line-number-at-pos start)
                     (line-number-at-pos end))))
      ('current-line
       (setq start            (line-beginning-position)
             end              (line-end-position)
             description       (format "current line (%d)"
                                       (line-number-at-pos (point)))))
      ('current-function
       (let ((bounds (bounds-of-thing-at-point 'defun)))
         (unless bounds
           (user-error "Not inside a function; cannot select current function"))
         (setq start            (car bounds)
               end              (cdr bounds)
               description       (format "function '%s' (lines %d–%d)"
                                        (which-function)
                                        (line-number-at-pos (car bounds))
                                        (line-number-at-pos (cdr bounds))))))
      ('whole-file
       (setq start            (point-min)
             end              (point-max)
             description       "the entire file"))
      (_
       (user-error "Unknown Flycheck scope %s" scope)))
    (list start end description)))

;;;###autoload
(defun ai-code-flycheck-fix-errors-in-scope ()
  "Ask AI to generate a patch fixing Flycheck errors.
If a region is active, operate on that region.
Otherwise prompt to choose scope: current line, current function (if any),
or whole file.  Requires the `flycheck` package to be installed and available."
  (interactive)
  (unless (featurep 'flycheck)
    (user-error "Flycheck package not found.  This feature is unavailable"))
  (unless buffer-file-name
    (user-error "Error: buffer-file-name must be available"))
  (when (bound-and-true-p flycheck-mode)
    (if (null flycheck-current-errors)
        (message "No Flycheck errors found in the current buffer.")
      (let* ((git-root (or (magit-toplevel) default-directory))
             (rel-file (file-relative-name buffer-file-name git-root))
             ;; determine start/end/scope-description via helper
             (scope-data (ai-code--choose-flycheck-scope))
             (start (nth 0 scope-data))
             (end (nth 1 scope-data))
             (scope-description (nth 2 scope-data)))
        ;; collect errors and bail if none in that scope
        (let ((errors-in-scope
               (ai-code-flycheck--get-errors-in-scope start end)))
          (if (null errors-in-scope)
              (message "No Flycheck errors found in %s." scope-description)
            (let* ((files-context-string (ai-code--get-context-files-string))
                   (error-list-string
                    (ai-code-flycheck--format-error-list errors-in-scope
                                                         rel-file))
                   (prompt
                    (if (string-equal "the entire file" scope-description)
                        (format "Please fix the following Flycheck errors in file %s:\n\n%s\n%s\nNote: Please make the code change described above."
                                rel-file error-list-string
                                files-context-string)
                      (format "Please fix the following Flycheck errors in %s of file %s:\n\n%s\n%s\nNote: Please make the code change described above."
                              scope-description
                              rel-file
                              error-list-string
                              files-context-string)))
                   (edited-prompt (ai-code-read-string "Edit prompt for AI: "
                                                       prompt)))
              (when (and edited-prompt (not (string-blank-p edited-prompt)))
                (ai-code--insert-prompt edited-prompt)
                (message "Generated prompt to fix %d Flycheck error(s) in %s."
                         (length errors-in-scope)
                         scope-description)))))))))

(provide 'ai-code-change)

;;; ai-code-change.el ends here
