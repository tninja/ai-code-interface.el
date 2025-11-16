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

(defun ai-code--is-comment-line (line)
  "Check if LINE is a comment line based on current buffer's comment syntax.
Returns non-nil if LINE starts with one or more comment characters,
ignoring leading whitespace."
  (when comment-start
    (let ((comment-str (string-trim-right comment-start)))
      (string-match-p (concat "^[ 	]*"
                              (regexp-quote comment-str)
                              "+")
                      (string-trim-left line)))))

(defun ai-code--get-function-name-for-comment ()
  "Get the appropriate function name when cursor is on a comment line.
If the comment precedes a function definition or is inside a function body,
returns that function's name. Otherwise returns the result of `which-function`."
  (interactive)
  (let* ((current-func (which-function))
         (resolved-func
          (save-excursion
            ;; Move to next non-comment, non-blank line
            (forward-line 1)
            (while (and (not (eobp))
                        (or (looking-at-p "^[ \t]*$")
                            (ai-code--is-comment-line
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position)))))
              (forward-line 1))
            ;; Get function name at this position, trying a short lookahead inside
            ;; the function body when `which-function` cannot resolve the def line.
            (unless (eobp)
              (let ((lookahead 5)
                    (next-func (which-function)))
                (while (and (> lookahead 0)
                            (or (null next-func)
                                (string= next-func current-func)))
                  (forward-line 1)
                  (setq lookahead (1- lookahead))
                  (unless (or (eobp)
                              (looking-at-p "^[ \t]*$")
                              (ai-code--is-comment-line
                               (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position))))
                    (setq next-func (which-function))))
                (cond
                 ;; No current function, use the next if found.
                 ((not current-func) next-func)
                 ;; No next function, keep the current context.
                 ((not next-func) current-func)
                 ;; Prefer the forward definition when it differs from current.
                 ((not (string= next-func current-func)) next-func)
                 ;; Otherwise fall back to current.
                 (t current-func)))))))
    ;; (when resolved-func
    ;;   (message "Identified function: %s" resolved-func))
    resolved-func))

;;;###autoload
(defun ai-code-code-change (arg)
  "Generate prompt to change code under cursor or in selected region.
With a prefix argument (C-u), append the clipboard contents as context.
If a region is selected, change that specific region.
Otherwise, change the function under cursor.
If nothing is selected and no function context, prompts for general code change.
Inserts the prompt into the AI prompt file and optionally sends to AI.
Argument ARG is the prefix argument."
  (interactive "P")
  (unless buffer-file-name
    (user-error "Error: buffer-file-name must be available"))
  (let* ((clipboard-context (when arg (ai-code--get-clipboard-text)))
         (function-name (which-function))
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
         (initial-prompt (ai-code-read-string prompt-label ""))
         (files-context-string (ai-code--get-context-files-string))
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
                  (when (and clipboard-context
                            (string-match-p "\\S-" clipboard-context))
                    (concat "\n\nClipboard context:\n" clipboard-context))
                  (if region-text
                      "\nNote: Please apply the code change to the selected region specified above."
                    "\nNote: Please make the code change described above."))))
    (ai-code--insert-prompt final-prompt)))

;;;###autoload
(defun ai-code-implement-todo (arg)
  "Generate prompt to implement TODO comments in current context.
With a prefix argument (universal-argument), implement code after the comment instead of replacing it in-place.
If region is selected, implement that specific region.
If cursor is on a comment line, implement that specific comment.
If cursor is inside a function, implement comments for that function.
Otherwise implement comments for the entire current file.
Argument ARG is the prefix argument."
  (interactive "P")
  (if (not buffer-file-name)
      (message "Error: buffer-file-name must be available")
    (let* ((current-line (string-trim (thing-at-point 'line t)))
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
           (files-context-string (ai-code--get-context-files-string))
           (initial-input
            (if arg
                ;; With prefix argument: implement after comment, not in-place
                (cond
                 (region-text
                  (format "Please implement code after this requirement comment block starting on line %d: '%s'. Leave the comment as-is and add the implementation code after it. Keep the existing code structure and add the implementation after this specific block.%s%s"
                          region-start-line region-text function-context files-context-string))
                 (is-comment
                  (format "Please implement code after this requirement comment on line %d: '%s'. Leave the comment as-is and add the implementation code after it. Keep the existing code structure and add the implementation after this specific comment.%s%s"
                          current-line-number current-line function-context files-context-string))
                 (function-name
                  (format "Please implement code after all TODO comments in function '%s'. The TODO are TODO comments. Leave the comments as-is and add implementation code after each comment. Keep the existing code structure and only add code after these marked items.%s"
                          function-name files-context-string))
                 (t
                  (format "Please implement code after all TODO comments in file '%s'. The TODO are TODO comments. Leave the comments as-is and add implementation code after each comment. Keep the existing code structure and only add code after these marked items.%s"
                          (file-name-nondirectory buffer-file-name) files-context-string)))
              ;; Without prefix argument: replace in-place (original behavior)
              (cond
               (region-text
                (format "Please implement this requirement comment block starting on line %d in-place: '%s'. It is already inside current code. Please replace it with implementation. Keep the existing code structure and implement just this specific block.%s%s"
                        region-start-line region-text function-context files-context-string))
               (is-comment
                (format "Please implement this requirement comment on line %d in-place: '%s'. It is already inside current code. Please replace it with implementation. Keep the existing code structure and implement just this specific comment.%s%s"
                        current-line-number current-line function-context files-context-string))
               (function-name
                (format "Please implement all TODO in-place in function '%s'. The TODO are TODO comments. Keep the existing code structure and only implement these marked items.%s"
                        function-name files-context-string))
               (t
                (format "Please implement all TODO in-place in file '%s'. The TODO are TODO comments. Keep the existing code structure and only implement these marked items.%s"
                        (file-name-nondirectory buffer-file-name) files-context-string)))))
           (prompt (ai-code-read-string "TODO implementation instruction: " initial-input)))
      (ai-code--insert-prompt prompt))))

;;; Flycheck integration
(defun ai-code-flycheck--get-errors-in-scope (start end)
  "Return a list of Flycheck errors within the given START and END buffer positions."
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
                        (format (concat "Please fix the following Flycheck "
                                        "errors in file %s:\n\n%s\n%s\n"
                                        "Note: Please make the code change "
                                        "described above.")
                                rel-file error-list-string files-context-string)
                      (format (concat "Please fix the following Flycheck "
                                      "errors in %s of file %s:\n\n%s\n%s\n"
                                      "Note: Please make the code "
                                      "change described above.")
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
