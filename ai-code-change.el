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
(require 'org)

(require 'ai-code-input)
(require 'ai-code-prompt-mode)

(declare-function ai-code-read-string "ai-code-input")
(declare-function ai-code--insert-prompt "ai-code-prompt-mode")
(declare-function ai-code--get-clipboard-text "ai-code-utils")
(declare-function ai-code--git-root "ai-code-utils" (&optional dir))
(declare-function ai-code--get-git-relative-paths "ai-code-discussion")
(declare-function ai-code--get-region-location-info "ai-code-discussion")
(declare-function ai-code--format-repo-context-info "ai-code-utils")
(declare-function dired-get-marked-files "dired"
                  (&optional localp arg filter distinguish-one-marked error-if-none-p))
(declare-function flycheck-error-pos "flycheck")
(declare-function flycheck-error-line "flycheck")
(declare-function flycheck-error-column "flycheck")
(declare-function flycheck-error-message "flycheck")

(defvar flycheck-current-errors)

(defconst ai-code-change--selected-region-note
  "Note: Please apply the code change to the selected region specified above."
  "Prompt note for code changes scoped to the selected region.")

(defconst ai-code-change--generic-note
  "Note: Please make the code change described above."
  "Prompt note for generic code-change requests.")

(defconst ai-code-change--selected-files-note
  "Note: Please make the code change described above for the selected files/directories."
  "Prompt note for code changes scoped to selected files or directories.")

(defconst ai-code-change--ask-question-note
  "Note: Please only answer the question about the code above, do not make any code changes."
  "Prompt note for question-only requests without code changes.")

(defconst ai-code-change--brief-default-boundaries
  (concat
   "Make only the requested code change. Avoid unrelated cleanup. "
   "Preserve existing functionality unless the goal requires changing it.")
  "Default boundaries section for structured code-change briefs.")

(defconst ai-code-change--brief-agent-responsibilities
  (concat
   "Inspect the relevant files before editing. Plan briefly, then edit the code. "
   "Run appropriate project verification for the change. "
   "Fix failures caused by the change.")
  "Default agent responsibilities section for structured code-change briefs.")

(defconst ai-code-change--brief-verification-evidence
  "Report the exact verification command(s), result, and any remaining risk or blocker."
  "Default verification evidence section for structured code-change briefs.")

(defconst ai-code-change--question-brief-default-boundaries
  "Answer the question only. Do not make code changes."
  "Default boundaries section for structured question briefs.")

(defconst ai-code-change--flycheck-brief-boundaries
  (concat
   "Fix only the listed Flycheck errors in the target scope. "
   "Preserve existing behavior and avoid unrelated cleanup or refactors.")
  "Boundaries section for Flycheck fix briefs.")

(defun ai-code--code-change-brief--section (title body)
  "Return a code-change brief section named TITLE with BODY.
When BODY is nil or blank, return nil."
  (when (and (stringp body)
             (not (string-blank-p body)))
    (format "%s:\n%s" title (string-trim body))))

(defun ai-code--compose-code-change-brief (&rest plist)
  "Return a structured agent brief for a code-change request.
PLIST accepts `:goal', `:scope', `:context', `:boundaries',
`:clipboard-context', and `:code-change-note'.  The brief is prompt text for
the agent; Emacs only prepares the handoff and does not perform the engineering
loop."
  (let* ((goal (plist-get plist :goal))
         (scope (plist-get plist :scope))
         (context (plist-get plist :context))
         (boundaries (or (plist-get plist :boundaries)
                         ai-code-change--brief-default-boundaries))
         (clipboard-context (plist-get plist :clipboard-context))
         (code-change-note (plist-get plist :code-change-note))
         (sections
          (delq nil
                (list
                 (ai-code--code-change-brief--section "Goal" goal)
                 (ai-code--code-change-brief--section "Scope" scope)
                 (ai-code--code-change-brief--section "Context" context)
                 (ai-code--code-change-brief--section "Clipboard context" clipboard-context)
                 (ai-code--code-change-brief--section "Boundaries" boundaries)
                 (ai-code--code-change-brief--section
                  "Agent responsibilities"
                  ai-code-change--brief-agent-responsibilities)
                 (ai-code--code-change-brief--section
                  "Verification evidence"
                  ai-code-change--brief-verification-evidence)
                 (ai-code--code-change-brief--section "Instruction" code-change-note)))))
    (mapconcat #'identity sections "\n\n")))

(defun ai-code--compose-question-brief (&rest plist)
  "Return a structured brief for question-only requests.
PLIST accepts `:goal', `:scope', `:context', `:clipboard-context',
`:boundaries', and `:instruction'."
  (let* ((goal (plist-get plist :goal))
         (scope (plist-get plist :scope))
         (context (plist-get plist :context))
         (clipboard-context (plist-get plist :clipboard-context))
         (boundaries (or (plist-get plist :boundaries)
                         ai-code-change--question-brief-default-boundaries))
         (instruction (plist-get plist :instruction))
         (sections
          (delq nil
                (list
                 (ai-code--code-change-brief--section "Goal" goal)
                 (ai-code--code-change-brief--section "Scope" scope)
                 (ai-code--code-change-brief--section "Context" context)
                 (ai-code--code-change-brief--section "Clipboard context" clipboard-context)
                 (ai-code--code-change-brief--section "Boundaries" boundaries)
                 (ai-code--code-change-brief--section "Instruction" instruction)))))
    (mapconcat #'identity sections "\n\n")))

(defun ai-code--flycheck-goal-string (scope-description rel-file error-list-string)
  "Return Flycheck-fix goal text for SCOPE-DESCRIPTION in REL-FILE.
ERROR-LIST-STRING is the formatted list of diagnostics."
  (if (string-equal "the entire file" scope-description)
      (format "Please fix the following Flycheck errors in file %s:\n\n%s"
              rel-file
              error-list-string)
    (format "Please fix the following Flycheck errors in %s of file %s:\n\n%s"
            scope-description
            rel-file
            error-list-string)))

(defun ai-code--is-comment-line (line)
  "Check if LINE is a comment line based on current buffer's comment syntax.
Returns non-nil if LINE starts with one or more comment characters,
ignoring leading whitespace.  Returns nil when the comment content
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
returns that function's name.  Otherwise returns the result of `which-function`."
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
  ;; DONE: this function should include org-mode TODO headline detection as well, as it used inside ai-code-implement-todo.
  (let ((text (if region-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'line t))))
    (or
     (when (and text comment-start)
       (let* ((first-line (car (split-string text "\n")))
              (comment-prefix-re (concat "^[ \t]*" (regexp-quote (string-trim-right comment-start)) "+[ \t]*")))
         (when (string-match comment-prefix-re first-line)
           (let ((rest (string-trim-left (substring first-line (match-end 0)))))
             (when (string-prefix-p "TODO" rest)
               (list text
                     (if region-active (region-beginning) (line-beginning-position))
                     (if region-active (region-end) (line-end-position))))))))
     (when (and (not region-active)
                (derived-mode-p 'org-mode)
                text)
       (save-excursion
         (when (and (org-at-heading-p)
                    (ignore-errors (org-back-to-heading t)))
           (let ((heading-line (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position))))
             (when (and (not (org-entry-is-done-p))
                        (not (string-match-p "^\\*+ DONE " heading-line)))
               (list heading-line
                     (line-beginning-position)
                     (line-end-position))))))))))

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
         (region-start-line (when region-active
                     (line-number-at-pos (region-beginning))))
         (region-location-info (when region-active
                                 (ai-code--get-region-location-info (region-beginning) (region-end))))
         (prompt-label (ai-code--generate-prompt-label clipboard-context region-active function-name))
         (initial-prompt (ai-code-read-string prompt-label ""))
         (files-context-string (ai-code--get-context-files-string))
         (repo-context-string (ai-code--format-repo-context-info))
         (scope-string
          (concat
           (format "Current file: %s" buffer-file-name)
           (when region-text
             (concat "\nSelected region:\n"
                     (cond
                      (region-location-info
                       (concat region-location-info "\n"))
                      (region-start-line
                       (format "Start line: %d\n" region-start-line)))
                     region-text))
           (when function-name (format "\nFunction: %s" function-name))
           files-context-string))
         (final-prompt
          (ai-code--compose-code-change-brief
           :goal initial-prompt
           :scope scope-string
           :context repo-context-string
           :clipboard-context (and clipboard-context
                                   (string-match-p "\\S-" clipboard-context)
                                   clipboard-context)
           :code-change-note (if region-text
                                 ai-code-change--selected-region-note
                               ai-code-change--generic-note))))
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
          (ai-code--compose-code-change-brief
           :goal initial-prompt
           :scope (concat "Selected files/directories:\n" files-str)
           :context repo-context-string
           :clipboard-context (and clipboard-context
                                   (string-match-p "\\S-" clipboard-context)
                                   clipboard-context)
           :code-change-note ai-code-change--selected-files-note)))
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
          ;; DONE: in this code path, is it possible to default to Code Change in ai-code-implement-todo, no need to ask user to choose Code Change or Ask Question?
          (ai-code-implement-todo arg "Code change")
        (ai-code--handle-regular-code-change arg region-active))))))

;;;###autoload
(defun ai-code-implement-todo (arg &optional default-action)
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
Argument ARG is the prefix argument.
Optional DEFAULT-ACTION skips the action prompt when non-nil."
  ;; DONE: I want to implement the idea inside https://github.com/tninja/ai-code-interface.el/issues/316, it could to either code change or ask question, given user's input with completing-read selection. The difference of this org-mode section TODO, with the existing comment todo is, it won't replace the TODO section with implementation. It just use the section headline and content inside this section as part of prompt, and send to AI.
  ;; DONE: for this command triggered from org-mode file buffer. We want to let user choose if they want to add the condense result summary as a section (org headline), at the end of current section under cursor. If user choose yes. The prompt should let AI know this and where to add (maybe let it know current file, headline / cursor position maybe), so that it can add result summary.
  ;; DONE: This result summary looks good. Is it possible to move this feature to the function just before sending prompt to AI, so that it can be applied to other code change or question asking command as well? The key point is to let user choose if they want to have this summary added to the file, and where to add, so that the prompt can include this requirement and context.
  (interactive "P")
  (if (not buffer-file-name)
      (user-error "Error: buffer-file-name must be available")
    (cl-block finalize
      (when (ai-code--implement-todo--handle-done-line)
        (cl-return-from finalize nil))
      (when (ai-code--implement-todo--handle-blank-line)
        (cl-return-from finalize nil))
      (ai-code--implement-todo--build-and-send-prompt arg default-action))))

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

(defun ai-code--implement-todo--get-org-todo-section-info ()
  "Return current Org TODO section info as a plist, or nil.
The plist contains `:heading-line', `:content', and `:line-number'."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (when (and (org-at-heading-p)
                 (ignore-errors (org-back-to-heading t)))
        (let* ((line-number (line-number-at-pos (point)))
               (heading-line (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))
          (when (and (not (org-entry-is-done-p))
                     (not (string-match-p "^\\*+ DONE " heading-line)))
            (let* ((content-start (save-excursion
                                    (forward-line 1)
                                    (point)))
                   (content-end (save-excursion
                                  (org-end-of-subtree t t)
                                  (point)))
                   (content (string-trim-right
                             (buffer-substring-no-properties
                              content-start
                              content-end))))
              (list :heading-line heading-line
                    :content content
                    :line-number line-number))))))))

(defun ai-code--implement-todo--format-org-section-block (org-todo-section-info)
  "Return formatted Org section text from ORG-TODO-SECTION-INFO."
  (when org-todo-section-info
    (let ((heading-line (plist-get org-todo-section-info :heading-line))
          (content (plist-get org-todo-section-info :content)))
      (concat (replace-regexp-in-string "\\`\\*+ " "" heading-line)
              (unless (string-blank-p content)
                (concat "\n" content))))))

(defun ai-code--implement-todo--org-todo-headline-p (heading-line)
  "Return non-nil when HEADING-LINE begins with a TODO-style Org headline."
  (string-match-p "^[[:space:]]*\\*+[[:space:]]+TODO:?\\(?:[[:space:]]\\|$\\)"
                  heading-line))

(defun ai-code--implement-todo--clipboard-context-present-p (context)
  "Return non-nil when CONTEXT has nonblank clipboard text."
  (let ((clipboard-context (plist-get context :clipboard-context)))
    (and clipboard-context
         (string-match-p "\\S-" clipboard-context))))

(defun ai-code--implement-todo--collect-prompt-context (arg)
  "Collect prompt context for `ai-code-implement-todo'.
ARG controls whether clipboard context is included."
  (let* ((clipboard-context (when arg (ai-code--get-clipboard-text)))
         (current-line (string-trim (thing-at-point 'line t)))
         (current-line-number (line-number-at-pos (point)))
         (is-comment (ai-code--is-comment-line current-line))
         (function-name (if is-comment
                            (ai-code--get-function-name-for-comment)
                          (which-function)))
         (org-todo-section-info (ai-code--implement-todo--get-org-todo-section-info))
         (org-section-block
          (ai-code--implement-todo--format-org-section-block
           org-todo-section-info))
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
         (files-context-string (ai-code--get-context-files-string)))
    (list :clipboard-context clipboard-context
          :current-line current-line
          :current-line-number current-line-number
          :is-comment is-comment
          :function-name function-name
          :org-todo-section-info org-todo-section-info
          :org-line-number (plist-get org-todo-section-info :line-number)
          :org-section-block org-section-block
          :function-context function-context
          :region-text region-text
          :region-location-line region-location-line
          :files-context-string files-context-string)))

(defun ai-code--implement-todo--validate-prompt-context (context)
  "Signal a user error when CONTEXT cannot drive TODO implementation."
  (let ((org-todo-section-info (plist-get context :org-todo-section-info))
        (region-text (plist-get context :region-text))
        (is-comment (plist-get context :is-comment)))
    (unless (or org-todo-section-info region-text is-comment)
      (user-error "Current line is not a TODO comment or Org headline and cannot proceed with `ai-code-implement-todo'.  Please select a TODO comment (not DONE), an Org headline (not DONE), a region of comments, or activate on a blank line"))
    (unless (or (not region-text)
                (ai-code--is-comment-block region-text))
      (user-error "Selected region must be a comment block"))))

(defun ai-code--implement-todo--read-action-intent (default-action)
  "Return DEFAULT-ACTION or prompt for the TODO action intent."
  (or default-action
      (completing-read "Select action: "
                       '("Code change" "Ask question")
                       nil t)))

(defun ai-code--implement-todo--prompt-label (context ask-question-p)
  "Return prompt label for CONTEXT and ASK-QUESTION-P."
  (let ((org-todo-section-info (plist-get context :org-todo-section-info))
        (region-text (plist-get context :region-text))
        (is-comment (plist-get context :is-comment))
        (function-name (plist-get context :function-name))
        (has-clipboard-context
         (ai-code--implement-todo--clipboard-context-present-p context)))
    (cond
     ((and ask-question-p org-todo-section-info)
      (if has-clipboard-context
          "Question about Org headline (clipboard context): "
        "Question about Org headline: "))
     (ask-question-p
      (if has-clipboard-context
          "Question about TODO comment (clipboard context): "
        "Question about TODO comment: "))
     ((and org-todo-section-info has-clipboard-context)
      "Implementation instruction for Org headline (clipboard context): ")
     (has-clipboard-context
      (cond
       (region-text "TODO implementation instruction (clipboard context): ")
       (is-comment "TODO implementation instruction (clipboard context): ")
       (function-name (format "TODO implementation instruction for function %s (clipboard context): " function-name))
       (t "TODO implementation instruction (clipboard context): ")))
     (org-todo-section-info "Implementation instruction for Org headline: ")
     (region-text "TODO implementation instruction: ")
     (is-comment "TODO implementation instruction: ")
     (function-name (format "TODO implementation instruction for function %s: " function-name))
     (t "TODO implementation instruction: "))))

(defun ai-code--implement-todo--initial-input (context ask-question-p)
  "Return initial prompt input for CONTEXT and ASK-QUESTION-P."
  (let ((is-comment (plist-get context :is-comment))
        (org-todo-section-info (plist-get context :org-todo-section-info))
        (region-text (plist-get context :region-text)))
    (cond
     ((and ask-question-p org-todo-section-info)
      "Please answer my question about this Org headline.")
     ((and ask-question-p region-text)
      "Please answer my question about this selected TODO comment block.")
     ((and ask-question-p is-comment)
      "Please answer my question about this TODO comment.")
     (org-todo-section-info
      "Please implement code for this Org headline first.")
     (region-text
      "Please implement code for this selected TODO comment block first.")
     (is-comment
      "Please implement code for this TODO comment first.")
     (t ""))))

(defun ai-code--implement-todo--scope (context)
  "Return structured TODO scope text for CONTEXT."
  (let ((current-line (plist-get context :current-line))
        (current-line-number (plist-get context :current-line-number))
        (is-comment (plist-get context :is-comment))
        (org-todo-section-info (plist-get context :org-todo-section-info))
        (org-line-number (plist-get context :org-line-number))
        (org-section-block (plist-get context :org-section-block))
        (function-context (plist-get context :function-context))
        (region-text (plist-get context :region-text))
        (region-location-line (plist-get context :region-location-line))
        (files-context-string (plist-get context :files-context-string)))
    (concat
     (cond
      (org-todo-section-info
       (format "Org headline on line %d:\n%s"
               org-line-number
               org-section-block))
      (region-text
       (format "%s\n%s"
               region-location-line
               region-text))
      (is-comment
       (format "TODO comment on line %d:\n%s"
               current-line-number
               current-line))
      (t "Current context"))
     function-context
     files-context-string)))

(defun ai-code--implement-todo--code-change-boundaries (context)
  "Return code-change boundaries for TODO CONTEXT."
  (let ((org-todo-section-info (plist-get context :org-todo-section-info))
        (region-text (plist-get context :region-text))
        (is-comment (plist-get context :is-comment)))
    (cond
     (org-todo-section-info
      "Keep the Org headline in place and keep changes scoped to the requested implementation.")
     ((or region-text is-comment)
      "Keep the TODO comment in place and ensure it is marked DONE after implementation; avoid unrelated cleanup.")
     (t ai-code-change--brief-default-boundaries))))

(defun ai-code--implement-todo--final-prompt (prompt context action-intent)
  "Return final prompt from PROMPT, CONTEXT, and ACTION-INTENT."
  (let* ((ask-question-p (string= action-intent "Ask question"))
         (clipboard-context
          (and (ai-code--implement-todo--clipboard-context-present-p context)
               (plist-get context :clipboard-context))))
    (if ask-question-p
        (ai-code--compose-question-brief
         :goal prompt
         :scope (ai-code--implement-todo--scope context)
         :context (ai-code--format-repo-context-info)
         :clipboard-context clipboard-context
         :instruction ai-code-change--ask-question-note)
      (ai-code--compose-code-change-brief
       :goal prompt
       :scope (ai-code--implement-todo--scope context)
       :context (ai-code--format-repo-context-info)
       :clipboard-context clipboard-context
       :boundaries (ai-code--implement-todo--code-change-boundaries context)
       :code-change-note (if (plist-get context :region-text)
                             ai-code-change--selected-region-note
                           ai-code-change--generic-note)))))

(defun ai-code--implement-todo--build-and-send-prompt (arg &optional default-action)
  "Build the TODO implementation prompt and insert it.
ARG is the prefix argument for clipboard context.
Optional DEFAULT-ACTION skips the `completing-read' prompt when non-nil."
  (let ((context (ai-code--implement-todo--collect-prompt-context arg)))
    (ai-code--implement-todo--validate-prompt-context context)
    (let* ((action-intent
            (ai-code--implement-todo--read-action-intent default-action))
           (ask-question-p (string= action-intent "Ask question"))
           (prompt-label
            (ai-code--implement-todo--prompt-label context ask-question-p))
           (initial-input
            (ai-code--implement-todo--initial-input context ask-question-p))
           (prompt (ai-code-read-string prompt-label initial-input))
           (final-prompt
            (ai-code--implement-todo--final-prompt
             prompt context action-intent)))
      (ai-code--insert-prompt final-prompt))))

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
      (let* ((git-root (or (ai-code--git-root) default-directory))
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
                   (repo-context-string (ai-code--format-repo-context-info))
                   (error-list-string
                    (ai-code-flycheck--format-error-list errors-in-scope
                                                         rel-file))
                   (scope-string
                    (concat
                     (format "Current file: %s\nTarget scope: %s"
                             buffer-file-name
                             scope-description)
                     files-context-string))
                   (goal-string
                    (ai-code--flycheck-goal-string scope-description
                                                   rel-file
                                                   error-list-string))
                   (prompt
                    (ai-code--compose-code-change-brief
                     :goal goal-string
                     :scope scope-string
                     :context repo-context-string
                     :boundaries ai-code-change--flycheck-brief-boundaries
                     :code-change-note ai-code-change--generic-note))
                   (edited-prompt (ai-code-read-string "Edit prompt for AI: "
                                                       prompt)))
              (when (and edited-prompt (not (string-blank-p edited-prompt)))
                (ai-code--insert-prompt edited-prompt)
                (message "Generated prompt to fix %d Flycheck error(s) in %s."
                         (length errors-in-scope)
                         scope-description)))))))))

(provide 'ai-code-change)

;;; ai-code-change.el ends here
