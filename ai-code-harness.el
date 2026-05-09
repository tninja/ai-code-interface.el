;;; ai-code-harness.el --- Harness support for ai-code -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Harness generation, auto-test suffix helpers, and send-time routing for ai-code.

;;; Code:

(require 'seq)
(require 'subr-x)

(require 'ai-code-agile)
(require 'ai-code-backends)
(require 'ai-code-change)
(require 'ai-code-discussion)
(require 'ai-code-prompt-mode)

(declare-function ai-code--ensure-files-directory "ai-code-prompt-mode" ())
(declare-function ai-code--git-root "ai-code-file" (&optional dir))
(declare-function ai-code-call-gptel-sync "ai-code-prompt-mode" (question))

(defvar ai-code-mcp-agent-enabled-backends)
(defvar ai-code-selected-backend)

;;;; Auto-Test Harness: Content and Cache

(defconst ai-code--diagnostics-first-harness-instruction
  "Record a diagnostics baseline with the get_diagnostics MCP tool before editing. After each edit, re-run get_diagnostics for the touched files and do not finish until they have no new diagnostics compared with the baseline."
  "Shared diagnostics-first harness guidance for code-change prompts.")

(defun ai-code--diagnostics-first-harness-instruction-inline ()
  "Return diagnostics-first guidance formatted for inline prompt text."
  (concat (downcase (substring ai-code--diagnostics-first-harness-instruction 0 1))
          (substring ai-code--diagnostics-first-harness-instruction 1)))

;;;###autoload
(defcustom ai-code-test-after-code-change-suffix
  "If any program code changes, run unit-tests and follow up on the test-result (fix code if there is an error)."
  "User-provided prompt suffix for test-after-code-change."
  :type '(choice (const nil) string)
  :group 'ai-code)

(defconst ai-code--auto-test-harness-file-version "v1"
  "Version tag appended to generated auto-test harness file names.")

;;;###autoload
(defcustom ai-code-auto-test-harness-cache-directory
  nil
  "Directory used to cache generated auto-test harness files.

When nil, store harness files under `harness/` inside the directory returned
by `ai-code--ensure-files-directory`.  In a Git repository, that is typically
`.ai.code.files/harness/` under the current repository so prompts can cite
them with `@`-prefixed repo-relative paths.  Outside a Git repository, this
falls back to `harness/` under `default-directory`.

Set this to a directory path to override the default location."
  :type '(choice
          (const :tag "Use default harness directory (.ai.code.files/harness in a repo, or harness under default-directory otherwise)"
                 nil)
                 directory)
  :group 'ai-code)

(defun ai-code--auto-test-harness-directory ()
  "Return the directory used for generated auto-test harness files."
  (let ((cache-directory (and (boundp 'ai-code-auto-test-harness-cache-directory)
                              ai-code-auto-test-harness-cache-directory)))
    (if cache-directory
        (expand-file-name cache-directory)
      (expand-file-name "harness/" (ai-code--ensure-files-directory)))))

(defun ai-code--auto-test-harness-prompt-path (file-path)
  "Return FILE-PATH formatted for prompt usage.
When FILE-PATH is inside the current git repository, return an `@`-prefixed
repo-relative path.  Otherwise return the absolute FILE-PATH."
  (if-let ((git-root (ai-code--git-root)))
      (let ((git-root-truename (file-name-as-directory (file-truename git-root)))
            (file-truename (file-truename file-path)))
        (if (file-in-directory-p file-truename git-root-truename)
            (concat "@" (file-relative-name file-truename git-root-truename))
          file-path))
    file-path))

(defun ai-code--auto-test-backend ()
  "Return the backend symbol used for auto-test prompt decisions."
  (if (fboundp 'ai-code--effective-backend)
      (or (ai-code--effective-backend) ai-code-selected-backend)
    ai-code-selected-backend))

(defun ai-code--diagnostics-harness-enabled-p ()
  "Return non-nil when the current backend should get diagnostics guidance."
  (memq (ai-code--auto-test-backend)
        ai-code-mcp-agent-enabled-backends))

(defun ai-code--maybe-append-diagnostics-harness-instruction (suffix &optional inline)
  "Append diagnostics harness guidance to SUFFIX when the backend supports it.
When INLINE is non-nil, use the inline-formatted diagnostics instruction."
  (if (and (stringp suffix)
           (> (length suffix) 0)
           (ai-code--diagnostics-harness-enabled-p))
      (let ((instruction (if inline
                             (ai-code--diagnostics-first-harness-instruction-inline)
                           ai-code--diagnostics-first-harness-instruction)))
        (concat suffix
                (if inline " " "")
                instruction))
    suffix))

(defun ai-code--test-after-code-change--resolve-tdd-suffix ()
  "Return the TDD-style suffix for test-after-code-change prompt text."
  (ai-code--maybe-append-diagnostics-harness-instruction
   (concat ai-code--tdd-red-green-base-instruction
           ai-code--tdd-red-green-tail-instruction
           ai-code--tdd-run-test-after-each-stage-instruction
           ai-code--tdd-test-pattern-instruction)))

(defun ai-code--test-after-code-change--resolve-tdd-with-refactoring-suffix ()
  "Return the TDD+refactoring suffix for test-after-code-change prompt text."
  (ai-code--maybe-append-diagnostics-harness-instruction
   (concat ai-code--tdd-red-green-base-instruction
           ai-code--tdd-with-refactoring-extension-instruction
           ai-code--tdd-red-green-tail-instruction
           ai-code--tdd-run-test-after-each-stage-instruction
           ai-code--tdd-test-pattern-instruction)))

(defun ai-code--auto-test-inline-suffix-for-type (type)
  "Return the inline prompt suffix for auto test TYPE."
  (pcase type
    ('test-after-change
     (ai-code--maybe-append-diagnostics-harness-instruction
      ai-code-test-after-code-change-suffix t))
    ('tdd (ai-code--test-after-code-change--resolve-tdd-suffix))
    ('tdd-with-refactoring (ai-code--test-after-code-change--resolve-tdd-with-refactoring-suffix))
    ('no-test "Do not write or run any test.")
    (_ nil)))

(defun ai-code--auto-test-harness-file-name (type)
  "Return the stable harness file name for auto test TYPE."
  (let ((base-name (symbol-name type)))
    (format "%s%s.%s.md"
            base-name
            (if (ai-code--diagnostics-harness-enabled-p)
                "-diagnostics"
              "")
            ai-code--auto-test-harness-file-version)))

(defun ai-code--ensure-auto-test-harness-cache-directory ()
  "Ensure the auto-test harness cache directory exists and return it."
  (let ((directory (ai-code--auto-test-harness-directory)))
    (unless (file-directory-p directory)
      (make-directory directory t))
    directory))

(defun ai-code--auto-test-harness-text-for-type (type)
  "Return the externalized harness text for auto test TYPE."
  (pcase type
    ('no-test nil)
    (_ (ai-code--auto-test-inline-suffix-for-type type))))

(defun ai-code--ensure-auto-test-harness-file (type)
  "Write and return the cached harness file path for auto test TYPE."
  (when-let ((content (ai-code--auto-test-harness-text-for-type type)))
    (let* ((directory (ai-code--ensure-auto-test-harness-cache-directory))
           (file-path (expand-file-name
                       (ai-code--auto-test-harness-file-name type)
                       directory)))
      (with-temp-file file-path
        (insert content)
        (unless (bolp)
          (insert "\n")))
      file-path)))

(defun ai-code--auto-test-harness-reference-suffix (type)
  "Return a short suffix that references the cached harness file for TYPE.

If the harness file cannot be prepared, fall back to the inline suffix."
  (condition-case err
      (when-let ((file-path (ai-code--ensure-auto-test-harness-file type)))
        (format
         "Read the local harness file: %s. Use its instructions for this work. Apply it without repeating its full contents."
         (ai-code--auto-test-harness-prompt-path file-path)))
    (file-error
     (message "Failed to prepare auto-test harness file for %s: %s"
              type
              (error-message-string err))
     (ai-code--auto-test-inline-suffix-for-type type))))

(defun ai-code--auto-test-suffix-for-type (type)
  "Return prompt suffix for auto test TYPE."
  (pcase type
    ((or 'test-after-change 'tdd 'tdd-with-refactoring)
     (ai-code--auto-test-harness-reference-suffix type))
    ('no-test "Do not write or run any test.")
    (_ nil)))

;;;; Send-Time Routing: State and User Settings

(defvar ai-code-auto-test-suffix ai-code-test-after-code-change-suffix
  "Default prompt suffix to request running tests after code changes.")

(defvar ai-code-auto-test-type nil
  "Forward declaration for `ai-code-auto-test-type'.
See the later `defcustom' for user-facing documentation and default.")

(defvar ai-code-discussion-auto-follow-up-suffix nil
  "Send-time prompt suffix that requests numbered next-step suggestions.")

(defvar ai-code-discussion-auto-follow-up-enabled t
  "Forward declaration for `ai-code-discussion-auto-follow-up-enabled'.
See the later `defcustom' for user-facing documentation and default.")

(defconst ai-code--auto-test-type-ask-choices
  '(("Run tests after code change" . test-after-change)
    ("Do not write or run tests" . no-test)
    ("TDD Red + Green (write failing test, then make it pass)" . tdd)
    ("TDD Red + Green + Blue (refactor after Green)" . tdd-with-refactoring))
  "Resolve auto test suffix choices for `ask-me` mode.")

(defconst ai-code--auto-test-type-persistent-choices
  '(("Ask every time" . ask-me)
    ("Off" . nil))
  "Persistent choices for `ai-code-auto-test-type`.")

(defconst ai-code--auto-test-type-legacy-persistent-modes
  '(test-after-change tdd tdd-with-refactoring)
  "Legacy persistent values still honored for backward compatibility.")

(defun ai-code--read-auto-test-type-choice ()
  "Read and return one prompt test type for this send action."
  (let* ((choice (completing-read "Choose test prompt type for this send: "
                                  (mapcar #'car ai-code--auto-test-type-ask-choices)
                                  nil t nil nil
                                  (caar ai-code--auto-test-type-ask-choices)))
         (choice-cell (assoc choice ai-code--auto-test-type-ask-choices)))
    (if choice-cell
        (cdr choice-cell)
      'test-after-change)))

(defun ai-code--read-auto-follow-up-choice ()
  "Read whether to request numbered next-step suggestions for this send action."
  (y-or-n-p "Discussion follow-up suggestions? "))

;;;###autoload
(defcustom ai-code-use-gptel-classify-prompt nil
  "Whether to use GPTel to classify prompts before send-time suffix routing.
When non-nil and `ai-code-auto-test-type` or
`ai-code-discussion-auto-follow-up-enabled` is non-nil, classify whether
the current prompt is about code changes.  This lets code-change prompts
skip discussion follow-up suggestions, and discussion prompts skip auto
test suffixes."
  :type 'boolean
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-next-step-suggestion-suffix
  (concat
   "At the end of your response, provide 3-4 numbered candidate next\n"
   "steps. Keep each option to one sentence. At least 2 candidates must\n"
   "be AI-actionable items as follow up: either a code change or tool usage. Mark the\n"
   "single best option with \"(Recommended)\". If the user replies with\n"
   "only a number such as 1, 2, 3, or 4, treat that as selecting the\n"
   "corresponding next step from your previous answer. The user may also\n"
   "ignore these options and send a different follow-up request instead.")
  "Prompt suffix for numbered next-step suggestions in discussion prompts."
  :type '(choice (const nil) string)
  :group 'ai-code)

;;;; Send-Time Routing: Prompt Classification

(defun ai-code--downcase-strings (strings)
  "Return STRINGS converted to lowercase."
  (mapcar #'downcase strings))

(defconst ai-code--code-change-prompt-markers
  (ai-code--downcase-strings
   (list ai-code-change--selected-region-note
         ai-code-change--generic-note
         ai-code-change--selected-files-note))
  "Prompt markers that clearly indicate a code-change request.")

(defconst ai-code--non-code-change-prompt-markers
  (append
   (ai-code--downcase-strings
    (list ai-code-discussion--question-only-note
          ai-code-discussion--selected-region-note))
   (ai-code--downcase-strings
    ai-code-discussion--explain-prompt-prefixes))
  "Prompt markers that clearly indicate a non-code-change request.")

(defun ai-code--prompt-contains-any-marker-p (text markers)
  "Return non-nil when any string in MARKERS appears in TEXT."
  (seq-some (lambda (marker)
              (string-match-p (regexp-quote marker) text))
            markers))

(defun ai-code--simple-classify-prompt-code-change (prompt-text)
  "Classify PROMPT-TEXT with cheap string matching before GPTel.
Return one of: `code-change`, `non-code-change`, or `unknown`."
  (let ((text (downcase (or prompt-text ""))))
    (cond
     ((ai-code--prompt-contains-any-marker-p text
                                             ai-code--code-change-prompt-markers)
      'code-change)
     ((ai-code--prompt-contains-any-marker-p text
                                             ai-code--non-code-change-prompt-markers)
      'non-code-change)
     (t 'unknown))))

(defun ai-code--classify-prompt-code-change (prompt-text)
  "Classify whether PROMPT-TEXT requests a code change.
Use simple string matching first, then fall back to GPTel."
  (let ((classification
         (ai-code--simple-classify-prompt-code-change prompt-text)))
    (if (eq classification 'unknown)
        (ai-code--gptel-classify-prompt-code-change prompt-text)
      classification)))

(defun ai-code--gptel-classify-prompt-code-change (prompt-text)
  "Classify whether PROMPT-TEXT requests a code change using GPTel.
Return one of: `code-change`, `non-code-change`, or `unknown`."
  (let ((classification
         (condition-case err
             (if (require 'gptel nil t)
                 (let* ((raw-answer (ai-code-call-gptel-sync
                                     (concat "Classify whether this user prompt requests program code changes in a repository.\n"
                                             "Reply with exactly one token: CODE_CHANGE or NOT_CODE_CHANGE.\n"
                                             "Return CODE_CHANGE only for changes to program code or test code.\n"
                                             "Treat documentation changes and any other non-program-code actions as NOT_CODE_CHANGE.\n"
                                             "Treat explain/summarize/discuss/review without editing as NOT_CODE_CHANGE.\n\n"
                                             "Prompt:\n" prompt-text)))
                        (answer (upcase (string-trim (or raw-answer "")))))
                   (cond
                    ((string-match-p "\\`CODE_CHANGE\\b" answer) 'code-change)
                    ((string-match-p "\\`NOT_CODE_CHANGE\\b" answer) 'non-code-change)
                    (t 'unknown)))
               'unknown)
           (error
            (message "GPTel prompt classification failed: %s" (error-message-string err))
            'unknown))))
    (message "GPTel prompt classification result: %s" classification)
    classification))

;;;; Send-Time Routing: Suffix Resolution

(defun ai-code--resolve-auto-test-type-for-send (&optional prompt-text classification)
  "Resolve the concrete auto test type for current send action for PROMPT-TEXT.
CLASSIFICATION is the optional prompt classification result."
  (if (eq ai-code-auto-test-type 'ask-me)
      (ai-code--resolve-ask-auto-test-type-for-send prompt-text classification)
    (and (memq ai-code-auto-test-type
               ai-code--auto-test-type-legacy-persistent-modes)
         ai-code-auto-test-type)))

(defun ai-code--resolve-ask-auto-test-type-for-send (&optional prompt-text classification)
  "Resolve the send-time auto test type for ask-me mode with PROMPT-TEXT.
CLASSIFICATION is the optional prompt classification result."
  (if ai-code-use-gptel-classify-prompt
      (pcase (or classification
                 (ai-code--classify-prompt-code-change prompt-text))
        ('code-change (ai-code--read-auto-test-type-choice))
        ('non-code-change nil)
        (_ (ai-code--read-auto-test-type-choice)))
    (ai-code--read-auto-test-type-choice)))

(defun ai-code--resolve-auto-follow-up-suffix-for-send (&optional prompt-text classification)
  "Resolve next-step suggestion suffix for current send action for PROMPT-TEXT.
CLASSIFICATION is the optional prompt classification result."
  (when (and ai-code-discussion-auto-follow-up-enabled
             ai-code-next-step-suggestion-suffix)
    (let ((classification (or classification
                              (and ai-code-use-gptel-classify-prompt
                                   (ai-code--classify-prompt-code-change prompt-text)))))
      (unless (eq classification 'code-change)
        (and (ai-code--read-auto-follow-up-choice)
             ai-code-next-step-suggestion-suffix)))))

(defun ai-code--resolve-auto-test-suffix-for-send (&optional prompt-text classification)
  "Resolve auto test suffix for current send action for PROMPT-TEXT.
CLASSIFICATION is the optional prompt classification result."
  (ai-code--auto-test-suffix-for-type
   (ai-code--resolve-auto-test-type-for-send prompt-text classification)))

(defun ai-code--classify-prompt-for-send (&optional prompt-text)
  "Return prompt classification for PROMPT-TEXT when needed.
Send-time routing uses this result for test and discussion follow-up suffixes."
  (when (and ai-code-use-gptel-classify-prompt
             (or ai-code-auto-test-type
                 ai-code-discussion-auto-follow-up-enabled))
    (ai-code--classify-prompt-code-change prompt-text)))

;;;; Send-Time Routing: Advice and Setters

(defun ai-code--with-auto-test-suffix-for-send (orig-fun prompt-text)
  "Resolve and bind send-time suffixes before calling ORIG-FUN with PROMPT-TEXT."
  (let* ((classification (ai-code--classify-prompt-for-send prompt-text))
         (ai-code-auto-test-suffix
          (ai-code--resolve-auto-test-suffix-for-send
           prompt-text classification))
         (ai-code-discussion-auto-follow-up-suffix
          (ai-code--resolve-auto-follow-up-suffix-for-send
           prompt-text classification)))
    (funcall orig-fun prompt-text)))

(unless (advice-member-p #'ai-code--with-auto-test-suffix-for-send
                         'ai-code--write-prompt-to-file-and-send)
  (advice-add 'ai-code--write-prompt-to-file-and-send
              :around
              #'ai-code--with-auto-test-suffix-for-send))

(defun ai-code--test-after-code-change--set (symbol value)
  "Set SYMBOL to VALUE and update related suffix behavior."
  (set-default symbol value)
  (set symbol value)
  (setq ai-code-auto-test-suffix
        (ai-code--auto-test-suffix-for-type value)))

(defun ai-code--apply-auto-test-type (value)
  "Set `ai-code-auto-test-type` to VALUE and refresh related suffix."
  (setq ai-code-auto-test-type value)
  (ai-code--test-after-code-change--set 'ai-code-auto-test-type value)
  value)

(defun ai-code--apply-discussion-auto-follow-up-enabled (value)
  "Set `ai-code-discussion-auto-follow-up-enabled` to VALUE."
  (setq ai-code-discussion-auto-follow-up-enabled value)
  value)

(defcustom ai-code-auto-test-type nil
  "Select how prompts request tests after code changes."
  :type '(choice (const :tag "Ask every time" ask-me)
                 (const :tag "Off" nil))
  :set #'ai-code--test-after-code-change--set
  :group 'ai-code)

(defcustom ai-code-discussion-auto-follow-up-enabled t
  "When non-nil, prompts may request numbered next-step suggestions.
This is enabled by default; customize it to nil to turn the send-time
choice off globally.  Pair it with `ai-code-use-gptel-classify-prompt`
when you want code-change prompts to skip these discussion follow-up
suggestions."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (set symbol value))
  :group 'ai-code)

(provide 'ai-code-harness)

;;; ai-code-harness.el ends here
