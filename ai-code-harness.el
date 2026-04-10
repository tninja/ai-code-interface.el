;;; ai-code-harness.el --- Harness support for ai-code -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Harness generation and prompt suffix helpers for ai-code.

;;; Code:

(require 'subr-x)

(require 'ai-code-agile)
(require 'ai-code-backends)

(declare-function ai-code--ensure-files-directory "ai-code-prompt-mode" ())
(declare-function ai-code--git-root "ai-code-file" (&optional dir))

(defvar ai-code-mcp-agent-enabled-backends)
(defvar ai-code-selected-backend)

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

(provide 'ai-code-harness)

;;; ai-code-harness.el ends here
