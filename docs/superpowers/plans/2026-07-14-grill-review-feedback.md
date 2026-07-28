# Grill Review Feedback Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Preserve direct slash-command execution and load Grill consistently through public autoloaded entry points.

**Architecture:** Prompt mode owns a shared direct-command predicate used by both prompt dispatch and Grill bypass. Prompt mode also becomes the single loading point for `ai-code-grill`, after the advised insert function is defined, so both full-package and autoloaded entry-module paths install the same advice.

**Tech Stack:** Emacs Lisp with lexical binding, Emacs advice/autoloads, ERT, isolated batch Emacs integration tests, byte compilation, and checkdoc.

---

### Task 1: Preserve Direct Slash Commands

**Files:**
- Modify: `ai-code-prompt-mode.el:537-550`
- Modify: `ai-code-grill.el:14-91`
- Test: `test/test_ai-code-prompt-mode.el`
- Test: `test/test_ai-code-grill.el`

- [ ] **Step 1: Add failing direct-command predicate tests**

Add these tests to `test/test_ai-code-prompt-mode.el`:

```elisp
(ert-deftest ai-code-test-direct-command-p-accepts-single-token-command ()
  "A single-token slash command should use direct command routing."
  (should (ai-code--direct-command-p "/status")))

(ert-deftest ai-code-test-direct-command-p-rejects-whitespace ()
  "Slash-prefixed text containing whitespace should be a normal prompt."
  (dolist (prompt '("/review this file"
                    "/review\tthis-file"
                    "/review\nthis-file"))
    (should-not (ai-code--direct-command-p prompt))))

(ert-deftest ai-code-test-direct-command-p-rejects-normal-prompt ()
  "Text without a slash prefix should be a normal prompt."
  (should-not (ai-code--direct-command-p "explain this file")))
```

- [ ] **Step 2: Add failing Grill routing tests**

Add this dependency before the existing `ai-code-grill` requirement in
`test/test_ai-code-grill.el`, because the shared predicate belongs to prompt
mode:

```elisp
(require 'ai-code-prompt-mode)
```

Then add these tests:

```elisp
(ert-deftest ai-code-grill-direct-command-bypasses-question ()
  "A direct slash command should bypass Grill unchanged."
  (let ((ai-code-grill-me-enabled t)
        (this-command 'ai-code-send-command))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (&rest _args)
                 (ert-fail "Direct commands should not ask about Grill"))))
      (should (equal (ai-code--maybe-add-grill-me-harness "/status")
                     "/status")))))

(ert-deftest ai-code-grill-whitespace-slash-prompt-remains-eligible ()
  "A slash-prefixed prompt with whitespace should remain Grill-eligible."
  (let ((ai-code-grill-me-enabled t)
        (this-command 'ai-code-send-command))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _args) t))
              ((symbol-function 'ai-code--grill-me-reference-suffix)
               (lambda () "Read @prompt/grilling.v1.md")))
      (should
       (equal
        (ai-code--maybe-add-grill-me-harness "/review this file")
        "/review this file\nRead @prompt/grilling.v1.md")))))
```

- [ ] **Step 3: Run the new tests and verify RED**

Run:

```bash
emacs -batch -L . -l test/test_00-bootstrap.el -l ert \
  -l ./ai-code-grill.el \
  -l test/test_ai-code-grill.el -l test/test_ai-code-prompt-mode.el \
  --eval "(ert-run-tests-batch-and-exit
           '(or ai-code-test-direct-command-p-accepts-single-token-command
                ai-code-test-direct-command-p-rejects-whitespace
                ai-code-test-direct-command-p-rejects-normal-prompt
                ai-code-grill-direct-command-bypasses-question
                ai-code-grill-whitespace-slash-prompt-remains-eligible))"
```

Expected: failures because `ai-code--direct-command-p` is undefined and the
existing Grill path reaches `y-or-n-p` for `/status`.

- [ ] **Step 4: Implement the shared predicate and routing**

Add before `ai-code--insert-prompt` in `ai-code-prompt-mode.el`:

```elisp
(defun ai-code--direct-command-p (prompt-text)
  "Return non-nil when PROMPT-TEXT is a single-token slash command."
  (and (string-prefix-p "/" prompt-text)
       (not (string-match-p "[[:space:]]" prompt-text))))
```

Replace the inline command condition in `ai-code--insert-prompt` with:

```elisp
(if (ai-code--direct-command-p processed-prompt)
    (ai-code--execute-command processed-prompt)
```

Declare the helper near the other declarations in `ai-code-grill.el`:

```elisp
(declare-function ai-code--direct-command-p "ai-code-prompt-mode"
                  (prompt-text))
```

Add the direct-command exclusion as the first condition in
`ai-code--maybe-add-grill-me-harness`:

```elisp
(if (and (not (ai-code--direct-command-p prompt-text))
         ai-code-grill-me-enabled
         (ai-code--grill-me-command-p)
         (y-or-n-p "Grill me before acting? "))
```

- [ ] **Step 5: Run focused suites and verify GREEN**

```bash
emacs -batch -L . -l test/test_00-bootstrap.el -l ert \
  -l ./ai-code-prompt-mode.el \
  -l ./ai-code-grill.el -l test/test_ai-code-prompt-mode.el \
  -l test/test_ai-code-grill.el -f ert-run-tests-batch-and-exit
```

Expected: all prompt-mode and Grill tests pass with zero unexpected results.

- [ ] **Step 6: Commit Task 1**

```bash
git add ai-code-prompt-mode.el ai-code-grill.el \
  test/test_ai-code-prompt-mode.el test/test_ai-code-grill.el
git commit -m "fix: preserve direct commands when grill is enabled"
```

### Task 2: Load Grill Through Prompt Mode

**Files:**
- Modify: `ai-code-prompt-mode.el:734`
- Modify: `ai-code.el:136`
- Test: `test/test_ai-code-grill.el`

- [ ] **Step 1: Add a clean-process integration helper and test**

Add this helper and test to `test/test_ai-code-grill.el`:

```elisp
(defun ai-code-grill-test--clean-emacs-result (form)
  "Run FORM in a clean batch Emacs and return its exit code and output."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (root (file-name-as-directory
                (expand-file-name default-directory)))
         (stubs (expand-file-name "test/stubs" root)))
    (with-temp-buffer
      (let ((exit-code
             (call-process emacs nil (current-buffer) nil
                           "-Q" "--batch"
                           "-L" root
                           "-L" stubs
                           "--eval" form)))
        (list exit-code (buffer-string))))))

(ert-deftest ai-code-grill-autoloaded-entry-loads-grill ()
  "Loading an autoloaded entry module should install Grill advice."
  (pcase-let
      ((`(,exit-code ,output)
        (ai-code-grill-test--clean-emacs-result
         (concat
          "(progn "
          "(setq load-prefer-newer t) "
          "(load \"ai-code-autoloads.el\" nil nil t) "
          "(require 'ai-code-change) "
          "(unless (and (featurep 'ai-code-grill) "
          "(advice-member-p #'ai-code--with-optional-grill-me "
          "'ai-code--insert-prompt) "
          "(advice-member-p #'ai-code--with-grill-me-origin "
          "'ai-code-code-change)) "
          "(kill-emacs 1)))"))))
    (should (equal exit-code 0))
    (should-not (string-match-p "Error:" output))))
```

- [ ] **Step 2: Run the integration test and verify RED**

```bash
emacs -batch -L . -l test/test_00-bootstrap.el -l ert \
  -l ./ai-code-grill.el \
  -l test/test_ai-code-grill.el \
  --eval "(ert-run-tests-batch-and-exit 'ai-code-grill-autoloaded-entry-loads-grill)"
```

Expected: FAIL because requiring `ai-code-change` in the clean child process
does not load `ai-code-grill`.

- [ ] **Step 3: Move the Grill requirement**

Remove this line from `ai-code.el`:

```elisp
(require 'ai-code-grill)
```

Add this immediately before `(provide 'ai-code-prompt-mode)` in
`ai-code-prompt-mode.el`:

```elisp
;; Load optional prompt-boundary extensions after the advised function exists.
(require 'ai-code-grill)
```

- [ ] **Step 4: Run the integration and focused tests and verify GREEN**

```bash
emacs -batch -L . -l test/test_00-bootstrap.el -l ert \
  -l ./ai-code-prompt-mode.el \
  -l test/test_ai-code-prompt-mode.el -l test/test_ai-code-grill.el \
  -f ert-run-tests-batch-and-exit
```

Expected: the clean-process autoload test and all focused tests pass.

- [ ] **Step 5: Commit Task 2**

```bash
git add ai-code-prompt-mode.el ai-code.el test/test_ai-code-grill.el
git commit -m "fix: load grill for autoloaded prompt commands"
```

### Task 3: Restore Test Metadata and Verify

**Files:**
- Modify: `test/test_ai-code-grill.el:1-7`
- Verify: `ai-code-prompt-mode.el`
- Verify: `ai-code-grill.el`
- Verify: `ai-code.el`

- [ ] **Step 1: Add standard test-file sections**

Insert after the file header in `test/test_ai-code-grill.el`:

```elisp

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for the optional Grill prompt flow.

;;; Code:
```

- [ ] **Step 2: Run focused tests**

```bash
emacs -batch -L . -l test/test_00-bootstrap.el -l ert \
  -l ./ai-code-prompt-mode.el \
  -l test/test_ai-code-prompt-mode.el -l test/test_ai-code-grill.el \
  -f ert-run-tests-batch-and-exit
```

Expected: zero unexpected results.

- [ ] **Step 3: Run the complete ERT suite**

```bash
emacs -batch -L . -l ert \
  --eval "(mapc #'load-file (file-expand-wildcards \"test/test_*.el\"))" \
  -f ert-run-tests-batch-and-exit
```

Expected: no failures beyond any explicitly documented pre-existing baseline
failures; no new Grill or prompt-mode failures.

- [ ] **Step 4: Byte-compile touched production files**

```bash
emacs -Q --batch -L . -L test/stubs \
  --eval "(progn
             (setq byte-compile-dest-file-function
                   (lambda (file)
                     (expand-file-name
                      (concat (file-name-base file) \".elc\")
                      temporary-file-directory)))
             (byte-compile-file \"ai-code-prompt-mode.el\")
             (byte-compile-file \"ai-code-grill.el\")
             (byte-compile-file \"ai-code.el\"))"
```

Expected: exit 0 with no new warnings.  If `emacs -Q` cannot load required
dependencies, repeat individual compilation through the active Emacs session
and report the batch failure rather than treating it as a pass.

- [ ] **Step 5: Run checkdoc**

```bash
emacs -Q --batch -L . -L test/stubs -l checkdoc \
  --eval "(progn
             (checkdoc-file \"ai-code-prompt-mode.el\")
             (checkdoc-file \"ai-code-grill.el\")
             (checkdoc-file \"ai-code.el\")
             (checkdoc-file \"test/test_ai-code-prompt-mode.el\")
             (checkdoc-file \"test/test_ai-code-grill.el\"))"
```

Expected: no new diagnostics in touched files.

- [ ] **Step 6: Inspect scope and commit metadata cleanup**

```bash
git diff --check
git status --short
git diff --stat HEAD~2
```

Expected: only intended source/tests plus pre-existing unrelated untracked files.

```bash
git add test/test_ai-code-grill.el
git commit -m "test: add grill test file metadata"
```
