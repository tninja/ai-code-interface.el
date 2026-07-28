# Prompt Suffix Providers Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace hard-coded prompt suffix assembly and per-feature injection advice with one send-boundary transformation backed by an ordered provider hook.

**Architecture:** `ai-code-prompt-mode.el` owns a generic prompt context, memoization helper, ordered abnormal hook, provider collector, and one `:filter-args` advice on `ai-code--write-prompt-to-file-and-send`. `ai-code-grill.el` and `ai-code-harness.el` register feature-specific providers while retaining their existing user settings and, for Grill, entry-command origin tracking.

**Tech Stack:** Emacs Lisp with lexical binding, Emacs advice and abnormal hooks, `cl-defstruct`, ERT, batch byte compilation, checkdoc, and diagnostics baseline comparisons.

---

### Task 1: Add the Generic Prompt Suffix Pipeline

**Files:**
- Modify: `test/test_ai-code-prompt-mode.el`
- Modify: `ai-code-prompt-mode.el`

- [ ] **Step 1: Write failing core provider tests**

Add focused ERT tests that define temporary providers and assert the wished-for API:

```elisp
(ert-deftest ai-code-test-apply-prompt-suffixes-preserves-provider-order ()
  "Prompt suffix providers should run in hook order."
  (let ((ai-code-prompt-suffix-functions
         (list (lambda (_context) "FIRST")
               (lambda (_context) nil)
               (lambda (_context) "SECOND"))))
    (should (equal (ai-code--apply-prompt-suffixes "Prompt")
                   "Prompt\nFIRST\nSECOND"))))

(ert-deftest ai-code-test-prompt-context-memoize-caches-nil-values ()
  "Prompt context memoization should evaluate each key once."
  (let ((context (ai-code--make-prompt-context :prompt-text "Prompt"))
        (calls 0))
    (dotimes (_ 2)
      (should-not
       (ai-code-prompt-context-memoize
        context 'classification
        (lambda ()
          (cl-incf calls)
          nil))))
    (should (= calls 1))))

(ert-deftest ai-code-test-prompt-suffix-provider-error-aborts-send ()
  "A failing suffix provider should stop the original send function."
  (let ((ai-code-prompt-suffix-functions
         (list (lambda (_context) (error "broken provider"))))
        called)
    (cl-letf (((symbol-function 'ai-code--write-prompt-to-file-and-send)
               (lambda (_prompt) (setq called t))))
      (should-error (ai-code--filter-prompt-suffix-args '("Prompt"))
                    :type 'user-error)
      (should-not called))))
```

Call `get_diagnostics` with `since="baseline"` for the touched test file.

- [ ] **Step 2: Run the new tests and verify RED**

Run:

```bash
emacs -batch -L . -l test/test_00-bootstrap.el -l ert \
  -l ./ai-code-prompt-mode.el -l test/test_ai-code-prompt-mode.el \
  --eval "(ert-run-tests-batch-and-exit \
           '(or ai-code-test-apply-prompt-suffixes-preserves-provider-order \
                ai-code-test-prompt-context-memoize-caches-nil-values \
                ai-code-test-prompt-suffix-provider-error-aborts-send))"
```

Expected: failures because the context, memoization helper, collector, and filter advice function do not exist.

- [ ] **Step 3: Implement the minimal generic pipeline**

In `ai-code-prompt-mode.el`, define:

```elisp
(cl-defstruct (ai-code-prompt-context
               (:constructor ai-code--make-prompt-context))
  "Context shared by prompt suffix providers."
  prompt-text
  origin-command
  backend
  (cache (make-hash-table :test #'eq)))

(defcustom ai-code-prompt-suffix-functions nil
  "Ordered abnormal hook that returns suffixes for a prompt context.
Each function receives one `ai-code-prompt-context` and returns either a
non-empty string or nil.  Provider errors abort the send."
  :type 'hook
  :group 'ai-code)

(defvar ai-code--prompt-origin-command nil
  "Originating interactive command for the current prompt request.")

(defun ai-code-prompt-context-memoize (context key producer)
  "Return the cached value for KEY in CONTEXT, calling PRODUCER once."
  (let* ((cache (ai-code-prompt-context-cache context))
         (missing (make-symbol "missing"))
         (value (gethash key cache missing)))
    (if (eq value missing)
        (let ((produced (funcall producer)))
          (puthash key produced cache)
          produced)
      value)))

(defun ai-code--prompt-suffix-from-provider (provider context)
  "Return the validated suffix from PROVIDER for CONTEXT."
  (condition-case err
      (let ((suffix (funcall provider context)))
        (cond
         ((null suffix) nil)
         ((not (stringp suffix))
          (error "returned %S instead of a string or nil" suffix))
         ((string-empty-p suffix) nil)
         (t suffix)))
    (error
     (user-error "Prompt suffix provider %S failed: %s"
                 provider (error-message-string err)))))

(defun ai-code--collect-prompt-suffixes (context)
  "Return non-empty suffix strings produced for CONTEXT in hook order."
  (let (suffixes)
    (run-hook-wrapped
     'ai-code-prompt-suffix-functions
     (lambda (provider prompt-context)
       (when-let ((suffix (ai-code--prompt-suffix-from-provider
                           provider prompt-context)))
         (push suffix suffixes))
       nil)
     context)
    (nreverse suffixes)))

(defun ai-code--apply-prompt-suffixes (prompt-text)
  "Return PROMPT-TEXT with all enabled prompt suffixes appended."
  (let* ((context (ai-code--make-prompt-context
                   :prompt-text prompt-text
                   :origin-command (or ai-code--prompt-origin-command
                                       this-command)
                   :backend (and (boundp 'ai-code-selected-backend)
                                 ai-code-selected-backend)))
         (suffixes (ai-code--collect-prompt-suffixes context)))
    (if suffixes
        (concat prompt-text "\n" (mapconcat #'identity suffixes "\n"))
      prompt-text)))

(defun ai-code--filter-prompt-suffix-args (args)
  "Return ARGS with prompt suffix providers applied to its prompt text."
  (list (ai-code--apply-prompt-suffixes (car args))))
```

Register a custom suffix provider at hook depth 10. It returns
`ai-code-prompt-suffix` only when `ai-code-use-prompt-suffix` is non-nil and the
suffix is a non-empty string.

Simplify `ai-code--write-prompt-to-file-and-send` so it stores `prompt-text`
exactly and sends `(concat prompt-text "\n")`. Install
`ai-code--filter-prompt-suffix-args` once as `:filter-args` advice after the
write/send function is defined.

Call `get_diagnostics` with `since="baseline"` for the touched source file.

- [ ] **Step 4: Run focused tests and verify GREEN**

Run the command from Step 2. Expected: all three selected tests pass with zero unexpected results.

- [ ] **Step 5: Perform the Blue refactor**

Review the source and test diff. Extract provider invocation and validation into
one small helper so `ai-code--collect-prompt-suffixes` only owns hook traversal
and ordering. Keep the public hook contract at string-or-nil and keep error
messages provider-specific.

Call `get_diagnostics` with `since="baseline"`, then rerun the selected tests.
Expected: all selected tests remain green.

- [ ] **Step 6: Commit Task 1**

```bash
git add ai-code-prompt-mode.el test/test_ai-code-prompt-mode.el
git commit -m "refactor: add prompt suffix provider pipeline"
```

### Task 2: Migrate Grill to a Prompt Suffix Provider

**Files:**
- Modify: `test/test_ai-code-grill.el`
- Modify: `ai-code-grill.el`

- [ ] **Step 1: Replace insert-advice tests with failing provider tests**

Update the clean-process integration test to require that
`ai-code--grill-me-suffix-provider` is in `ai-code-prompt-suffix-functions`,
that `ai-code--filter-prompt-suffix-args` advises
`ai-code--write-prompt-to-file-and-send`, and that origin advice remains on the
entry command.

Replace direct tests of `ai-code--maybe-add-grill-me-harness` with provider
tests using `ai-code--make-prompt-context`. Cover disabled, unrelated command,
declined, accepted, and preserved-origin behavior. Keep the direct-command
integration test, but call `ai-code--insert-prompt "/status"` and assert that
Grill never asks because the write/send boundary is not reached.

Call `get_diagnostics` with `since="baseline"` for the touched test file.

- [ ] **Step 2: Run Grill tests and verify RED**

```bash
emacs -batch -L . -l test/test_00-bootstrap.el -l ert \
  -l ./ai-code-prompt-mode.el -l ./ai-code-grill.el \
  -l test/test_ai-code-grill.el -f ert-run-tests-batch-and-exit
```

Expected: provider-oriented tests fail because Grill still injects around
`ai-code--insert-prompt` and has no provider function.

- [ ] **Step 3: Implement the Grill provider**

In `ai-code-grill.el`:

```elisp
(defun ai-code--grill-me-suffix-provider (context)
  "Return the optional Grill suffix for prompt CONTEXT."
  (when (and ai-code-grill-me-enabled
             (memq (ai-code-prompt-context-origin-command context)
                   ai-code--grill-me-commands)
             (y-or-n-p "Grill me before acting? "))
    (ai-code--grill-me-reference-suffix)))
```

Register it at hook depth 20. Remove the advice that transformed
`ai-code--insert-prompt`, along with its private prompt transformation helpers.
Retain entry-command advice, but bind `ai-code--prompt-origin-command` so the
generic context captures the original command across minibuffer editing.

Call `get_diagnostics` with `since="baseline"` for the touched source file.

- [ ] **Step 4: Run Grill tests and verify GREEN**

Run the command from Step 2. Expected: all Grill tests pass with zero unexpected results.

- [ ] **Step 5: Perform the Blue refactor**

Review the diff and simplify origin lookup so the context is the only input to
the Grill provider. Improve docstrings to distinguish origin advice from suffix
injection. Do not reintroduce direct-command checks inside Grill.

Call `get_diagnostics` with `since="baseline"`, then rerun the Grill suite.
Expected: the suite remains green.

- [ ] **Step 6: Commit Task 2**

```bash
git add ai-code-grill.el test/test_ai-code-grill.el
git commit -m "refactor: register grill as a suffix provider"
```

### Task 3: Migrate Harness Routing to Providers

**Files:**
- Modify: `test/test_ai-code-harness.el`
- Modify: `ai-code-harness.el`

- [ ] **Step 1: Write failing harness provider tests**

Add focused tests for:

```elisp
(ert-deftest ai-code-test-built-in-prompt-suffix-provider-order ()
  "Built-in providers should append custom, Grill, test, then follow-up."
  (let ((ai-code-use-prompt-suffix t)
        (ai-code-prompt-suffix "CUSTOM")
        (ai-code-grill-me-enabled t)
        (ai-code--prompt-origin-command 'ai-code-code-change)
        (ai-code-auto-test-type 'test-after-change)
        (ai-code-discussion-auto-follow-up-enabled 'always))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _args) t))
              ((symbol-function 'ai-code--grill-me-reference-suffix)
               (lambda () "GRILL"))
              ((symbol-function 'ai-code--resolve-auto-test-suffix-for-send)
               (lambda (&rest _args) "AUTO"))
              ((symbol-function 'ai-code--resolve-auto-follow-up-suffix-for-send)
               (lambda (&rest _args) "FOLLOW")))
      (should (equal (ai-code--apply-prompt-suffixes "Prompt")
                     "Prompt\nCUSTOM\nGRILL\nAUTO\nFOLLOW")))))

(ert-deftest ai-code-test-use-prompt-suffix-does-not-disable-grill ()
  "The legacy suffix switch should not become a Grill master switch."
  (let ((ai-code-use-prompt-suffix nil)
        (ai-code-prompt-suffix "CUSTOM")
        (ai-code-grill-me-enabled t)
        (ai-code--prompt-origin-command 'ai-code-code-change)
        (ai-code-auto-test-type 'test-after-change)
        (ai-code-discussion-auto-follow-up-enabled 'always))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _args) t))
              ((symbol-function 'ai-code--grill-me-reference-suffix)
               (lambda () "GRILL"))
              ((symbol-function 'ai-code--resolve-auto-test-suffix-for-send)
               (lambda (&rest _args)
                 (ert-fail "Auto-test provider should be disabled")))
              ((symbol-function 'ai-code--resolve-auto-follow-up-suffix-for-send)
               (lambda (&rest _args)
                 (ert-fail "Follow-up provider should be disabled"))))
      (should (equal (ai-code--apply-prompt-suffixes "Prompt")
                     "Prompt\nGRILL")))))

(ert-deftest ai-code-test-harness-providers-share-prompt-classification ()
  "Auto-test and follow-up providers should classify a prompt once."
  (let ((ai-code-use-prompt-suffix t)
        (ai-code-prompt-suffix nil)
        (ai-code-grill-me-enabled nil)
        (ai-code-use-gptel-classify-prompt t)
        (ai-code-auto-test-type 'ask-me)
        (ai-code-discussion-auto-follow-up-enabled 'always)
        (classify-count 0)
        classifications)
    (cl-letf (((symbol-function 'ai-code--classify-prompt-code-change)
               (lambda (_prompt)
                 (cl-incf classify-count)
                 'non-code-change))
              ((symbol-function 'ai-code--resolve-auto-test-suffix-for-send)
               (lambda (_prompt classification)
                 (push classification classifications)
                 "AUTO"))
              ((symbol-function 'ai-code--resolve-auto-follow-up-suffix-for-send)
               (lambda (_prompt classification)
                 (push classification classifications)
                 "FOLLOW")))
      (should (equal (ai-code--apply-prompt-suffixes "Prompt")
                     "Prompt\nAUTO\nFOLLOW"))
      (should (= classify-count 1))
      (should (equal classifications
                     '(non-code-change non-code-change))))))
```

Use `cl-letf` only at external seams: harness reference resolution, user choice,
and prompt classification. Assert the final transformed prompt and the
classifier call count rather than internal variable bindings.

Call `get_diagnostics` with `since="baseline"` for the touched test file.

- [ ] **Step 2: Run the new tests and verify RED**

```bash
emacs -batch -L . -l test/test_00-bootstrap.el -l ert \
  -l ./ai-code-prompt-mode.el -l ./ai-code-harness.el \
  -l test/test_ai-code-harness.el \
  --eval "(ert-run-tests-batch-and-exit \
           '(or ai-code-test-built-in-prompt-suffix-provider-order \
                ai-code-test-use-prompt-suffix-does-not-disable-grill \
                ai-code-test-harness-providers-share-prompt-classification))"
```

Expected: failures because Auto-test and follow-up are still dynamically bound
by the old around advice rather than registered providers.

- [ ] **Step 3: Implement Auto-test and follow-up providers**

Add a memoized classification accessor backed by
`ai-code-prompt-context-memoize`. Define separate provider functions that check
their own feature settings plus the legacy `ai-code-use-prompt-suffix` switch,
then call the existing resolver functions with the context prompt and shared
classification. Register Auto-test at depth 30 and Discussion follow-up at depth
40.

Remove `ai-code--with-auto-test-suffix-for-send` and its around advice. Keep the
legacy suffix variables and user-facing custom variables for compatibility, but
remove all suffix-specific knowledge from the write/send function.

Call `get_diagnostics` with `since="baseline"` for the touched source file.

- [ ] **Step 4: Run harness tests and verify GREEN**

```bash
emacs -batch -L . -l test/test_00-bootstrap.el -l ert \
  -l ./ai-code-harness.el -l test/test_ai-code-harness.el \
  -f ert-run-tests-batch-and-exit
```

Expected: all harness tests pass with zero unexpected results.

- [ ] **Step 5: Perform the Blue refactor**

Review the complete provider diff. Consolidate duplicated enablement and context
lookup without merging the two providers, retain documented hook depths, and
remove declarations in prompt mode that are no longer used there.

Call `get_diagnostics` with `since="baseline"`, then run the focused prompt-mode,
Grill, and harness suites. Expected: all focused tests remain green.

- [ ] **Step 6: Commit Task 3**

```bash
git add ai-code-harness.el test/test_ai-code-harness.el ai-code-prompt-mode.el
git commit -m "refactor: route harness suffixes through providers"
```

### Task 4: Verify the Integrated Refactor

**Files:**
- Verify: `ai-code-prompt-mode.el`
- Verify: `ai-code-grill.el`
- Verify: `ai-code-harness.el`
- Verify: `test/test_ai-code-prompt-mode.el`
- Verify: `test/test_ai-code-grill.el`
- Verify: `test/test_ai-code-harness.el`

- [ ] **Step 1: Run focused suites**

```bash
emacs -batch -L . -l test/test_00-bootstrap.el -l ert \
  -l ./ai-code-prompt-mode.el -l ./ai-code-grill.el -l ./ai-code-harness.el \
  -l test/test_ai-code-prompt-mode.el -l test/test_ai-code-grill.el \
  -l test/test_ai-code-harness.el -f ert-run-tests-batch-and-exit
```

Expected: zero unexpected results.

- [ ] **Step 2: Run the complete ERT suite**

```bash
emacs -batch -L . -l ert \
  --eval "(mapc #'load-file (file-expand-wildcards \"test/test_*.el\"))" \
  -f ert-run-tests-batch-and-exit
```

Expected: zero unexpected results; existing optional-dependency skips may remain.

- [ ] **Step 3: Byte-compile touched production files**

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
             (byte-compile-file \"ai-code-harness.el\"))"
```

Expected: exit 0 with no new byte-compilation warnings.

- [ ] **Step 4: Run checkdoc**

```bash
emacs -Q --batch -L . -L test/stubs -l checkdoc \
  --eval "(progn
             (checkdoc-file \"ai-code-prompt-mode.el\")
             (checkdoc-file \"ai-code-grill.el\")
             (checkdoc-file \"ai-code-harness.el\")
             (checkdoc-file \"test/test_ai-code-prompt-mode.el\")
             (checkdoc-file \"test/test_ai-code-grill.el\")
             (checkdoc-file \"test/test_ai-code-harness.el\"))"
```

Expected: no new checkdoc diagnostics in touched files.

- [ ] **Step 5: Run final diagnostics and diff checks**

Call `get_diagnostics` with `since="baseline"` for every touched source and test
file; status must be `clean`.

```bash
git diff --check
git status --short
```

Expected: no whitespace errors and only intentional plan, source, and test changes.

- [ ] **Step 6: Request code review and address findings**

Dispatch a focused reviewer with the approved provider architecture, base SHA,
and branch HEAD. Fix all Critical and Important findings through new Red-Green-
Blue cycles, then repeat the relevant verification commands.
