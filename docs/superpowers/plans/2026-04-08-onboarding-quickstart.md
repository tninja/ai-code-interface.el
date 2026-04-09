# Onboarding Quickstart Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a one-time quickstart buffer that appears the first time `ai-code-menu` is opened, remains manually accessible from the menu, and shows a short backend-aware next-step hint after explicit backend switches.

**Architecture:** Keep onboarding isolated in a new `ai-code-onboarding.el` module that owns state, rendering, buttons, and backend hint formatting. Integrate it at two narrow edges only: `ai-code-menu` for first-run/manual entry and `ai-code-select-backend` for explicit backend-switch hints. Reuse existing commands and current session-detection primitives instead of creating a second workflow system.

**Tech Stack:** Emacs Lisp, `transient`, `special-mode`, text buttons, ERT, `loaddefs-generate`, batch byte compilation, `checkdoc`

---

## Execution Notes

- Use `@subagent-driven-development` or `@executing-plans` to execute this plan.
- Use `@test-driven-development` for each behavior change.
- Use `@verification-before-completion` before claiming the feature is done.
- Do not expand the scope into general help, setup diagnostics, MCP onboarding, or behavior-system education.
- Do not couple onboarding to `ai-code-behaviors.el`. Prefer existing backend metadata such as `ai-code-current-backend-label`, `ai-code-selected-backend`, `ai-code-cli`, `ai-code--backend-spec`, and `ai-code-backends-infra--find-session-buffers`.

## File Structure

### Files to Create

- `ai-code-onboarding.el`
  - Own all onboarding state, rendering, best-effort session-status summary, help buffer commands, button callbacks, and backend hint formatting
- `test/test_ai-code-onboarding.el`
  - Cover onboarding state gates, quickstart buffer rendering, button callbacks, and `ai-code-menu` first-run integration

### Files to Modify

- `ai-code.el:473-540`
  - Require the onboarding module, add a manual `Help / Quick Start` entry to the transient menu, and invoke the one-time quickstart gate from `ai-code-menu`
- `ai-code-backends.el:481-510`
  - Show a backend-specific next-step hint only after explicit interactive backend selection
- `README.org:50-80`
  - Mention the first-run quickstart and the manual reopen path
- `ai-code-autoloads.el`
  - Regenerate so the new onboarding command and custom variables are available through autoloads

### Files to Verify but Not Intentionally Modify

- `test/test_ai-code.el`
  - Existing menu tests should continue to pass after menu integration
- `test/test_ai-code-backends.el`
  - Extend with backend-hint coverage; keep existing backend selection behavior intact
- `test/test_ai-code-package-hygiene.el`
  - Must continue passing after autoload regeneration

## Chunk 1: Foundation and Isolated Onboarding Module

### Task 1: Prepare a clean execution lane and capture the baseline

**Files:**
- Modify: none
- Verify: `test/test_ai-code.el`, `test/test_ai-code-backends.el`, `test/test_ai-code-prompt-mode.el`

- [ ] **Step 1: Create a dedicated worktree and branch**

```bash
git worktree add ../ai-code-onboarding -b feat/onboarding-quickstart HEAD
cd ../ai-code-onboarding
```

- [ ] **Step 2: Confirm the worktree starts clean enough for focused implementation**

Run:

```bash
git status --short
```

Expected:

- only worktree-local or intentionally carried changes
- no accidental edits to unrelated tracked files before implementation starts

- [ ] **Step 3: Run the baseline menu and backend tests before changing anything**

Run:

```bash
emacs -Q --batch -L . -l ert -l test/test_ai-code.el -l test/test_ai-code-backends.el -f ert-run-tests-batch-and-exit
```

Expected:

- PASS for the current baseline

- [ ] **Step 4: Note the exact menu and backend integration points before editing**

Inspect:

- `ai-code.el:473-540`
- `ai-code-backends.el:481-510`
- `ai-code-backends-infra.el:553-760`

- [ ] **Step 5: Commit only if baseline or worktree prep required an intentional repo change**

If no repo files changed, skip this step.

If a change was required:

```bash
git add <files>
git commit --file=- <<'EOF'
Prepare a clean lane for onboarding quickstart work

This commit only captures setup work needed to keep the
onboarding implementation isolated and reviewable.

Constraint: Preserve unrelated in-flight repository changes
Confidence: high
Scope-risk: narrow
Reversibility: clean
Directive: Do not mix setup-only edits with onboarding behavior changes
Tested: Baseline targeted test run
Not-tested: Feature behavior
EOF
```

### Task 2: Add the standalone onboarding module behind failing tests

**Files:**
- Create: `ai-code-onboarding.el`
- Create: `test/test_ai-code-onboarding.el`
- Test: `test/test_ai-code-onboarding.el`

- [ ] **Step 1: Write failing tests for the standalone onboarding behaviors**

Add tests for:

- `ai-code-onboarding-open-quickstart` creates a dedicated quickstart buffer
- the quickstart buffer contains the core sections from the spec
- the quickstart header includes the current backend label and a session status line
- `ai-code-onboarding-disable-auto-show` flips the customization state cleanly
- backend hint formatting works for a representative backend and for `<none>`

Suggested starting test skeleton:

```elisp
(ert-deftest ai-code-test-onboarding-open-quickstart-creates-buffer ()
  (let ((ai-code-onboarding-auto-show t)
        (ai-code-onboarding-seen nil))
    (ai-code-onboarding-open-quickstart)
    (with-current-buffer "*AI Code Quick Start*"
      (should (derived-mode-p 'ai-code-onboarding-mode))
      (should (string-match-p "Start Here" (buffer-string)))
      (should (string-match-p "Most Useful Actions" (buffer-string))))))
```

- [ ] **Step 2: Run the new onboarding test file and confirm it fails for the right reason**

Run:

```bash
emacs -Q --batch -L . -l ert -l test/test_ai-code-onboarding.el -f ert-run-tests-batch-and-exit
```

Expected:

- FAIL with missing onboarding functions or file load errors

- [ ] **Step 3: Implement the minimal onboarding module**

Add `ai-code-onboarding.el` with:

- package header, Commentary, SPDX, `provide`
- `defgroup` for onboarding settings
- `defcustom ai-code-onboarding-auto-show`
- `defcustom ai-code-onboarding-seen`
- `define-derived-mode ai-code-onboarding-mode` from `special-mode`
- `ai-code-onboarding-open-quickstart`
- `ai-code-onboarding-maybe-show-quickstart`
- `ai-code-onboarding-disable-auto-show`
- `ai-code-onboarding-backend-hint`
- private helper(s) for best-effort session status using backend `:cli` metadata
  plus `ai-code-backends-infra--session-working-directory` and
  `ai-code-backends-infra--find-session-buffers`
- private rendering helpers for header, section bodies, and text buttons

Target shape:

```elisp
(define-derived-mode ai-code-onboarding-mode special-mode "AI Code Quick Start"
  "Major mode for the AI Code onboarding quickstart buffer.")

(defun ai-code-onboarding-open-quickstart ()
  "Open the onboarding quickstart buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*AI Code Quick Start*")))
    (with-current-buffer buffer
      (ai-code-onboarding-mode)
      (setq-local inhibit-read-only t)
      (erase-buffer)
      (ai-code-onboarding--render)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))
```

- [ ] **Step 4: Re-run the onboarding tests and make them pass**

Run:

```bash
emacs -Q --batch -L . -l ert -l test/test_ai-code-onboarding.el -f ert-run-tests-batch-and-exit
```

Expected:

- PASS for the new onboarding-only tests

- [ ] **Step 5: Commit the isolated onboarding module**

```bash
git add ai-code-onboarding.el test/test_ai-code-onboarding.el
git commit --file=- <<'EOF'
Add a standalone onboarding quickstart module

This creates the new onboarding surface in isolation so the UI,
state, and button behavior are testable before menu integration.

Constraint: Keep onboarding independent from prompt and behavior pipelines
Rejected: Embedding onboarding logic directly in ai-code.el | would blur UI and state ownership
Confidence: high
Scope-risk: narrow
Reversibility: clean
Directive: Keep all onboarding rendering and state in ai-code-onboarding.el
Tested: emacs -Q --batch -L . -l ert -l test/test_ai-code-onboarding.el -f ert-run-tests-batch-and-exit
Not-tested: Menu integration and backend hint wiring
EOF
```

## Chunk 2: Menu Integration and First-Run Gate

### Task 3: Wire the quickstart into the menu without changing core menu behavior

**Files:**
- Modify: `ai-code.el:473-540`
- Modify: `ai-code-onboarding.el`
- Modify: `test/test_ai-code-onboarding.el`
- Test: `test/test_ai-code.el`
- Test: `test/test_ai-code-onboarding.el`

- [ ] **Step 1: Add failing integration tests for first-run menu behavior**

Cover:

- `ai-code-menu` still calls the transient prefix command
- when `ai-code-onboarding-auto-show` is non-nil and `ai-code-onboarding-seen` is nil, `ai-code-menu` also opens quickstart once
- when onboarding has already been seen, `ai-code-menu` does not reopen it automatically
- the manual quickstart command remains callable even after `ai-code-onboarding-seen` becomes non-nil

Suggested pattern:

```elisp
(ert-deftest ai-code-test-menu-auto-opens-quickstart-on-first-run ()
  (let ((quickstart-called nil)
        (menu-called nil)
        (ai-code-onboarding-auto-show t)
        (ai-code-onboarding-seen nil))
    (cl-letf (((symbol-function 'ai-code-onboarding-maybe-show-quickstart)
               (lambda () (setq quickstart-called t)))
              ((symbol-function 'call-interactively)
               (lambda (_fn) (setq menu-called t))))
      (ai-code-menu)
      (should quickstart-called)
      (should menu-called))))
```

- [ ] **Step 2: Run the menu + onboarding tests and verify they fail before wiring**

Run:

```bash
emacs -Q --batch -L . -l ert -l test/test_ai-code.el -l test/test_ai-code-onboarding.el -f ert-run-tests-batch-and-exit
```

Expected:

- FAIL because `ai-code-menu` does not yet consult the onboarding gate

- [ ] **Step 3: Integrate onboarding into `ai-code.el`**

Make these minimal changes:

- `(require 'ai-code-onboarding)`
- add a manual transient entry with an unused key such as `h`
- call `ai-code-onboarding-maybe-show-quickstart` from `ai-code-menu` before opening the transient

Recommended command shape:

```elisp
(transient-define-group ai-code--menu-other-tools
  ...
  ("h" "Help / Quick Start" ai-code-onboarding-open-quickstart)
  ...)

(defun ai-code-menu ()
  "Show the AI Code transient menu selected by `ai-code-menu-layout`."
  (interactive)
  (ai-code-onboarding-maybe-show-quickstart)
  (call-interactively (ai-code--menu-prefix-command)))
```

- [ ] **Step 4: Re-run the targeted tests and verify the menu still behaves correctly**

Run:

```bash
emacs -Q --batch -L . -l ert -l test/test_ai-code.el -l test/test_ai-code-onboarding.el -f ert-run-tests-batch-and-exit
```

Expected:

- PASS for existing menu tests
- PASS for the new first-run onboarding tests

- [ ] **Step 5: Commit the menu integration**

```bash
git add ai-code.el ai-code-onboarding.el test/test_ai-code-onboarding.el
git commit --file=- <<'EOF'
Expose the quickstart from the main AI Code menu

This attaches onboarding at the menu boundary so first-run users
see the quickstart once and can reopen it later without changing
the existing command workflows.

Constraint: Preserve ai-code-menu as the stable entry point
Rejected: Replacing the transient with a wizard on first run | would interrupt established Emacs workflows
Confidence: high
Scope-risk: narrow
Reversibility: clean
Directive: Keep onboarding as a sidecar to ai-code-menu, not a replacement for it
Tested: emacs -Q --batch -L . -l ert -l test/test_ai-code.el -l test/test_ai-code-onboarding.el -f ert-run-tests-batch-and-exit
Not-tested: Backend switch hint and autoload regeneration
EOF
```

## Chunk 3: Backend Hint, Documentation, Generated Files, and Verification

### Task 4: Add explicit backend-switch guidance and finish verification

**Files:**
- Modify: `ai-code-backends.el:481-510`
- Modify: `ai-code-onboarding.el`
- Modify: `test/test_ai-code-backends.el`
- Modify: `README.org:50-80`
- Modify: `ai-code-autoloads.el`
- Test: `test/test_ai-code-backends.el`
- Test: `test/test_ai-code-package-hygiene.el`

- [ ] **Step 1: Add failing tests for explicit backend-switch hints**

Cover:

- `ai-code-select-backend` emits a short onboarding hint after explicit interactive selection
- the hint uses the current backend label and next actions
- the hint includes `G to open <agent file>` only when the backend declares `:agent-file`
- noninteractive backend changes through lower-level helpers do not need to emit onboarding hints

Suggested test shape:

```elisp
(ert-deftest ai-code-test-select-backend-shows-onboarding-hint ()
  (let ((shown nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "OpenAI Codex CLI"))
              ((symbol-function 'ai-code-onboarding-show-backend-switch-hint)
               (lambda () (setq shown t)))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (ai-code-select-backend)
      (should shown))))
```

- [ ] **Step 2: Run the backend tests and confirm the new expectation fails**

Run:

```bash
emacs -Q --batch -L . -l ert -l test/test_ai-code-backends.el -f ert-run-tests-batch-and-exit
```

Expected:

- FAIL because `ai-code-select-backend` does not yet invoke the onboarding hint

- [ ] **Step 3: Implement backend hint wiring and keep it explicit-selection-only**

Update `ai-code-backends.el` so the hint is called from `ai-code-select-backend` after `ai-code-set-backend key`, not from `ai-code--apply-backend`.

Add or finalize helper(s) in `ai-code-onboarding.el`:

- `ai-code-onboarding-backend-hint`
- `ai-code-onboarding-show-backend-switch-hint`

Suggested helper shape:

```elisp
(defun ai-code-onboarding-backend-hint ()
  "Return the onboarding next-step hint for the current backend."
  (let* ((spec (ai-code--backend-spec ai-code-selected-backend))
         (plist (cdr spec))
         (agent-file (plist-get plist :agent-file)))
    (concat
     (format "Backend switched to %s. Next: a to start, g to edit config"
             (ai-code-current-backend-label))
     (if agent-file
         (format ", G to open %s." agent-file)
       "."))))

(defun ai-code-onboarding-show-backend-switch-hint ()
  "Echo a backend-specific onboarding hint after explicit backend changes."
  (message "%s" (ai-code-onboarding-backend-hint)))
```

- [ ] **Step 4: Update lightweight user-facing documentation**

Update `README.org` in the quickstart area to mention:

- the one-time quickstart buffer
- the manual reopen path from the main menu

Keep this to one short paragraph or a small bullet, not a full new section.

- [ ] **Step 5: Regenerate autoloads and verify the generated file still satisfies package hygiene**

Run:

```bash
emacs -Q --batch -L . --eval "(let ((generated-autoload-file (expand-file-name \"ai-code-autoloads.el\" default-directory))) (loaddefs-generate default-directory generated-autoload-file))"
```

Expected:

- `ai-code-autoloads.el` gains autoloads/custom-autoloads for the onboarding command and settings

Then run:

```bash
emacs -Q --batch -L . -l ert -l test/test_ai-code-backends.el -l test/test_ai-code-onboarding.el -l test/test_ai-code-package-hygiene.el -f ert-run-tests-batch-and-exit
```

Expected:

- PASS for backend tests
- PASS for onboarding tests
- PASS for package hygiene tests

- [ ] **Step 6: Run checkdoc on touched Lisp files**

Run:

```bash
emacs -Q --batch -L . --eval "(progn (require 'checkdoc) (checkdoc-file \"ai-code-onboarding.el\") (checkdoc-file \"ai-code.el\") (checkdoc-file \"ai-code-backends.el\"))"
```

Expected:

- no new checkdoc warnings for touched Lisp files

- [ ] **Step 7: Byte-compile the touched Lisp files**

Run:

```bash
emacs -batch -L . -f batch-byte-compile ai-code-onboarding.el ai-code.el ai-code-backends.el
```

Expected:

- successful compilation
- no new warnings introduced by onboarding changes

- [ ] **Step 8: Run the repository test suite**

Run:

```bash
emacs -batch -L . -l ert --eval "(mapc #'load-file (file-expand-wildcards \"test/test_*.el\"))" -f ert-run-tests-batch-and-exit
```

Expected:

- full PASS

- [ ] **Step 9: Perform a short manual verification pass in Emacs**

Verify:

1. Fresh onboarding state opens quickstart once when `M-x ai-code-menu` runs
2. Closing quickstart leaves the menu usable
3. `Help / Quick Start` reopens the buffer after first-run state is set
4. `Do Not Show Again` prevents future auto-open
5. `Start Session` and `Switch Backend` buttons invoke existing commands
6. Explicit backend switching shows a short next-step hint

- [ ] **Step 10: Commit the completed feature**

```bash
git add ai-code-onboarding.el ai-code.el ai-code-backends.el README.org ai-code-autoloads.el test/test_ai-code-onboarding.el test/test_ai-code-backends.el
git commit --file=- <<'EOF'
Clarify first-run AI Code usage with a quickstart buffer

This adds a one-time onboarding buffer and a manual quickstart
entry so Emacs-native users can discover the core AI Code flow
without reading the full feature surface. Explicit backend switches
now also emit a short next-step hint.

Constraint: Must improve first-run discoverability without adding a second workflow system
Rejected: Backend hint inside ai-code--apply-backend | would spam noninteractive backend activation paths
Confidence: high
Scope-risk: moderate
Reversibility: clean
Directive: Keep quickstart limited to first-minute actions and avoid expanding it into general help
Tested: Targeted onboarding/backend/package tests, checkdoc, batch byte-compile, full test suite, manual Emacs smoke pass
Not-tested: Cross-platform window placement differences outside the local Emacs setup
EOF
```
