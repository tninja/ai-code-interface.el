# Grill Review Feedback Design

## Problem

Pull request 436 has two valid functional review findings:

1. Accepting Grill for a direct backend command such as `/status` appends the
   harness suffix and changes the command into a normal prompt.
2. Loading an existing public entry command through its autoloaded module does
   not load `ai-code-grill`, because the module is currently required only by
   the main `ai-code` feature.

The review also identifies missing standard metadata sections in the new Grill
test file.

## Desired Behavior

- A single-token slash command such as `/status` bypasses Grill and reaches the
  existing direct-command execution path unchanged.
- Slash-prefixed text containing spaces, tabs, or newlines is not a direct
  command and remains eligible for Grill.
- Directly invoking autoloaded code-change, TODO, or discussion entry points
  loads Grill and installs the same advice as loading the main `ai-code`
  feature.
- Existing Helm origin preservation, deferred advice installation, and
  enabled/disabled behavior remain unchanged.
- The Grill test file follows repository SPDX, Commentary, and Code-section
  conventions.

## Design

### Shared direct-command predicate

Define `ai-code--direct-command-p` in `ai-code-prompt-mode.el`.  It returns
non-nil only when its text begins with `/` and contains no whitespace
characters.  Replace the inline predicate in `ai-code--insert-prompt` with this
helper.

Declare the helper in `ai-code-grill.el` and call it before the enablement,
origin-command, and confirmation checks.  Direct commands return unchanged and
never invoke `y-or-n-p`; slash-prefixed prompts containing whitespace continue
through the normal optional Grill path.

Keeping the predicate in the prompt-routing module gives direct execution and
Grill bypass one definition, preventing semantic drift.

### Shared loading point

Move the `ai-code-grill` requirement from `ai-code.el` to
`ai-code-prompt-mode.el`, after `ai-code--insert-prompt` is defined and before
`ai-code-prompt-mode` is provided.  Every supported autoloaded entry module
already requires prompt mode, so this installs Grill for both direct autoload
usage and full-package usage.

The existing immediate and deferred entry-command advice installation remains
unchanged.  When an entry module finishes loading, its existing
`with-eval-after-load` callback installs the origin-preserving advice.

## Testing

Use TDD in two independent red-green cycles:

1. Add tests proving `/status` bypasses Grill without asking, whitespace-bearing
   slash text remains eligible, and spaces, tabs, and newlines disqualify direct
   commands.  Run them before defining the helper and observe the expected
   failure.
2. Add an isolated clean-process integration test that loads an autoloaded entry
   module without loading the main `ai-code` feature, then asserts that
   `ai-code-grill`, the shared insert advice, and the relevant entry advice are
   installed.  Run it before moving the require and observe the expected
   failure.

After both cycles, run the focused Grill and prompt-mode ERT suites, the complete
ERT suite, reproducible byte compilation for touched production files, checkdoc
for every touched Emacs Lisp file, and `git diff --check`.

## Scope

Modify only `ai-code-prompt-mode.el`, `ai-code-grill.el`, `ai-code.el`, and the
minimum relevant test files.  Do not change `defconst
ai-code--grill-me-commands`, add redundant `fboundp` retry logic for the shared
insert advice, reply to reviewers, resolve threads, or otherwise mutate GitHub.
