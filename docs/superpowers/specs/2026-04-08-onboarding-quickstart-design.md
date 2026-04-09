# Onboarding Quickstart Buffer for AI Code Interface

## Summary

Add a lightweight onboarding flow for first-time users of `ai-code-interface.el`.
The flow should appear once when the user first opens the main AI Code menu, show
the shortest path to productive use, and remain available later through an
explicit menu entry.

This design is intentionally small. It does not add a new workflow engine,
wizard, or tutorial system. It exposes existing high-value commands more
clearly and reduces the time from installation to first successful action.

## Problem

The package already offers a broad feature set, but discovery is harder than it
needs to be for users who are comfortable with Emacs and unfamiliar with this
package. The most important early actions are present, but the user must infer:

- which command starts the session
- which commands matter in the first minute
- how command scope is determined
- what `C-u` changes during prompt creation

The result is a strong feature surface with weak first-run guidance.

## Target User

Primary target:

- users who already know Emacs reasonably well
- users who do not yet know the package-specific workflow

Secondary benefit:

- returning users who want a quick reminder of the core command set

## Goals

- Make the first successful session start obvious
- Make the first three useful actions obvious
- Explain prompt scope and prefix behavior in one screen
- Reuse existing commands instead of creating onboarding-only behavior
- Keep the implementation small, local, and easy to remove or revise

## Non-Goals

- No multi-step wizard
- No installation or environment diagnostics
- No overlay arrows, pulse effects, or complex guided tours
- No telemetry or usage tracking
- No duplicate implementation of existing commands
- No attempt to teach advanced features such as MCP, behaviors, worktrees,
  review flows, or prompt suffixes

## Proposed UX

### Entry Points

The onboarding flow has two entry points:

1. Automatic one-time display when the user invokes `ai-code-menu` for the first
   time
2. Manual reopening through a new menu item such as `Help / Quick Start`

This keeps first-run help visible without making it hard to find later.

### Primary Presentation

The onboarding content is rendered in a read-only help-style buffer, not in the
minibuffer and not as a transient submenu.

Recommended display behavior:

- open in another window or side window
- keep the source buffer intact
- allow immediate close with `q`
- allow direct invocation of existing commands through buttons or links

This should feel like a short product-specific quickstart, not like external
documentation.

### Quickstart Structure

The buffer should fit in one screen and contain only the minimum useful
information:

1. Header
   - current backend label
   - whether an AI session already exists for the current project
   - one short orientation sentence
2. Start Here
   - `a` start AI session
   - `z` switch back to active AI session
   - `s` switch backend
3. Most Useful Actions
   - `c` change current function or selected region
   - `q` ask about current function or file
   - `i` implement TODO at point
4. How Context Works
   - active region wins
   - otherwise current function is used when available
   - `C-u` adds broader context such as clipboard or file/repository context
5. Try It Now
   - Start Session
   - Ask About This Function
   - Change Selected Code
   - Open Prompt File
   - Switch Backend
6. Footer Actions
   - Do Not Show Again
   - Show README
   - Close

### Backend-Specific Guidance

When the user switches backend, show a short, single-line follow-up message in
the minibuffer. Do not open a second onboarding buffer.

Example:

`Backend switched to OpenAI Codex. Next: a to start, g to edit config, G to open AGENTS.md.`

This message should be backend-aware and derived from existing backend metadata
where practical.

## Implementation Design

### New Module

Add a small new module, tentatively `ai-code-onboarding.el`.

Responsibilities:

- render the quickstart buffer
- expose a command to open the quickstart manually
- store whether auto-display is enabled
- store whether the user has already seen the quickstart
- generate a short backend-specific guidance string

Non-responsibilities:

- starting sessions
- sending prompts
- changing backend state
- inspecting installation health

### Integration Points

#### `ai-code.el`

Integrate onboarding at the menu layer:

- check whether the quickstart should auto-display when `ai-code-menu` runs
- keep the existing menu flow intact
- add an explicit quickstart entry to the transient menu

The menu remains the stable entry point. Onboarding is attached to it, not
embedded into command logic.

#### `ai-code-backends.el`

Integrate the backend-specific minibuffer hint after backend selection is
applied successfully.

The hint must remain informational only. It should not alter backend behavior or
start sessions automatically.

### State Model

Use only two persistent user-facing settings:

- `ai-code-onboarding-auto-show`
  - whether the package may show the quickstart automatically
- `ai-code-onboarding-seen`
  - whether the user has already seen the quickstart

These should be simple customization variables.

Do not add a complex onboarding state machine.

### Buffer Mechanics

Use built-in Emacs mechanisms only:

- a dedicated major mode derived from `special-mode`, or equivalent read-only
  help buffer behavior
- buttons or text properties for actions
- normal `quit-window` behavior

No external dependency is required.

## Edge Cases

- If the quickstart is reopened manually after first run, it should still work
  even when `ai-code-onboarding-seen` is non-nil
- If no backend is selected, the header should degrade gracefully and show a
  neutral label
- If no session exists, the status line should say so explicitly
- If buttons invoke commands that require a file-backed buffer, command failures
  should remain owned by the existing commands rather than reimplemented in the
  onboarding layer
- If the user disables auto-show, the package must not reopen the quickstart
  automatically

## Why This Is Small But Valuable

This feature improves usability by clarifying the first minute of usage without
introducing a new concept. It works by exposing the package's strongest
existing commands:

- start a session
- ask a question
- change code
- switch backend

That makes it a high-leverage addition with low architectural risk.

## Testing Strategy

### Automated Tests

Add ERT coverage for:

- automatic quickstart display gate
- manual quickstart command availability
- "do not show again" state changes
- backend switch message generation
- quickstart buffer action wiring for at least one or two representative actions

### Manual Verification

Verify the following manually:

1. Fresh state: running `ai-code-menu` shows the quickstart once
2. Closing the quickstart returns the user to a usable editing state
3. Clicking `Start Session` invokes the existing session command
4. Reopening quickstart from the menu works after the first-run flag is set
5. Switching backend prints a short next-step hint

## Risks

- If the quickstart appears too aggressively, experienced users may see it as
  noise
- If the quickstart contains too much text, it will recreate the original
  discovery problem in another form
- If action buttons do not clearly reuse existing commands, maintenance cost
  will drift upward

These risks are controlled by keeping the scope narrow and the content short.

## Deliberate Omissions

The following ideas were considered and rejected for this design:

- Full wizard flow
  - too interruptive for Emacs-native users
- Interactive setup doctor
  - expands scope into diagnostics and environment support
- Overlay-based tutorial
  - higher implementation and maintenance cost
- Advanced feature education in first run
  - increases cognitive load before the user completes a basic task

## Readiness For Planning

This spec is intentionally scoped to a single, bounded feature:

- one new onboarding module
- one menu integration point
- one backend hint integration point
- a small set of tests

It is ready for implementation planning without requiring further product
decomposition.
