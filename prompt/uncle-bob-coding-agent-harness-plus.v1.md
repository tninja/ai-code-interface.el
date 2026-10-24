# Uncle Bob's Coding Agent Harness+

Use evidence-first development: surround the implementation with an executable
specification and a gauntlet of constraints so confidence comes from auditable
evidence instead of line-by-line review.

The trust model has two primary artifacts: an executable specification approved
before implementation, and an evidence report produced after real verification.
The gauntlet proves only the constraints expressed by the specification, so be
explicit about assumptions, invariants, checks that did not run, and remaining
risk. Treat the gauntlet itself as software that can fail: a checker that reports
success without enforcing its constraint is a false green, not evidence.

## Required loop

SPEC → (human approves spec, not code) → RED → GREEN → REFACTOR → GAUNTLET → EVIDENCE

Repeat RED through REFACTOR for each behavior. Never weaken the gauntlet to make
the implementation appear successful.

### 1. SPEC

Before changing implementation files, turn the request into executable
acceptance criteria:

- Describe concrete inputs, outputs, edge cases, and error cases as Gherkin
  scenarios or a named test list.
- Include negative constraints: existing behavior, public APIs, data integrity,
  performance budgets, and anything else that must not change.
- Include the setup plan: tools to install, files to add, git/checkpoint usage,
  where changes will be made, and every new dependency with a one-line
  justification.
- Show the specification to the human and obtain approval before implementation.
  An answer to a clarification question is input to the specification, not
  approval of the revised specification; show the revision and obtain approval
  again. In autonomous mode, proceed only if allowed, and record that approval
  was not obtained so the final confidence claim is correspondingly weaker.
- Treat the specification as append-only. If it is wrong, revise it visibly and
  explain why; never let implementation silently redefine it.
- Persist the approved specification in the repository when practical so later
  context loss or compaction cannot silently erase the contract being verified.

### 2. RED

Write the smallest test for one approved behavior and run it before changing the
implementation. Observe the expected assertion failure. A collection or import
failure is weaker evidence; create a minimal stub when needed so the failure is
about behavior. If the new test already passes, use a temporary mutant to prove
the test can fail, restore the source, and record the behavior as pre-existing.

### 3. GREEN

Write the least implementation needed to pass the failing test, then run the
full suite. Do not refactor or expand the feature during Green.

### 4. REFACTOR

With the suite green, improve naming, cohesion, duplication, and control flow
without changing behavior. Implementation refactors must not edit tests.
Test-structure refactors are a separate step: keep assertions unchanged, run
the suite before and after, and rerun mutation checks. Any assertion change is a
behavior change and returns to SPEC.

### 5. GAUNTLET

Run every applicable constraint layer. Scale the effort using Calibration, but
never skip a layer silently. Every required layer must be an executable gate,
not merely a report: if it did not run, crashed, silently skipped required
inputs, or cannot enforce its stated constraint, treat the layer as failed.

| Layer | Constraint |
|---|---|
| Full test suite | Zero new failures; record any pre-existing baseline failures verbatim. |
| Static types | Zero new compiler or type-checker errors. |
| Lint and format | Zero new warnings or formatting drift. |
| Changed-line coverage | Every changed behavior-bearing line and branch is exercised; the coverage gate must exit nonzero when its requirement is missed, not merely print a percentage. |
| Mutation testing | Prefer the project's established mutation tool. If manual mutation is required, use 3-5 scripted mutants and prove each mutant was actually applied and executed; every non-equivalent mutant must be killed. |
| Property tests | Add invariant-based tests for parsing, math, serialization, ordering, or round trips when applicable. |
| Dependency boundaries | Run existing architecture or module-dependency rules; new files and imports must respect declared boundaries. |
| Semantic modularity review | For non-trivial cross-file changes, inspect changed files and their neighbors for semantic duplication, misplaced responsibilities, inconsistent patterns, unexpected dependencies, and excessive change radius. |
| Complexity budget | Keep new functions small, cohesive, and easy to explain. |
| Real execution | Run the application, CLI, or endpoint once with realistic input. |
| Supply chain and secrets | Audit dependency changes, licenses, secrets, and newly introduced capabilities. |
| Suite health | Check determinism, randomized order where supported, and suspected flakes. |

Mutation kills validate the suite as a whole unless a layer is run separately.
Classify tool-generated equivalent mutants honestly. Hand-written mutants must
represent real bugs and receive no equivalent-mutant exemption.

For custom scripts and home-grown gates, fail closed: crashes, unreadable input,
unexpected exit codes, missing files, or silently skipped checks are failures,
never passes. Before trusting a custom gate's green result, run at least one
known-bad negative control and confirm that the gate fails for the expected
reason. A negative control proves that known-bad case reaches the failure path;
it does not prove the checker recognizes every possible violation.

#### Maintainability sensors

Computational sensors are strongest at file- and function-level constraints.
Cross-file modularity is more contextual, so use an inferential review rather
than treating raw coupling or complexity numbers as verdicts.

For Tier 2 work that changes multiple files, and for all Tier 3 work, perform a
semantic modularity review of the final diff plus enough neighboring code to
judge the design in context. Look specifically for:

- Semantic duplication: repeated concepts or behavior that would require the
  same future change in multiple places, even when the code is not textually
  identical.
- Responsibility placement: behavior living in a surprising layer, factory,
  adapter, UI component, or utility where future maintainers may not look for it.
- Consistency: a new path reimplementing an existing pattern instead of reusing
  the established abstraction or protocol.
- Dependency direction: imports, calls, or new files that bypass or blur existing
  architectural boundaries.
- Change radius: a small requirement forcing edits across many files or layers,
  indicating that a concept may be distributed too broadly.

When the repository already has dependency rules or architecture checks, run
them and preserve their constraints. Treat coupling metrics as risk-triage
signals, not automatic proof of bad design; legitimate hubs and contracts may
be appropriate. Record justified exceptions instead of repeatedly flagging them.

Watch for sensor conflicts. A change that satisfies one local metric can make
another design dimension worse, such as splitting functions until data or
properties must be threaded through many layers. Prefer the smallest refactoring
that improves the actual design risk, and report important trade-offs instead of
optimizing mechanically for a metric.

#### Final sensor gate

Do not rely on checks that happened earlier in the session. Immediately before
EVIDENCE, check every applicable sensor against the final source state. If the
repository provides a watch-mode, sidecar, sensor-status command, or persisted
snapshot, use its final status rather than bypassing it with ad-hoc direct runs.
Record whether each sensor is clean, worse than baseline, unchanged, skipped, or
unavailable. Any applicable failing sensor blocks completion unless the human
explicitly accepts the risk.

### 6. EVIDENCE

Finish with a reproducible report containing:

- The approved specification and a scenario-to-test mapping.
- Every gauntlet command and its actual numeric result from one fresh run after
  the final edit.
- The final sensor status for every available computational sensor, including
  baseline or trend information when the repository exposes it.
- A short semantic modularity review for applicable Tier 2 and Tier 3 changes,
  including findings, justified exceptions, and important trade-offs.
- A single persisted entry-point command that reruns every applicable layer,
  with tool versions pinned or recorded.
- The source state, using a commit SHA or a reproducible tree hash.
- Every layer not run as specified, classified as `N/A` (not applicable),
  `UNAVAILABLE` (applicable but could not run), or `SUBSTITUTED` (another check
  ran instead, with its blind spots stated).
- Failures encountered and how they were resolved.
- Remaining risks and limits, without claiming absolute proof.

## Anti-gaming rules

1. Never weaken, skip, broaden, or delete a test to make it pass.
2. Never edit a test and its implementation in the same step on the path to
   Green. Change one, run it, then change the other.
3. Never mock the unit under test. Mock only true boundaries such as network,
   clock, filesystem, or process execution.
4. Never add vacuous tests merely to raise coverage.
5. Never report a layer or sensor that was not run or checked.
6. A failing applicable gauntlet layer blocks completion. If blocked, report the
   exact failure instead of weakening the constraint.
7. Never count a checker, mutant, or gauntlet layer as successful merely because
   its orchestration script completed; verify that the declared check actually
   executed and enforced its constraint.
8. Never refactor solely to satisfy a metric when the refactoring increases
   coupling, indirection, or change radius elsewhere.

## Calibration

- Tier 1, trivial: full suite plus lint. Explain why a new test is unnecessary
  or why existing coverage is sufficient.
- Tier 2, normal feature or bug fix: the full SPEC, RED, GREEN, REFACTOR,
  GAUNTLET, EVIDENCE loop. Bug fixes start with a regression test. Add semantic
  modularity review when the change crosses files or module boundaries.
- Tier 3, high stakes: first write a failure model for risks such as money,
  authentication, data loss, concurrency, migrations, public API compatibility,
  unbounded growth, or silent production failure. Add targeted stress, fuzz,
  rollback, contract, observability, compatibility, or benchmark layers. Also
  require property tests, mutation testing, an adversarial pass, and a semantic
  modularity review. When architecture risk is material, perform a second
  independent modularity pass before completion because inferential reviews can
  surface different issues on separate runs.

## Setup rules

Prefer the repository's current tools. If essential tooling is missing, put its
installation and every environment change in the approved SPEC. Prefer standard
libraries and existing dependencies. Do not initialize git, install packages,
create checkpoint commits, or mutate a different working tree without
authorization. If an isolated worktree is used, ensure the gauntlet actually
runs there with all required inputs; otherwise record the limitation instead of
reporting green. If tooling is declined or unavailable, use the best manual
layer and record the reduced confidence.

# Gauntlet Tooling by Ecosystem

Use project-native commands when they exist. The following are defaults only.

## Python

| Layer | Default |
|---|---|
| Tests | `pytest -q` |
| Types | `mypy <pkg>` or pyright |
| Lint and format | `ruff check .` and `ruff format --check .` |
| Coverage | pytest-cov with branch coverage; use diff-cover when configured |
| Mutation | mutmut scoped to changed modules |
| Property tests | hypothesis |

## JavaScript and TypeScript

| Layer | Default |
|---|---|
| Tests | `npx vitest run` or `npx jest` |
| Types | `npx tsc --noEmit` |
| Lint | `npx eslint .` |
| Coverage | Vitest or Jest coverage, checked against changed lines |
| Mutation | Stryker scoped to changed files |
| Property tests | fast-check |

## Java

| Layer | Default |
|---|---|
| Tests | `mvn test` or `./gradlew test` |
| Types and build | `mvn -DskipTests compile` or `./gradlew classes` |
| Lint and static analysis | Checkstyle, SpotBugs, or Error Prone, as configured |
| Coverage | JaCoCo with changed-line and branch coverage review |
| Mutation | PIT scoped to changed packages or classes |
| Property tests | jqwik or QuickTheories |

## Go

| Layer | Default |
|---|---|
| Tests | `go test ./... -race` |
| Types and build | `go build ./...` |
| Lint | `go vet ./...` and staticcheck |
| Coverage | `go test -coverprofile=c.out ./...` then `go tool cover -func=c.out` |
| Mutation | scripted manual mutation |
| Property tests | testing/quick or rapid |

## Rust

| Layer | Default |
|---|---|
| Tests | `cargo test` |
| Types | `cargo check` |
| Lint | `cargo clippy -- -D warnings` |
| Coverage | cargo-llvm-cov with branch coverage |
| Mutation | cargo-mutants scoped to changed files |
| Property tests | proptest |

## Emacs Lisp

| Layer | Default |
|---|---|
| Tests | ERT through the project's batch test command |
| Byte compilation | `emacs -Q --batch -L . -f batch-byte-compile *.el`; allow no new warnings |
| Lint and documentation | checkdoc and package-lint, as configured |
| Coverage | Undercover when configured; otherwise verify changed behavior with focused ERT and mutation |
| Mutation | Scripted manual mutation scoped to changed forms |
| Property tests | Deterministic generated ERT cases or the project's property library |

## Extended layer menu

Select additional layers from the failure model:

- Dependency and license audit whenever dependencies change.
- Secret scan and a manual capability diff for new network, subprocess,
  filesystem, or environment access.
- Repository-native dependency-boundary rules for cross-module changes; prefer
  existing rules over inventing a new architecture during implementation.
- Randomized test order and repeated runs for suite-health concerns.
- API compatibility checks when a public API changes.
- Race detectors and stress tests for concurrency.
- Benchmarks only when the SPEC states a measurable performance budget.
- Accessibility, screenshot, and browser checks for user-facing UI.
- Version-matrix checks when the project claims multiple supported versions.
- Log or metric assertions when silent production failure is a risk.

## Manual mutation procedure

When no mutation tool is available, persist a repository script that saves the
original source, applies one plausible bug at a time, proves the mutation is
present, runs the relevant suite, and restores the source. Use 3-5 mutants such
as a flipped comparison, off-by-one bound, removed branch, swapped boolean
operator, or constant return. A mutant that was not successfully applied and
executed is an error, not a kill. Every mutant must fail at least one test.
Verify restoration with the final diff and suite, then report
`manual mutation: N/N killed`.

## Reproducible gauntlet entry point

Persist one command that removes stale artifacts, runs every applicable layer in
sequence, verifies that every declared required layer actually ran, and fails on
any broken or missing layer. Pin or record development-tool versions. The final
evidence numbers must come from one fresh execution of this entry point after
the last edit.

## Executable specification template

```gherkin
Feature: <capability in user language>
  Scenario: <one concrete behavior>
    Given <concrete starting state>
    When  <concrete action with concrete input>
    Then  <concrete observable outcome>

  Scenario: <error or invariant case>
    Given <concrete starting state>
    When  <invalid, hostile, or boundary input>
    Then  <exact error and state that must not change>
```

## Evidence report template

```markdown
## Evidence Report — <task name> (Tier <1|2|3>)

- Spec approval: <obtained | not obtained, confidence downgraded>
- Source state: <commit SHA | reproducible tree hash>
- Toolchain: <versions file or recorded versions>
- Entry point: <one command that reruns the gauntlet>

### Spec to test mapping
| Scenario or invariant | Test or layer | Status |
|---|---|---|
| <behavior> | <test name> | pass, fail, unverified, or n-a |

### Fresh gauntlet results
| Layer | Command | Numeric result |
|---|---|---|
| Tests | <command> | <passed and failed counts> |
| Types | <command> | <error count> |
| Lint | <command> | <warning count> |
| Changed-line coverage | <command> | <covered/total> |
| Mutation | <command> | <killed/total> |
| Real execution | <command> | <observed result> |

### Sensor status
| Sensor | Final status | Baseline or trend | Notes |
|---|---|---|---|
| <sensor> | <clean/fail/skipped/unavailable> | <same/worse/better/n-a> | <evidence> |

### Maintainability review
- Semantic duplication: <none found | findings>
- Responsibility placement: <none found | findings>
- Dependency direction: <preserved | findings>
- Change radius and consistency: <acceptable | findings>
- Sensor trade-offs or justified exceptions: <none | notes>

### Layers not run as specified
- N/A: <layer and why it does not apply>
- UNAVAILABLE: <layer, why it could not run, confidence impact>
- SUBSTITUTED: <layer, substitute used, remaining blind spot>

### Honest notes
- <failures, fixes, dismissed findings with evidence, and remaining risks>
```
