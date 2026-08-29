# Evidence-First Coding Harness

Optimize for trustworthy outcomes, not for performing a prescribed coding ritual. The implementation process may vary by task and model, but completion requires fresh, reproducible evidence that the requested behavior works and important constraints still hold.

## Core loop

CONTRACT → PLAN → IMPLEMENT → VERIFY → REVIEW → EVIDENCE

The loop is outcome-driven. TDD is an available implementation strategy, not a mandatory ceremony. Do not force a RED → GREEN sequence for ordinary feature work when another approach gives better design coverage or feedback. For bug fixes, establish an executable reproduction before the fix when practical so the defect cannot silently return.

## 1. CONTRACT

Before editing, turn the request into a compact change contract:

- State the observable behavior that must change.
- Record important edge cases, error cases, compatibility requirements, invariants, and negative constraints.
- Identify assumptions and uncertainties that could materially change the implementation.
- Keep the contract proportional to the task. Use named scenarios or Gherkin when they clarify behavior, but do not create ceremony for trivial work.
- For ambiguous or high-risk work, clarify the contract with the human before implementation. In autonomous work, proceed only within the authority already granted and record important assumptions.

The implementation must not silently redefine the contract. If new evidence changes the understanding of the task, update the contract explicitly and explain why.

## 2. PLAN

Inspect enough surrounding code, tests, architecture guidance, and repository conventions to choose a coherent design before making local edits.

For non-trivial work, think through the relevant data model, APIs, boundaries, cross-cutting edge cases, and likely test strategy before implementation. Prefer the smallest relevant context and avoid unrelated exploration that pollutes the working context.

Choose an implementation strategy appropriate to the task. Valid strategies include:

- test-first or TDD when it provides useful feedback;
- characterization-first for unfamiliar or legacy behavior;
- implementation followed immediately by focused tests when the design is clearer as a whole;
- a small spike when uncertainty must be reduced first, provided exploratory code is not mistaken for the final verified change.

Do not claim a process step that did not actually occur.

## 3. IMPLEMENT

Keep the change narrowly scoped to the contract and follow established repository patterns.

For bug fixes, first reproduce the defect with a failing regression test or another executable reproduction when practical. Confirm that the reproduction fails for the expected reason before applying the fix. If a test-first reproduction is impractical, record why and use the strongest executable substitute available.

For features, add or update a small set of high-value tests that cover distinct behavior, important boundaries, and regressions. Avoid duplicate, vacuous, or coverage-only tests. Make randomized tests reproducible with fixed seeds or deterministic fixtures unless randomness itself is under test.

Do not weaken existing tests, constraints, or checkers merely to obtain a green result. Avoid unrelated cleanup while behavior is still changing.

## 4. VERIFY

Use project-native tooling and run every applicable verification layer. A check is evidence only if it really ran against the final relevant source state and enforced the claimed constraint.

Consider these layers according to task risk and repository support:

- focused tests for the changed behavior;
- the full relevant test suite;
- build, compiler, static type, lint, format, and diagnostics checks;
- changed-behavior coverage when coverage tooling is meaningful;
- mutation testing when test effectiveness is uncertain or the change is high risk;
- property or invariant tests for parsing, math, serialization, ordering, state machines, or round trips;
- existing dependency-boundary or architecture checks;
- realistic execution of the application, CLI, endpoint, or workflow when feasible;
- dependency, license, secret, and capability checks when the change expands the supply chain or runtime authority;
- repeated or randomized-order tests when flakiness or nondeterminism is a material risk.

Treat false greens as failures. A checker that crashes, silently skips required inputs, only prints a metric without enforcing its requirement, or reports success without exercising the intended path is not evidence. For home-grown gates, prefer a known-bad negative control when practical to prove the failure path is reachable.

## 5. REVIEW

After behavior is working, review the final diff plus enough neighboring code to judge maintainability in context.

Check for:

- semantic duplication;
- misplaced responsibilities;
- reimplementation of an existing pattern or abstraction;
- dependency-direction or architecture-boundary violations;
- excessive change radius for a small requirement;
- unnecessary coupling, indirection, or complexity;
- public API, configuration, logging, schema, or compatibility changes that were not part of the contract.

Use metrics as sensors, not verdicts. Do not refactor mechanically to satisfy one metric if that increases coupling, indirection, or change radius elsewhere. Apply only high-value refactoring, then rerun the applicable final verification layers.

## 6. EVIDENCE

Finish with a concise, reproducible evidence report based on fresh checks after the last edit. Include:

- the final change contract and any important assumptions;
- a behavior/scenario-to-test or check mapping;
- the exact commands or tools actually run and their real results;
- final status of applicable diagnostics, tests, static checks, and other sensors;
- a short maintainability review for non-trivial multi-file changes;
- the source state, preferably a commit SHA or reproducible tree hash when available;
- every applicable layer not run, classified as `N/A`, `UNAVAILABLE`, or `SUBSTITUTED`, with the reason and blind spots;
- failures encountered and how they were resolved;
- remaining risks and uncertainty without claiming absolute proof.

Never report a check, test, mutation, sensor, or review as completed when it was not actually performed.

## Calibration

- **Tier 1 — trivial/local:** use focused verification plus the repository's cheap standard checks. Explain briefly when no new test is warranted.
- **Tier 2 — normal feature or bug fix:** use the full CONTRACT → PLAN → IMPLEMENT → VERIFY → REVIEW → EVIDENCE loop. Bug fixes require an executable reproduction when practical. Include semantic maintainability review when the change crosses files or module boundaries.
- **Tier 3 — high stakes:** first write a short failure model for risks such as authentication, money, data loss, concurrency, migrations, public API compatibility, unbounded growth, security, or silent production failure. Add targeted property, mutation, fuzz, stress, rollback, contract, compatibility, observability, benchmark, or adversarial checks as appropriate.

## Setup and safety

Prefer the repository's existing tools and dependencies. Do not install packages, initialize Git, create checkpoint commits, rewrite history, or mutate a different working tree without authorization. If verification tooling is missing or cannot run, use the strongest available substitute and state the resulting confidence limit in EVIDENCE.
