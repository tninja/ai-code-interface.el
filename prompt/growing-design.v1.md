# Growing Design Harness

Grow a working software system through one independently valuable, verifiable increment at a time.

The current repository and completed, verified increments are the trusted baseline. Do not mechanically execute an upfront roadmap. Use what the current system actually does, what has already been verified, and what the last increment taught us to decide the next growth step.

## Non-negotiable rules

1. This is a design-only step. Modify only the current Org task file under its top-level `* Growing Design` section. Do not modify program code, tests, configuration, generated files, or other project files.
2. At most one `TODO Increment` may exist at a time. If one already exists, refine it instead of creating another.
3. Every increment must deliver observable user value. Prefer a small end-to-end capability over infrastructure that is useful only for hypothetical future work.
4. The increment must leave the software in a usable, testable state. If development stopped after this increment, the resulting version should still be worth having.
5. Prefer the smallest vertical slice that creates meaningful value and learning while introducing few new assumptions.
6. Make verification concrete before implementation. State what evidence would make a human confident that the increment works and delivers the intended value.
7. Preserve prior completed increments and their history. Do not rewrite the design to pretend the final architecture was known in advance.
8. After updating the design document, stop. Do not implement the increment and do not continue to the following increment.

## How to choose the next increment

Inspect the task description, investigation notes, existing Growing Design history, current code, tests, and relevant repository evidence. Then evaluate up to three candidate growth steps internally using these questions:

- What useful system exists now?
- What important user problem or friction remains?
- What is the smallest end-to-end capability that noticeably improves that problem?
- What user value becomes available immediately after this increment?
- What important uncertainty or assumption will this increment test?
- Can the result be independently verified?
- Does it build naturally on the trusted baseline without speculative infrastructure?

Prefer value and learning over architectural completeness. A good sequence resembles progressively more capable useful products, not a pile of components that become useful only at the end.

For a brand-new task, choose the smallest useful walking skeleton: a real input-to-output path that provides genuine value and validates the most important early assumptions.

## Update the task file

Keep or create this structure under `* Growing Design` as appropriate:

```org
* Growing Design

** Verified Baseline
Describe only capabilities that are supported by evidence. Mark uncertainty explicitly.

** DONE Increment 1: <name>
*** Value Delivered
*** User Scenario
*** Why This Growth Step
*** New Assumption
*** Scope
*** Out of Scope
*** Verification
*** Result
*** What We Learned

** TODO Increment 2: <name>
*** Value Delivered
State what a user can do after this increment that they could not do before.

*** User Scenario
Describe one concrete end-to-end use case.

*** Why This Growth Step
Explain why this is the best next growth from the current verified baseline and recent learning.

*** New Assumption
Name the main new assumption or uncertainty introduced by this increment.

*** Scope
Define the smallest complete capability required to deliver the value.

*** Out of Scope
Explicitly defer tempting adjacent features and speculative infrastructure.

*** Verification
Define concrete automated and/or domain evidence required before a human should trust this increment.

** Future Possibilities
Optional candidate branches. These are ideas, not commitments or a roadmap.
```

Do not mark an increment `DONE` merely because implementation or automated tests exist. Human or domain verification may still be required. Treat `DONE` increments as the trusted growth history only when the task file already records them that way.

When a newly completed increment lacks `Result` or `What We Learned`, update those from available evidence before choosing the next increment. Let those learnings change the next design when appropriate.
