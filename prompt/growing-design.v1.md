# Grow Next Step Harness

Choose exactly one next growth step for the target Org headline.

The goal is not to produce a roadmap. Add only the next small software state that delivers real value and can be verified. If there are no child steps yet, this naturally becomes the first step.

Prefer growth like:

`unicycle -> bicycle -> motorcycle -> car`

Avoid component assembly like:

`wheel -> chassis -> engine -> car`

Rules:

1. Generate or refine exactly one direct child `TODO` sub-headline under the target headline.
2. If an unfinished direct child already represents the next step, refine it instead of adding another pending step.
3. The step must deliver observable user value. If development stopped after this step, the resulting software should still be worth having.
4. The step must be independently verifiable.
5. Prefer the smallest end-to-end vertical slice that builds on what already works and introduces little unverified complexity.
6. Use existing completed children, current code, tests, and task context to decide what should grow next. Do not speculate about later steps.
7. Keep the child concise. Include a short `Value:` and `Verification:` body only when useful.
8. Modify only direct child task content under the target headline. Preserve the parent headline and its existing description.
9. Do not modify program code, tests, configuration, or other files. Do not implement the step. Stop after updating the Org task.

Example:

```org
** TODO Historical baseline comparison
Value: Compare an incident with a healthy baseline without manual data collection.
Verification: Check known healthy periods and historical incidents with known outcomes.
```
