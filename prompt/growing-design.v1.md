# Grow Next Step Harness

Discuss exactly one next growth step for the target Org headline.

The goal is not to produce a roadmap or immediately edit the task file. First help the user think through the next small software state that delivers real value and can be verified. If there are no child steps yet, this naturally becomes the first step.

Prefer growth like:

`unicycle -> bicycle -> motorcycle -> car`

Avoid component assembly like:

`wheel -> chassis -> engine -> car`

Rules:

1. Recommend exactly one next step. Do not speculate about later steps or generate a full breakdown.
2. The step must deliver observable user value. If development stopped after this step, the resulting software should still be worth having.
3. The step must be independently verifiable.
4. Prefer the smallest end-to-end vertical slice that builds on what already works and introduces little unverified complexity.
5. Use existing completed children, current code, tests, and task context to decide what should grow next.
6. Keep the discussion concise. Explain the proposed step using only what is useful: what value it delivers, why it is the right next step now, how to verify it, and the main new uncertainty when relevant.
7. On the first turn, do not modify the Org file or any other file.
8. End by asking whether the user wants to discuss the proposal further or write it back to the Org task.
9. Only after the user explicitly asks to write it back, create or refine exactly one direct child `TODO` under the target headline. Preserve the parent headline and its existing description. Keep the child concise; include `Value:` and `Verification:` only when useful.
10. Writing the step back must not modify program code, tests, configuration, or other files, and must not implement the step.

A useful discussion might conclude with a proposal such as:

```text
Next step: Historical baseline comparison
Value: Compare an incident with a healthy baseline without manual data collection.
Verification: Check known healthy periods and historical incidents with known outcomes.

Would you like to discuss this step further, or should I write it back to the Org task?
```
