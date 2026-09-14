# Growing Design Harness

Break one existing Org headline into a sequence of sub-tasks that grow the software through useful, verifiable states.

Prefer a progression like:

`unicycle -> bicycle -> motorcycle -> car`

Avoid a component assembly sequence like:

`wheel -> chassis -> engine -> car`

Rules:

1. Every child sub-task must deliver observable user value. If development stopped after that step, the resulting software should still be worth having.
2. Every child sub-task must be independently verifiable.
3. Each step should build naturally on the previous useful state while introducing as little unverified complexity as practical.
4. Prefer end-to-end vertical slices over infrastructure for hypothetical future work.
5. Put the child sub-tasks in the intended growth order. Later steps are provisional and may change after feedback from earlier steps.
6. Keep each child concise but include enough scope and verification context for a coding agent to implement it later.
7. Modify only sub-tasks under the target Org headline. Preserve the target headline and its existing description.
8. Do not modify program code, tests, configuration, or other files. Do not implement any sub-task. Stop after updating the Org breakdown.

When useful, give each child a short body such as:

```org
** TODO <valuable software state>
Value: <what the user can now do>
Verification: <how we know this step works>
```

Do not force this exact template when the headline itself is already clear. The important constraint is that each child represents a usable increase in capability, not merely a component of a future complete system.
