# Org result-summary guidance

Apply these instructions only when the current prompt explicitly requests an
opt-in Org result summary. Append one new subheading (with optional child detail
headings) after the actual work or read-only verification discussion finishes. Preserve existing human notes,
headings, and task states.

## Outcome headline, timestamp, and agent attribution

For the newly appended Org result subheading, use the literal Org TODO keyword AI
immediately after the stars, followed by a concise, task-specific title describing the
actual outcome or finding. Prefer a concrete subject and action (for example: Fixed
config merge precedence, Investigated cache misses, or Verified PR #537 against
acceptance criteria). Do not use generic headings such as Result Summary, AI Summary, or
Verification Notes; do not claim a fix, test pass, or verification that did not occur.
Choose the title based on the completed work, not just the original request. Keep the
new heading one Org level below the selected parent; the two stars in the example are
illustrative only. Put the actual writing agent name in an uppercase Org headline tag at
the end. For example:
** AI Fixed config merge precedence :CODEX:
[YYYY-MM-DD Day HH:MM]
The inactive Org timestamp is the first content line directly below the heading, never
part of the headline. Use actual values from the local system clock at summary writeback
time, not at prompt-send time. Do not invent a time: if the clock is unavailable, state
that the timestamp is unavailable instead of using a made-up date. Alternative agent
tags include :CLAUDE_CODE: and :GEMINI:. Normalize multiword names with underscores. Use
:UNKNOWN: when the agent identity cannot be verified. Never use the agent name as the
TODO keyword or a generic :AI_GENERATED: tag. Org recognizes AI as a TODO keyword when
configured in org-todo-keywords; do not edit #+TODO: declarations or the user's TODO
configuration, existing states, parent headings, or human notes. Do not add a property
drawer or unrelated metadata lines. Keep the title, timestamp and attribution in the
same newly appended subheading. Follow the source-evidence section below for references
rather than repeating its rules.

## Summary first, foldable details

Write for a human skimming the Org file. Directly below the timestamp, write a
self-contained summary of about 3-8 lines (a short paragraph or a few list items):
what was asked, what was found or changed and why, the status (done, partial,
blocked, or verified), and any remaining risk or next step. A reader who never
expands the details must still understand the outcome.

Put all supporting detail in child headings one level below the result subheading,
so Org folds them by default, for example (levels illustrative):
*** Evidence
*** Changes
*** Verification
*** Open issues
Child headings are part of the same result-summary subtree; do not give them the AI
keyword, timestamps, or agent tags. Omit empty ones. Keep items to one line, include
only facts needed to resume or verify the task, and avoid repeating the summary. The
sections below define what may appear in these child headings, not a quota to fill.

## Original-source evidence and provenance

When writing that same result-summary subheading, automatically preserve useful
original-source references gathered during the work so the task can be resumed or
verified later. This is an incremental record of actual discoveries, not a separate
evidence-entry workflow. For the key human requirements, decisions, diagnostic
observations, and verification claims touched in this task, include concise context and a
direct raw reference when available (for example a Slack thread, Radar issue, GitHub
discussion, source file/line, Splunk query with time range, or exact test/log artifact).
Distinguish original human instructions and decisions from observed logs, AI-generated
interpretations, and unverified assumptions. Preserve the original URL, ID, or file
location; do not replace it with an AI-generated resume note, a derived design, or a PR
summary. If an original is unavailable or its provenance is uncertain, say so explicitly
instead of treating the derived description as authoritative. Do not invent, fabricate,
or silently strengthen evidence. Avoid copying references already recorded in this task
unless new observations materially change their relevance. Append this context within
the same result-summary subheading (in its Evidence child heading), leaving earlier
human-authored notes unchanged. This
guidance adds no new permission to read restricted sources, modify code or Git, or write
to task files before a read-only verification discussion has been concluded.

## Verified artifacts from the completed task

When writing that same result-summary subheading, automatically collect relevant
artifacts after the work or verification discussion has actually finished, rather than
taking a snapshot at prompt-send time. Keep the summary concise; include only artifacts
you can substantiate from this task and the actual workspace. When relevant, include:
(1) changed files with precise file references and their purpose; (2) the observed
worktree/branch and HEAD commit SHA, plus task-specific commits or PR URLs only if
verified, distinguishing committed code from current staged/unstaged edits; for a remote
PR, record its fetched head SHA, not local HEAD; (3) verification commands, actual
results, CI or deployment evidence, and date/environment when known; (4) missing tests,
uncertainties or unfinished work. If this task changed code, read Git and existing
records after the work is complete to establish these facts. Do not invent hashes, PR
links, test results, or claim passing checks that were not run. Do not attribute
repository-wide changes from other tasks or sessions to this task. If there are no
relevant code artifacts, simply omit those fields. Record artifacts within the same
result-summary subheading in the current Org task (in its Changes, Verification, or Open
issues child headings); do not create another catalog or
change human-authored requirements. This is a writeback formatting instruction only: it
does not authorize extra tests, code edits, Git mutations, or early notes in a read-only
Verify discussion.
