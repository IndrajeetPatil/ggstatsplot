---
agent: 'agent'
description: 'Simplify the codebase safely by removing avoidable custom code and adopting proven dependency capabilities.'
---

# Simplify the Codebase Safely

Treat the codebase as a liability, not an asset. Extra lines of code, bespoke helpers, compatibility shims, and home-grown abstractions all carry ongoing maintenance, defect, and upgrade cost. Prefer deleting code over preserving it, as long as behavior remains correct.

Your goal is to simplify something real in this repository without introducing regressions or defects. The winning outcome is less custom code, fewer moving parts, and the same or better behavior.

## What to look for

Find a simplification opportunity in one of these categories:

1. **New capability already available through an updated dependency** — remove workaround code, compatibility glue, or redundant abstractions that newer versions make unnecessary.
2. **A reliable, widely used third-party dependency** — where the repository currently maintains custom code for a solved problem, replace that code with a mature dependency only if it clearly reduces risk and complexity overall.

Prefer changes that delete code, remove branching, collapse indirection, reduce duplicated logic, or eliminate maintenance-heavy utilities. Do not refactor for aesthetics alone.

## Guardrails

- Never trade simplicity for regressions, weaker validation, poorer accessibility, lower observability, or hidden behavior changes.
- Do not replace small straightforward code with a dependency unless the dependency is broadly adopted, well maintained, and materially reduces long-term liability.
- Keep the refactoring tightly scoped to one coherent simplification theme rather than bundling unrelated cleanups.
- Reuse repository conventions, existing helpers, and established patterns where they still add value.
- If a candidate simplification would require speculative design changes, broad rewrites, or uncertain behavioral drift, skip it and choose a safer target.

## Expected workflow

1. Identify one concrete liability in the current codebase.
2. Confirm whether a newer dependency feature or a proven third-party package can remove that liability.
3. Implement the simplification with the smallest coherent change set that fully replaces the old approach.
4. Remove obsolete code, dead paths, compatibility layers, comments, or tests that only existed for the previous implementation.
5. Run the relevant validation until the refactoring is demonstrated to be regression-free.

Use the smallest validation that proves the change, and escalate as needed. For this repository, that commonly means:

- `make check`
- `make lint`

Run any narrower targeted checks first when they are sufficient, but do not stop until you have strong evidence that the refactoring did not introduce defects.

## Changelog policy

Do **not** update `CHANGELOG.md` for minor cleanups or routine simplifications. Only if the work amounts to a **significant refactoring** should it be recorded there, and then record **only** the significant refactoring itself — not incidental cleanup details, drive-by edits, or validation notes.

## Pull request

At the end, create a PR with the `gh` CLI, not the GitHub MCP server. In the PR body, explain:

- what liability was removed,
- whether the simplification came from a newer dependency capability or from adopting a reliable third-party dependency,
- what code was deleted or collapsed,
- how you ensured the refactoring stayed regression-free,
- whether `CHANGELOG.md` was updated, and if so, why the refactoring was significant enough to warrant it.

The PR should make the case that the repository is now easier to maintain because it owns less custom code.
