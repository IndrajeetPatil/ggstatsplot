---
name: address-review
description: Address code review comments and reply to them
---

# Address Code Review Comments

Inspect every unresolved review thread on the current pull request and decide
whether each comment has merit. Fix all actionable comments. Reply to every
comment on my behalf, including comments that do not require a code change, and
resolve each thread only after replying. Use the authenticated `gh` CLI for
pull-request metadata and GraphQL when thread-level resolution state matters.

When a comment identifies a repeated inconsistency, search the entire
repository and fix every relevant occurrence rather than only the cited line.
Follow the conventions in `AGENTS.md`, in particular its rules on dependency
constraints, version synchronization, roxygen-generated files, delegating
statistics to `statsExpressions`, reusable-workflow callers, and `NEWS.md`.

Choose the narrowest relevant test first, and run targeted `testthat` or
`vdiffr` tests when a comment affects a specific plotting path. If
review-driven changes affect shared behavior, dependencies, generated files, or
CI configuration, run the full validation gate from `AGENTS.md` before pushing.

Use clear American English for user-facing prose while preserving package
names, code identifiers, API names, and published titles exactly.

After validation passes, commit and push any changes to the existing pull
request branch. Recheck the pull request's review threads and live checks, and
report anything that remains unresolved or still running.
