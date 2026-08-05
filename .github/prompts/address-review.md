---
name: address-review
description: Address code review comments and reply to them
disable-model-invocation: true
---

# Address Code Review Comments

Inspect every unresolved review thread on the current pull request and decide
whether each comment has merit. Fix all actionable comments. Reply to every
comment on my behalf, including comments that do not require a code change, and
resolve each thread only after replying. Use the authenticated `gh` CLI for
pull-request metadata and GraphQL when thread-level resolution state matters.

When a comment identifies a repeated inconsistency, search the entire
repository and fix every relevant occurrence rather than only the cited line.
In particular:

- Treat `DESCRIPTION` as the source of truth for dependency constraints and the
  minimum supported R version.
- Keep the package version synchronized across `DESCRIPTION`, `codemeta.json`,
  and the first `NEWS.md` heading.
- Regenerate `NAMESPACE` and `man/*.Rd` with
  `Rscript -e 'roxygen2::roxygenise()'`; do not edit generated Rd files by hand.
- Keep statistical computation in `statsExpressions` and plotting concerns in
  this package.
- Update reusable-workflow callers under `.github/workflows/`; do not copy the
  shared workflow implementations into this repository.

Choose the narrowest relevant test first. If review-driven changes affect
shared behavior, dependencies, generated files, or CI configuration, run the
full local gates before pushing:

```bash
air format . --check
make lint
make hooks
make check
```

If Air reports formatting drift, run `air format .`, inspect the result, and
rerun its check. Run `make document` when README source or generated README
content changes, and run targeted `testthat` or `vdiffr` tests when a comment
affects a specific plotting path.

Record user-facing compatibility or behavior changes in `NEWS.md`, but omit
routine dependency, lint, formatting, test, and CI maintenance. Use clear
American English for user-facing prose while preserving package names, code
identifiers, API names, and published titles exactly.

After validation passes, commit and push any changes to the existing pull
request branch. Recheck the pull request's review threads and live checks, and
report anything that remains unresolved or still running.
