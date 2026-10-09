---
name: update-deps
description: Update dependencies and ensure compatibility with current releases
---

# Update Dependencies and Refactor Codebase

Run the repository-owned dependency maintenance commands:

```bash
make update_deps
prek update
```

`make update_deps` tidies `DESCRIPTION`, raises CRAN dependency constraints to
the latest published versions, regenerates roxygen output, and rewrites
`codemeta.json`. `prek update` bumps the hook revisions in
`.pre-commit-config.yaml`. Inspect every generated change and keep only
intentional updates. Follow the `AGENTS.md` rules for `DESCRIPTION`,
roxygen-generated files, version synchronization, and `NEWS.md`.

Review the changelogs and current documentation for upgraded R packages. Apply
small compatibility fixes or simplifications when a newer dependency API lets
the package remove a workaround or reduce local complexity without changing
behavior. Keep statistical computation in `statsExpressions` rather than
duplicating it in this plotting frontend.

If the minimum supported R version changes, update `DESCRIPTION` and any
version-sensitive code or tests (including `tests/testthat.R` and snapshot
variants) together. Keep README support wording independent of specific R
release numbers.

Inspect `.github/workflows/` for caller compatibility with the shared
`IndrajeetPatil/workflows` interfaces; update callers when needed and do not
copy shared workflows locally.

Iterate until the relevant tests and the full validation gate from `AGENTS.md`
pass. Fix breaking API changes, R-devel incompatibilities, generated-file
drift, snapshot regressions, coverage gaps, lint findings, and check failures
introduced by the refresh; do not weaken checks or suppress legitimate
failures. Follow the `AGENTS.md` guidance on renderer-specific snapshot diffs
before accepting new snapshots.

Create a ready-for-review pull request, or update the current pull request when
one already exists. Summarize dependency changes, compatibility fixes,
simplifications, generated files, and validation commands in the pull-request
body. Recheck the live GitHub Actions status after pushing and report which
checks passed, failed, or remain in progress; do not wait for completion unless
explicitly requested.
