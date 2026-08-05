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

`make update_deps` refreshes CRAN dependency constraints in `DESCRIPTION`,
regenerates roxygen output, and rewrites `codemeta.json`. Inspect every generated
change and keep only intentional updates. Treat `DESCRIPTION` as the source of
truth for package dependencies; do not hand-edit generated `NAMESPACE` or
`man/*.Rd` files.

Review the changelogs and current documentation for upgraded R packages. Apply
small compatibility fixes or simplifications when a newer dependency API lets
the package remove a workaround or reduce local complexity without changing
behavior. Keep statistical computation in `statsExpressions` rather than
duplicating it in this plotting frontend.

If the minimum supported R version changes, update `DESCRIPTION` and any
version-sensitive code or tests together. Keep README support wording
independent of specific R release numbers because CI tracks R-devel, release,
and oldrel through reusable workflows.

If the package version changes, keep these declarations synchronized:

- `DESCRIPTION`
- `codemeta.json`
- the first heading in `NEWS.md`

Do not add a `NEWS.md` entry for routine dependency, formatting, lint, test, or
CI maintenance. Add one only for a user-facing compatibility or behavior
change.

Inspect `.github/workflows/` for caller compatibility with the shared
`IndrajeetPatil/workflows` interfaces. This repository intentionally delegates
workflow implementation to that repository, so update callers when needed and
do not copy shared actions or workflows locally.

Iterate until the relevant tests and all full local gates pass:

```bash
air format . --check
make lint
make hooks
make check
```

If Air reports drift, run `air format .`, inspect the result, and rerun its
check. Also run `make document` when README source or generated README content
changes. Fix breaking API changes, R-devel incompatibilities, generated-file
drift, snapshot regressions, coverage gaps, lint findings, and check failures
introduced by the refresh; do not weaken checks or suppress legitimate
failures.

Create a ready-for-review pull request, or update the current pull request when
one already exists. Summarize dependency changes, compatibility fixes,
simplifications, generated files, and validation commands in the pull-request
body. Recheck the live GitHub Actions status after pushing and report which
checks passed, failed, or remain in progress; do not wait for completion unless
explicitly requested.
