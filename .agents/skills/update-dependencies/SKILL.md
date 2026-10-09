---
name: update-dependencies
description: Update dependencies to their latest versions and keep the package compatible, change the minimum supported R version, or add, remove, or move a package dependency in DESCRIPTION. Use only for dependency maintenance, not for installing the current dependency set.
---

# Update dependencies

`DESCRIPTION` is the source of truth for R and package dependency constraints.
Regenerate `codemeta.json`, `NAMESPACE`, `API`, `R/globals.R`, and `man/*.Rd`
from it instead of editing them by hand.

## Refresh constraints

1. Run `make update_deps` and `prek update`. `make update_deps` tidies
   `DESCRIPTION`, raises the minimum versions of all Imports and Suggests to
   the latest CRAN releases, re-runs roxygen, and regenerates `codemeta.json`.
   `prek update` bumps the hook revisions in `.pre-commit-config.yaml`. Use
   `make install_deps` instead when the goal is only to install the
   dependencies already declared.
2. Inspect the diff and confirm that every updated constraint is the latest
   suitable stable release and that none requires an R version newer than the
   package's minimum. Read the upstream changelogs and documentation for
   upgraded packages.
3. Fix breaking API changes, R-devel incompatibilities, generated-file drift,
   snapshot regressions, coverage gaps, lint findings, and check failures. Do
   not weaken checks or suppress legitimate failures. Follow the `AGENTS.md`
   guidance on renderer-specific snapshot diffs before accepting new
   snapshots.
4. Apply small compatibility fixes or simplifications when a newer dependency
   API lets the package remove a workaround without changing behavior. Keep
   statistical computation in `statsExpressions` rather than duplicating it in
   this plotting frontend.
5. A newer roxygen2 or `statsExpressions` release changes generated
   documentation; see the roxygen notes in `AGENTS.md`.

## Add, remove, or move a dependency

- Edit `DESCRIPTION` directly, then run `Rscript -e 'roxygen2::roxygenise()'`
  and `Rscript -e 'codemetar::write_codemeta()'`.
- Guard `Suggests` packages with `skip_if_not_installed()` in tests and with
  conditional use in examples and vignettes; the `pkgdown-no-suggests` and
  `R-CMD-check-hard` jobs build without them.
- Declare packages needed only by CI checks in `Config/Needs/check`, not in
  workflow caller inputs.

## Minimum R version

- Change `Depends` in `DESCRIPTION` together with version-sensitive code and
  tests, including `tests/testthat.R` and snapshot variants, and the CI matrix
  described in the `maintain-ci` skill.
- Keep README support wording as "R-devel, the current R release, and the
  previous R release" rather than hard-coding version numbers.

## Validate and publish

1. Start with the affected `testthat` files and snapshots, then iterate until
   the full validation gate in `AGENTS.md` passes.
2. Keep validation serial when R processes share an installation library. Run
   `make clean` afterwards and verify that only intended files changed.
3. Follow the `NEWS.md` policy in `AGENTS.md`: record a higher minimum R version
   or a newly required package, but omit routine constraint bumps. Commit
   dependency bumps as `chore(deps): ...`.
4. If the branch already has a pull request, update its body with the
   dependency changes, compatibility fixes, simplifications, generated files,
   and validation commands; otherwise open one following `AGENTS.md`.
