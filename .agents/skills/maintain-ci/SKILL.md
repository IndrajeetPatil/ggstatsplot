---
name: maintain-ci
description: Change, debug, or add GitHub Actions workflows under .github/workflows/ for this R package. Use only for CI/CD work, not for ordinary code changes.
---

# Maintain CI/CD

## Workflow layout

Workflows under `.github/workflows/` are thin callers of reusable workflows in
`IndrajeetPatil/workflows`
(`uses: IndrajeetPatil/workflows/.github/workflows/<name>.yaml@main`).

- Change the caller's `with:` inputs rather than copying a reusable workflow
  into this repository.
- When a fix belongs in the shared workflow, make it in
  `IndrajeetPatil/workflows` and keep this repository's caller compatible.
- If a caller uses a public action directly, verify its latest stable release
  and keep the repository's pinning convention.
- Declare check-only packages as described in the `update-dependencies` skill,
  not in caller inputs.

The callers cover:

- `R-CMD-check`: Ubuntu R-devel, release, and oldrel-1, plus macOS and Windows
  release.
- `R-CMD-check-hard`: pull requests only, with hard dependencies only.
- `test-coverage`: Codecov upload (thresholds in `codecov.yaml`).
- `check-extra`: examples, tests, and vignettes with warnings as errors, tests
  in random order, and README rendering.
- `check-docs`: link checking with lychee (`lychee.toml`) and spell checking
  with typos (`_typos.toml`).
- `check-formatting` (Air), `lint` (lintr), and `pre-commit` (prek hooks).
- `pkgdown` builds the site and deploys it from `main` and releases;
  `pkgdown-no-suggests` checks on pull requests that the site builds without
  suggested packages; `seo-files` runs after a successful `pkgdown` run on
  `main`.
- `submit-cran`: manual release workflow; see the `create-release` skill.

## Check matrix

The shared R CMD check matrix intentionally covers R-devel, release, and
oldrel. Do not reintroduce `oldrel-2` unless the package support policy
changes. Tests run only on Linux and macOS (see `tests/testthat.R`), so the
Windows job runs `R CMD check` without tests.

## Reproduce locally

| CI job           | Local command                                       |
| ---------------- | --------------------------------------------------- |
| R-CMD-check      | `make check`                                        |
| lint             | `make lint`                                         |
| check-formatting | `air format . --check`                              |
| pre-commit       | `make hooks`                                        |
| check-docs       | `lychee .` (links) and `typos` (spelling)           |
| pkgdown          | `Rscript -e 'pkgdown::build_site()'`                |

CI pins the Air version in the shared `check-formatting` workflow, so an older
local Air can disagree with CI.
