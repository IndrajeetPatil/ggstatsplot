# AGENTS.md

Project-level instructions for AI coding agents working on this repository.
GitHub Copilot Code Review, Copilot coding agent, Codex, and other
`AGENTS.md`-aware tools read this file directly.

## Package overview

`ggstatsplot` is an R package that creates `ggplot2`-based plots with
statistical details included in the plots themselves. It serves as a
visualization frontend for `statsExpressions`.

## Architecture

### Main functions (`R/`)

- Visualization functions: `ggbetweenstats()`, `ggwithinstats()`,
  `gghistostats()`, `ggdotplotstats()`, `ggscatterstats()`, `ggcorrmat()`,
  `ggpiestats()`, `ggbarstats()`, and `ggcoefstats()`.
- Eight visualization functions have `grouped_*` variants that repeat the same
  analysis across a grouping variable. `ggcoefstats()` does not.
- Most statistical plot functions expose a `type` selector built around the
  package's parametric, nonparametric, robust, and Bayesian vocabulary. Check
  the function documentation because the supported analyses vary.
  `ggcoefstats()` instead has model-specific controls such as `effectsize.type`
  and `meta.type`.
- Functions return `ggplot` or patchwork-compatible plot objects with
  statistical annotations.

### Key helper functions

- `extract_stats()`: Extract statistical details from a ggstatsplot object.
- `extract_subtitle()`: Extract the expression in a plot subtitle.
- `extract_caption()`: Extract the expression in a plot caption.
- `theme_ggstatsplot()`: Default theme for plots.
- `combine_plots()`: Combine multiple plots using patchwork.

### Dependencies

Core dependencies include `ggplot2`, `statsExpressions`, the tidyverse stack
(`dplyr`, `purrr`, `tidyr`, and `rlang`), `patchwork`, `paletteer`, and the
easystats ecosystem (`insight`, `parameters`, `performance`, `datawizard`, and
`correlation`). Treat `DESCRIPTION` as the source of truth for dependency
constraints.

The minimum supported R version is 4.5. CI covers R-devel, the current R
release, and the previous R release; keep README support wording independent of
specific version numbers.

## Developer workflow

Use the repository `Makefile` for routine package tasks:

```bash
make install_deps  # Install dependencies declared in DESCRIPTION
make build         # Install dependencies, then build the package tarball
make check         # Build and run R CMD check --no-manual
make install       # Build and install the package locally
make document      # Build, install into .local-lib, and render README.Rmd
make lint          # Run lintr::lint_package()
make format        # Format R code with Air (air format .)
make hooks         # Run all prek hooks on all files
make hooks_install # Install the prek Git hooks
make clean         # Remove package build and check artifacts
make update_deps   # Refresh dependency constraints, docs, and codemeta
```

`make update_deps` is a maintenance operation that can rewrite dependency
constraints and generated metadata. Do not use it merely to install the current
dependency set. To refresh the pinned prek hook revisions, run `prek update`.

### Validation gate

This is the single source of truth for the full local validation gate used by
the repository prompts and skills:

```bash
air format . --check
make lint
make hooks
make check
git diff --check
```

If Air reports drift, run `make format` (or `air format .`), inspect the
result, and rerun the check. CI pins the Air version in the shared
`check-formatting` workflow, so an older local Air can disagree with CI. Run
`make document` when `README.Rmd` or generated README content changes. Run the
narrowest relevant `testthat` file first, for example
`Rscript -e 'testthat::test_local(filter = "ggbetweenstats")'`.

### Versioning and changelog

- Development versions use a fourth-component `.9000` suffix.
- Keep the version in `DESCRIPTION`, `codemeta.json`, and the first `NEWS.md`
  heading synchronized.
- Record user-facing compatibility or behavior changes in `NEWS.md`; omit
  routine dependency, formatting, lint, test, CI, and generated-file
  maintenance.

### Repository skills and prompts

- Use `.agents/skills/create-release/SKILL.md` only when asked to prepare,
  submit, resume, or publish a CRAN release.
- Reusable task prompts live in `.github/prompts/`: `update-deps.md`,
  `address-review.md`, and `simplify-codebase.md`.

### pkgdown site

The site configuration is `pkgdown/_pkgdown.yml`, and web-only articles live in
`vignettes/web_only/`. Build it locally with
`Rscript -e 'pkgdown::build_site()'`; the output in `docs/` is ignored by Git.
The `pkgdown` workflow deploys the site to the `gh-pages` branch from `main`
and on releases.

## Testing

- The package uses `testthat` edition 3 with parallel execution.
- `make check` runs `R CMD check`, including the tests; see the validation gate
  above for the full local gate.
- Tests mirror the relevant source area, but helper and shared source files may
  be covered by broader test files rather than a one-to-one filename match.
- Plot output is covered by `vdiffr` snapshots using
  `expect_doppelganger()` after `vdiffr` is attached by the test helper.
- Treat broad snapshot diffs after dependency updates as potentially
  renderer-specific. Before accepting them, compare the old and new dependency
  builds under the same R and graphics stack and verify whether rendered text,
  statistics, or geometry actually changed.
- If both dependency builds reproduce the same SVG changes, restore the
  repository snapshots instead of committing local renderer churn. Confirm
  legitimate baseline updates with CI-native output across supported platforms.
- The top-level test runner (`tests/testthat.R`) executes package tests only
  with R 4.5 or newer on Linux or macOS because graphics and text rendering
  changed across R versions. The Windows CI job therefore runs `R CMD check`
  without tests.
- Snapshots live in `tests/testthat/_snaps/`. A few plots use `variant =` in
  `expect_doppelganger()` for platform (`darwin/`, `linux/`) or R-version
  (`r-4.7/`) differences; add a variant only when the difference is confirmed
  renderer-specific.
- Failing snapshot tests write `.new.svg` files next to the baselines. Inspect
  them with `testthat::snapshot_review()` in an interactive R session, then
  accept intentional changes with `testthat::snapshot_accept("ggbetweenstats/")`
  or discard them with `testthat::snapshot_reject()`. When local and CI
  renderers disagree, use the snapshot artifact that the R-CMD-check workflow
  uploads on failure.
- Codecov requires 100% project and patch coverage.

When adding visual tests, use the repository's existing style:

```r
test_that("descriptive name", {
  set.seed(123)
  expect_doppelganger(
    title = "descriptive-name",
    fig = function_under_test(data = dataset, x = var1, y = var2)
  )
})
```

## Code conventions

- Use `lintr` (configured in `.lintr`) for linting and Air (configured in
  `air.toml`) for formatting.
- Use snake_case for functions and variables.
- Use the base R pipe (`|>`), not the magrittr pipe (`%>%`).
- Set seeds before tests that use random or Bayesian computations.
- Use `skip_if_not_installed()` for optional dependencies.
- Suppress warnings only when a test intentionally exercises a warning-producing
  path.

### Roxygen documentation

- Roxygen uses Markdown and the `pkgapi` and `roxyglobals` roclets configured in
  `DESCRIPTION`.
- Use `@autoglobal` from `roxyglobals` where appropriate.
- `pkgapi` is not on CRAN; install it with `pak::pak("r-lib/pkgapi")` before
  regenerating documentation.
- Shared documentation lives in two places. `man/rmd-fragments/*.Rmd` are
  pulled into function docs by roxygen Markdown chunks such as
  ```` ```{r child="man/rmd-fragments/ggbetweenstats_graphics.Rmd"} ````.
  `man/md-fragments/*.md` are included by vignettes with
  ```` ```{asis, file="../../man/md-fragments/reporting.md"} ````. Edit the
  fragment, not the generated output.
- After changing roxygen comments, run
  `Rscript -e 'roxygen2::roxygenise()'` and commit the generated `NAMESPACE`,
  `man/*.Rd`, `API` (from `pkgapi`), and `R/globals.R` (from `roxyglobals`)
  changes. Do not edit generated files by hand.
- Regenerate with the roxygen2 release recorded in `Config/roxygen2/version`.
  A newer roxygen2 rewrites that field (and may reformat `NAMESPACE`); commit
  that bump deliberately, together with the regenerated output, rather than
  as incidental churn.
- Several parameters are inherited from `statsExpressions` via
  `@inheritParams`, so the generated `.Rd` text depends on the installed
  `statsExpressions`. Regenerate against its CRAN release, not a local
  development build.
- `make document` renders `README.Rmd`; it is not the roxygen regeneration
  command in this repository.

### Common function parameters

- `data`: Input data frame.
- `x`, `y`: Unquoted column names using tidy evaluation.
- `type`: Usually one of `"parametric"`, `"nonparametric"`, `"robust"`, or
  `"bayes"` where supported.
- `paired`: Whether the design is paired or within-subjects.
- `results.subtitle`: Whether to show statistical results in the subtitle.
- `centrality.plotting`: Whether to show the centrality measure.
- `bf.message`: Whether to show the Bayes factor message in the caption.
- `ggtheme`: The ggplot2 theme to use.
- `palette`: A single `"package::palette"` string understood by `paletteer`
  (default `"ggthemes::gdoc"`).

## Important patterns

### Plot construction

Functions build plots layer by layer with `ggplot2`, add expressions returned by
`statsExpressions`, and finish with `theme_ggstatsplot()` or another supplied
theme.

### Statistical analysis delegation

Statistical computation belongs in `statsExpressions`; plotting functions in
this package should delegate to that backend rather than duplicate statistical
logic.

### Grouped functions

Grouped functions map the corresponding plotting function across groups and
combine the results with patchwork. Follow the existing `purrr` and
`patchwork::wrap_plots()` patterns.

## Files to update together

When modifying a function, consider all relevant surfaces:

1. `R/<function>.R` or its helper file.
2. The corresponding files under `tests/testthat/`.
3. Generated `man/<function>.Rd` after roxygen regeneration.
4. `vignettes/web_only/<function>.Rmd` when that vignette exists.
5. `NEWS.md` for user-facing changes.

## CI/CD

Workflows under `.github/workflows/` are thin callers of reusable workflows in
`IndrajeetPatil/workflows`; update the callers rather than copying those
workflows into this repository. They cover:

- `R-CMD-check`: Ubuntu R-devel, release, and oldrel-1, plus macOS and Windows
  release. Do not reintroduce `oldrel-2` unless the package support policy
  changes.
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

Open pull requests as ready for review rather than as drafts. Unless explicitly
requested, do not wait for CI/CD checks to finish after pushing; report that the
checks were triggered and include the pull request or workflow link.
