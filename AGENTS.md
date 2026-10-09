# AGENTS.md

Project-level instructions for AI coding agents working on this
repository. GitHub Copilot Code Review, Copilot coding agent, Codex, and
other `AGENTS.md`-aware tools read this file directly.

## Package overview

`ggstatsplot` is an R package that creates `ggplot2`-based plots with
statistical details included in the plots themselves. It serves as a
visualization frontend for `statsExpressions`.

## Architecture

### Main functions (`R/`)

- Visualization functions:
  [`ggbetweenstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggbetweenstats.md),
  [`ggwithinstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggwithinstats.md),
  [`gghistostats()`](https://www.indrapatil.com/ggstatsplot/reference/gghistostats.md),
  [`ggdotplotstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggdotplotstats.md),
  [`ggscatterstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggscatterstats.md),
  [`ggcorrmat()`](https://www.indrapatil.com/ggstatsplot/reference/ggcorrmat.md),
  [`ggpiestats()`](https://www.indrapatil.com/ggstatsplot/reference/ggpiestats.md),
  [`ggbarstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggbarstats.md),
  and
  [`ggcoefstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggcoefstats.md).
- Eight visualization functions have `grouped_*` variants that repeat
  the same analysis across a grouping variable.
  [`ggcoefstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggcoefstats.md)
  does not.
- Most statistical plot functions expose a `type` selector built around
  the package’s parametric, nonparametric, robust, and Bayesian
  vocabulary. Check the function documentation because the supported
  analyses vary.
  [`ggcoefstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggcoefstats.md)
  instead has model-specific controls such as `effectsize.type` and
  `meta.type`.
- Functions return `ggplot` or patchwork-compatible plot objects with
  statistical annotations.

### Key helper functions

- [`extract_stats()`](https://www.indrapatil.com/ggstatsplot/reference/extract_stats.md):
  Extract statistical details from a ggstatsplot object.
- [`extract_subtitle()`](https://www.indrapatil.com/ggstatsplot/reference/extract_stats.md):
  Extract the expression in a plot subtitle.
- [`extract_caption()`](https://www.indrapatil.com/ggstatsplot/reference/extract_stats.md):
  Extract the expression in a plot caption.
- [`theme_ggstatsplot()`](https://www.indrapatil.com/ggstatsplot/reference/theme_ggstatsplot.md):
  Default theme for plots.
- [`combine_plots()`](https://www.indrapatil.com/ggstatsplot/reference/combine_plots.md):
  Combine multiple plots using patchwork.

### Dependencies

Core dependencies include `ggplot2`, `statsExpressions`, the tidyverse
stack (`dplyr`, `purrr`, `tidyr`, and `rlang`), `patchwork`,
`paletteer`, and the easystats ecosystem (`insight`, `parameters`,
`performance`, `datawizard`, and `correlation`). Treat `DESCRIPTION` as
the source of truth for dependency constraints and the minimum supported
R version.

## Developer workflow

Use the repository `Makefile` for routine package tasks:

``` bash
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
make update_deps   # Refresh dependency constraints (maintenance only)
```

### Validation gate

This is the single source of truth for the full local validation gate
used by the repository prompts and skills:

``` bash
air format . --check
make lint
make hooks
make check
git diff --check
```

If Air reports drift, run `make format` (or `air format .`), inspect the
result, and rerun the check. Run `make document` when `README.Rmd` or
generated README content changes. Run the narrowest relevant `testthat`
file first, for example
`Rscript -e 'testthat::test_local(filter = "ggbetweenstats")'`.

### Versioning and changelog

- Development versions use a fourth-component `.9000` suffix.
- Keep the version in `DESCRIPTION`, `codemeta.json`, and the first
  `NEWS.md` heading synchronized.
- Record user-facing compatibility or behavior changes in `NEWS.md`;
  omit routine dependency, formatting, lint, test, CI, and
  generated-file maintenance.

### pkgdown site

The site configuration is `pkgdown/_pkgdown.yml`, and web-only articles
live in `vignettes/web_only/`. Build it locally with
`Rscript -e 'pkgdown::build_site()'`; the output in `docs/` is ignored
by Git.

## Testing

- The package uses `testthat` edition 3 with parallel execution.
- `make check` runs `R CMD check`, including the tests; see the
  validation gate above for the full local gate.
- Tests mirror the relevant source area, but helper and shared source
  files may be covered by broader test files rather than a one-to-one
  filename match.
- Plot output is covered by `vdiffr` snapshots using
  `expect_doppelganger()` after `vdiffr` is attached by the test helper.
- Treat broad snapshot diffs after dependency updates as potentially
  renderer-specific. Before accepting them, compare the old and new
  dependency builds under the same R and graphics stack and verify
  whether rendered text, statistics, or geometry actually changed.
- If both dependency builds reproduce the same SVG changes, restore the
  repository snapshots instead of committing local renderer churn.
  Confirm legitimate baseline updates with CI-native output across
  supported platforms.
- The top-level test runner (`tests/testthat.R`) executes package tests
  only with R 4.5 or newer on Linux or macOS because graphics and text
  rendering changed across R versions.
- Snapshots live in `tests/testthat/_snaps/`. A few plots use
  `variant =` in `expect_doppelganger()` for platform (`darwin/`,
  `linux/`) or R-version (`r-4.7/`) differences; add a variant only when
  the difference is confirmed renderer-specific.
- Failing snapshot tests write `.new.svg` files next to the baselines.
  Inspect them with
  [`testthat::snapshot_review()`](https://testthat.r-lib.org/reference/snapshot_accept.html)
  in an interactive R session, then accept intentional changes with
  `testthat::snapshot_accept("ggbetweenstats/")` or discard them with
  [`testthat::snapshot_reject()`](https://testthat.r-lib.org/reference/snapshot_accept.html).
  When local and CI renderers disagree, use the snapshot artifact that
  the R-CMD-check workflow uploads on failure.
- Codecov requires 100% project and patch coverage.

When adding visual tests, use the repository’s existing style:

\
`test_that``(``"descriptive name"``, ``{`\
`  `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
`  ``expect_doppelganger``(`\
`    title ``=`` ``"descriptive-name"``,`\
`    fig ``=`` ``function_under_test``(``data ``=`` ``dataset``, x ``=`` ``var1``, y ``=`` ``var2``)`\
`  ``)`\
`}``)`

## Code conventions

- Use `lintr` (configured in `.lintr`) for linting and Air (configured
  in `air.toml`) for formatting.
- Use snake_case for functions and variables.
- Use the base R pipe (`|>`), not the magrittr pipe (`%>%`).
- Set seeds before tests that use random or Bayesian computations.
- Use `skip_if_not_installed()` for optional dependencies.
- Suppress warnings only when a test intentionally exercises a
  warning-producing path.

### Roxygen documentation

- Roxygen uses Markdown and the `pkgapi` and `roxyglobals` roclets
  configured in `DESCRIPTION`.
- Use `@autoglobal` from `roxyglobals` where appropriate.
- `pkgapi` is not on CRAN; install it with `pak::pak("r-lib/pkgapi")`
  before regenerating documentation.
- Shared documentation lives in two places. `man/rmd-fragments/*.Rmd`
  are pulled into function docs by roxygen Markdown chunks such as
  ```` ```{r child="man/rmd-fragments/ggbetweenstats_graphics.Rmd"} ````.
  `man/md-fragments/*.md` are included by vignettes with
  ```` ```{asis, file="../../man/md-fragments/reporting.md"} ````. The
  per-function articles in `vignettes/web_only/` include the graphics
  tables from `man/rmd-fragments/` the same way. Use `asis` file chunks
  rather than knitr `child=` in articles: pkgdown renders them from a
  temporary file, so relative child paths do not resolve. Edit the
  fragment, not the generated output.
- After changing roxygen comments, run
  `Rscript -e 'roxygen2::roxygenise()'` and commit the generated
  `NAMESPACE`, `man/*.Rd`, `API` (from `pkgapi`), and `R/globals.R`
  (from `roxyglobals`) changes. Do not edit generated files by hand.
- Regenerate with the roxygen2 release recorded in
  `Config/roxygen2/version`. A newer roxygen2 rewrites that field (and
  may reformat `NAMESPACE`); commit that bump deliberately, together
  with the regenerated output, rather than as incidental churn.
- Several parameters are inherited from `statsExpressions` via
  `@inheritParams`, so the generated `.Rd` text depends on the installed
  `statsExpressions`. Regenerate against its CRAN release, not a local
  development build.
- `make document` renders `README.Rmd`; it is not the roxygen
  regeneration command in this repository.

### Common function parameters

- `data`: Input data frame.
- `x`, `y`: Unquoted column names using tidy evaluation.
- `type`: Usually one of `"parametric"`, `"nonparametric"`, `"robust"`,
  or `"bayes"` where supported.
- `paired`: Whether the design is paired or within-subjects.
- `results.subtitle`: Whether to show statistical results in the
  subtitle.
- `centrality.plotting`: Whether to show the centrality measure.
- `bf.message`: Whether to show the Bayes factor message in the caption.
- `ggtheme`: The ggplot2 theme to use.
- `palette`: A single `"package::palette"` string understood by
  `paletteer` (default `"ggthemes::gdoc"`).

## Important patterns

### Plot construction

Functions build plots layer by layer with `ggplot2`, add expressions
returned by `statsExpressions`, and finish with
[`theme_ggstatsplot()`](https://www.indrapatil.com/ggstatsplot/reference/theme_ggstatsplot.md)
or another supplied theme.

### Statistical analysis delegation

Statistical computation belongs in `statsExpressions`; plotting
functions in this package should delegate to that backend rather than
duplicate statistical logic.

### Grouped functions

Grouped functions map the corresponding plotting function across groups
and combine the results with patchwork. Follow the existing `purrr` and
[`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
patterns.

## Files to update together

When modifying a function, consider all relevant surfaces:

1.  `R/<function>.R` or its helper file.
2.  The corresponding files under `tests/testthat/`.
3.  Generated `man/<function>.Rd` after roxygen regeneration.
4.  `vignettes/web_only/<function>.Rmd` when that vignette exists.
5.  `NEWS.md` for user-facing changes.

## Repository skills

Task-specific instructions live in `.agents/skills/`. Read a skill only
when the task matches it:

- `create-release`: prepare, submit, resume, or publish a CRAN release.
- `update-dependencies`: update dependencies to their latest versions,
  change the minimum R version, or add, remove, or move a dependency.
- `maintain-ci`: change or debug workflows under `.github/workflows/`.

User-invoked prompts for other tasks (`address-review.md` and
`simplify-codebase.md`) live in `.github/prompts/`. Keep each topic in
exactly one place: `AGENTS.md` for every-session rules, a skill or a
prompt for task-specific procedures.

## Pull requests

Open pull requests as ready for review rather than as drafts. Unless
explicitly requested, do not wait for CI/CD checks to finish after
pushing; report that the checks were triggered and include the pull
request or workflow link.
