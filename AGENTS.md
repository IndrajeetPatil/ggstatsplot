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
the source of truth for dependency constraints.

## Developer workflow

Use the repository `Makefile` for routine package tasks:

``` bash
make install_deps # Install dependencies declared in DESCRIPTION
make build        # Build the package tarball
make check        # Build and run R CMD check --no-manual
make install      # Build and install the package locally
make document     # Build, install into .local-lib, and render README.Rmd
make lint         # Run lintr::lint_package()
make format       # Run styler::style_pkg()
make hooks        # Run all prek hooks
make clean        # Remove package build and check artifacts
make update_deps  # Refresh dependency constraints, docs, and codemeta
```

`make update_deps` is a maintenance operation that can rewrite
dependency constraints and generated metadata. Do not use it merely to
install the current dependency set.

## Testing

- The package uses `testthat` edition 3 with parallel execution.
- `make check` is the canonical full local validation command.
- Tests mirror the relevant source area, but helper and shared source
  files may be covered by broader test files rather than a one-to-one
  filename match.
- Plot output is covered by `vdiffr` snapshots using
  `expect_doppelganger()` after `vdiffr` is attached by the test helper.
- The top-level test runner executes package tests only with R 4.5 or
  newer on Linux or macOS because graphics and text rendering changed
  across R versions.
- Codecov requires 100% project and patch coverage.

When adding visual tests, use the repository’s existing style:

``` r

test_that("descriptive name", {
  set.seed(123)
  expect_doppelganger(
    title = "descriptive-name",
    fig = function_under_test(data = dataset, x = var1, y = var2)
  )
})
```

## Code conventions

- Use `lintr` for linting and `styler` for formatting.
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
- Shared documentation lives in `man/md-fragments/` and
  `man/rmd-fragments/`.
- After changing roxygen comments, run
  `Rscript -e 'roxygen2::roxygenise()'` and commit the generated
  `NAMESPACE` or `man/*.Rd` changes. Do not edit generated `.Rd` files
  by hand.
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
- `palette`, `package`: Color palette specifications.

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

## CI/CD

Workflows under `.github/workflows/` run standard and hard R CMD checks,
coverage, documentation and extra checks, formatting, linting, prek
hooks, pkgdown builds, and deployment tasks. Most jobs call reusable
workflows from `IndrajeetPatil/workflows`; update the callers rather
than copying those workflows into this repository.
