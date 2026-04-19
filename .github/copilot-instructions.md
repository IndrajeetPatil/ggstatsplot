# Copilot Instructions for ggstatsplot

## Package Overview

`ggstatsplot` is an R package that creates `ggplot2`-based plots with statistical details included in the plots themselves. It serves as a visualization frontend for `statsExpressions`.

## Architecture

### Main Functions (R/)

- **Visualization functions**: `ggbetweenstats()`, `ggwithinstats()`, `gghistostats()`, `ggdotplotstats()`, `ggscatterstats()`, `ggcorrmat()`, `ggpiestats()`, `ggbarstats()`, `ggcoefstats()`
- **Grouped variants**: `grouped_ggbetweenstats()`, etc. - repeat same analysis across grouping variable
- **Each function supports 4 types** via `type` parameter: `"parametric"`, `"nonparametric"`, `"robust"`, `"bayes"`
- **Output**: All functions return ggplot objects with statistical annotations

### Key Helper Functions

- `extract_stats()`: Extract statistical details from a ggstatsplot object
- `extract_subtitle()`: Extract expression from subtitle
- `extract_caption()`: Extract expression from caption
- `theme_ggstatsplot()`: Default theme for plots
- `combine_plots()`: Combine multiple plots using patchwork

### Dependencies

Core: `ggplot2`, `statsExpressions`, tidyverse stack (`dplyr`, `purrr`, `tidyr`, `rlang`), `patchwork`, `paletteer`, easystats ecosystem (`insight`, `parameters`, `performance`, `datawizard`, `correlation`)

## Developer Workflow

### Common Commands (via Makefile)

```bash
make document     # Generate documentation (roxygen2)
make build        # Build package tarball
make check        # Run R CMD check
make install      # Install package locally
make lint         # Run lintr checks
make format       # Format code with styler
make install_deps # Install/update dependencies
```

### Testing

- Framework: `testthat` (edition 3) with parallel execution
- Run tests: `devtools::test()` or `make check`
- **Snapshot tests**: Used for plot output verification via `vdiffr`
- Test files mirror source files: `R/ggbetweenstats.R` -> `tests/testthat/test-ggbetweenstats.R`
- Coverage requirement: **100%** (enforced by codecov)

### Adding New Tests

```r
test_that("descriptive name", {
  set.seed(123)
  p <- function_under_test(data = dataset, x = var1, y = var2)
  expect_s3_class(p, "ggplot")
  vdiffr::expect_doppelganger("descriptive-name", p)
})
```

## Code Conventions

### Style

- **Linting**: Uses `lintr::all_linters()` with exceptions in `.lintr`
- **Formatting**: `styler::style_pkg()` (run via `make format`)
- **Naming**: snake_case for functions and variables
- **Pipes**: Use base R `|>` pipe (NOT magrittr `%>%`)

### Roxygen Documentation

- Uses `roxygen2` with markdown support (`Roxygen: list(markdown = TRUE)`)
- Uses `@autoglobal` annotation from `roxyglobals` for global variables
- Template files in `man-roxygen/` for shared documentation
- Run `make document` after modifying roxygen comments

### Function Parameters

Common parameters across functions:

- `data`: Input dataframe
- `x`, `y`: Column names (unquoted, uses tidy evaluation)
- `type`: One of `"parametric"`, `"nonparametric"`, `"robust"`, `"bayes"`
- `paired`: Logical for paired/within-subjects designs
- `results.subtitle`: Whether to show statistical results in subtitle
- `centrality.plotting`: Whether to show centrality measure
- `bf.message`: Whether to show Bayes factor message in caption
- `ggtheme`: ggplot2 theme to use
- `palette`, `package`: Color palette specifications

## Important Patterns

### Plot Construction

Functions build plots layer by layer using ggplot2:

```r
ggplot(data, aes(x = x, y = y)) +
  geom_violin() +
  geom_boxplot() +
  labs(subtitle = stats_expression, caption = bf_expression) +
  theme_ggstatsplot()
```

### Statistical Analysis Delegation

All statistical computations are delegated to `statsExpressions`:

```r
statsExpressions::two_sample_test(data, x, y, type = type)
```

### Grouped Functions

Use `purrr::pmap()` to apply function across groups:

```r
purrr::pmap(list_of_args, ggbetweenstats) |>
  patchwork::wrap_plots()
```

## Testing Tips

- Set seeds before tests: `set.seed(123)`
- Use `skip_if_not_installed()` for optional dependencies
- Use `vdiffr::expect_doppelganger()` for visual regression tests
- Use `suppressWarnings()` when tests intentionally trigger warnings

## Files to Update Together

When modifying a function:

1. `R/<function>.R` - Source code
2. `man/<function>.Rd` - Will auto-generate from roxygen
3. `tests/testthat/test-<function>.R` - Tests
4. `vignettes/web_only/<function>.Rmd` - Vignette (if exists)
5. `NEWS.md` - Document user-facing changes

## CI/CD

- GitHub Actions workflows in `.github/workflows/`
- Automated R CMD check on multiple platforms
- Code coverage uploaded to Codecov (100% required)
- Visual regression tests with vdiffr
- Dependency updates via Dependabot
