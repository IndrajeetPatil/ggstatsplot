# Contributing to ggstatsplot

Thanks for taking the time to contribute.

## Reporting bugs and requesting features

Open an [issue](https://github.com/IndrajeetPatil/ggstatsplot/issues) with a
minimal reproducible example (ideally created with the
[`reprex`](https://reprex.tidyverse.org/) package) and the output of
`sessionInfo()`.

`ggstatsplot` is a plotting frontend: the statistical results shown in the
subtitles and captions are computed by
[`statsExpressions`](https://github.com/IndrajeetPatil/statsExpressions). If a
reported statistic itself is wrong, that package is usually the right place for
the issue.

## Pull requests

For anything beyond a small fix, open an issue first to discuss the change.

1. Use R 4.5 or newer, and install the development dependencies with
   `make install_deps`. Regenerating documentation also needs
   [`pkgapi`](https://github.com/r-lib/pkgapi), which is not on CRAN:
   `pak::pak("r-lib/pkgapi")`.
2. Install [Air](https://posit-dev.github.io/air/) for formatting and
   [prek](https://prek.j178.dev/) for Git hooks (`make hooks_install`).
3. Add or update tests under `tests/testthat/`, including `vdiffr` snapshots
   for plot changes, and add a `NEWS.md` entry for user-facing changes.
4. Run the validation gate before opening the pull request.

[`AGENTS.md`](../AGENTS.md) is the single source of truth for repository
conventions, the validation gate, snapshot handling, roxygen documentation,
and CI. It applies to human contributors as well as AI coding agents.
