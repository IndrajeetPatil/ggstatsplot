# ggstatsplot: 'ggplot2' Based Plots with Statistical Details

`{ggstatsplot}` is an extension of `{ggplot2}` package. It creates
graphics with details from statistical tests included in the plots
themselves. It provides an easier `API` to generate information-rich
plots for statistical analysis of continuous (violin plots,
scatterplots, histograms, dot plots, dot-and-whisker plots) or
categorical (pie and bar charts) data. Currently, it supports the most
common types of statistical tests: parametric, nonparametric, robust,
and Bayesian versions of *t*-test/ANOVA, correlation analyses,
contingency table analysis, meta-analysis, and regression analyses.

## Details

`ggstatsplot`

The main functions are:

- [`ggbetweenstats()`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.md)
  function to produce information-rich comparison plot *between*
  different groups or conditions with `{ggplot2}` and details from the
  statistical tests in the subtitle.

- [`ggwithinstats()`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggwithinstats.md)
  function to produce information-rich comparison plot *within*
  different groups or conditions with `{ggplot2}` and details from the
  statistical tests in the subtitle.

- [`ggscatterstats()`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggscatterstats.md)
  function to produce `{ggplot2}` scatterplots along with a marginal
  distribution plots from `{ggside}` package and details from the
  statistical tests in the subtitle.

- [`ggpiestats()`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.md)
  function to produce pie chart with details from the statistical tests
  in the subtitle.

- [`ggbarstats()`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggbarstats.md)
  function to produce stacked bar chart with details from the
  statistical tests in the subtitle.

- [`gghistostats()`](https://indrajeetpatil.github.io/ggstatsplot/reference/gghistostats.md)
  function to produce histogram for a single variable with results from
  one sample test displayed in the subtitle.

- [`ggdotplotstats()`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggdotplotstats.md)
  function to produce Cleveland-style dot plots/charts for a single
  variable with labels and results from one sample test displayed in the
  subtitle.

- [`ggcorrmat()`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.md)
  function to visualize the correlation matrix.

- [`ggcoefstats()`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggcoefstats.md)
  function to visualize results from regression analyses.

- [`combine_plots()`](https://indrajeetpatil.github.io/ggstatsplot/reference/combine_plots.md)
  helper function to combine multiple `{ggstatsplot}` plots using
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).

References: Patil (2021)
[doi:10.21105/joss.03236](https://doi.org/10.21105/joss.03236) .

For more documentation, see the dedicated
[Website](https://indrajeetpatil.github.io/ggstatsplot/).

## See also

Useful links:

- <https://indrajeetpatil.github.io/ggstatsplot/>

- <https://github.com/IndrajeetPatil/ggstatsplot>

- Report bugs at <https://github.com/IndrajeetPatil/ggstatsplot/issues>

## Author

**Maintainer**: Indrajeet Patil <patilindrajeet.science@gmail.com>
([ORCID](https://orcid.org/0000-0003-1995-6531)) \[copyright holder\]
