# ggstatsplot: 'ggplot2' Based Plots with Statistical Details

`{ggstatsplot}` is an extension of `{ggplot2}` that creates graphics
with details from statistical tests included in the plots themselves. It
provides an easier API to generate information-rich plots for
statistical analysis of continuous (box/violin plots, scatterplots,
histograms, dot plots, dot-and-whisker plots) or categorical (pie and
bar charts) data. It supports the most common types of statistical
tests: parametric, nonparametric, robust, and Bayesian versions of
*t*-test/ANOVA, correlation analyses, contingency table analysis,
meta-analysis, and regression analyses.

## Details

The main functions are:

- [`ggbetweenstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggbetweenstats.md):
  box/violin plots for comparisons *between* groups or conditions.

- [`ggwithinstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggwithinstats.md):
  box/violin plots for comparisons *within* groups or conditions
  (repeated measures).

- [`ggscatterstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggscatterstats.md):
  scatterplots with marginal distributions from `{ggside}`.

- [`ggpiestats()`](https://www.indrapatil.com/ggstatsplot/reference/ggpiestats.md)
  and
  [`ggbarstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggbarstats.md):
  pie charts and stacked bar charts for categorical data.

- [`gghistostats()`](https://www.indrapatil.com/ggstatsplot/reference/gghistostats.md):
  histograms for a single numeric variable, with results from a
  one-sample test.

- [`ggdotplotstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggdotplotstats.md):
  Cleveland-style dot plots for labeled numeric data, with results from
  a one-sample test.

- [`ggcorrmat()`](https://www.indrapatil.com/ggstatsplot/reference/ggcorrmat.md):
  correlation matrices.

- [`ggcoefstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggcoefstats.md):
  dot-and-whisker plots for regression analyses.

All of them except
[`ggcoefstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggcoefstats.md)
have `grouped_` variants that repeat the analysis across the levels of a
grouping variable and combine the plots with
[`combine_plots()`](https://www.indrapatil.com/ggstatsplot/reference/combine_plots.md).

References: Patil (2021)
[doi:10.21105/joss.03167](https://doi.org/10.21105/joss.03167) .

For more documentation, see the dedicated
[Website](https://www.indrapatil.com/ggstatsplot/).

## See also

Useful links:

- <https://www.indrapatil.com/ggstatsplot/>

- <https://github.com/IndrajeetPatil/ggstatsplot>

- Report bugs at <https://github.com/IndrajeetPatil/ggstatsplot/issues>

## Author

**Maintainer**: Indrajeet Patil <patilindrajeet.science@gmail.com>
([ORCID](https://orcid.org/0000-0003-1995-6531)) \[copyright holder\]

Authors:

- Indrajeet Patil <patilindrajeet.science@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-1995-6531)) \[copyright holder\]
