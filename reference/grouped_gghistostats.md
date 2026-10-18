# Grouped histograms for distribution of a numeric variable

Applies
[`gghistostats()`](https://www.indrapatil.com/ggstatsplot/reference/gghistostats.md)
across all levels of `grouping.var` and combines the resulting plots
using
[`combine_plots()`](https://www.indrapatil.com/ggstatsplot/reference/combine_plots.md).

## Usage

``` r
grouped_gghistostats(
  data,
  x,
  grouping.var,
  binwidth = NULL,
  plotgrid.args = list(),
  annotation.args = list(),
  ...
)
```

## Arguments

- data:

  A data frame (or a tibble) from which variables specified are to be
  taken. Other data types (e.g., matrix, table, array, etc.) will
  **not** be accepted. Additionally, grouped data frames from `{dplyr}`
  should be ungrouped before they are entered as `data`.

- x:

  A numeric variable from the data frame `data`.

- grouping.var:

  A single grouping variable. A separate plot is created for each of its
  levels (in factor level order, or in order of appearance for character
  variables). Rows with a missing value in this variable are removed.

- binwidth:

  The width of the histogram bins (see
  [`gghistostats()`](https://www.indrapatil.com/ggstatsplot/reference/gghistostats.md)).
  If `NULL` (default), it is computed once from the non-missing values
  of `x` in the full data (not per group), so that all plots share the
  same bin width.

- plotgrid.args:

  A `list` of additional arguments passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html),
  except for `guides`, which has its own argument.

- annotation.args:

  A `list` of additional arguments passed to
  [`patchwork::plot_annotation()`](https://patchwork.data-imaginist.com/reference/plot_annotation.html).

- ...:

  Arguments passed on to
  [`gghistostats`](https://www.indrapatil.com/ggstatsplot/reference/gghistostats.md)

  `bin.args`

  : A list of additional aesthetic arguments passed to
    [`ggplot2::stat_bin()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html),
    which displays the bins. Use the dedicated `binwidth` argument (not
    this list) to set the bin width.

  `centrality.line.args`

  : A list of additional aesthetic arguments passed to
    [`ggplot2::geom_vline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html),
    which displays the line for the centrality measure.

  `type`

  : A character specifying the type of statistical approach:

    - `"parametric"`

    - `"nonparametric"`

    - `"robust"`

    - `"bayes"`

    You can specify just the initial letter (e.g. `"np"` or `"bf"`).
    Matching is on the initial lowercase letter only, so any other value
    (including capitalized values such as `"Bayes"`) falls back to
    `"parametric"` without a warning.

  `test.value`

  : A number indicating the true value of the mean (Default: `0`).

  `alternative`

  : a character string specifying the alternative hypothesis, must be
    one of `"two.sided"` (default), `"greater"` or `"less"`. You can
    specify just the initial letter.

  `digits`

  : Number of digits for rounding or significant figures. May also be
    `"signif"` to return significant figures or `"scientific"` to return
    scientific notation. Control the number of digits by adding the
    value as suffix, e.g. `digits = "scientific4"` to have scientific
    notation with 4 decimal places, or `digits = "signif5"` for 5
    significant figures (see also
    [`signif()`](https://rdrr.io/r/base/Round.html)).

  `conf.level`

  : Scalar between `0` and `1` (default: `95%` confidence/credible
    intervals, `0.95`).

  `tr`

  : Trim level for the mean when carrying out `robust` tests. In case of
    an error, try reducing the value of `tr`, which is by default set to
    `0.2`. Lowering the value might help.

  `bf.prior`

  : A number between `0.5` and `2` (default `0.707`), the prior width to
    use in calculating Bayes factors and posterior estimates. In
    addition to numeric arguments, several named values are also
    recognized: `"medium"`, `"wide"`, and `"ultrawide"`, corresponding
    to *r* scale values of `1/2`, `sqrt(2)/2`, and `1`, respectively. In
    case of an ANOVA, this value corresponds to scale for fixed effects.

  `xlab`

  : Label for `x` axis variable. If `NULL` (default), variable name for
    `x` will be used.

  `bf.message`

  : Logical that decides whether to display the Bayes Factor in favor of
    the *null* hypothesis as a caption. Relevant only for parametric
    tests (Default: `TRUE`).

  `results.subtitle`

  : Logical that decides whether the results of statistical tests are
    displayed as a subtitle (Default: `TRUE`). Setting this to `FALSE`
    also skips the automatic Bayes Factor caption. Other plot details
    have their own arguments.

  `subtitle`

  : The text for the plot subtitle. Used only if
    `results.subtitle = FALSE`.

  `caption`

  : The text for the plot caption. It will be replaced by the Bayes
    Factor caption whenever that is displayed (see `bf.message`).

  `centrality.plotting`

  : Logical that decides whether central tendency measure is to be
    displayed as a point with a label (Default: `TRUE`). Function
    decides which central tendency measure to show depending on the
    `type` argument.

    - **mean** for parametric statistics

    - **median** for non-parametric statistics

    - **trimmed mean** for robust statistics

    - **MAP estimator** for Bayesian statistics

    Use `centrality.type` to choose a different centrality measure.

  `centrality.type`

  : Decides which centrality measure is displayed. Defaults to the same
    value as `type`. Can be one of:

    - `"parametric"` (for **mean**)

    - `"nonparametric"` (for **median**)

    - `"robust"` (for **trimmed mean**)

    - `"bayes"` (for **MAP estimator**)

    As with `type`, abbreviations are accepted.

  `ggplot.component`

  : A `{ggplot2}` component, or a list of components, to add to the plot
    (Default: `NULL`). This is mostly helpful for the `grouped_`
    variants, where it is applied to each individual plot.

  `ggtheme`

  : A `{ggplot2}` theme (Default:
    [`theme_ggstatsplot()`](https://www.indrapatil.com/ggstatsplot/reference/theme_ggstatsplot.md)).
    Any `{ggplot2}` theme (e.g.,
    [`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html))
    or theme from an extension package (e.g.,
    `ggthemes::theme_fivethirtyeight()`) can be used. Note that some
    themes remove plot details. For example,
    `ggthemes::theme_fivethirtyeight()` removes the secondary Y-axis,
    which
    [`ggbetweenstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggbetweenstats.md)
    uses to show details about pairwise comparisons.

## Value

A `patchwork` object combining one plot per level of `grouping.var` (see
[`combine_plots()`](https://www.indrapatil.com/ggstatsplot/reference/combine_plots.md)).
Applying
[`extract_stats()`](https://www.indrapatil.com/ggstatsplot/reference/extract_stats.md)
to it returns a list with the statistical details for each of these
plots.

## Details

Missing values in `x` are removed before plotting and analysis.

For details, see:
<https://www.indrapatil.com/ggstatsplot/articles/web_only/gghistostats.html>

## See also

[`gghistostats()`](https://www.indrapatil.com/ggstatsplot/reference/gghistostats.md),
[`ggdotplotstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggdotplotstats.md),
[`grouped_ggdotplotstats()`](https://www.indrapatil.com/ggstatsplot/reference/grouped_ggdotplotstats.md)

## Examples

``` r
# for reproducibility
set.seed(123)

grouped_gghistostats(
  data            = iris,
  x               = Sepal.Length,
  test.value      = 5,
  grouping.var    = Species,
  plotgrid.args   = list(nrow = 1),
  annotation.args = list(tag_levels = "i")
)
```
