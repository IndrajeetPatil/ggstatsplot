# Grouped scatterplots with marginal distributions and statistical results

Applies
[`ggscatterstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggscatterstats.md)
across all levels of `grouping.var` and combines the resulting plots
using
[`combine_plots()`](https://www.indrapatil.com/ggstatsplot/reference/combine_plots.md).

## Usage

``` r
grouped_ggscatterstats(
  data,
  ...,
  grouping.var,
  plotgrid.args = list(),
  annotation.args = list()
)
```

## Arguments

- data:

  A data frame (or a tibble) from which variables specified are to be
  taken. Other data types (e.g., matrix, table, array, etc.) will
  **not** be accepted. Additionally, grouped data frames from `{dplyr}`
  should be ungrouped before they are entered as `data`.

- ...:

  Arguments passed on to
  [`ggscatterstats`](https://www.indrapatil.com/ggstatsplot/reference/ggscatterstats.md)

  `label.var`

  : Variable to use for point labels entered as a symbol (e.g. `var1`).

  `label.expression`

  : An expression evaluating to a logical vector that determines the
    subset of data points to label (e.g. `y < 4 & z < 20`). While using
    this argument with
    [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html),
    you will have to provide a quoted expression (e.g.
    `quote(y < 4 & z < 20)`).

  `point.label.args`

  : A list of additional aesthetic arguments passed to
    [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html),
    which displays the point labels.

  `smooth.line.args`

  : A list of additional aesthetic arguments passed to
    [`ggplot2::geom_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.html),
    which displays the regression line. Its confidence band uses
    `conf.level`.

  `marginal`

  : Logical that decides whether marginal distributions are plotted on
    the axes using `{ggside}` (Default: `TRUE`).

  `point.width.jitter,point.height.jitter`

  : Amount of jitter (in data units) in `x` and `y` direction,
    respectively, passed to
    [`ggplot2::position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html)
    (Default: `0`, i.e., no jitter). Do not specify the jitter in
    `point.args`, because it is shared by the **points** and their
    **labels**.

  `xsidehistogram.args,ysidehistogram.args`

  : A list of arguments passed to
    [`ggside::geom_xsidehistogram()`](https://rdrr.io/pkg/ggside/man/geom_xsidehistogram.html)
    and
    [`ggside::geom_ysidehistogram()`](https://rdrr.io/pkg/ggside/man/geom_xsidehistogram.html),
    respectively, to change the marginal histograms.

  `xsidehistogram.scale,ysidehistogram.scale`

  : A list of arguments passed to
    [`ggside::scale_xsidey_continuous()`](https://rdrr.io/pkg/ggside/man/ggside-scales-continuous.html)
    and
    [`ggside::scale_ysidex_continuous()`](https://rdrr.io/pkg/ggside/man/ggside-scales-continuous.html),
    respectively, to control the scale of marginal histograms (e.g.,
    `breaks`, `limits`, `transform`). Default is
    [`list()`](https://rdrr.io/r/base/list.html) (no modifications).

  `x`

  : The column in `data` containing the explanatory variable to be
    plotted on the `x`-axis.

  `y`

  : The column in `data` containing the response (outcome) variable to
    be plotted on the `y`-axis.

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

  `ylab`

  : Label for `y` axis variable. If `NULL` (default), variable name for
    `y` will be used.

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

  `point.args`

  : A list of additional aesthetic arguments passed to
    [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

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

- grouping.var:

  A single grouping variable. A separate plot is created for each of its
  levels (in factor level order, or in order of appearance for character
  variables). Rows with a missing value in this variable are removed.

- plotgrid.args:

  A `list` of additional arguments passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html),
  except for `guides`, which has its own argument.

- annotation.args:

  A `list` of additional arguments passed to
  [`patchwork::plot_annotation()`](https://patchwork.data-imaginist.com/reference/plot_annotation.html).

## Value

A `patchwork` object combining one plot per level of `grouping.var` (see
[`combine_plots()`](https://www.indrapatil.com/ggstatsplot/reference/combine_plots.md)).
Applying
[`extract_stats()`](https://www.indrapatil.com/ggstatsplot/reference/extract_stats.md)
to it returns a list with the statistical details for each of these
plots.

## Details

Rows with a missing value in either `x` or `y` are removed before
plotting and analysis.

For details, see:
<https://www.indrapatil.com/ggstatsplot/articles/web_only/ggscatterstats.html>

## See also

[`ggscatterstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggscatterstats.md),
[`ggcorrmat()`](https://www.indrapatil.com/ggstatsplot/reference/ggcorrmat.md),
[`grouped_ggcorrmat()`](https://www.indrapatil.com/ggstatsplot/reference/grouped_ggcorrmat.md)

## Examples

``` r
# for reproducibility
set.seed(123)

library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

grouped_ggscatterstats(
  data             = filter(movies_long, genre == "Comedy" | genre == "Drama"),
  x                = length,
  y                = rating,
  type             = "robust",
  grouping.var     = genre,
  ggplot.component = list(geom_rug(sides = "b"))
)


# label points matching an expression
grouped_ggscatterstats(
  data             = filter(ggplot2::mpg, cyl != 5),
  x                = displ,
  y                = hwy,
  grouping.var     = cyl,
  type             = "robust",
  label.var        = manufacturer,
  label.expression = hwy > 25 & displ > 2.5,
  ggplot.component = scale_y_continuous(sec.axis = dup_axis())
)


# label all points
grouped_ggscatterstats(
  data            = filter(movies_long, rating == 7, genre %in% c("Drama", "Comedy")),
  x               = budget,
  y               = length,
  grouping.var    = genre,
  bf.message      = FALSE,
  label.var       = "title",
  annotation.args = list(tag_levels = "a")
)
```
