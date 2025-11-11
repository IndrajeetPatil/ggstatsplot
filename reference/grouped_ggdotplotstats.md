# Grouped histograms for distribution of a labeled numeric variable

Helper function for
[`ggstatsplot::ggdotplotstats()`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggdotplotstats.md)
to apply this function across multiple levels of a given factor and
combining the resulting plots using
[`ggstatsplot::combine_plots()`](https://indrajeetpatil.github.io/ggstatsplot/reference/combine_plots.md).

## Usage

``` r
grouped_ggdotplotstats(
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
  taken. Other data types (e.g., matrix,table, array, etc.) will **not**
  be accepted. Additionally, grouped data frames from `{dplyr}` should
  be ungrouped before they are entered as `data`.

- ...:

  Arguments passed on to
  [`ggdotplotstats`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggdotplotstats.md)

  `y`

  :   Label or grouping variable.

  `centrality.line.args`

  :   A list of additional aesthetic arguments to be passed to the
      [`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
      used to display the lines corresponding to the centrality
      parameter.

  `x`

  :   A numeric variable from the data frame `data`.

  `type`

  :   A character specifying the type of statistical approach:

      - `"parametric"`

      - `"nonparametric"`

      - `"robust"`

      - `"bayes"`

      You can specify just the initial letter.

  `test.value`

  :   A number indicating the true value of the mean (Default: `0`).

  `digits`

  :   Number of digits for rounding or significant figures. May also be
      `"signif"` to return significant figures or `"scientific"` to
      return scientific notation. Control the number of digits by adding
      the value as suffix, e.g. `digits = "scientific4"` to have
      scientific notation with 4 decimal places, or `digits = "signif5"`
      for 5 significant figures (see also
      [`signif()`](https://rdrr.io/r/base/Round.html)).

  `conf.level`

  :   Scalar between `0` and `1` (default: `95%` confidence/credible
      intervals, `0.95`). If `NULL`, no confidence intervals will be
      computed.

  `tr`

  :   Trim level for the mean when carrying out `robust` tests. In case
      of an error, try reducing the value of `tr`, which is by default
      set to `0.2`. Lowering the value might help.

  `bf.prior`

  :   A number between `0.5` and `2` (default `0.707`), the prior width
      to use in calculating Bayes factors and posterior estimates. In
      addition to numeric arguments, several named values are also
      recognized: `"medium"`, `"wide"`, and `"ultrawide"`, corresponding
      to *r* scale values of `1/2`, `sqrt(2)/2`, and `1`, respectively.
      In case of an ANOVA, this value corresponds to scale for fixed
      effects.

  `effsize.type`

  :   Type of effect size needed for *parametric* tests. The argument
      can be `"d"` (for Cohen's *d*) or `"g"` (for Hedge's *g*).

  `xlab`

  :   Label for `x` axis variable. If `NULL` (default), variable name
      for `x` will be used.

  `bf.message`

  :   Logical that decides whether to display Bayes Factor in favor of
      the *null* hypothesis. This argument is relevant only **for
      parametric test** (Default: `TRUE`).

  `results.subtitle`

  :   Decides whether the results of statistical tests are to be
      displayed as a subtitle (Default: `TRUE`). If set to `FALSE`, only
      the plot will be returned.

  `subtitle`

  :   The text for the plot subtitle. Will work only if
      `results.subtitle = FALSE`.

  `caption`

  :   The text for the plot caption. This argument is relevant only if
      `bf.message = FALSE`.

  `centrality.plotting`

  :   Logical that decides whether centrality tendency measure is to be
      displayed as a point with a label (Default: `TRUE`). Function
      decides which central tendency measure to show depending on the
      `type` argument.

      - **mean** for parametric statistics

      - **median** for non-parametric statistics

      - **trimmed mean** for robust statistics

      - **MAP estimator** for Bayesian statistics

      If you want default centrality parameter, you can specify this
      using `centrality.type` argument.

  `centrality.type`

  :   Decides which centrality parameter is to be displayed. The default
      is to choose the same as `type` argument. You can specify this to
      be:

      - `"parameteric"` (for **mean**)

      - `"nonparametric"` (for **median**)

      - `robust` (for **trimmed mean**)

      - `bayes` (for **MAP estimator**)

      Just as `type` argument, abbreviations are also accepted.

  `ggplot.component`

  :   A `ggplot` component to be added to the plot prepared by
      `{ggstatsplot}`. This argument is primarily helpful for `grouped_`
      variants of all primary functions. Default is `NULL`. The argument
      should be entered as a `{ggplot2}` function or a list of
      `{ggplot2}` functions.

  `ggtheme`

  :   A `{ggplot2}` theme. Default value is
      [`theme_ggstatsplot()`](https://indrajeetpatil.github.io/ggstatsplot/reference/theme_ggstatsplot.md).
      Any of the `{ggplot2}` themes (e.g.,
      [`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)),
      or themes from extension packages are allowed (e.g.,
      `ggthemes::theme_fivethirtyeight()`,
      `hrbrthemes::theme_ipsum_ps()`, etc.). But note that sometimes
      these themes will remove some of the details that `{ggstatsplot}`
      plots typically contains. For example, if relevant,
      [`ggbetweenstats()`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.md)
      shows details about multiple comparison test as a label on the
      secondary Y-axis. Some themes (e.g.
      `ggthemes::theme_fivethirtyeight()`) will remove the secondary
      Y-axis and thus the details as well.

  `conf.int`

  :   Logical. Decides whether to display confidence intervals as error
      bars (Default: `TRUE`).

  `errorbar.args`

  :   Additional arguments that will be passed to
      [`geom_errorbar()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)
      geom. Please see documentation for that function to know more
      about these arguments.

  `ylab`

  :   Labels for `y` axis variable. If `NULL` (default), variable name
      for `y` will be used.

  `point.args`

  :   A list of additional aesthetic arguments to be passed to the
      [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

- grouping.var:

  A single grouping variable.

- plotgrid.args:

  A `list` of additional arguments passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html),
  except for `guides` argument which is already separately specified
  here.

- annotation.args:

  A `list` of additional arguments passed to
  [`patchwork::plot_annotation()`](https://patchwork.data-imaginist.com/reference/plot_annotation.html).

## Details

For details, see:
<https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggdotplotstats.html>

## See also

[`grouped_gghistostats`](https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_gghistostats.md),
[`ggdotplotstats`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggdotplotstats.md),
[`gghistostats`](https://indrajeetpatil.github.io/ggstatsplot/reference/gghistostats.md)

## Examples

``` r
# for reproducibility
set.seed(123)
library(dplyr, warn.conflicts = FALSE)

# removing factor level with very few no. of observations
df <- filter(ggplot2::mpg, cyl %in% c("4", "6", "8"))

# plot
grouped_ggdotplotstats(
  data         = df,
  x            = cty,
  y            = manufacturer,
  grouping.var = cyl,
  test.value   = 15.5
)
```
