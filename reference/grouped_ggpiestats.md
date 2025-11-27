# Grouped pie charts with statistical tests

Helper function for
[`ggstatsplot::ggpiestats`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.md)
to apply this function across multiple levels of a given factor and
combining the resulting plots using
[`ggstatsplot::combine_plots`](https://indrajeetpatil.github.io/ggstatsplot/reference/combine_plots.md).

## Usage

``` r
grouped_ggpiestats(
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
  [`ggpiestats`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.md)

  `x`

  :   The variable to use as the **rows** in the contingency table.
      Please note that if there are empty factor levels in your
      variable, they will be dropped.

  `y`

  :   The variable to use as the **columns** in the contingency table.
      Please note that if there are empty factor levels in your
      variable, they will be dropped. Default is `NULL`. If `NULL`,
      one-sample proportion test (a goodness of fit test) will be run
      for the `x` variable. Otherwise an appropriate association test
      will be run. This argument can not be `NULL` for
      [`ggbarstats()`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggbarstats.md).

  `proportion.test`

  :   Decides whether proportion test for `x` variable is to be carried
      out for each level of `y`. Defaults to `results.subtitle`. In
      [`ggbarstats()`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggbarstats.md),
      only *p*-values from this test will be displayed.

  `digits.perc`

  :   Numeric that decides number of decimal places for percentage
      labels (Default: `0L`).

  `label`

  :   Character decides what information needs to be displayed on the
      label in each pie slice. Possible options are `"percentage"`
      (default), `"counts"`, `"both"`.

  `label.args`

  :   Additional aesthetic arguments that will be passed to
      [`ggplot2::geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html).

  `label.repel`

  :   Whether labels should be repelled using `{ggrepel}` package. This
      can be helpful in case of overlapping labels.

  `legend.title`

  :   Title text for the legend.

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

  `ggplot.component`

  :   A `ggplot` component to be added to the plot prepared by
      `{ggstatsplot}`. This argument is primarily helpful for `grouped_`
      variants of all primary functions. Default is `NULL`. The argument
      should be entered as a `{ggplot2}` function or a list of
      `{ggplot2}` functions.

  `package,palette`

  :   Name of the package from which the given palette is to be
      extracted. The available palettes and packages can be checked by
      running `View(paletteer::palettes_d_names)`.

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

  `type`

  :   A character specifying the type of statistical approach:

      - `"parametric"`

      - `"nonparametric"`

      - `"robust"`

      - `"bayes"`

      You can specify just the initial letter.

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

  `paired`

  :   Logical indicating whether data came from a within-subjects or
      repeated measures design study (Default: `FALSE`).

  `counts`

  :   The variable in data containing counts, or `NULL` if each row
      represents a single observation.

  `ratio`

  :   A vector of proportions: the expected proportions for the
      proportion test (should sum to `1`). Default is `NULL`, which
      means the null is equal theoretical proportions across the levels
      of the nominal variable. E.g., `ratio = c(0.5, 0.5)` for two
      levels, `ratio = c(0.25, 0.25, 0.25, 0.25)` for four levels, etc.

  `sampling.plan`

  :   Character describing the sampling plan. Possible options:

      - `"indepMulti"` (independent multinomial; default)

      - `"poisson"`

      - `"jointMulti"` (joint multinomial)

      - `"hypergeom"` (hypergeometric). For more, see
        [`BayesFactor::contingencyTableBF()`](https://rdrr.io/pkg/BayesFactor/man/contingencyTableBF.html).

  `fixed.margin`

  :   For the independent multinomial sampling plan, which margin is
      fixed (`"rows"` or `"cols"`). Defaults to `"rows"`.

  `prior.concentration`

  :   Specifies the prior concentration parameter, set to `1` by
      default. It indexes the expected deviation from the null
      hypothesis under the alternative, and corresponds to Gunel and
      Dickey's (1974) `"a"` parameter.

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
<https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggpiestats.html>

## See also

[`ggbarstats`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggbarstats.md),
[`ggpiestats`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.md),
[`grouped_ggbarstats`](https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbarstats.md)

## Examples

``` r
set.seed(123)
# grouped one-sample proportion test
grouped_ggpiestats(
  data = mtcars,
  x = cyl,
  grouping.var = am,
  annotation.args = list(title = "Cylinder distribution by transmission type")
)
```
