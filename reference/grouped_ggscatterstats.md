# Scatterplot with marginal distributions for all levels of a grouping variable

Grouped scatterplots from `{ggplot2}` combined with marginal
distribution plots with statistical details added as a subtitle.

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
  taken. Other data types (e.g., matrix,table, array, etc.) will **not**
  be accepted. Additionally, grouped data frames from `{dplyr}` should
  be ungrouped before they are entered as `data`.

- ...:

  Arguments passed on to
  [`ggscatterstats`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggscatterstats.md)

  `label.var`

  :   Variable to use for points labels entered as a symbol (e.g.
      `var1`).

  `label.expression`

  :   An expression evaluating to a logical vector that determines the
      subset of data points to label (e.g. `y < 4 & z < 20`). While
      using this argument with
      [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html),
      you will have to provide a quoted expression (e.g.
      `quote(y < 4 & z < 20)`).

  `point.label.args`

  :   A list of additional aesthetic arguments to be passed to
      [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)geom
      used to display the labels.

  `smooth.line.args`

  :   A list of additional aesthetic arguments to be passed to
      `geom_smooth` geom used to display the regression line.

  `marginal`

  :   Decides whether marginal distributions will be plotted on axes
      using `{ggside}` functions. The default is `TRUE`. The package
      `{ggside}` must already be installed by the user.

  `point.width.jitter,point.height.jitter`

  :   Degree of jitter in `x` and `y` direction, respectively. Defaults
      to `0` (0%) of the resolution of the data. Note that the jitter
      should not be specified in the `point.args` because this
      information will be passed to two different `geom`s: one
      displaying the **points** and the other displaying the
      \***labels** for these points.

  `xsidehistogram.args,ysidehistogram.args`

  :   A list of arguments passed to respective `geom_`s from the
      `{ggside}` package to change the marginal distribution histograms
      plots.

  `x`

  :   The column in `data` containing the explanatory variable to be
      plotted on the `x`-axis.

  `y`

  :   The column in `data` containing the response (outcome) variable to
      be plotted on the `y`-axis.

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

  `xlab`

  :   Label for `x` axis variable. If `NULL` (default), variable name
      for `x` will be used.

  `ylab`

  :   Labels for `y` axis variable. If `NULL` (default), variable name
      for `y` will be used.

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

  `point.args`

  :   A list of additional aesthetic arguments to be passed to the
      [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

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
<https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggscatterstats.html>

## See also

[`ggscatterstats`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggscatterstats.md),
[`ggcorrmat`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.md),
[`grouped_ggcorrmat`](https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggcorrmat.md)

## Examples

``` r
# to ensure reproducibility
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
#> `stat_xsidebin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_ysidebin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_xsidebin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_ysidebin()` using `bins = 30`. Pick better value `binwidth`.


# using labeling
# (also show how to modify basic plot from within function call)
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
#> `stat_xsidebin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_ysidebin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_xsidebin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_ysidebin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_xsidebin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_ysidebin()` using `bins = 30`. Pick better value `binwidth`.


# labeling without expression
grouped_ggscatterstats(
  data            = filter(movies_long, rating == 7, genre %in% c("Drama", "Comedy")),
  x               = budget,
  y               = length,
  grouping.var    = genre,
  bf.message      = FALSE,
  label.var       = "title",
  annotation.args = list(tag_levels = "a")
)
#> `stat_xsidebin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_ysidebin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_xsidebin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_ysidebin()` using `bins = 30`. Pick better value `binwidth`.
```
