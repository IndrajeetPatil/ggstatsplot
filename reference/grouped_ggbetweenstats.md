# Violin plots for group or condition comparisons in between-subjects designs repeated across all levels of a grouping variable.

Helper function for
[`ggstatsplot::ggbetweenstats`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.md)
to apply this function across multiple levels of a given factor and
combining the resulting plots using
[`ggstatsplot::combine_plots`](https://indrajeetpatil.github.io/ggstatsplot/reference/combine_plots.md).

## Usage

``` r
grouped_ggbetweenstats(
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
  [`ggbetweenstats`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.md)

  `xlab`

  :   Label for `x` axis variable. If `NULL` (default), variable name
      for `x` will be used.

  `ylab`

  :   Labels for `y` axis variable. If `NULL` (default), variable name
      for `y` will be used.

  `p.adjust.method`

  :   Adjustment method for *p*-values for multiple comparisons.
      Possible methods are: `"holm"` (default), `"hochberg"`,
      `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.

  `pairwise.display`

  :   Decides *which* pairwise comparisons to display. Available options
      are:

      - `"significant"` (abbreviation accepted: `"s"`)

      - `"non-significant"` (abbreviation accepted: `"ns"`)

      - `"all"`

      You can use this argument to make sure that your plot is not
      uber-cluttered when you have multiple groups being compared and
      scores of pairwise comparisons being displayed. If set to
      `"none"`, no pairwise comparisons will be displayed.

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

  `point.args`

  :   A list of additional aesthetic arguments to be passed to the
      [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

  `boxplot.args`

  :   A list of additional aesthetic arguments passed on to
      [`ggplot2::geom_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html).
      By default, the whiskers extend to 1.5 times the interquartile
      range (IQR) from the box (Tukey-style). To customize whisker
      length, you can use the `coef` parameter, e.g.,
      `boxplot.args = list(coef = 3)` for whiskers extending to 3 \*
      IQR, or `boxplot.args = list(coef = 0)` to show only the range of
      the data.

  `violin.args`

  :   A list of additional aesthetic arguments to be passed to the
      [`ggplot2::geom_violin()`](https://ggplot2.tidyverse.org/reference/geom_violin.html).

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

  `centrality.point.args,centrality.label.args`

  :   A list of additional aesthetic arguments to be passed to
      [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
      and
      [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
      geoms, which are involved in mean plotting.

  `ggsignif.args`

  :   A list of additional aesthetic arguments to be passed to
      [`ggsignif::geom_signif()`](https://const-ae.github.io/ggsignif/reference/stat_signif.html).

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

  `x`

  :   The grouping (or independent) variable from `data`. In case of a
      repeated measures or within-subjects design, if `subject.id`
      argument is not available or not explicitly specified, the
      function assumes that the data has already been sorted by such an
      id by the user and creates an internal identifier. So if your data
      is **not** sorted, the results *can* be inaccurate when there are
      more than two levels in `x` and there are `NA`s present. The data
      is expected to be sorted by user in subject-1, subject-2, ...,
      pattern.

  `y`

  :   The response (or outcome or dependent) variable from `data`.

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

  `effsize.type`

  :   Type of effect size needed for *parametric* tests. The argument
      can be `"eta"` (partial eta-squared) or `"omega"` (partial
      omega-squared).

  `var.equal`

  :   a logical variable indicating whether to treat the two variances
      as being equal. If `TRUE` then the pooled variance is used to
      estimate the variance otherwise the Welch (or Satterthwaite)
      approximation to the degrees of freedom is used.

  `bf.prior`

  :   A number between `0.5` and `2` (default `0.707`), the prior width
      to use in calculating Bayes factors and posterior estimates. In
      addition to numeric arguments, several named values are also
      recognized: `"medium"`, `"wide"`, and `"ultrawide"`, corresponding
      to *r* scale values of `1/2`, `sqrt(2)/2`, and `1`, respectively.
      In case of an ANOVA, this value corresponds to scale for fixed
      effects.

  `tr`

  :   Trim level for the mean when carrying out `robust` tests. In case
      of an error, try reducing the value of `tr`, which is by default
      set to `0.2`. Lowering the value might help.

  `nboot`

  :   Number of bootstrap samples for computing confidence interval for
      the effect size (Default: `100L`).

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

## See also

[`ggbetweenstats`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.md),
[`ggwithinstats`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggwithinstats.md),
[`grouped_ggwithinstats`](https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggwithinstats.md)

## Examples

``` r
# for reproducibility
set.seed(123)

library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

grouped_ggbetweenstats(
  data = filter(ggplot2::mpg, drv != "4"),
  x = year,
  y = hwy,
  grouping.var = drv
)


# modifying individual plots using `ggplot.component` argument
grouped_ggbetweenstats(
  data = filter(
    movies_long,
    genre %in% c("Action", "Comedy"),
    mpaa %in% c("R", "PG")
  ),
  x = genre,
  y = rating,
  grouping.var = mpaa,
  ggplot.component = scale_y_continuous(
    breaks = seq(1, 9, 1),
    limits = (c(1, 9))
  ),
  annotation.args = list(title = "Ratings by genre for different MPAA ratings")
)
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
```
