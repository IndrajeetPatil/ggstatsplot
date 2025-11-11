# Visualization of a correlalogram (or correlation matrix) for all levels of a grouping variable

Helper function for
[`ggstatsplot::ggcorrmat()`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.md)
to apply this function across multiple levels of a given factor and
combining the resulting plots using
[`ggstatsplot::combine_plots()`](https://indrajeetpatil.github.io/ggstatsplot/reference/combine_plots.md).

## Usage

``` r
grouped_ggcorrmat(
  data,
  ...,
  grouping.var,
  plotgrid.args = list(),
  annotation.args = list()
)
```

## Arguments

- data:

  A data frame from which variables specified are to be taken.

- ...:

  Arguments passed on to
  [`ggcorrmat`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.md)

  `cor.vars`

  :   List of variables for which the correlation matrix is to be
      computed and visualized. If `NULL` (default), all numeric
      variables from `data` will be used.

  `cor.vars.names`

  :   Optional list of names to be used for `cor.vars`. The names should
      be entered in the same order.

  `partial`

  :   Can be `TRUE` for partial correlations. For Bayesian partial
      correlations, "full" instead of pseudo-Bayesian partial
      correlations (i.e., Bayesian correlation based on frequentist
      partialization) are returned.

  `matrix.type`

  :   Character, `"upper"` (default), `"lower"`, or `"full"`, display
      full matrix, lower triangular or upper triangular matrix.

  `sig.level`

  :   Significance level (Default: `0.05`). If the *p*-value in
      *p*-value matrix is bigger than `sig.level`, then the
      corresponding correlation coefficient is regarded as insignificant
      and flagged as such in the plot.

  `colors`

  :   A vector of 3 colors for low, mid, and high correlation values. If
      set to `NULL`, manual specification of colors will be turned off
      and 3 colors from the specified `palette` from `package` will be
      selected.

  `pch`

  :   Decides the point shape to be used for insignificant correlation
      coefficients (only valid when `insig = "pch"`). Default:
      `pch = "cross"`.

  `ggcorrplot.args`

  :   A list of additional (mostly aesthetic) arguments that will be
      passed to
      [`ggcorrplot::ggcorrplot()`](https://rdrr.io/pkg/ggcorrplot/man/ggcorrplot.html)
      function. The list should avoid any of the following arguments
      since they are already internally being used: `corr`, `method`,
      `p.mat`, `sig.level`, `ggtheme`, `colors`, `lab`, `pch`,
      `legend.title`, `digits`.

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

  `p.adjust.method`

  :   Adjustment method for *p*-values for multiple comparisons.
      Possible methods are: `"holm"` (default), `"hochberg"`,
      `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.

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
<https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcorrmat.html>

## See also

[`ggcorrmat`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.md),
[`ggscatterstats`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggscatterstats.md),
[`grouped_ggscatterstats`](https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggscatterstats.md)

## Examples

``` r
set.seed(123)

grouped_ggcorrmat(
  data = iris,
  grouping.var = Species,
  type = "robust",
  p.adjust.method = "holm",
  plotgrid.args = list(ncol = 1L),
  annotation.args = list(tag_levels = "i")
)
```
