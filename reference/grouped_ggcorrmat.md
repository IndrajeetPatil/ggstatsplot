# Grouped visualizations of correlation matrices

Applies
[`ggcorrmat()`](https://www.indrapatil.com/ggstatsplot/reference/ggcorrmat.md)
across all levels of `grouping.var` and combines the resulting plots
using
[`combine_plots()`](https://www.indrapatil.com/ggstatsplot/reference/combine_plots.md).

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
  [`ggcorrmat`](https://www.indrapatil.com/ggstatsplot/reference/ggcorrmat.md)

  `cor.vars`

  : Variables for which the correlation matrix is to be computed and
    visualized, specified using `{tidyselect}` syntax (e.g.,
    `c(var1, var2)` or `starts_with("Sepal")`). If `NULL` (default), all
    numeric variables from `data` will be used.

  `cor.vars.names`

  : Optional list of names to be used for `cor.vars`. The names should
    be entered in the same order.

  `partial`

  : Can be `TRUE` for partial correlations. For Bayesian partial
    correlations, "full" instead of pseudo-Bayesian partial correlations
    (i.e., Bayesian correlation based on frequentist partialization) are
    returned.

  `matrix.type`

  : Character, `"upper"` (default), `"lower"`, or `"full"`, to display
    the upper triangular, lower triangular, or full matrix,
    respectively.

  `sig.level`

  : Significance level (Default: `0.05`). If the (adjusted; see
    `p.adjust.method`) *p*-value is bigger than `sig.level`, then the
    corresponding correlation coefficient is regarded as insignificant
    and flagged as such in the plot. Ignored for `type = "bayes"`.

  `pch`

  : Decides the point shape to be used for insignificant correlation
    coefficients. Default: `pch = "cross"`. The caption explaining the
    insignificance marker is added only for `pch = "cross"` (or `4`).

  `colors`

  : A character vector of exactly three colors for the gradient: low
    (negative correlations), mid (zero), and high (positive
    correlations). Must be a **diverging** palette so that the sign of
    the correlation is visually obvious. Default:
    `c("#EA4335", "white", "#4285F4")` (red-white-blue).

  `ggcorrplot.args`

  : A list of additional (mostly aesthetic) arguments passed to
    [`ggcorrplot::ggcorrplot()`](https://rpkgs.datanovia.com/ggcorrplot/reference/ggcorrplot.html).
    Do not include the following arguments, which are set internally:
    `corr`, `p.mat`, `sig.level`, `ggtheme`, `colors`, `type`, `lab`,
    `pch`, `legend.title`, `digits`.

  `subtitle`

  : The text for the plot subtitle.

  `caption`

  : The text for the plot caption. If the insignificance marker caption
    is shown (see `pch`), this text is displayed above it.

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

  `p.adjust.method`

  : Adjustment method for *p*-values for multiple comparisons. Possible
    methods are: `"holm"` (default), `"hochberg"`, `"hommel"`,
    `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.

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

## Details

Missing values are handled using pairwise deletion (each correlation
uses all complete pairs of observations). If sample sizes then differ
across pairs, the legend shows their minimum, mode, and maximum. Partial
correlations use only complete cases.

For details, see:
<https://www.indrapatil.com/ggstatsplot/articles/web_only/ggcorrmat.html>

## See also

[`ggcorrmat()`](https://www.indrapatil.com/ggstatsplot/reference/ggcorrmat.md),
[`ggscatterstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggscatterstats.md),
[`grouped_ggscatterstats()`](https://www.indrapatil.com/ggstatsplot/reference/grouped_ggscatterstats.md)

## Examples

``` r
set.seed(123)

grouped_ggcorrmat(
  data = iris,
  grouping.var = Species,
  type = "robust",
  colors = c("#0072B2", "white", "#D55E00"),
  plotgrid.args = list(ncol = 1L),
  annotation.args = list(tag_levels = "i")
)
```
