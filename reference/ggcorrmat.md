# Visualization of a correlation matrix

Correlation matrix containing results from pairwise correlation tests.
If you want a data frame of (grouped) correlation matrix, use
[`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
instead. It can also do grouped analysis when used with output from
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

## Usage

``` r
ggcorrmat(
  data,
  cor.vars = NULL,
  cor.vars.names = NULL,
  matrix.type = "upper",
  type = "parametric",
  tr = 0.2,
  partial = FALSE,
  digits = 2L,
  sig.level = 0.05,
  conf.level = 0.95,
  bf.prior = 0.707,
  p.adjust.method = "holm",
  colors = c("#EA4335", "white", "#4285F4"),
  pch = "cross",
  ggcorrplot.args = list(method = "square", outline.color = "black", pch.cex = 14),
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  ggplot.component = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ...
)
```

## Arguments

- data:

  A data frame from which variables specified are to be taken.

- cor.vars:

  Variables for which the correlation matrix is to be computed and
  visualized, specified using `{tidyselect}` syntax (e.g.,
  `c(var1, var2)` or `starts_with("Sepal")`). If `NULL` (default), all
  numeric variables from `data` will be used.

- cor.vars.names:

  Optional list of names to be used for `cor.vars`. The names should be
  entered in the same order.

- matrix.type:

  Character, `"upper"` (default), `"lower"`, or `"full"`, to display the
  upper triangular, lower triangular, or full matrix, respectively.

- type:

  A character specifying the type of statistical approach:

  - `"parametric"`

  - `"nonparametric"`

  - `"robust"`

  - `"bayes"`

  You can specify just the initial letter.

- tr:

  Trim level for the mean when carrying out `robust` tests. In case of
  an error, try reducing the value of `tr`, which is by default set to
  `0.2`. Lowering the value might help.

- partial:

  Can be `TRUE` for partial correlations. For Bayesian partial
  correlations, "full" instead of pseudo-Bayesian partial correlations
  (i.e., Bayesian correlation based on frequentist partialization) are
  returned.

- digits:

  Number of digits for rounding or significant figures. May also be
  `"signif"` to return significant figures or `"scientific"` to return
  scientific notation. Control the number of digits by adding the value
  as suffix, e.g. `digits = "scientific4"` to have scientific notation
  with 4 decimal places, or `digits = "signif5"` for 5 significant
  figures (see also [`signif()`](https://rdrr.io/r/base/Round.html)).

- sig.level:

  Significance level (Default: `0.05`). If the (adjusted; see
  `p.adjust.method`) *p*-value is bigger than `sig.level`, then the
  corresponding correlation coefficient is regarded as insignificant and
  flagged as such in the plot. Ignored for `type = "bayes"`.

- conf.level:

  Scalar between `0` and `1` (default: `95%` confidence/credible
  intervals, `0.95`). If `NULL`, no confidence intervals will be
  computed.

- bf.prior:

  A number between `0.5` and `2` (default `0.707`), the prior width to
  use in calculating Bayes factors and posterior estimates. In addition
  to numeric arguments, several named values are also recognized:
  `"medium"`, `"wide"`, and `"ultrawide"`, corresponding to *r* scale
  values of `1/2`, `sqrt(2)/2`, and `1`, respectively. In case of an
  ANOVA, this value corresponds to scale for fixed effects.

- p.adjust.method:

  Adjustment method for *p*-values for multiple comparisons. Possible
  methods are: `"holm"` (default), `"hochberg"`, `"hommel"`,
  `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.

- colors:

  A character vector of exactly three colors for the gradient: low
  (negative correlations), mid (zero), and high (positive correlations).
  Must be a **diverging** palette so that the sign of the correlation is
  visually obvious. Default: `c("#EA4335", "white", "#4285F4")`
  (red–white–blue).

- pch:

  Decides the point shape to be used for insignificant correlation
  coefficients. Default: `pch = "cross"`. The caption explaining the
  insignificance marker is added only for `pch = "cross"` (or `4`).

- ggcorrplot.args:

  A list of additional (mostly aesthetic) arguments that will be passed
  to
  [`ggcorrplot::ggcorrplot()`](https://rpkgs.datanovia.com/ggcorrplot/reference/ggcorrplot.html)
  function. The list should avoid any of the following arguments since
  they are already internally being used: `corr`, `p.mat`, `sig.level`,
  `ggtheme`, `colors`, `type`, `lab`, `pch`, `legend.title`, `digits`.

- ggtheme:

  A `{ggplot2}` theme. Default value is
  [`theme_ggstatsplot()`](https://www.indrapatil.com/ggstatsplot/reference/theme_ggstatsplot.md).
  Any of the `{ggplot2}` themes (e.g.,
  [`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)),
  or themes from extension packages are allowed (e.g.,
  `ggthemes::theme_fivethirtyeight()`, `hrbrthemes::theme_ipsum_ps()`,
  etc.). But note that sometimes these themes will remove some of the
  details that `{ggstatsplot}` plots typically contain. For example, if
  relevant,
  [`ggbetweenstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggbetweenstats.md)
  shows details about multiple comparison test as a label on the
  secondary Y-axis. Some themes (e.g.
  `ggthemes::theme_fivethirtyeight()`) will remove the secondary Y-axis
  and thus the details as well.

- ggplot.component:

  A `ggplot` component to be added to the plot prepared by
  `{ggstatsplot}`. This argument is primarily helpful for `grouped_`
  variants of all primary functions. Default is `NULL`. The argument
  should be entered as a `{ggplot2}` function or a list of `{ggplot2}`
  functions.

- title:

  The text for the plot title.

- subtitle:

  The text for the plot subtitle.

- caption:

  The text for the plot caption. If the insignificance marker caption is
  shown (see `pch`), this text is displayed above it.

- ...:

  Currently ignored.

## Value

A `ggplot` object, which can be further modified with `{ggplot2}`
functions. Note that
[`extract_stats()`](https://www.indrapatil.com/ggstatsplot/reference/extract_stats.md)
does not return statistical details for this plot; use
[`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
instead.

## Details

Missing values are handled using pairwise deletion (each correlation
uses all complete pairs of observations), in which case the legend shows
the minimum, mode, and maximum sample size across pairs. Partial
correlations use only complete cases.

For details, see:
<https://www.indrapatil.com/ggstatsplot/articles/web_only/ggcorrmat.html>

## Summary of graphics

|  |  |  |
|----|----|----|
| graphical element | `geom` used | argument for further modification |
| correlation matrix | [`ggcorrplot::ggcorrplot()`](https://rpkgs.datanovia.com/ggcorrplot/reference/ggcorrplot.html) | `ggcorrplot.args` |

## Correlation analyses

The table below provides summary about:

- statistical test carried out for inferential statistics

- type of effect size estimate and a measure of uncertainty for this
  estimate

- functions used internally to compute these details

**Hypothesis testing** and **Effect size estimation**

|  |  |  |  |
|----|----|----|----|
| Type | Test | CI available? | Function used |
| Parametric | Pearson's correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |
| Non-parametric | Spearman's rank correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |
| Robust | Winsorized Pearson's correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |
| Bayesian | Bayesian Pearson's correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |

## See also

[`grouped_ggcorrmat`](https://www.indrapatil.com/ggstatsplot/reference/grouped_ggcorrmat.md)
[`ggscatterstats`](https://www.indrapatil.com/ggstatsplot/reference/ggscatterstats.md)
[`grouped_ggscatterstats`](https://www.indrapatil.com/ggstatsplot/reference/grouped_ggscatterstats.md)

## Examples

``` r
set.seed(123)
library(ggcorrplot)
ggcorrmat(iris)


# with data containing NAs (uses pairwise complete observations)
ggcorrmat(airquality)


# selecting specific variables
ggcorrmat(iris, cor.vars = c(Sepal.Length, Petal.Length, Petal.Width))
```
