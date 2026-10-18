# Grouped box/violin plots for repeated measures comparisons

Applies
[`ggwithinstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggwithinstats.md)
across all levels of `grouping.var` and combines the resulting plots
using
[`combine_plots()`](https://www.indrapatil.com/ggstatsplot/reference/combine_plots.md).

## Usage

``` r
grouped_ggwithinstats(
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
  [`ggwithinstats`](https://www.indrapatil.com/ggstatsplot/reference/ggwithinstats.md)

  `x`

  : The condition (or repeated-measures) variable from `data`.

  `point.path,centrality.path`

  : Logical that decides whether individual data points and centrality
    measures, respectively, are connected using
    [`ggplot2::geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
    (Default: `TRUE`). `point.path` is relevant only when there are two
    groups (i.e., for a *t*-test). With many data points, consider
    `point.path = FALSE`, as these lines can overwhelm the plot.

  `centrality.path.args,point.path.args`

  : A list of additional aesthetic arguments passed to
    [`ggplot2::geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
    connecting centrality measures and raw data points, respectively.

  `subject.id`

  : The subject (or repeated measures) identifier from `data`. Each
    subject must appear at most once per level of `x`. If `NULL`
    (default), observations are paired by their row order within each
    level of `x`. **If you are not sure the data are sorted this way,
    specify this argument.** Rows with a missing `x` or `subject.id` are
    ignored, and subjects with a missing (`NA`) dependent variable in
    any condition are dropped list-wise (see
    [`statsExpressions::complete_subjects()`](https://www.indrapatil.com/statsExpressions/reference/complete_subjects.html)),
    so the plot, its labels, and the statistical results are all based
    on the same complete subjects.

  `xlab`

  : Label for `x` axis variable. If `NULL` (default), variable name for
    `x` will be used.

  `ylab`

  : Label for `y` axis variable. If `NULL` (default), variable name for
    `y` will be used.

  `p.adjust.method`

  : Adjustment method for *p*-values for multiple comparisons. Possible
    methods are: `"holm"` (default), `"hochberg"`, `"hommel"`,
    `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.

  `pairwise.display`

  : Decides *which* pairwise comparisons to display. Available options
    are:

    - `"significant"` (abbreviation accepted: `"s"`)

    - `"non-significant"` (abbreviation accepted: `"ns"`)

    - `"all"`

    - `"none"`

    Use this to keep the plot uncluttered when many groups are compared.
    Pairwise comparisons are carried out (and displayed) only when `x`
    has three or more levels. For `type = "bayes"`, there are no
    *p*-values, so all comparisons are displayed unless this is
    `"none"`.

  `pairwise.alpha`

  : Numeric alpha threshold used to decide which pairwise comparisons
    are displayed when `pairwise.display = "significant"` or
    `pairwise.display = "non-significant"` (Default: `0.05`). It is
    compared against *p*-values adjusted using `p.adjust.method`.

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

  `point.args`

  : A list of additional aesthetic arguments passed to
    [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

  `boxplot.args`

  : A list of additional aesthetic arguments passed to
    [`ggplot2::geom_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html).
    By default, the whiskers extend to 1.5 times the interquartile range
    (IQR) from the box (Tukey-style). Use `coef` to change this, e.g.,
    `boxplot.args = list(coef = 3)` extends the whiskers to the most
    extreme observations within 3 \* IQR of the hinges.

  `violin.args`

  : A list of additional aesthetic arguments passed to
    [`ggplot2::geom_violin()`](https://ggplot2.tidyverse.org/reference/geom_violin.html).

  `ggplot.component`

  : A `{ggplot2}` component, or a list of components, to add to the plot
    (Default: `NULL`). This is mostly helpful for the `grouped_`
    variants, where it is applied to each individual plot.

  `palette`

  : Name of the discrete palette in `"package::palette"` format to be
    used for coloring (via `{paletteer}`). Run
    `View(paletteer::palettes_d_names)` to see all available options.

  `centrality.point.args,centrality.label.args`

  : A list of additional aesthetic arguments passed to
    [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
    and
    [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html),
    respectively, which display the centrality measure.

  `ggsignif.args`

  : A list of additional aesthetic arguments passed to
    [`ggsignif::geom_signif()`](https://const-ae.github.io/ggsignif/reference/stat_signif.html).

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

  `y`

  : The response (or outcome or dependent) variable from `data`.

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

  `bf.prior`

  : A number between `0.5` and `2` (default `0.707`), the prior width to
    use in calculating Bayes factors and posterior estimates. In
    addition to numeric arguments, several named values are also
    recognized: `"medium"`, `"wide"`, and `"ultrawide"`, corresponding
    to *r* scale values of `1/2`, `sqrt(2)/2`, and `1`, respectively. In
    case of an ANOVA, this value corresponds to scale for fixed effects.

  `tr`

  : Trim level for the mean when carrying out `robust` tests. In case of
    an error, try reducing the value of `tr`, which is by default set to
    `0.2`. Lowering the value might help.

  `alternative`

  : a character string specifying the alternative hypothesis, must be
    one of `"two.sided"` (default), `"greater"` or `"less"`. You can
    specify just the initial letter.

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

## See also

[`ggwithinstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggwithinstats.md),
[`ggbetweenstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggbetweenstats.md),
[`grouped_ggbetweenstats()`](https://www.indrapatil.com/ggstatsplot/reference/grouped_ggbetweenstats.md)

## Examples

``` r
# for reproducibility
set.seed(123)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

# the most basic function call
grouped_ggwithinstats(
  data             = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
  x                = condition,
  y                = desire,
  subject.id       = subject,
  grouping.var     = gender,
  type             = "np",
  # additional modifications for **each** plot using `{ggplot2}` functions
  ggplot.component = scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)),
  annotation.args  = list(title = "Desire ratings by condition for each gender")
)
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
```
