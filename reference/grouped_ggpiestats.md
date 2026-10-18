# Grouped pie charts with statistical tests

Applies
[`ggpiestats()`](https://www.indrapatil.com/ggstatsplot/reference/ggpiestats.md)
across all levels of `grouping.var` and combines the resulting plots
using
[`combine_plots()`](https://www.indrapatil.com/ggstatsplot/reference/combine_plots.md).

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
  taken. Other data types (e.g., matrix, table, array, etc.) will
  **not** be accepted. Additionally, grouped data frames from `{dplyr}`
  should be ungrouped before they are entered as `data`.

- ...:

  Arguments passed on to
  [`ggpiestats`](https://www.indrapatil.com/ggstatsplot/reference/ggpiestats.md)

  `x`

  : The variable to use as the **rows** in the contingency table. Note
    that empty factor levels are *not* dropped: they are shown in the
    legend and counted (with zero observations) in the statistical test.
    Use [`droplevels()`](https://rdrr.io/r/base/droplevels.html)
    beforehand if this is not intended.

  `y`

  : The variable to use as the **columns** in the contingency table. If
    `NULL` (default), a one-sample proportion test (a goodness of fit
    test) is run for the `x` variable. Otherwise, an appropriate
    association test is run.

  `proportion.test`

  : Logical that decides whether a proportion test for `x` is carried
    out for each level of `y`. Defaults to `results.subtitle`.
    [`ggbarstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggbarstats.md)
    displays only the *p*-values from this test. Always `FALSE` when
    `type = "bayes"`, when `y` is `NULL`, or when `x` has only one
    level.

  `bf.message`

  : Logical that decides whether to display the Bayes Factor in favor of
    the *null* hypothesis as a caption. Relevant only for non-Bayesian
    `type`s and unpaired designs (`paired = FALSE`) (Default: `TRUE`).

  `digits.perc`

  : Number of decimal places for percentage labels (Default: `0L`).

  `label`

  : Character that decides what each label displays: `"percentage"`
    (default), `"counts"`, or `"both"`.

  `label.args`

  : A list of additional aesthetic arguments passed to
    [`ggplot2::geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
    (or
    [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
    if `label.repel = TRUE`).

  `label.repel`

  : Logical that decides whether labels are repelled using `{ggrepel}`
    to avoid overlaps (Default: `FALSE`).

  `legend.title`

  : Title text for the legend. If `NULL` (default), the name of the `x`
    variable is used.

  `p.adjust.method`

  : Adjustment method for *p*-values for multiple comparisons. Possible
    methods are: `"holm"` (default), `"hochberg"`, `"hommel"`,
    `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.

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

  `ggplot.component`

  : A `{ggplot2}` component, or a list of components, to add to the plot
    (Default: `NULL`). This is mostly helpful for the `grouped_`
    variants, where it is applied to each individual plot.

  `palette`

  : Name of the discrete palette in `"package::palette"` format to be
    used for coloring (via `{paletteer}`). Run
    `View(paletteer::palettes_d_names)` to see all available options.

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

  `alternative`

  : a character string specifying the alternative hypothesis, must be
    one of `"two.sided"` (default), `"greater"` or `"less"`. You can
    specify just the initial letter.

  `paired`

  : Logical indicating whether data came from a within-subjects or
    repeated measures design study (Default: `FALSE`). Only relevant for
    two-way tables (i.e., when `y` is supplied), and paired designs are
    only supported for frequentist tests (McNemar's test). For a two-way
    table with `type = "bayes"`, `paired` is ignored and the Bayesian
    test of independence for unpaired data is run instead. For one-way
    tables (`y = NULL`), `paired` has no effect.

  `counts`

  : The variable in data containing counts, or `NULL` if each row
    represents a single observation.

  `ratio`

  : A vector of proportions: the expected proportions for the proportion
    test (should sum to `1`). Default is `NULL`, which means the null is
    equal theoretical proportions across the levels of the nominal
    variable. E.g., `ratio = c(0.5, 0.5)` for two levels,
    `ratio = c(0.25, 0.25, 0.25, 0.25)` for four levels, etc.

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

Rows with a missing value in `x`, `y`, or `counts` are removed before
plotting and analysis.

For details, see:
<https://www.indrapatil.com/ggstatsplot/articles/web_only/ggpiestats.html>

## See also

[`ggpiestats()`](https://www.indrapatil.com/ggstatsplot/reference/ggpiestats.md),
[`ggbarstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggbarstats.md),
[`grouped_ggbarstats()`](https://www.indrapatil.com/ggstatsplot/reference/grouped_ggbarstats.md)

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
