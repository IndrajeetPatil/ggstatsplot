# Dot-and-whisker plots for regression analyses

Plot with the regression coefficients' point estimates as dots with
confidence interval whiskers and other statistical details included as
labels.

Although the statistical models displayed in the plot may differ based
on the class of models being investigated, there are a few aspects of
the plot that will be invariant across models:

- Each dot represents an **estimate**, and its whiskers the **confidence
  interval** (`95%` by default). Estimates can be effect sizes (for
  tests based on the `F`-statistic) or regression coefficients (for
  tests based on the `t`-, `chi^2`-, or `z`-statistic), etc. The
  confidence intervals can be asymmetric if bootstrapping was used.

- The label attached to each dot provides details from the statistical
  test, typically the estimate, statistic, and *p*-value.

- The caption contains model diagnostics, if available, that can be
  useful for model selection: the smaller the Akaike's Information
  Criterion (**AIC**) and the Bayesian Information Criterion (**BIC**)
  values, the "better" the model.

## Usage

``` r
ggcoefstats(
  x,
  statistic = NULL,
  conf.int = TRUE,
  conf.level = 0.95,
  digits = 2L,
  exclude.intercept = FALSE,
  effectsize.type = "omega",
  meta.analytic.effect = FALSE,
  meta.type = "parametric",
  bf.message = TRUE,
  sort = "none",
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  only.significant = FALSE,
  point.args = list(size = 3, color = "blue", na.rm = TRUE),
  errorbar.args = list(width = 0, na.rm = TRUE),
  vline = TRUE,
  vline.args = list(linewidth = 1, linetype = "dashed"),
  stats.labels = TRUE,
  stats.label.color = NULL,
  stats.label.args = list(size = 3, direction = "y", min.segment.length = 0, na.rm =
    TRUE),
  palette = "ggthemes::gdoc",
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  ...
)
```

## Arguments

- x:

  A model object to be tidied, or a tidy data frame from a regression
  model. Function internally uses
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  to get a tidy data frame. If a data frame, it *must* contain a column
  named `estimate` (estimates of coefficients or other quantities of
  interest). A `term` column (names of predictors) is recommended; if
  absent, terms are named `term_1`, `term_2`, etc. Columns `conf.low`
  and `conf.high` are needed for error bars, `statistic` and `p.value`
  for labels, and `std.error` for meta-analysis.

- statistic:

  Relevant statistic for the model (`"t"`, `"f"`, `"z"`, or `"chi"`) in
  the label. Relevant only if `x` is a *data frame*; if it is `NULL`
  (default), no labels are shown for data frames.

- conf.int:

  Logical that decides whether to display confidence intervals as error
  bars (Default: `TRUE`).

- conf.level:

  Scalar between `0` and `1` (default: `95%` confidence/credible
  intervals, `0.95`).

- digits:

  Number of digits for rounding or significant figures. May also be
  `"signif"` to return significant figures or `"scientific"` to return
  scientific notation. Control the number of digits by adding the value
  as suffix, e.g. `digits = "scientific4"` to have scientific notation
  with 4 decimal places, or `digits = "signif5"` for 5 significant
  figures (see also [`signif()`](https://rdrr.io/r/base/Round.html)).

- exclude.intercept:

  Logical that decides whether the intercept should be excluded from the
  plot (Default: `FALSE`).

- effectsize.type:

  Effect size for ANOVA-like objects, passed as the `es_type` argument
  of
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  (Default: `"omega"`, the unbiased estimator).

- meta.analytic.effect:

  Logical that decides whether to display a subtitle with results from a
  meta-analysis via linear (mixed-effects) models (Default: `FALSE`). If
  `TRUE`, `subtitle` is ignored. This is mostly relevant if a data frame
  with estimates and their standard errors is entered.

- meta.type:

  Type of statistics used to carry out random-effects meta-analysis. If
  `"parametric"` (default),
  [`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html)
  will be used. If `"robust"`,
  [`metaplus::metaplus()`](https://rdrr.io/pkg/metaplus/man/metaplus.html)
  will be used. If `"bayes"`,
  [`metaBMA::meta_random()`](https://danheck.github.io/metaBMA/reference/meta_random.html)
  will be used.

- bf.message:

  Logical that decides whether results from running a Bayesian
  meta-analysis assuming that the effect size *d* varies across studies
  with standard deviation *t* (i.e., a random-effects analysis) are
  displayed in the caption. Relevant only if
  `meta.analytic.effect = TRUE` and `meta.type = "parametric"` (Default:
  `TRUE`).

- sort:

  If `"none"` (default), do not sort; if `"ascending"` or
  `"descending"`, sort by increasing or decreasing coefficient value,
  respectively.

- xlab:

  Label for `x` axis. If `NULL` (default), `"estimate"` is used.

- ylab:

  Label for `y` axis. If `NULL` (default), `"term"` is used.

- title:

  The text for the plot title.

- subtitle:

  The text for the plot subtitle. Ignored if
  `meta.analytic.effect = TRUE`.

- caption:

  The text for the plot caption. It will be replaced by model fit
  indices (AIC and BIC), if available, or by the Bayesian meta-analysis
  results (see `bf.message`).

- only.significant:

  If `TRUE`, only stats labels for significant effects (*p* \< `0.05`)
  are shown (Default: `FALSE`). This can be helpful when a large number
  of regression coefficients are to be displayed in a single plot.

- point.args:

  A list of additional aesthetic arguments passed to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

- errorbar.args:

  A list of additional arguments passed to
  [`ggplot2::geom_errorbar()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html).

- vline:

  Logical that decides whether to display a vertical line at `0`
  (Default: `TRUE`).

- vline.args:

  A list of additional arguments passed to
  [`ggplot2::geom_vline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html).

- stats.labels:

  Logical that decides whether the statistic and *p*-value for each
  coefficient are attached to its dot as a label using `{ggrepel}`
  (Default: `TRUE`).

- stats.label.color:

  Color for the labels: either a single color or a vector with one color
  per term. If set to `NULL` (default), colors will be chosen from the
  specified `palette`.

- stats.label.args:

  A list of additional arguments passed to
  [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html).

- palette:

  Name of the discrete palette in `"package::palette"` format to be used
  for coloring (via `{paletteer}`). Run
  `View(paletteer::palettes_d_names)` to see all available options.

- ggtheme:

  A `{ggplot2}` theme (Default:
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

- ...:

  Additional arguments passed to
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html).

## Value

A `ggplot` object, which can be further modified with `{ggplot2}`
functions. Use
[`extract_stats()`](https://www.indrapatil.com/ggstatsplot/reference/extract_stats.md)
to get the statistical details shown in the plot as data frames.

## Details

For details, see:
<https://www.indrapatil.com/ggstatsplot/articles/web_only/ggcoefstats.html>

## Note

1.  For meta-analysis, you will be asked to install the needed package
    (`{metafor}`, `{metaplus}`, or `{metaBMA}`) if it is unavailable.

2.  Rows with a missing `estimate` are always removed. If labels are
    requested, rows where `statistic`, `std.error`, or `p.value` is
    missing are kept but get no label.

3.  New model classes are frequently added to `{easystats}` packages, so
    it can help to install their development versions with
    `easystats::install_latest()`.

## Summary of graphics

|  |  |  |
|----|----|----|
| graphical element | `geom` used | argument for further modification |
| regression estimate | [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html) | `point.args` |
| error bars | [`ggplot2::geom_errorbar()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html) | `errorbar.args` |
| vertical line | [`ggplot2::geom_vline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html) | `vline.args` |
| label with statistical details | [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html) | `stats.label.args` |

## Random-effects meta-analysis

Below is a summary of:

- the statistical test carried out for inferential statistics

- the type of effect size estimate and a measure of uncertainty for this
  estimate

- the functions used internally to compute these details

**Hypothesis testing and effect size estimation**

|  |  |  |  |  |
|----|----|----|----|----|
| Type | Test | Effect size | CI available? | Function used |
| Parametric | Meta-analysis via random-effects models | *beta* | Yes | [`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html) |
| Robust | Meta-analysis via robust random-effects models | *beta* | Yes | [`metaplus::metaplus()`](https://rdrr.io/pkg/metaplus/man/metaplus.html) |
| Bayesian | Meta-analysis via Bayesian random-effects models | *beta* | Yes | [`metaBMA::meta_random()`](https://danheck.github.io/metaBMA/reference/meta_random.html) |

## Examples

``` r
# for reproducibility
set.seed(123)

# model object
mod <- lm(formula = mpg ~ cyl * am, data = mtcars)

# creating a plot
p <- ggcoefstats(mod)

# looking at the plot
p


# extracting details from statistical tests
extract_stats(p)
#> $subtitle_data
#> NULL
#> 
#> $caption_data
#> NULL
#> 
#> $pairwise_comparisons_data
#> NULL
#> 
#> $descriptive_data
#> NULL
#> 
#> $one_sample_data
#> NULL
#> 
#> $tidy_data
#> # A tibble: 4 × 11
#>   term        estimate std.error conf.level conf.low conf.high statistic
#>   <fct>          <dbl>     <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
#> 1 (Intercept)    30.9      3.19        0.95    24.3     37.4        9.68
#> 2 cyl            -1.98     0.449       0.95    -2.89    -1.06      -4.40
#> 3 am             10.2      4.30        0.95     1.36    19.0        2.36
#> 4 cyl:am         -1.31     0.707       0.95    -2.75     0.143     -1.85
#>   df.error  p.value conf.method expression
#>      <int>    <dbl> <chr>       <list>    
#> 1       28 1.95e-10 Wald        <language>
#> 2       28 1.41e- 4 Wald        <language>
#> 3       28 2.53e- 2 Wald        <language>
#> 4       28 7.55e- 2 Wald        <language>
#> 
#> $glance_data
#> # A tibble: 1 × 8
#>     AIC  AICc   BIC    R2 R2_adjusted  RMSE Sigma expression  
#>   <dbl> <dbl> <dbl> <dbl>       <dbl> <dbl> <dbl> <list>      
#> 1  166.  168.  173. 0.785       0.762  2.75  2.94 <expression>
#> 
#> attr(,"class")
#> [1] "ggstatsplot_stats" "list"             

# exclude intercept from the plot
ggcoefstats(mod, exclude.intercept = TRUE)


# only show significant labels
ggcoefstats(mod, only.significant = TRUE)


# ANOVA model (F-statistic)
ggcoefstats(aov(mpg ~ cyl * am, data = mtcars))


# a tidy data frame can also be passed directly (model-free use)
ggcoefstats(data.frame(term = c("a", "b", "c"), estimate = c(0.5, -0.2, 1.1)))


# without a `term` column (auto-generated)
ggcoefstats(data.frame(estimate = c(0.5, -0.2, 1.1)))


# tidy data frames can also include stats-label inputs directly
df_tidy <- parameters::model_parameters(stats::lm(wt ~ am * cyl, mtcars), ci = 0.95)
names(df_tidy) <- c(
  "term", "estimate", "std.error", "conf.level", "conf.low",
  "conf.high", "statistic", "df.error", "p.value"
)
df_tidy$p.value[2L] <- 0.42

ggcoefstats(
  df_tidy,
  statistic = "t",
  only.significant = TRUE,
  stats.label.color = c("firebrick", "grey50", "forestgreen", "navy")
)

# further arguments can be passed to `parameters::model_parameters()`
library(lme4)
#> Loading required package: Matrix
ggcoefstats(lmer(Reaction ~ Days + (Days | Subject), sleepstudy), effects = "fixed")
```
