# Box/Violin plots for repeated measures comparisons

A combination of box and violin plots along with raw (unjittered) data
points for within-subjects designs with statistical details included in
the plot as a subtitle.

The data must be in *long* format: one row per subject per level of `x`
(see
[`bugs_long`](https://www.indrapatil.com/ggstatsplot/reference/bugs_long.md)
for an example).

## Usage

``` r
ggwithinstats(
  data,
  x,
  y,
  type = "parametric",
  subject.id = NULL,
  pairwise.display = "significant",
  pairwise.alpha = 0.05,
  p.adjust.method = "holm",
  bf.prior = 0.707,
  bf.message = TRUE,
  results.subtitle = TRUE,
  xlab = NULL,
  ylab = NULL,
  caption = NULL,
  title = NULL,
  subtitle = NULL,
  digits = 2L,
  conf.level = 0.95,
  tr = 0.2,
  alternative = "two.sided",
  centrality.plotting = TRUE,
  centrality.type = type,
  centrality.point.args = list(size = 5, color = "darkred"),
  centrality.label.args = list(size = 3, nudge_x = 0.4, segment.linetype = 4),
  centrality.path = TRUE,
  centrality.path.args = list(linewidth = 1, color = "red", alpha = 0.5),
  point.args = list(size = 3, alpha = 0.5, na.rm = TRUE),
  point.path = TRUE,
  point.path.args = list(alpha = 0.5, linetype = "dashed"),
  boxplot.args = list(width = 0.2, alpha = 0.5, na.rm = TRUE),
  violin.args = list(width = 0.5, alpha = 0.2, na.rm = TRUE),
  ggsignif.args = list(textsize = 3, tip_length = 0.01, na.rm = TRUE),
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  palette = "ggthemes::gdoc",
  ggplot.component = NULL,
  ...
)
```

## Arguments

- data:

  A data frame (or a tibble) from which variables specified are to be
  taken. Other data types (e.g., matrix, table, array, etc.) will
  **not** be accepted. Additionally, grouped data frames from `{dplyr}`
  should be ungrouped before they are entered as `data`.

- x:

  The condition (or repeated-measures) variable from `data`.

- y:

  The response (or outcome or dependent) variable from `data`.

- type:

  A character specifying the type of statistical approach:

  - `"parametric"`

  - `"nonparametric"`

  - `"robust"`

  - `"bayes"`

  You can specify just the initial letter (e.g. `"np"` or `"bf"`).
  Matching is on the initial lowercase letter only, so any other value
  (including capitalized values such as `"Bayes"`) falls back to
  `"parametric"` without a warning.

- subject.id:

  The subject (or repeated measures) identifier from `data`. Each
  subject must appear at most once per level of `x`. If `NULL`
  (default), observations are paired by their row order within each
  level of `x`. **If you are not sure the data are sorted this way,
  specify this argument.** Rows with a missing `x` or `subject.id` are
  ignored, and subjects with a missing (`NA`) dependent variable in any
  condition are dropped list-wise (see
  [`statsExpressions::complete_subjects()`](https://www.indrapatil.com/statsExpressions/reference/complete_subjects.html)),
  so the plot, its labels, and the statistical results are all based on
  the same complete subjects.

- pairwise.display:

  Decides *which* pairwise comparisons to display. Available options
  are:

  - `"significant"` (abbreviation accepted: `"s"`)

  - `"non-significant"` (abbreviation accepted: `"ns"`)

  - `"all"`

  - `"none"`

  Use this to keep the plot uncluttered when many groups are compared.
  Pairwise comparisons are carried out (and displayed) only when `x` has
  three or more levels. For `type = "bayes"`, there are no *p*-values,
  so all comparisons are displayed unless this is `"none"`.

- pairwise.alpha:

  Numeric alpha threshold used to decide which pairwise comparisons are
  displayed when `pairwise.display = "significant"` or
  `pairwise.display = "non-significant"` (Default: `0.05`). It is
  compared against *p*-values adjusted using `p.adjust.method`.

- p.adjust.method:

  Adjustment method for *p*-values for multiple comparisons. Possible
  methods are: `"holm"` (default), `"hochberg"`, `"hommel"`,
  `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.

- bf.prior:

  A number between `0.5` and `2` (default `0.707`), the prior width to
  use in calculating Bayes factors and posterior estimates. In addition
  to numeric arguments, several named values are also recognized:
  `"medium"`, `"wide"`, and `"ultrawide"`, corresponding to *r* scale
  values of `1/2`, `sqrt(2)/2`, and `1`, respectively. In case of an
  ANOVA, this value corresponds to scale for fixed effects.

- bf.message:

  Logical that decides whether to display the Bayes Factor in favor of
  the *null* hypothesis as a caption. Relevant only for parametric tests
  (Default: `TRUE`).

- results.subtitle:

  Logical that decides whether the results of statistical tests are
  displayed as a subtitle (Default: `TRUE`). Setting this to `FALSE`
  also skips the automatic Bayes Factor caption. Other plot details have
  their own arguments.

- xlab:

  Label for `x` axis variable. If `NULL` (default), variable name for
  `x` will be used.

- ylab:

  Label for `y` axis variable. If `NULL` (default), variable name for
  `y` will be used.

- caption:

  The text for the plot caption. It will be replaced by the Bayes Factor
  caption whenever that is displayed (see `bf.message`).

- title:

  The text for the plot title.

- subtitle:

  The text for the plot subtitle. Used only if
  `results.subtitle = FALSE`.

- digits:

  Number of digits for rounding or significant figures. May also be
  `"signif"` to return significant figures or `"scientific"` to return
  scientific notation. Control the number of digits by adding the value
  as suffix, e.g. `digits = "scientific4"` to have scientific notation
  with 4 decimal places, or `digits = "signif5"` for 5 significant
  figures (see also [`signif()`](https://rdrr.io/r/base/Round.html)).

- conf.level:

  Scalar between `0` and `1` (default: `95%` confidence/credible
  intervals, `0.95`).

- tr:

  Trim level for the mean when carrying out `robust` tests. In case of
  an error, try reducing the value of `tr`, which is by default set to
  `0.2`. Lowering the value might help.

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of `"two.sided"` (default), `"greater"` or `"less"`. You can specify
  just the initial letter.

- centrality.plotting:

  Logical that decides whether central tendency measure is to be
  displayed as a point with a label (Default: `TRUE`). Function decides
  which central tendency measure to show depending on the `type`
  argument.

  - **mean** for parametric statistics

  - **median** for non-parametric statistics

  - **trimmed mean** for robust statistics

  - **MAP estimator** for Bayesian statistics

  Use `centrality.type` to choose a different centrality measure.

- centrality.type:

  Decides which centrality measure is displayed. Defaults to the same
  value as `type`. Can be one of:

  - `"parametric"` (for **mean**)

  - `"nonparametric"` (for **median**)

  - `"robust"` (for **trimmed mean**)

  - `"bayes"` (for **MAP estimator**)

  As with `type`, abbreviations are accepted.

- centrality.point.args, centrality.label.args:

  A list of additional aesthetic arguments passed to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
  and
  [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html),
  respectively, which display the centrality measure.

- centrality.path.args, point.path.args:

  A list of additional aesthetic arguments passed to
  [`ggplot2::geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
  connecting centrality measures and raw data points, respectively.

- point.args:

  A list of additional aesthetic arguments passed to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

- point.path, centrality.path:

  Logical that decides whether individual data points and centrality
  measures, respectively, are connected using
  [`ggplot2::geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
  (Default: `TRUE`). `point.path` is relevant only when there are two
  groups (i.e., for a *t*-test). With many data points, consider
  `point.path = FALSE`, as these lines can overwhelm the plot.

- boxplot.args:

  A list of additional aesthetic arguments passed to
  [`ggplot2::geom_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html).
  By default, the whiskers extend to 1.5 times the interquartile range
  (IQR) from the box (Tukey-style). Use `coef` to change this, e.g.,
  `boxplot.args = list(coef = 3)` extends the whiskers to the most
  extreme observations within 3 \* IQR of the hinges.

- violin.args:

  A list of additional aesthetic arguments passed to
  [`ggplot2::geom_violin()`](https://ggplot2.tidyverse.org/reference/geom_violin.html).

- ggsignif.args:

  A list of additional aesthetic arguments passed to
  [`ggsignif::geom_signif()`](https://const-ae.github.io/ggsignif/reference/stat_signif.html).

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

- palette:

  Name of the discrete palette in `"package::palette"` format to be used
  for coloring (via `{paletteer}`). Run
  `View(paletteer::palettes_d_names)` to see all available options.

- ggplot.component:

  A `{ggplot2}` component, or a list of components, to add to the plot
  (Default: `NULL`). This is mostly helpful for the `grouped_` variants,
  where it is applied to each individual plot.

- ...:

  Currently ignored.

## Value

A `ggplot` object, which can be further modified with `{ggplot2}`
functions. Use
[`extract_stats()`](https://www.indrapatil.com/ggstatsplot/reference/extract_stats.md)
to get the statistical details shown in the plot as data frames.

## Details

For details, see:
<https://www.indrapatil.com/ggstatsplot/articles/web_only/ggwithinstats.html>

## Summary of graphics

|  |  |  |
|----|----|----|
| graphical element | `geom` used | argument for further modification |
| raw data | [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html) | `point.args` |
| point path | [`ggplot2::geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html) | `point.path.args` |
| box plot | [`ggplot2::geom_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html) | `boxplot.args` |
| density plot | [`ggplot2::geom_violin()`](https://ggplot2.tidyverse.org/reference/geom_violin.html) | `violin.args` |
| centrality measure point | [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html) | `centrality.point.args` |
| centrality measure point path | [`ggplot2::geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html) | `centrality.path.args` |
| centrality measure label | [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html) | `centrality.label.args` |
| pairwise comparisons | [`ggsignif::geom_signif()`](https://const-ae.github.io/ggsignif/reference/stat_signif.html) | `ggsignif.args` |

## Centrality measures

|  |  |  |
|----|----|----|
| Type | Measure | Function used |
| Parametric | mean | [`datawizard::describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html) |
| Non-parametric | median | [`datawizard::describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html) |
| Robust | trimmed mean | [`datawizard::describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html) |
| Bayesian | MAP | [`datawizard::describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html) |

## Two-sample tests

Below is a summary of:

- the statistical test carried out for inferential statistics

- the type of effect size estimate and a measure of uncertainty for this
  estimate

- the functions used internally to compute these details

### Between-subjects

**Hypothesis testing**

|  |  |  |  |
|----|----|----|----|
| Type | No. of groups | Test | Function used |
| Parametric | 2 | Student's or Welch's *t*-test | [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html) |
| Non-parametric | 2 | Mann-Whitney *U* test | [`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) |
| Robust | 2 | Yuen's test for trimmed means | [`WRS2::yuen()`](https://rdrr.io/pkg/WRS2/man/yuen.html) |
| Bayesian | 2 | Student's *t*-test | [`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html) |

**Effect size estimation**

|  |  |  |  |  |
|----|----|----|----|----|
| Type | No. of groups | Effect size | CI available? | Function used |
| Parametric | 2 | Cohen's *d*, Hedges' *g* | Yes | [`effectsize::cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.html), [`effectsize::hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.html) |
| Non-parametric | 2 | *r* (rank-biserial correlation) | Yes | [`effectsize::rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.html) |
| Robust | 2 | Algina-Keselman-Penfield robust standardized difference | Yes | [`WRS2::akp.effect()`](https://rdrr.io/pkg/WRS2/man/yuen.html) |
| Bayesian | 2 | difference | Yes | [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html) |

### Within-subjects

**Paired data**: See `complete_subjects()` for subject matching,
duplicate observations, and removal of incomplete subjects.

**Hypothesis testing**

|  |  |  |  |
|----|----|----|----|
| Type | No. of groups | Test | Function used |
| Parametric | 2 | Student's *t*-test | [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html) |
| Non-parametric | 2 | Wilcoxon signed-rank test | [`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) |
| Robust | 2 | Yuen's test on trimmed means for dependent samples | [`WRS2::yuend()`](https://rdrr.io/pkg/WRS2/man/yuend.html) |
| Bayesian | 2 | Student's *t*-test | [`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html) |

**Effect size estimation**

|  |  |  |  |  |
|----|----|----|----|----|
| Type | No. of groups | Effect size | CI available? | Function used |
| Parametric | 2 | Cohen's *d*, Hedges' *g* | Yes | [`effectsize::cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.html), [`effectsize::hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.html) |
| Non-parametric | 2 | *r* (rank-biserial correlation) | Yes | [`effectsize::rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.html) |
| Robust | 2 | Algina-Keselman-Penfield robust standardized difference | Yes | [`WRS2::wmcpAKP()`](https://rdrr.io/pkg/WRS2/man/wmcpAKP.html) |
| Bayesian | 2 | difference | Yes | [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html) |

## One-way ANOVA

Below is a summary of:

- the statistical test carried out for inferential statistics

- the type of effect size estimate and a measure of uncertainty for this
  estimate

- the functions used internally to compute these details

### Between-subjects

**Hypothesis testing**

|  |  |  |  |
|----|----|----|----|
| Type | No. of groups | Test | Function used |
| Parametric | \> 2 | Fisher's or Welch's one-way ANOVA | [`stats::oneway.test()`](https://rdrr.io/r/stats/oneway.test.html) |
| Non-parametric | \> 2 | Kruskal-Wallis one-way ANOVA | [`stats::kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html) |
| Robust | \> 2 | Heteroscedastic one-way ANOVA for trimmed means | [`WRS2::t1way()`](https://rdrr.io/pkg/WRS2/man/t1way.html) |
| Bayesian | \> 2 | Fisher's ANOVA | [`BayesFactor::anovaBF()`](https://rdrr.io/pkg/BayesFactor/man/anovaBF.html) |

**Effect size estimation**

|  |  |  |  |  |
|----|----|----|----|----|
| Type | No. of groups | Effect size | CI available? | Function used |
| Parametric | \> 2 | partial eta-squared, partial omega-squared | Yes | [`effectsize::omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html), [`effectsize::eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html) |
| Non-parametric | \> 2 | rank epsilon squared | Yes | [`effectsize::rank_epsilon_squared()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.html) |
| Robust | \> 2 | Explanatory measure of effect size | Yes | [`WRS2::t1way()`](https://rdrr.io/pkg/WRS2/man/t1way.html) |
| Bayesian | \> 2 | Bayesian R-squared | Yes | [`performance::r2_bayes()`](https://easystats.github.io/performance/reference/r2_bayes.html) |

### Within-subjects

**Paired data**: See `complete_subjects()` for subject matching,
duplicate observations, and removal of incomplete subjects.

**Hypothesis testing**

|  |  |  |  |
|----|----|----|----|
| Type | No. of groups | Test | Function used |
| Parametric | \> 2 | One-way repeated measures ANOVA | [`afex::aov_ez()`](https://rdrr.io/pkg/afex/man/aov_car.html) |
| Non-parametric | \> 2 | Friedman rank sum test | [`stats::friedman.test()`](https://rdrr.io/r/stats/friedman.test.html) |
| Robust | \> 2 | Heteroscedastic one-way repeated measures ANOVA for trimmed means | [`WRS2::rmanova()`](https://rdrr.io/pkg/WRS2/man/rmanova.html) |
| Bayesian | \> 2 | One-way repeated measures ANOVA | [`BayesFactor::anovaBF()`](https://rdrr.io/pkg/BayesFactor/man/anovaBF.html) |

**Effect size estimation**

|  |  |  |  |  |
|----|----|----|----|----|
| Type | No. of groups | Effect size | CI available? | Function used |
| Parametric | \> 2 | partial eta-squared, partial omega-squared | Yes | [`effectsize::omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html), [`effectsize::eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html) |
| Non-parametric | \> 2 | Kendall's coefficient of concordance | Yes | [`effectsize::kendalls_w()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.html) |
| Robust | \> 2 | Algina-Keselman-Penfield robust standardized difference average | Yes | [`WRS2::wmcpAKP()`](https://rdrr.io/pkg/WRS2/man/wmcpAKP.html) |
| Bayesian | \> 2 | Bayesian R-squared | Yes | [`performance::r2_bayes()`](https://easystats.github.io/performance/reference/r2_bayes.html) |

## Pairwise comparison tests

Below is a summary of:

- the statistical test carried out for inferential statistics

- the type of effect size estimate and a measure of uncertainty for this
  estimate

- the functions used internally to compute these details

### Between-subjects

**Hypothesis testing**

|  |  |  |  |  |
|----|----|----|----|----|
| Type | Equal variance? | Test | *p*-value adjustment? | Function used |
| Parametric | No | Games-Howell test | Yes | [`PMCMRplus::gamesHowellTest()`](https://rdrr.io/pkg/PMCMRplus/man/gamesHowellTest.html) |
| Parametric | Yes | Student's *t*-test | Yes | [`stats::pairwise.t.test()`](https://rdrr.io/r/stats/pairwise.t.test.html) |
| Non-parametric | No | Dunn test | Yes | [`PMCMRplus::kwAllPairsDunnTest()`](https://rdrr.io/pkg/PMCMRplus/man/kwAllPairsDunnTest.html) |
| Robust | No | Yuen's trimmed means test | Yes | [`WRS2::lincon()`](https://rdrr.io/pkg/WRS2/man/t1way.html) |
| Bayesian | `NA` | Student's *t*-test | `NA` | [`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html) |

**Effect size estimation**

Not supported.

### Within-subjects

**Paired data**: See `complete_subjects()` for subject matching,
duplicate observations, and removal of incomplete subjects.

**Hypothesis testing**

|  |  |  |  |
|----|----|----|----|
| Type | Test | *p*-value adjustment? | Function used |
| Parametric | Student's *t*-test | Yes | [`stats::pairwise.t.test()`](https://rdrr.io/r/stats/pairwise.t.test.html) |
| Non-parametric | Durbin-Conover test | Yes | [`PMCMRplus::durbinAllPairsTest()`](https://rdrr.io/pkg/PMCMRplus/man/durbinAllPairsTest.html) |
| Robust | Yuen's trimmed means test | Yes | [`WRS2::rmmcp()`](https://rdrr.io/pkg/WRS2/man/rmanova.html) |
| Bayesian | Student's *t*-test | `NA` | [`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html) |

**Effect size estimation**

Not supported.

## See also

[`grouped_ggwithinstats()`](https://www.indrapatil.com/ggstatsplot/reference/grouped_ggwithinstats.md),
[`ggbetweenstats()`](https://www.indrapatil.com/ggstatsplot/reference/ggbetweenstats.md),
[`grouped_ggbetweenstats()`](https://www.indrapatil.com/ggstatsplot/reference/grouped_ggbetweenstats.md)

## Examples

``` r
# for reproducibility
set.seed(123)
library(dplyr, warn.conflicts = FALSE)

# create a plot
p <- ggwithinstats(
  data       = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
  x          = condition,
  y          = desire,
  type       = "np",
  subject.id = subject
)

# looking at the plot
p


# extracting details from statistical tests
extract_stats(p)
#> $subtitle_data
#> # A tibble: 1 × 14
#>   parameter1 parameter2 statistic  p.value method                    alternative
#>   <chr>      <chr>          <dbl>    <dbl> <chr>                     <chr>      
#> 1 desire     condition       1796 0.000430 Wilcoxon signed rank test two.sided  
#>   effectsize        estimate conf.level conf.low conf.high conf.method n.obs
#>   <chr>                <dbl>      <dbl>    <dbl>     <dbl> <chr>       <int>
#> 1 r (rank biserial)    0.487       0.95    0.285     0.648 normal         90
#>   expression
#>   <list>    
#> 1 <language>
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
#> NULL
#> 
#> $glance_data
#> NULL
#> 
#> attr(,"class")
#> [1] "ggstatsplot_stats" "list"             

# if the data are already arranged in repeated-measures order, `subject.id`
# can be omitted
ggwithinstats(
  data             = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
  x                = condition,
  y                = desire,
  results.subtitle = FALSE
)


# use a stricter alpha threshold for significant pairwise comparisons
ggwithinstats(
  data = bugs_long,
  x = condition,
  y = desire,
  subject.id = subject,
  pairwise.alpha = 0.001
)


# modifying defaults
ggwithinstats(
  data       = bugs_long,
  x          = condition,
  y          = desire,
  type       = "robust",
  subject.id = subject
)


# you can remove a specific geom to reduce complexity of the plot
ggwithinstats(
  data = bugs_long,
  x = condition,
  y = desire,
  subject.id = subject,
  # to remove violin plot
  violin.args = list(width = 0, linewidth = 0, colour = NA),
  # to remove boxplot
  boxplot.args = list(width = 0),
  # to remove points
  point.args = list(alpha = 0)
)
```
