# Stacked bar charts with statistical tests

Bar charts for categorical data with statistical details included in the
plot as a subtitle.

## Usage

``` r
ggbarstats(
  data,
  x,
  y,
  counts = NULL,
  type = "parametric",
  paired = FALSE,
  results.subtitle = TRUE,
  label = "percentage",
  label.args = list(alpha = 1, fill = "white"),
  sample.size.label.args = list(size = 4),
  digits = 2L,
  proportion.test = results.subtitle,
  digits.perc = 0L,
  bf.message = TRUE,
  ratio = NULL,
  conf.level = 0.95,
  sampling.plan = "indepMulti",
  fixed.margin = "rows",
  prior.concentration = 1,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  legend.title = NULL,
  xlab = NULL,
  ylab = NULL,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Dark2",
  ggplot.component = NULL,
  ...
)
```

## Arguments

- data:

  A data frame (or a tibble) from which variables specified are to be
  taken. Other data types (e.g., matrix,table, array, etc.) will **not**
  be accepted. Additionally, grouped data frames from `{dplyr}` should
  be ungrouped before they are entered as `data`.

- x:

  The variable to use as the **rows** in the contingency table. Please
  note that if there are empty factor levels in your variable, they will
  be dropped.

- y:

  The variable to use as the **columns** in the contingency table.
  Please note that if there are empty factor levels in your variable,
  they will be dropped. Default is `NULL`. If `NULL`, one-sample
  proportion test (a goodness of fit test) will be run for the `x`
  variable. Otherwise an appropriate association test will be run. This
  argument can not be `NULL` for `ggbarstats()`.

- counts:

  The variable in data containing counts, or `NULL` if each row
  represents a single observation.

- type:

  A character specifying the type of statistical approach:

  - `"parametric"`

  - `"nonparametric"`

  - `"robust"`

  - `"bayes"`

  You can specify just the initial letter.

- paired:

  Logical indicating whether data came from a within-subjects or
  repeated measures design study (Default: `FALSE`).

- results.subtitle:

  Decides whether the results of statistical tests are to be displayed
  as a subtitle (Default: `TRUE`). If set to `FALSE`, only the plot will
  be returned.

- label:

  Character decides what information needs to be displayed on the label
  in each pie slice. Possible options are `"percentage"` (default),
  `"counts"`, `"both"`.

- label.args:

  Additional aesthetic arguments that will be passed to
  [`ggplot2::geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html).

- sample.size.label.args:

  Additional aesthetic arguments that will be passed to
  [`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html).

- digits:

  Number of digits for rounding or significant figures. May also be
  `"signif"` to return significant figures or `"scientific"` to return
  scientific notation. Control the number of digits by adding the value
  as suffix, e.g. `digits = "scientific4"` to have scientific notation
  with 4 decimal places, or `digits = "signif5"` for 5 significant
  figures (see also [`signif()`](https://rdrr.io/r/base/Round.html)).

- proportion.test:

  Decides whether proportion test for `x` variable is to be carried out
  for each level of `y`. Defaults to `results.subtitle`. In
  `ggbarstats()`, only *p*-values from this test will be displayed.

- digits.perc:

  Numeric that decides number of decimal places for percentage labels
  (Default: `0L`).

- bf.message:

  Logical that decides whether to display Bayes Factor in favor of the
  *null* hypothesis. This argument is relevant only **for parametric
  test** (Default: `TRUE`).

- ratio:

  A vector of proportions: the expected proportions for the proportion
  test (should sum to `1`). Default is `NULL`, which means the null is
  equal theoretical proportions across the levels of the nominal
  variable. E.g., `ratio = c(0.5, 0.5)` for two levels,
  `ratio = c(0.25, 0.25, 0.25, 0.25)` for four levels, etc.

- conf.level:

  Scalar between `0` and `1` (default: `95%` confidence/credible
  intervals, `0.95`). If `NULL`, no confidence intervals will be
  computed.

- sampling.plan:

  Character describing the sampling plan. Possible options:

  - `"indepMulti"` (independent multinomial; default)

  - `"poisson"`

  - `"jointMulti"` (joint multinomial)

  - `"hypergeom"` (hypergeometric). For more, see
    [`BayesFactor::contingencyTableBF()`](https://rdrr.io/pkg/BayesFactor/man/contingencyTableBF.html).

- fixed.margin:

  For the independent multinomial sampling plan, which margin is fixed
  (`"rows"` or `"cols"`). Defaults to `"rows"`.

- prior.concentration:

  Specifies the prior concentration parameter, set to `1` by default. It
  indexes the expected deviation from the null hypothesis under the
  alternative, and corresponds to Gunel and Dickey's (1974) `"a"`
  parameter.

- title:

  The text for the plot title.

- subtitle:

  The text for the plot subtitle. Will work only if
  `results.subtitle = FALSE`.

- caption:

  The text for the plot caption. This argument is relevant only if
  `bf.message = FALSE`.

- legend.title:

  Title text for the legend.

- xlab:

  Label for `x` axis variable. If `NULL` (default), variable name for
  `x` will be used.

- ylab:

  Labels for `y` axis variable. If `NULL` (default), variable name for
  `y` will be used.

- ggtheme:

  A `{ggplot2}` theme. Default value is
  [`theme_ggstatsplot()`](https://indrajeetpatil.github.io/ggstatsplot/reference/theme_ggstatsplot.md).
  Any of the `{ggplot2}` themes (e.g.,
  [`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)),
  or themes from extension packages are allowed (e.g.,
  `ggthemes::theme_fivethirtyeight()`, `hrbrthemes::theme_ipsum_ps()`,
  etc.). But note that sometimes these themes will remove some of the
  details that `{ggstatsplot}` plots typically contains. For example, if
  relevant,
  [`ggbetweenstats()`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.md)
  shows details about multiple comparison test as a label on the
  secondary Y-axis. Some themes (e.g.
  `ggthemes::theme_fivethirtyeight()`) will remove the secondary Y-axis
  and thus the details as well.

- package, palette:

  Name of the package from which the given palette is to be extracted.
  The available palettes and packages can be checked by running
  `View(paletteer::palettes_d_names)`.

- ggplot.component:

  A `ggplot` component to be added to the plot prepared by
  `{ggstatsplot}`. This argument is primarily helpful for `grouped_`
  variants of all primary functions. Default is `NULL`. The argument
  should be entered as a `{ggplot2}` function or a list of `{ggplot2}`
  functions.

- ...:

  Currently ignored.

## Details

For details, see:
<https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggpiestats.html>

## Summary of graphics

|  |  |  |
|----|----|----|
| graphical element | `geom` used | argument for further modification |
| bars | [`ggplot2::geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.html) | `NA` |
| descriptive labels | [`ggplot2::geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html) | `label.args` |
| sample size labels | [`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html) | `sample.size.label.args` |

## Contingency table analyses

The table below provides summary about:

- statistical test carried out for inferential statistics

- type of effect size estimate and a measure of uncertainty for this
  estimate

- functions used internally to compute these details

### two-way table

**Hypothesis testing**

|  |  |  |  |
|----|----|----|----|
| Type | Design | Test | Function used |
| Parametric/Non-parametric | Unpaired | Pearson's chi-squared test | [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) |
| Bayesian | Unpaired | Bayesian Pearson's chi-squared test | [`BayesFactor::contingencyTableBF()`](https://rdrr.io/pkg/BayesFactor/man/contingencyTableBF.html) |
| Parametric/Non-parametric | Paired | McNemar's chi-squared test | [`stats::mcnemar.test()`](https://rdrr.io/r/stats/mcnemar.test.html) |
| Bayesian | Paired | No | No |

**Effect size estimation**

|  |  |  |  |  |
|----|----|----|----|----|
| Type | Design | Effect size | CI available? | Function used |
| Parametric/Non-parametric | Unpaired | Cramer's *V* | Yes | [`effectsize::cramers_v()`](https://easystats.github.io/effectsize/reference/phi.html) |
| Bayesian | Unpaired | Cramer's *V* | Yes | [`effectsize::cramers_v()`](https://easystats.github.io/effectsize/reference/phi.html) |
| Parametric/Non-parametric | Paired | Cohen's *g* | Yes | [`effectsize::cohens_g()`](https://easystats.github.io/effectsize/reference/cohens_g.html) |
| Bayesian | Paired | No | No | No |

### one-way table

**Hypothesis testing**

|  |  |  |
|----|----|----|
| Type | Test | Function used |
| Parametric/Non-parametric | Goodness of fit chi-squared test | [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) |
| Bayesian | Bayesian Goodness of fit chi-squared test | (custom) |

**Effect size estimation**

|  |  |  |  |
|----|----|----|----|
| Type | Effect size | CI available? | Function used |
| Parametric/Non-parametric | Pearson's *C* | Yes | [`effectsize::pearsons_c()`](https://easystats.github.io/effectsize/reference/phi.html) |
| Bayesian | No | No | No |

## See also

[`grouped_ggbarstats`](https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbarstats.md),
[`ggpiestats`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.md),
[`grouped_ggpiestats`](https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggpiestats.md)

## Examples

``` r
# for reproducibility
set.seed(123)

# creating a plot
p <- ggbarstats(mtcars, x = vs, y = cyl)

# looking at the plot
p


# extracting details from statistical tests
extract_stats(p)
#> $subtitle_data
#> # A tibble: 1 × 13
#>   statistic    df   p.value method                     effectsize       
#>       <dbl> <int>     <dbl> <chr>                      <chr>            
#> 1      21.3     2 0.0000232 Pearson's Chi-squared test Cramer's V (adj.)
#>   estimate conf.level conf.low conf.high conf.method conf.distribution n.obs
#>      <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
#> 1    0.789       0.95    0.371         1 ncp         chisq                32
#>   expression
#>   <list>    
#> 1 <language>
#> 
#> $caption_data
#> # A tibble: 1 × 15
#>   term  conf.level effectsize estimate conf.low conf.high
#>   <chr>      <dbl> <chr>         <dbl>    <dbl>     <dbl>
#> 1 Ratio       0.95 Cramers_v     0.683    0.436     0.840
#>   prior.distribution      prior.location prior.scale   bf10
#>   <chr>                            <dbl>       <dbl>  <dbl>
#> 1 independent multinomial              0           1 30129.
#>   method                              conf.method log_e_bf10 n.obs expression
#>   <chr>                               <chr>            <dbl> <int> <list>    
#> 1 Bayesian contingency table analysis ETI               10.3    32 <language>
#> 
#> $pairwise_comparisons_data
#> NULL
#> 
#> $descriptive_data
#> # A tibble: 5 × 5
#>   cyl   vs    counts   perc .label
#>   <fct> <fct>  <int>  <dbl> <chr> 
#> 1 4     1         10  90.9  91%   
#> 2 6     1          4  57.1  57%   
#> 3 4     0          1   9.09 9%    
#> 4 6     0          3  42.9  43%   
#> 5 8     0         14 100    100%  
#> 
#> $one_sample_data
#> # A tibble: 3 × 19
#>   cyl   counts  perc N        statistic    df p.value method effectsize estimate
#>   <fct>  <int> <dbl> <chr>        <dbl> <dbl>   <dbl> <chr>  <chr>         <dbl>
#> 1 8         14  43.8 (n = 14)    14         1 1.83e-4 Chi-s… Pearson's…    0.707
#> 2 6          7  21.9 (n = 7)      0.143     1 7.05e-1 Chi-s… Pearson's…    0.141
#> 3 4         11  34.4 (n = 11)     7.36      1 6.66e-3 Chi-s… Pearson's…    0.633
#> # ℹ 9 more variables: conf.level <dbl>, conf.low <dbl>, conf.high <dbl>,
#> #   conf.method <chr>, conf.distribution <chr>, n.obs <int>, expression <list>,
#> #   .label <glue>, .p.label <glue>
#> 
#> $tidy_data
#> NULL
#> 
#> $glance_data
#> NULL
#> 
#> attr(,"class")
#> [1] "ggstatsplot_stats" "list"             
```
