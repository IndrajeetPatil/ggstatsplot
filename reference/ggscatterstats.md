# Scatterplot with marginal distributions and statistical results

Scatterplots from `{ggplot2}` combined with marginal distribution plots
with statistical details.

## Usage

``` r
ggscatterstats(
  data,
  x,
  y,
  type = "parametric",
  conf.level = 0.95,
  bf.prior = 0.707,
  bf.message = TRUE,
  tr = 0.2,
  digits = 2L,
  results.subtitle = TRUE,
  label.var = NULL,
  label.expression = NULL,
  marginal = TRUE,
  point.args = list(size = 3, alpha = 0.4, stroke = 0),
  point.width.jitter = 0,
  point.height.jitter = 0,
  point.label.args = list(size = 3, max.overlaps = 1e+06),
  smooth.line.args = list(linewidth = 1.5, color = "blue", method = "lm", formula = y ~
    x),
  xsidehistogram.args = list(fill = "#4285F4", color = "black", na.rm = TRUE),
  ysidehistogram.args = list(fill = "#EA4335", color = "black", na.rm = TRUE),
  xsidehistogram.scale = list(),
  ysidehistogram.scale = list(),
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
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

  The column in `data` containing the explanatory variable to be plotted
  on the `x`-axis.

- y:

  The column in `data` containing the response (outcome) variable to be
  plotted on the `y`-axis.

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

- conf.level:

  Scalar between `0` and `1` (default: `95%` confidence/credible
  intervals, `0.95`).

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

- tr:

  Trim level for the mean when carrying out `robust` tests. In case of
  an error, try reducing the value of `tr`, which is by default set to
  `0.2`. Lowering the value might help.

- digits:

  Number of digits for rounding or significant figures. May also be
  `"signif"` to return significant figures or `"scientific"` to return
  scientific notation. Control the number of digits by adding the value
  as suffix, e.g. `digits = "scientific4"` to have scientific notation
  with 4 decimal places, or `digits = "signif5"` for 5 significant
  figures (see also [`signif()`](https://rdrr.io/r/base/Round.html)).

- results.subtitle:

  Logical that decides whether the results of statistical tests are
  displayed as a subtitle (Default: `TRUE`). Setting this to `FALSE`
  also skips the automatic Bayes Factor caption. Other plot details have
  their own arguments.

- label.var:

  Variable to use for point labels entered as a symbol (e.g. `var1`).

- label.expression:

  An expression evaluating to a logical vector that determines the
  subset of data points to label (e.g. `y < 4 & z < 20`). While using
  this argument with
  [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html),
  you will have to provide a quoted expression (e.g.
  `quote(y < 4 & z < 20)`).

- marginal:

  Logical that decides whether marginal distributions are plotted on the
  axes using `{ggside}` (Default: `TRUE`).

- point.args:

  A list of additional aesthetic arguments passed to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

- point.width.jitter, point.height.jitter:

  Amount of jitter (in data units) in `x` and `y` direction,
  respectively, passed to
  [`ggplot2::position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html)
  (Default: `0`, i.e., no jitter). Do not specify the jitter in
  `point.args`, because it is shared by the **points** and their
  **labels**.

- point.label.args:

  A list of additional aesthetic arguments passed to
  [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html),
  which displays the point labels.

- smooth.line.args:

  A list of additional aesthetic arguments passed to
  [`ggplot2::geom_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.html),
  which displays the regression line. Its confidence band uses
  `conf.level`.

- xsidehistogram.args, ysidehistogram.args:

  A list of arguments passed to
  [`ggside::geom_xsidehistogram()`](https://rdrr.io/pkg/ggside/man/geom_xsidehistogram.html)
  and
  [`ggside::geom_ysidehistogram()`](https://rdrr.io/pkg/ggside/man/geom_xsidehistogram.html),
  respectively, to change the marginal histograms.

- xsidehistogram.scale, ysidehistogram.scale:

  A list of arguments passed to
  [`ggside::scale_xsidey_continuous()`](https://rdrr.io/pkg/ggside/man/ggside-scales-continuous.html)
  and
  [`ggside::scale_ysidex_continuous()`](https://rdrr.io/pkg/ggside/man/ggside-scales-continuous.html),
  respectively, to control the scale of marginal histograms (e.g.,
  `breaks`, `limits`, `transform`). Default is
  [`list()`](https://rdrr.io/r/base/list.html) (no modifications).

- xlab:

  Label for `x` axis variable. If `NULL` (default), variable name for
  `x` will be used.

- ylab:

  Label for `y` axis variable. If `NULL` (default), variable name for
  `y` will be used.

- title:

  The text for the plot title.

- subtitle:

  The text for the plot subtitle. Used only if
  `results.subtitle = FALSE`.

- caption:

  The text for the plot caption. It will be replaced by the Bayes Factor
  caption whenever that is displayed (see `bf.message`).

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

- ggplot.component:

  A `{ggplot2}` component, or a list of components, to add to the plot
  (Default: `NULL`). This is mostly helpful for the `grouped_` variants,
  where it is applied to each individual plot.

- ...:

  Additional arguments (currently ignored).

## Value

A `ggplot` object, which can be further modified with `{ggplot2}`
functions. Use
[`extract_stats()`](https://www.indrapatil.com/ggstatsplot/reference/extract_stats.md)
to get the statistical details shown in the plot as data frames.

## Details

Rows with a missing value in either `x` or `y` are removed before
plotting and analysis.

For details, see:
<https://www.indrapatil.com/ggstatsplot/articles/web_only/ggscatterstats.html>

## Note

Point labels use
[`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
to keep them from overlapping, so plotting can become very slow (and the
plot file large) if many labels overlap.

## Summary of graphics

|  |  |  |
|----|----|----|
| graphical element | `geom` used | argument for further modification |
| raw data | [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html) | `point.args` |
| labels for raw data | [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html) | `point.label.args` |
| smooth line | [`ggplot2::geom_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.html) | `smooth.line.args` |
| marginal histograms | [`ggside::geom_xsidehistogram()`](https://rdrr.io/pkg/ggside/man/geom_xsidehistogram.html), [`ggside::geom_ysidehistogram()`](https://rdrr.io/pkg/ggside/man/geom_xsidehistogram.html) | `xsidehistogram.args`, `ysidehistogram.args` |

## Correlation analyses

Below is a summary of:

- the statistical test carried out for inferential statistics

- the type of effect size estimate and a measure of uncertainty for this
  estimate

- the functions used internally to compute these details

**Hypothesis testing and effect size estimation**

|  |  |  |  |
|----|----|----|----|
| Type | Test | CI available? | Function used |
| Parametric | Pearson's correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |
| Non-parametric | Spearman's rank correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |
| Robust | Winsorized Pearson's correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |
| Bayesian | Bayesian Pearson's correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |

## See also

[`grouped_ggscatterstats()`](https://www.indrapatil.com/ggstatsplot/reference/grouped_ggscatterstats.md),
[`ggcorrmat()`](https://www.indrapatil.com/ggstatsplot/reference/ggcorrmat.md),
[`grouped_ggcorrmat()`](https://www.indrapatil.com/ggstatsplot/reference/grouped_ggcorrmat.md)

## Examples

``` r
set.seed(123)

# creating a plot
p <- ggscatterstats(
  iris,
  x = Sepal.Width,
  y = Petal.Length,
  label.var = Species,
  label.expression = Sepal.Length > 7.6
) +
  ggplot2::geom_rug(sides = "b")

# looking at the plot
p


# extracting details from statistical tests
extract_stats(p)
#> $subtitle_data
#> # A tibble: 1 × 14
#>   parameter1  parameter2   effectsize          estimate conf.level conf.low
#>   <chr>       <chr>        <chr>                  <dbl>      <dbl>    <dbl>
#> 1 Sepal.Width Petal.Length Pearson correlation   -0.428       0.95   -0.551
#>   conf.high statistic df.error      p.value method              n.obs
#>       <dbl>     <dbl>    <int>        <dbl> <chr>               <int>
#> 1    -0.288     -5.77      148 0.0000000451 Pearson correlation   150
#>   conf.method expression
#>   <chr>       <list>    
#> 1 normal      <language>
#> 
#> $caption_data
#> # A tibble: 1 × 17
#>   parameter1  parameter2   effectsize                   estimate conf.level
#>   <chr>       <chr>        <chr>                           <dbl>      <dbl>
#> 1 Sepal.Width Petal.Length Bayesian Pearson correlation   -0.422       0.95
#>   conf.low conf.high    pd rope.percentage prior.distribution prior.location
#>      <dbl>     <dbl> <dbl>           <dbl> <chr>                       <dbl>
#> 1   -0.551    -0.290     1               0 beta                         1.41
#>   prior.scale    bf10 method                       n.obs conf.method expression
#>         <dbl>   <dbl> <chr>                        <int> <chr>       <list>    
#> 1        1.41 312665. Bayesian Pearson correlation   150 HDI         <language>
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

# customize marginal histogram bins and scales
ggscatterstats(
  mtcars,
  x = wt,
  y = mpg,
  results.subtitle = FALSE,
  xsidehistogram.args = list(fill = "#4285F4", color = "black", na.rm = TRUE, binwidth = 0.5),
  ysidehistogram.args = list(fill = "#EA4335", color = "black", na.rm = TRUE, bins = 15),
  xsidehistogram.scale = list(breaks = seq(0, 15, 5)),
  ysidehistogram.scale = list(breaks = seq(0, 15, 5))
)

```
