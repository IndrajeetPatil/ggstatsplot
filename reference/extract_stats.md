# Extracting data frames or expressions from `{ggstatsplot}` plots

Extracting data frames or expressions from `{ggstatsplot}` plots

## Usage

``` r
extract_stats(p)

extract_subtitle(p)

extract_caption(p)
```

## Arguments

- p:

  A plot from `{ggstatsplot}` package

## Value

A list of tibbles containing summaries of various statistical analyses.
The exact details included will depend on the function.

## Details

These are convenience functions to extract data frames or expressions
with statistical details that are used to create expressions displayed
in `{ggstatsplot}` plots as subtitle, caption, etc. Note that all of
this analysis is carried out by the `{statsExpressions}`
[package](https://indrajeetpatil.github.io/statsExpressions/). And so if
you are using these functions only to extract data frames, you are
better off using that package.

The only exception is the
[`ggcorrmat()`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.md)
function. But, if a data frame is what you want, you shouldn't be using
[`ggcorrmat()`](https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.md)
anyway. You can use
[`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
function which provides tidy data frames by default.

## Examples

``` r
set.seed(123)

# non-grouped plot
p1 <- ggbetweenstats(mtcars, cyl, mpg)

# grouped plot
p2 <- grouped_ggbarstats(Titanic_full, Survived, Sex, grouping.var = Age)

# extracting expressions -----------------------------

extract_subtitle(p1)
#> list(italic("F")["Welch"](2, 18.03) == "31.62", italic(p) == 
#>     "1.27e-06", widehat(omega["p"]^2) == "0.74", CI["95%"] ~ 
#>     "[" * "0.53", "1.00" * "]", italic("n")["obs"] == "32")
extract_caption(p1)
#> list(log[e] * (BF["01"]) == "-14.92", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
#>     "0.71", CI["95%"]^HDI ~ "[" * "0.57", "0.79" * "]", italic("r")["Cauchy"]^"JZS" == 
#>     "0.71")

extract_subtitle(p2)
#> [[1]]
#> list(chi["Pearson"]^2 * "(" * 1 * ")" == "460.87", italic(p) == 
#>     "3.11e-102", widehat(italic("V"))["Cramer"] == "0.47", CI["95%"] ~ 
#>     "[" * "0.43", "0.51" * "]", italic("n")["obs"] == "2,092")
#> 
#> [[2]]
#> list(chi["Pearson"]^2 * "(" * 1 * ")" == "3.03", italic(p) == 
#>     "0.08", widehat(italic("V"))["Cramer"] == "0.14", CI["95%"] ~ 
#>     "[" * "0.00", "0.34" * "]", italic("n")["obs"] == "109")
#> 
extract_caption(p2)
#> [[1]]
#> list(log[e] * (BF["01"]) == "-213.79", widehat(italic("V"))["Cramer"]^"posterior" == 
#>     "0.47", CI["95%"]^ETI ~ "[" * "0.43", "0.51" * "]", italic("a")["Gunel-Dickey"] == 
#>     "1.00")
#> 
#> [[2]]
#> list(log[e] * (BF["01"]) == "-0.03", widehat(italic("V"))["Cramer"]^"posterior" == 
#>     "0.13", CI["95%"]^ETI ~ "[" * "0.00", "0.33" * "]", italic("a")["Gunel-Dickey"] == 
#>     "1.00")
#> 

# extracting data frames -----------------------------

extract_stats(p1)
#> $subtitle_data
#> # A tibble: 1 × 14
#>   statistic    df df.error    p.value
#>       <dbl> <dbl>    <dbl>      <dbl>
#> 1      31.6     2     18.0 0.00000127
#>   method                                                   effectsize estimate
#>   <chr>                                                    <chr>         <dbl>
#> 1 One-way analysis of means (not assuming equal variances) Omega2        0.744
#>   conf.level conf.low conf.high conf.method conf.distribution n.obs expression
#>        <dbl>    <dbl>     <dbl> <chr>       <chr>             <int> <list>    
#> 1       0.95    0.531         1 ncp         F                    32 <language>
#> 
#> $caption_data
#> # A tibble: 6 × 17
#>   term     pd prior.distribution prior.location prior.scale     bf10
#>   <chr> <dbl> <chr>                       <dbl>       <dbl>    <dbl>
#> 1 mu    1     cauchy                          0       0.707 3008850.
#> 2 cyl-4 1     cauchy                          0       0.707 3008850.
#> 3 cyl-6 0.780 cauchy                          0       0.707 3008850.
#> 4 cyl-8 1     cauchy                          0       0.707 3008850.
#> 5 sig2  1     cauchy                          0       0.707 3008850.
#> 6 g_cyl 1     cauchy                          0       0.707 3008850.
#>   method                          log_e_bf10 effectsize         estimate std.dev
#>   <chr>                                <dbl> <chr>                 <dbl>   <dbl>
#> 1 Bayes factors for linear models       14.9 Bayesian R-squared    0.714  0.0503
#> 2 Bayes factors for linear models       14.9 Bayesian R-squared    0.714  0.0503
#> 3 Bayes factors for linear models       14.9 Bayesian R-squared    0.714  0.0503
#> 4 Bayes factors for linear models       14.9 Bayesian R-squared    0.714  0.0503
#> 5 Bayes factors for linear models       14.9 Bayesian R-squared    0.714  0.0503
#> 6 Bayes factors for linear models       14.9 Bayesian R-squared    0.714  0.0503
#>   conf.level conf.low conf.high conf.method n.obs expression
#>        <dbl>    <dbl>     <dbl> <chr>       <int> <list>    
#> 1       0.95    0.574     0.788 HDI            32 <language>
#> 2       0.95    0.574     0.788 HDI            32 <language>
#> 3       0.95    0.574     0.788 HDI            32 <language>
#> 4       0.95    0.574     0.788 HDI            32 <language>
#> 5       0.95    0.574     0.788 HDI            32 <language>
#> 6       0.95    0.574     0.788 HDI            32 <language>
#> 
#> $pairwise_comparisons_data
#> # A tibble: 3 × 9
#>   group1 group2 statistic   p.value alternative distribution p.adjust.method
#>   <chr>  <chr>      <dbl>     <dbl> <chr>       <chr>        <chr>          
#> 1 4      6          -6.67 0.00110   two.sided   q            Holm           
#> 2 4      8         -10.7  0.0000140 two.sided   q            Holm           
#> 3 6      8          -7.48 0.000257  two.sided   q            Holm           
#>   test         expression
#>   <chr>        <list>    
#> 1 Games-Howell <language>
#> 2 Games-Howell <language>
#> 3 Games-Howell <language>
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

extract_stats(p2)
#> [[1]]
#> $subtitle_data
#> # A tibble: 1 × 13
#>   statistic    df   p.value method                     effectsize       
#>       <dbl> <int>     <dbl> <chr>                      <chr>            
#> 1      461.     1 3.11e-102 Pearson's Chi-squared test Cramer's V (adj.)
#>   estimate conf.level conf.low conf.high conf.method conf.distribution n.obs
#>      <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
#> 1    0.469       0.95    0.426     0.512 ncp         chisq              2092
#>   expression
#>   <list>    
#> 1 <language>
#> 
#> $caption_data
#> # A tibble: 1 × 15
#>   term  conf.level effectsize estimate conf.low conf.high
#>   <chr>      <dbl> <chr>         <dbl>    <dbl>     <dbl>
#> 1 Ratio       0.95 Cramers_v     0.468    0.426     0.509
#>   prior.distribution      prior.location prior.scale    bf10
#>   <chr>                            <dbl>       <dbl>   <dbl>
#> 1 independent multinomial              0           1 7.02e92
#>   method                              conf.method log_e_bf10 n.obs expression
#>   <chr>                               <chr>            <dbl> <int> <list>    
#> 1 Bayesian contingency table analysis ETI               214.  2092 <language>
#> 
#> $pairwise_comparisons_data
#> NULL
#> 
#> $descriptive_data
#> # A tibble: 4 × 5
#>   Sex    Survived counts  perc .label
#>   <fct>  <fct>     <int> <dbl> <chr> 
#> 1 Female Yes         316  74.4 74%   
#> 2 Male   Yes         338  20.3 20%   
#> 3 Female No          109  25.6 26%   
#> 4 Male   No         1329  79.7 80%   
#> 
#> $one_sample_data
#> # A tibble: 2 × 19
#>   Sex    counts  perc N     statistic    df   p.value method effectsize estimate
#>   <fct>   <int> <dbl> <chr>     <dbl> <dbl>     <dbl> <chr>  <chr>         <dbl>
#> 1 Male     1667  79.7 (n =…      589.     1 3.87e-130 Chi-s… Pearson's…    0.511
#> 2 Female    425  20.3 (n =…      101.     1 1.01e- 23 Chi-s… Pearson's…    0.438
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
#> 
#> [[2]]
#> $subtitle_data
#> # A tibble: 1 × 13
#>   statistic    df p.value method                     effectsize        estimate
#>       <dbl> <int>   <dbl> <chr>                      <chr>                <dbl>
#> 1      3.03     1  0.0818 Pearson's Chi-squared test Cramer's V (adj.)    0.137
#>   conf.level conf.low conf.high conf.method conf.distribution n.obs expression
#>        <dbl>    <dbl>     <dbl> <chr>       <chr>             <int> <list>    
#> 1       0.95        0     0.343 ncp         chisq               109 <language>
#> 
#> $caption_data
#> # A tibble: 1 × 15
#>   term  conf.level effectsize estimate conf.low conf.high
#>   <chr>      <dbl> <chr>         <dbl>    <dbl>     <dbl>
#> 1 Ratio       0.95 Cramers_v     0.130        0     0.329
#>   prior.distribution      prior.location prior.scale  bf10
#>   <chr>                            <dbl>       <dbl> <dbl>
#> 1 independent multinomial              0           1  1.03
#>   method                              conf.method log_e_bf10 n.obs expression
#>   <chr>                               <chr>            <dbl> <int> <list>    
#> 1 Bayesian contingency table analysis ETI             0.0313   109 <language>
#> 
#> $pairwise_comparisons_data
#> NULL
#> 
#> $descriptive_data
#> # A tibble: 4 × 5
#>   Sex    Survived counts  perc .label
#>   <fct>  <fct>     <int> <dbl> <chr> 
#> 1 Female Yes          28  62.2 62%   
#> 2 Male   Yes          29  45.3 45%   
#> 3 Female No           17  37.8 38%   
#> 4 Male   No           35  54.7 55%   
#> 
#> $one_sample_data
#> # A tibble: 2 × 19
#>   Sex    counts  perc N       statistic    df p.value method effectsize estimate
#>   <fct>   <int> <dbl> <chr>       <dbl> <dbl>   <dbl> <chr>  <chr>         <dbl>
#> 1 Male       64  58.7 (n = 6…     0.562     1   0.453 Chi-s… Pearson's…   0.0933
#> 2 Female     45  41.3 (n = 4…     2.69      1   0.101 Chi-s… Pearson's…   0.237 
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
#> 
```
