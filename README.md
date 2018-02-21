
<!-- README.md is generated from README.Rmd. Please edit that file -->

Overview
--------

ggstatsplot is an extension of `ggplot2` package for creating graphics with details from statistical tests included in the plots themselves and targeted primarily at behavioral sciences community to provide a one-line code to produce information-rich figures. Currently, it supports only the most common types of tests used in analysis (parametric, nonparametric, and robust versions of t-tets, anova, and contingency tables). Future versions will include other types of analyses as well.

Installation
------------

``` r
# You can get the development version from GitHub:
# install.packages("devtools")
devtools::install_github("IndrajeetPatil/ggstatsplot")
```

Examples
--------

Here are examples of the three main functions currently supported in `ggstatsplot`:

`ggbetweenstats`
================

This function creates a violin plot for between-group or between-condition comparisons with results from statistical tests in subtitle:

``` r
ggstatsplot::ggbetweenstats(data = iris, x = Species, y = Sepal.Length)
#> Note: Bartlett's test for homogeneity of variances: p-value =  < 0.001
```

![](man/figures/README-unnamed-chunk-3-1.png)

Number of other arguments can be specified to make this plot even more informative:

``` r
ggstatsplot::ggbetweenstats(data = iris, x = Species, y = Sepal.Length, mean.plotting = TRUE, 
                            type = "robust", outlier.tagging = TRUE, outlier.label = Petal.Length, # outlier tags can be from a different variable in the dataframe
                            title = "Dataset: iris", caption = "Note: this is a demo")
#> Note: Bartlett's test for homogeneity of variances: p-value =  < 0.001
#> Warning: Removed 149 rows containing missing values (geom_label_repel).
```

![](man/figures/README-unnamed-chunk-4-1.png)
