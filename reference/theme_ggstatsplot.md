# Default theme used in `{ggstatsplot}`

Common theme used across all plots generated in `{ggstatsplot}` and
*assumed* by the author to be aesthetically pleasing to the user. The
theme is a wrapper around
[`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

All `{ggstatsplot}` functions have a `ggtheme` parameter that let you
choose a different theme.

## Usage

``` r
theme_ggstatsplot()
```

## Value

A `ggplot` object.

## Examples

``` r
library(ggplot2)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  theme_ggstatsplot()
```
