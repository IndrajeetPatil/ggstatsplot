# Default theme used in `{ggstatsplot}`

Theme used by default in all `{ggstatsplot}` plots, built on
[`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).
Use the `ggtheme` argument of any `{ggstatsplot}` function to choose a
different theme.

## Usage

``` r
theme_ggstatsplot()
```

## Value

A `{ggplot2}` theme object.

## Examples

``` r
library(ggplot2)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  theme_ggstatsplot()
```
