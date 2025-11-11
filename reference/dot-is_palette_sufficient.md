# Check if palette has enough number of colors

Informs the user about not using the default color palette when the
number of factor levels is greater than 8, the maximum number of colors
allowed by `"Dark2"` palette from the `{RColorBrewer}` package.

## Usage

``` r
.is_palette_sufficient(package, palette, min_length)
```

## Examples

``` r
ggstatsplot:::.is_palette_sufficient("RColorBrewer", "Dark2", 6L)
#> [1] TRUE
ggstatsplot:::.is_palette_sufficient("RColorBrewer", "Dark2", 12L)
#> Warning: ✖ Number of labels is greater than default palette color count.
#> ℹ Select another color `palette` (and/or `package`).
#> [1] FALSE
```
