# Check if palette has enough number of colors

Aborts with an informative error if the number of factor levels exceeds
the number of colors available in the specified palette.

## Usage

``` r
.is_palette_sufficient(palette, min_length)
```

## Examples

``` r
ggstatsplot:::.is_palette_sufficient("ggthemes::gdoc", 6L)
try(ggstatsplot:::.is_palette_sufficient("ggthemes::gdoc", 30L))
#> Error in ggstatsplot:::.is_palette_sufficient("ggthemes::gdoc", 30L) : 
#>   ✖ Palette 'ggthemes::gdoc' has only 24 colors, but 30 are needed.
#> ℹ Select a `palette` with enough colors. Run `View(paletteer::palettes_d_names)` to see options.
```
