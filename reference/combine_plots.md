# Combining and arranging multiple plots in a grid

Wrapper around
[`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
that will return a combined grid of plots with annotations. In case you
want to create a grid of plots, it is **highly recommended** that you
use `{patchwork}` package directly and not this wrapper around it which
is mostly useful with `{ggstatsplot}` plots. It is exported only for
backward compatibility.

## Usage

``` r
combine_plots(
  plotlist,
  plotgrid.args = list(),
  annotation.args = list(),
  guides = "collect",
  ...
)
```

## Arguments

- plotlist:

  A list containing `ggplot` objects.

- plotgrid.args:

  A `list` of additional arguments passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html),
  except for `guides` argument which is already separately specified
  here.

- annotation.args:

  A `list` of additional arguments passed to
  [`patchwork::plot_annotation()`](https://patchwork.data-imaginist.com/reference/plot_annotation.html).

- guides:

  A string specifying how guides should be treated in the layout.
  `'collect'` will collect guides below to the given nesting level,
  removing duplicates. `'keep'` will stop collection at this level and
  let guides be placed alongside their plot. `auto` will allow guides to
  be collected if a upper level tries, but place them alongside the plot
  if not. If you modify default guide "position" with
  [theme(legend.position=...)](https://ggplot2.tidyverse.org/reference/theme.html)
  while also collecting guides you must apply that change to the overall
  patchwork (see example).

- ...:

  Currently ignored.

## Value

A combined plot with annotation labels.

## Examples

``` r
library(ggplot2)

# first plot
p1 <- ggplot(
  data = subset(iris, iris$Species == "setosa"),
  aes(x = Sepal.Length, y = Sepal.Width)
) +
  geom_point() +
  labs(title = "setosa")

# second plot
p2 <- ggplot(
  data = subset(iris, iris$Species == "versicolor"),
  aes(x = Sepal.Length, y = Sepal.Width)
) +
  geom_point() +
  labs(title = "versicolor")

# combining the plot with a title and a caption
combine_plots(
  plotlist = list(p1, p2),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    tag_levels = "a",
    title = "Dataset: Iris Flower dataset",
    subtitle = "Edgar Anderson collected this data",
    caption = "Note: Only two species of flower are displayed",
    theme = theme(
      plot.subtitle = element_text(size = 20),
      plot.title = element_text(size = 30)
    )
  )
)
```
