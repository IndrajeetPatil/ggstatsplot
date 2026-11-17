# load needed dependencies
library(statsExpressions, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# load suggested dependencies that are non-optional
library(vdiffr, warn.conflicts = FALSE)

options(lifecycle_verbosity = "warning")

# no layer of a plot draws a vertical line
expect_no_vline <- function(p) {
  geoms <- vapply(p$layers, function(l) class(l$geom)[[1L]], character(1L))
  expect_false("GeomVline" %in% geoms)
}
