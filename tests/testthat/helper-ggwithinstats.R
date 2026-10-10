# shared fixtures for test-ggwithinstats.R

# build a plot of `score` by `condition` without statistical annotations, for
# checking which observations end up in the plotting data
build_within_plot <- function(data, ...) {
  ggplot2::ggplot_build(ggwithinstats(
    data = data,
    x = condition,
    y = score,
    type = "p",
    pairwise.display = "none",
    results.subtitle = FALSE,
    ...
  ))
}
