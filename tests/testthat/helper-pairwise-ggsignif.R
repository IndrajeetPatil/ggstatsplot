# helpers for test-pairwise-ggsignif.R

# snapshot one plot per `pairwise.display` value; `title` is the common prefix
# of the snapshot titles and `...` is passed on to `plot_fn`
expect_pairwise_displays <- function(title, plot_fn, ...) {
  display_labels <- c(
    ns = "only non-significant",
    s = "only significant",
    all = "all"
  )

  for (display in names(display_labels)) {
    set.seed(123)
    vdiffr::expect_doppelganger(
      title = paste(title, display_labels[[display]], sep = " - "),
      fig = plot_fn(
        ...,
        results.subtitle = FALSE,
        pairwise.display = display,
        digits = 3L
      )
    )
  }
}

# built data of the last layer, which holds the ggsignif brackets
signif_layer_data <- function(plot) {
  layer_data <- ggplot2::ggplot_build(plot)$data
  layer_data[[length(layer_data)]]
}
