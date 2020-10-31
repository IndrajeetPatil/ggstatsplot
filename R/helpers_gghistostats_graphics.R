#' @title Custom function for adding labeled lines for `x`-axis variable.
#' @name histo_labeller
#' @description Helper function for adding centrality parameter value and/or a
#'   test value for the continuous, numeric `x`-axis variable.
#'
#' @import ggplot2
#'
#' @param plot A `ggplot` object for which the labeled lines need to be added
#'   for a test value and/or a centrality parameter (mean/median) value.
#' @param ... Currently ignored.
#' @inheritParams statsExpressions::expr_t_onesample
#' @param centrality.parameter Decides *which* measure of central tendency
#'   (`"mean"` or `"median"`) is to be displayed as a vertical line. To not show
#'   any parameter, set this to `"none"`.
#' @param centrality.line.args,test.value.line.args A list of additional
#'   aesthetic arguments to be passed to the `geom_line` used to display the
#'   lines corresponding to the centrality parameter and test value.
#' @param centrality.label.args,test.value.label.args A list of additional
#'   aesthetic arguments to be passed to the `geom_label` used to display the
#'   label corresponding to the centrality parameter and test value.
#' @param test.value.line Logical that decides whether a line corresponding to
#'   the `test.value` should be superimposed on the plot.
#' @param test.value.line.args A list of additional aesthetic arguments to be
#'   passed to the `geom_line` used to display the line corresponding to
#'   `test.value`.
#' @param test.k Integer denoting the number of decimal places expected for
#'   `test.value` label. (Default: `0` ).
#' @param centrality.k Integer denoting the number of decimal places expected
#'   for centrality parameter label. (Default: `2`).
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#'
#' # creating a plot; lines and labels will be superposed on this plot
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#'
#' # computing `y`-axis positions for line labels
#' y_label_pos <- median(
#'   x = ggplot2::layer_scales(p)$y$range$range,
#'   na.rm = TRUE
#' )
#' # adding labels
#' ggstatsplot:::histo_labeller(
#'   plot = p,
#'   x = mtcars$wt,
#'   y.label.position = y_label_pos,
#'   test.value.line = TRUE
#' )
#' }
#' @keywords internal

# function body
histo_labeller <- function(plot,
                           x,
                           y.label.position,
                           test.value = 0,
                           test.k = 0,
                           test.value.line = FALSE,
                           test.value.line.args = list(size = 1),
                           test.value.label.args = list(),
                           centrality.parameter = "mean",
                           centrality.k = 2,
                           centrality.line.args = list(color = "blue", size = 1),
                           centrality.label.args = list(color = "blue"),
                           ...) {
  # computing summary statistics needed for displaying labels
  x_mean <- mean(x, na.rm = TRUE)
  x_median <- median(x, na.rm = TRUE)

  # -------------------------- test value ------------------------------------

  # if test value is to be added
  if (isTRUE(test.value.line)) {
    plot <- plot +
      rlang::exec(
        .f = ggplot2::geom_vline,
        xintercept = test.value,
        na.rm = TRUE,
        linetype = "dashed",
        !!!test.value.line.args
      )

    # adding a text label with test value
    plot <-
      line_labeller(
        plot = plot,
        x = test.value,
        y = y.label.position,
        label.text = "test",
        k = test.k,
        jitter = -0.25,
        label.args = test.value.label.args
      )
  }

  # -------------------------- centrality parameter ---------------------------

  # if central tendency parameter is to be added
  if (centrality.parameter == "mean") {
    x.intercept <- x_mean
    label.text <- "mean"
  } else if (centrality.parameter == "median") {
    x.intercept <- x_median
    label.text <- "median"
  } else {
    return(plot)
  }

  # adding a vertical line corresponding to centrality parameter
  plot <- plot +
    rlang::exec(
      .f = ggplot2::geom_vline,
      xintercept = x.intercept,
      !!!centrality.line.args,
      linetype = "dashed",
      na.rm = TRUE
    )

  # adding a text label with mean value
  line_labeller(
    plot = plot,
    x = x.intercept,
    y = y.label.position,
    label.text = label.text,
    k = centrality.k,
    jitter = 0.25,
    label.args = centrality.label.args
  )
}


#' @title Adds a label to the horizontal or vertical line.
#' @name line_labeller
#'
#' @param ... Currently ignored.
#' @param plot A `ggplot` object in which the label needs to be displayed.
#' @param x,y The `x`- and `y`-axes coordinates for the label.
#' @param label.text The text to include in the label (e.g., `"mean"`).
#' @param label.args A list of additional aesthetic arguments to be
#'   passed to `geom_label`.
#' @param jitter Numeric that specifies how much the label should be jittered in
#'   the vertical direction (default:  `0.25`). The sign will determine the
#'   direction (upwards or downwards).
#' @inheritParams gghistostats
#'
#' @import ggplot2
#'
#' @examples
#' # creating a basic plot
#' set.seed(123)
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#'
#' # adding a label
#' ggstatsplot:::line_labeller(
#'   plot = p,
#'   x = median(mtcars$wt),
#'   y = mean(mtcars$mpg),
#'   k = 2,
#'   label.args = list(color = "red"),
#'   label.text = "median"
#' )
#' @keywords internal

# function body
line_labeller <- function(plot,
                          x,
                          y,
                          k = 2L,
                          label.text,
                          label.args = list(),
                          jitter = 0.25,
                          ...) {
  # assigning `x` and `y` values to new position variables to avoid confusion
  x_pos <- x
  y_pos <- y * (1 + jitter)
  label.value <- x_pos

  # adding label to the plot
  plot +
    rlang::exec(
      .f = ggplot2::geom_label,
      mapping = ggplot2::aes(
        label = list(bquote(.(label.text) == .(specify_decimal_p(label.value, k)))),
        x = x_pos,
        y = y_pos
      ),
      show.legend = FALSE,
      parse = TRUE,
      na.rm = TRUE,
      alpha = 0.5,
      !!!label.args
    )
}
