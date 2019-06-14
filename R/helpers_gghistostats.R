#' @title Adds a label to the horizontal or vertical line.
#' @name line_labeller
#' @author Indrajeet Patil
#'
#' @param plot A `ggplot` object in which the label needs to be displayed.
#' @param x,y The `x`- and `y`-axes coordinates for the label.
#' @param color Color of the label.
#' @param label.text The text to include in the label (e.g., `"mean"`).
#' @param jitter Numeric that specifies how much the label should be jittered in
#'   the vertical direction (default:  `0.25`). The sign will determine the
#'   direction (upwards or downwards).
#' @param line.direction Character that specifies whether the line on which
#'   label is to be attached is vertical (`"vline"`, default) or horizontal
#'   (`"hline"`) line.
#' @inheritParams gghistostats
#'
#' @import ggplot2
#'
#' @examples
#' # creating a basic plot
#' set.seed(123)
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # adding a label
#' ggstatsplot:::line_labeller(
#'   plot = p,
#'   x = median(mtcars$wt),
#'   y = mean(mtcars$mpg),
#'   k = 2,
#'   color = "red",
#'   label.text = "median"
#' )
#' @keywords internal

# function body
line_labeller <- function(plot,
                          x,
                          y,
                          k = 2,
                          color,
                          label.text,
                          line.direction = "vline",
                          jitter = 0.25) {
  # assigning `x` and `y` values to new position variables to avoid confusion
  if (line.direction == "vline") {
    x_pos <- x
    y_pos <- y * (1 + jitter)
    label.value <- x_pos
  } else {
    x_pos <- x * (1 + jitter)
    y_pos <- y
    label.value <- y_pos
  }

  # adding label to the plot
  plot <-
    plot +
    ggplot2::geom_label(
      mapping = ggplot2::aes(
        label = list(bquote(.(label.text) == .(
          specify_decimal_p(x = label.value, k = k)
        ))),
        x = x_pos,
        y = y_pos
      ),
      show.legend = FALSE,
      parse = TRUE,
      color = color,
      alpha = 0.5,
      na.rm = TRUE
    )

  # return the plot with label
  return(plot)
}

#' @title Custom function for adding labelled lines for `x`-axis variable.
#' @name histo_labeller
#' @description Helper function for adding centrality parameter value and/or a
#'   test value for the continuous, numeric `x`-axis variable.
#' @author Indrajeet Patil
#'
#' @import ggplot2
#'
#' @param plot A `ggplot` object for which the labelled lines need to be
#'   added for a test value and/or a centrality parameter (mean/median) value.
#' @inheritParams subtitle_t_onesample
#' @param centrality.para Decides *which* measure of central tendency (`"mean"`
#'   or `"median"`) is to be displayed as a vertical line.
#' @param centrality.color Decides color for the vertical line for centrality
#'   parameter (Default: `"blue"`).
#' @param centrality.size Decides size for the vertical line for centrality
#'   parameter (Default: `1.2`).
#' @param centrality.linetype Decides linetype for the vertical line for
#'   centrality parameter (Default: `"dashed"`).
#' @param test.value.size Decides size for the vertical line for test value
#'   (Default: `1.2`).
#' @param test.value.linetype Decides linetype for the vertical line for test
#'   value (Default: `"dashed"`).
#' @param test.value.line Decides whether test value is to be displayed as a
#'   vertical line (Default: `FALSE`).
#' @param test.value.color Decides color for the vertical line denoting test
#'   value (Default: `"black"`).
#' @param test.line.labeller A logical that decides whether line labels should
#'   be displayed for **test.value** line (Default: `TRUE`).
#' @param centrality.line.labeller A logical that decides whether line labels
#'   should be displayed for the **centrality.para** line (Default: `TRUE`).
#' @param test.k Integer denoting the number of decimal places expected for
#'   `test.value` label. (Default: `0` ).
#' @param centrality.k Integer denoting the number of decimal places expected
#'   for centrality parameter label. (Default: `2`).
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#'
#' # creating a ploton which lines and labels are to be superposed
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
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
#'
#' @keywords internal

# function body
histo_labeller <- function(plot,
                           x,
                           y.label.position,
                           centrality.para = "mean",
                           centrality.color = "blue",
                           centrality.size = 1.0,
                           centrality.linetype = "dashed",
                           centrality.line.labeller = TRUE,
                           centrality.k = 2,
                           test.value = 0,
                           test.value.line = FALSE,
                           test.value.color = "black",
                           test.value.size = 1.0,
                           test.value.linetype = "dashed",
                           test.line.labeller = TRUE,
                           test.k = 0) {
  # computing summary statistics needed for displaying labels
  x_mean <- mean(x, na.rm = TRUE)
  x_median <- median(x, na.rm = TRUE)

  # if test value is to be added
  if (isTRUE(test.value.line)) {
    plot <- plot +
      ggplot2::geom_vline(
        xintercept = test.value,
        linetype = test.value.linetype,
        color = test.value.color,
        size = test.value.size,
        na.rm = TRUE
      )

    if (isTRUE(test.line.labeller)) {
      # adding a text label with test value
      plot <-
        line_labeller(
          plot = plot,
          x = test.value,
          y = y.label.position,
          label.text = "test",
          k = test.k,
          jitter = -0.25,
          color = test.value.color
        )
    }
  }

  # if central tendency parameter is to be added
  if (!is.null(centrality.para)) {
    if (isTRUE(centrality.para) || centrality.para == "mean") {
      plot <- plot +
        ggplot2::geom_vline(
          xintercept = x_mean,
          linetype = centrality.linetype,
          color = centrality.color,
          size = centrality.size,
          na.rm = TRUE
        )

      if (isTRUE(centrality.line.labeller)) {
        # adding a text label with mean value
        plot <-
          line_labeller(
            plot = plot,
            x = x_mean,
            y = y.label.position,
            label.text = "mean",
            k = centrality.k,
            jitter = 0.25,
            color = centrality.color
          )
      }
    } else if (centrality.para == "median") {
      plot <- plot +
        ggplot2::geom_vline(
          xintercept = x_median,
          linetype = centrality.linetype,
          color = centrality.color,
          size = centrality.size,
          na.rm = TRUE
        )

      # adding a text label with median value
      if (isTRUE(centrality.line.labeller)) {
        plot <-
          line_labeller(
            plot = plot,
            x = x_median,
            y = y.label.position,
            label.text = "median",
            k = centrality.k,
            jitter = 0.25,
            color = centrality.color
          )
      }
    }
  }

  # return the plot with labels
  return(plot)
}
