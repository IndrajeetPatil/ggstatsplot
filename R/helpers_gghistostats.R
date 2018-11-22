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
#' \dontrun{
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
  x_mean <- mean(x = x, na.rm = TRUE)
  x_median <- median(x = x, na.rm = TRUE)

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
        plot +
        ggplot2::geom_label(
          mapping = ggplot2::aes(
            label = list(bquote(
              "test" == .(
                ggstatsplot::specify_decimal_p(x = test.value, k = test.k)
              )
            )),
            x = test.value,
            y = y.label.position * (1 - 0.25)
          ),
          show.legend = FALSE,
          parse = TRUE,
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
          plot +
          ggplot2::geom_label(
            mapping = ggplot2::aes(
              label = list(bquote(
                "mean" == .(
                  ggstatsplot::specify_decimal_p(x = x_mean, k = centrality.k)
                )
              )),
              x = x_mean,
              y = y.label.position * (1 + 0.25)
            ),
            show.legend = FALSE,
            parse = TRUE,
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
          plot +
          ggplot2::geom_label(
            mapping = ggplot2::aes(
              label = list(bquote(
                "median" == .(
                  ggstatsplot::specify_decimal_p(x = x_median, k = centrality.k)
                )
              )),
              x = x_median,
              y = y.label.position * (1 + 0.25)
            ),
            show.legend = FALSE,
            parse = TRUE,
            color = centrality.color
          )
      }
    }
  }

  # return the plot with labels
  return(plot)
}
