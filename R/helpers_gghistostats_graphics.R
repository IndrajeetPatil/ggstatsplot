#' @title Custom function for adding labeled lines for `x`-axis variable.
#' @name histo_labeller
#' @description Helper function for adding centrality parameter value and/or a
#'   test value for the continuous, numeric `x`-axis variable.
#'
#' @import ggplot2
#' @importFrom utils tail
#'
#' @param plot A `ggplot` object for which the labeled lines need to be added
#'   for a test value and/or a centrality parameter (mean/median) value.
#' @param ... Currently ignored.
#' @inheritParams statsExpressions::expr_t_onesample
#' @param centrality.line.args,test.value.line.args A list of additional
#'   aesthetic arguments to be passed to the `geom_line` used to display the
#'   lines corresponding to the centrality parameter and test value.
#' @param centrality.label.args,test.value.label.args A list of additional
#'   aesthetic arguments to be passed to the `geom_label` used to display the
#'   label corresponding to the centrality parameter and test value.
#' @param centrality.k Integer denoting the number of decimal places expected
#'   for centrality parameter label. (Default: `2L`).
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
                           type = "parametric",
                           tr = 0.1,
                           centrality.k = 2L,
                           centrality.line.args = list(color = "blue", size = 1),
                           centrality.label.args = list(color = "blue"),
                           ...) {

  # -------------------------- label ----------------------------------------

  y.label.position <- median(ggplot2::layer_scales(plot)$y$range$range, na.rm = TRUE)

  dat_temp <- data.frame(.temp = ".temp", "var" = x)

  centrality_df <- centrality_data(dat_temp, .temp, var, type = type, tr = tr, k = centrality.k)

  # -------------------------- centrality parameter ---------------------------

  # adding a vertical line corresponding to centrality parameter
  plot <- plot +
    rlang::exec(
      .f = ggplot2::geom_vline,
      xintercept = centrality_df$var[[1]],
      !!!centrality.line.args,
      linetype = "dashed",
      na.rm = TRUE
    )

  # adding a text label with mean value
  plot +
    rlang::exec(
      .f = ggplot2::geom_label,
      mapping = ggplot2::aes(
        label = centrality_df$label[[1]],
        x = centrality_df$var[[1]],
        y = y.label.position * 1.25
      ),
      show.legend = FALSE,
      parse = TRUE,
      na.rm = TRUE,
      alpha = 0.5,
      !!!centrality.label.args
    )
}
