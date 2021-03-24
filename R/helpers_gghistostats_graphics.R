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
#' @inheritParams statsExpressions::one_sample_test
#' @param centrality.line.args A list of additional aesthetic arguments to be
#'   passed to the `geom_line` used to display the lines corresponding to the
#'   centrality parameter.
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#'
#' # creating a plot; lines and labels will be superposed on this plot
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#'
#' # adding labels
#' ggstatsplot:::histo_labeller(
#'   plot = p,
#'   x = mtcars$wt
#' )
#' }
#' @keywords internal

# function body
histo_labeller <- function(plot,
                           x,
                           type = "parametric",
                           tr = 0.2,
                           k = 2L,
                           centrality.line.args = list(color = "blue", size = 1),
                           ...) {

  # create a temporary dataframe
  dat_temp <- data.frame(.temp = ".temp", "var" = x)

  # compute centrality measure
  centrality_df <- centrality_data(dat_temp, .temp, var, type = type, tr = tr, k = k)

  # adding a vertical line corresponding to centrality parameter
  plot +
    rlang::exec(
      .f = ggplot2::geom_vline,
      xintercept = centrality_df$var[[1]],
      !!!centrality.line.args,
      linetype = "dashed",
      na.rm = TRUE
    ) +
    ggplot2::scale_x_continuous(
      sec.axis = ggplot2::sec_axis(
        trans = ~.,
        name = NULL,
        labels = parse(text = centrality_df$label[[1]]),
        breaks = centrality_df$var[[1]]
      )
    )
}
