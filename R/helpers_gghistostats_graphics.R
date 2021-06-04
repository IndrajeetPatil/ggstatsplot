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
#' @noRd

# function body
histo_labeller <- function(plot,
                           x,
                           centrality.line.args = list(
                             color = "blue",
                             size = 1,
                             linetype = "dashed"
                           ),
                           ...) {
  # compute centrality measure (with a temporary dataframe)
  df_central <- suppressWarnings(centrality_data(tibble(.x = ".x", "var" = x), .x, var, ...))

  # adding a vertical line corresponding to centrality parameter
  plot +
    rlang::exec(
      ggplot2::geom_vline,
      xintercept = df_central$var[[1]],
      !!!centrality.line.args
    ) +
    ggplot2::scale_x_continuous(
      sec.axis = ggplot2::sec_axis(
        trans = ~.,
        name = NULL,
        labels = parse(text = df_central$label[[1]]),
        breaks = df_central$var[[1]]
      )
    )
}
