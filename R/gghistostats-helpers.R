#' @title Custom function for adding labeled lines for `x`-axis variable.
#' @name .histo_labeller
#'
#' @description
#' Helper function for adding centrality parameter value and/or a test value for
#' the continuous, numeric `x`-axis variable.
#'
#' @param plot A `ggplot` object for which the labeled lines need to be added
#'   for a test value and/or a centrality parameter (mean/median) value.
#' @param ... Currently ignored.
#' @inheritParams statsExpressions::one_sample_test
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' library(ggplot2)
#'
#' # creating a plot; lines and labels will be superposed on this plot
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#'
#' # adding labels
#' ggstatsplot:::.histo_labeller(
#'   plot = p,
#'   x = mtcars$wt,
#'   centrality.line.args = list(color = "blue", linewidth = 1, linetype = "dashed"),
#' )
#'
#' @keywords internal
#' @autoglobal
#' @noRd
.histo_labeller <- function(plot, x, centrality.line.args, ...) {
  # compute centrality measure (with a temporary data frame)
  df_central <- suppressWarnings(centrality_description(tibble(.x = ".x", var = x), .x, var, ...))

  # adding a vertical line corresponding to centrality parameter
  plot +
    exec(geom_vline, xintercept = df_central$var, !!!centrality.line.args) +
    scale_x_continuous(
      sec.axis = sec_axis(
        trans = ~.,
        name = NULL,
        labels = parse(text = df_central$expression),
        breaks = df_central$var
      )
    )
}
