#' @title Compute subtitle and caption for one-sample test plots
#' @name .one_sample_subtitle_caption
#'
#' @description
#'
#' Shared helper for `gghistostats()` and `ggdotplotstats()` that runs the
#' one-sample test and optionally computes a Bayes Factor caption.
#'
#' @param type Character: statistical test type (e.g. `"parametric"`).
#' @param bf.message Logical: include Bayes Factor caption?
#' @param .f.args A named list of arguments forwarded to
#'   [statsExpressions::one_sample_test()].
#'
#' @return A list with elements `subtitle` and `caption`.
#'
#' @autoglobal
#' @noRd
.one_sample_subtitle_caption <- function(type, bf.message, .f.args) {
  .subtitle_caption(one_sample_test, .f.args, type, bf.message)
}


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
  df_central <- suppressWarnings(centrality_description(
    tibble(.x = ".x", var = x),
    .x,
    var,
    ...
  ))

  # adding a vertical line corresponding to centrality parameter
  plot +
    exec(geom_vline, xintercept = df_central$var, !!!centrality.line.args) +
    scale_x_continuous(
      sec.axis = sec_axis(
        transform = ~.,
        name = NULL,
        labels = as.expression(df_central$expression),
        breaks = df_central$var
      )
    )
}
