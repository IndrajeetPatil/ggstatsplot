#' @title Visualization of a correlalogram (or correlation matrix) for all
#'   levels of a grouping variable
#' @name grouped_ggcorrmat
#'
#' @description
#'
#' Helper function for `ggstatsplot::ggcorrmat` to apply this function across
#' multiple levels of a given factor and combining the resulting plots using
#' `ggstatsplot::combine_plots`.
#'
#' @inheritParams ggcorrmat
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggcorrmat -title
#'
#' @importFrom dplyr select bind_rows
#' @importFrom rlang !! enquo quo_name ensym
#' @importFrom purrr map pmap
#'
#' @seealso \code{\link{ggcorrmat}}, \code{\link{ggscatterstats}},
#'   \code{\link{grouped_ggscatterstats}}
#'
#' @inherit ggcorrmat return references
#' @inherit ggcorrmat return details
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # for plot
#' if (require("ggcorrplot")) {
#'   grouped_ggcorrmat(
#'     data = iris,
#'     grouping.var = Species,
#'     type = "robust",
#'     p.adjust.method = "holm",
#'     plotgrid.args = list(ncol = 1),
#'     annotation.args = list(tag_levels = "i")
#'   )
#' }
#'
#' # for dataframe
#' grouped_ggcorrmat(
#'   data = ggplot2::msleep,
#'   grouping.var = vore,
#'   type = "bayes",
#'   output = "dataframe"
#' )
#' }
#' @export

# defining the function
grouped_ggcorrmat <- function(data,
                              cor.vars = NULL,
                              grouping.var,
                              output = "plot",
                              plotgrid.args = list(),
                              annotation.args = list(),
                              ...) {

  # ========================= preparing dataframe =============================

  # create a list of function call to check for label.expression
  param_list <- as.list(match.call())

  # getting the dataframe ready
  if ("cor.vars" %in% names(param_list)) {
    data %<>% dplyr::select({{ grouping.var }}, {{ cor.vars }})
  }

  # creating a list for grouped analysis
  df <-
    grouped_list(data = data, grouping.var = {{ grouping.var }}) %>%
    purrr::map(.x = ., .f = ~ dplyr::select(.x, -{{ grouping.var }}))

  # ===================== grouped analysis ===================================

  # creating a list of results
  p_ls <-
    purrr::pmap(
      .l = list(data = df, title = names(df)),
      .f = ggstatsplot::ggcorrmat,
      output = output,
      ...
    )

  # ===================== combining results ===================================

  # combining the list of plots into a single plot
  # inform user this can't be modified further with ggplot commands
  if (output == "plot") {
    return(combine_plots(
      plotlist = p_ls,
      guides = "keep", # each legend is going to be different
      plotgrid.args = plotgrid.args,
      annotation.args = annotation.args
    ))
  } else {
    return(dplyr::bind_rows(p_ls, .id = rlang::as_name(rlang::ensym(grouping.var))))
  }
}
