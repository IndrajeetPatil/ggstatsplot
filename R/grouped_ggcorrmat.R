#' @title Visualization of a correlalogram (or correlation matrix) for all
#'   levels of a grouping variable
#' @name grouped_ggcorrmat
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("maturing")}
#'
#' Helper function for `ggstatsplot::ggcorrmat` to apply this function across
#' multiple levels of a given factor and combining the resulting plots using
#' `ggstatsplot::combine_plots2`.
#'
#' @inheritParams ggcorrmat
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggcorrmat -title
#'
#' @importFrom dplyr select bind_rows
#' @importFrom rlang !! enquo quo_name ensym %||%
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
#'
#' # for plot
#' ggstatsplot::grouped_ggcorrmat(
#'   data = iris,
#'   grouping.var = Species,
#'   type = "robust",
#'   p.adjust.method = "holm"
#' )
#'
#' # for dataframe
#' ggstatsplot::grouped_ggcorrmat(
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
                              cor.vars.names = NULL,
                              grouping.var,
                              title.prefix = NULL,
                              output = "plot",
                              ...,
                              plotgrid.args = list(),
                              title.text = NULL,
                              title.args = list(size = 16, fontface = "bold"),
                              caption.text = NULL,
                              caption.args = list(size = 10),
                              sub.text = NULL,
                              sub.args = list(size = 12)) {

  # ========================= preparing dataframe =============================

  # create a list of function call to check for label.expression
  param_list <- as.list(match.call())

  # if `title.prefix` is not provided, use the variable `grouping.var` name
  if (is.null(title.prefix)) title.prefix <- rlang::as_name(rlang::ensym(grouping.var))

  # getting the dataframe ready
  if ("cor.vars" %in% names(param_list)) {
    data %<>% dplyr::select(.data = ., {{ grouping.var }}, {{ cor.vars }})
  }

  # creating a list for grouped analysis
  df <-
    grouped_list(data = data, grouping.var = {{ grouping.var }}) %>%
    purrr::map(.x = ., .f = ~ dplyr::select(.x, -{{ grouping.var }}))

  # ===================== grouped analysis ===================================

  # creating a list of results
  plotlist_purrr <-
    purrr::pmap(
      .l = list(data = df, title = paste0(title.prefix, ": ", names(df))),
      .f = ggstatsplot::ggcorrmat,
      cor.vars.names = cor.vars.names,
      output = output,
      ...
    )

  # ===================== combining results ===================================

  # combining the list of plots into a single plot
  # inform user this can't be modified further with ggplot commands
  if (output == "plot") {
    return(ggstatsplot::combine_plots2(
      plotlist = plotlist_purrr,
      plotgrid.args = plotgrid.args,
      title.text = title.text,
      title.args = title.args,
      caption.text = caption.text,
      caption.args = caption.args,
      sub.text = sub.text,
      sub.args = sub.args
    ))
  } else {
    return(dplyr::bind_rows(plotlist_purrr, .id = title.prefix))
  }
}
