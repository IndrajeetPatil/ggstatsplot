#' @title Violin plots for group or condition comparisons in within-subjects
#'   designs repeated across all levels of a grouping variable.
#' @name grouped_ggwithinstats
#' @description A combined plot of comparison plot created for levels of a
#'   grouping variable.
#'
#' @inheritParams ggwithinstats
#' @inheritDotParams ggwithinstats -title
#' @inheritParams grouped_ggbetweenstats
#'
#' @import ggplot2
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo quo_name ensym !!!
#' @importFrom purrr pmap
#'
#' @seealso \code{\link{ggwithinstats}}, \code{\link{ggbetweenstats}},
#' \code{\link{grouped_ggbetweenstats}}
#'
#' @inherit ggwithinstats return references
#'
#' @examples
#' \donttest{
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # the most basic function call
#' ggstatsplot::grouped_ggwithinstats(
#'   data = VR_dilemma,
#'   x = modality,
#'   y = score,
#'   grouping.var = order,
#'   ggplot.component = ggplot2::scale_y_continuous(
#'     breaks = seq(0, 1, 0.1),
#'     limits = c(0, 1)
#'   )
#' )
#' }
#' @export

# defining the function
grouped_ggwithinstats <- function(data,
                                  x,
                                  y,
                                  grouping.var,
                                  outlier.label = NULL,
                                  title.prefix = NULL,
                                  output = "plot",
                                  plotgrid.args = list(),
                                  annotation.args = list(),
                                  ...) {

  # ======================== preparing dataframe =============================

  # if `title.prefix` is not provided, use the variable `grouping.var` name
  if (is.null(title.prefix)) title.prefix <- rlang::as_name(rlang::ensym(grouping.var))

  # creating a dataframe
  df <-
    dplyr::select(
      .data = data,
      {{ grouping.var }},
      {{ x }},
      {{ y }},
      {{ outlier.label }}
    ) %>%
    grouped_list(data = ., grouping.var = {{ grouping.var }})

  # ============== creating a list of plots using `pmap`======================

  plotlist_purrr <-
    purrr::pmap(
      .l = list(data = df, title = paste0(title.prefix, ": ", names(df))),
      .f = ggstatsplot::ggwithinstats,
      # put common parameters here
      x = {{ x }},
      y = {{ y }},
      outlier.label = {{ outlier.label }},
      output = output,
      ...
    )

  # combining the list of plots into a single plot
  if (output == "plot") {
    return(combine_plots(plotlist_purrr, plotgrid.args = plotgrid.args, annotation.args = annotation.args))
  } else {
    # subtitle list
    return(plotlist_purrr)
  }
}
