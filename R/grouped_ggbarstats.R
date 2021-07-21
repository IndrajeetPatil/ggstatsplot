#' @title Grouped bar (column) charts with statistical tests
#' @name grouped_ggbarstats
#'
#' @description
#'
#'
#'
#' Helper function for `ggstatsplot::ggbarstats` to apply this function across
#' multiple levels of a given factor and combining the resulting plots using
#' `ggstatsplot::combine_plots`.
#'
#' @inheritParams ggbarstats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggbarstats -title
#'
#' @import ggplot2
#'
#' @importFrom dplyr select
#' @importFrom rlang quo_name ensym
#' @importFrom purrr pmap
#'
#' @seealso \code{\link{ggbarstats}}, \code{\link{ggpiestats}},
#'  \code{\link{grouped_ggpiestats}}
#'
#' @inherit ggbarstats return references
#' @inherit ggbarstats return details
#' @inherit ggbarstats return return
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # let's create a smaller dataframe
#' diamonds_short <- ggplot2::diamonds %>%
#'   dplyr::filter(cut %in% c("Very Good", "Ideal")) %>%
#'   dplyr::filter(clarity %in% c("SI1", "SI2", "VS1", "VS2")) %>%
#'   dplyr::sample_frac(tbl = ., size = 0.05)
#'
#' # plot
#' # let's skip statistical analysis
#' grouped_ggbarstats(
#'   data = diamonds_short,
#'   x = color,
#'   y = clarity,
#'   grouping.var = cut,
#'   plotgrid.args = list(nrow = 2)
#' )
#' }
#' @export

# defining the function
grouped_ggbarstats <- function(data,
                               x,
                               y,
                               counts = NULL,
                               grouping.var,
                               output = "plot",
                               plotgrid.args = list(),
                               annotation.args = list(),
                               ...) {

  # dataframe ------------------------------------------

  # creating a dataframe
  df <- dplyr::select(data, {{ grouping.var }}, {{ x }}, {{ y }}, {{ counts }}) %>%
    grouped_list(grouping.var = {{ grouping.var }})

  # creating a list of return objects ----------------------------

  # creating a list of plots using `pmap`
  p_ls <- purrr::pmap(
    .l = list(data = df, title = names(df)),
    .f = ggstatsplot::ggbarstats,
    # common parameters
    x = {{ x }},
    y = {{ y }},
    counts = {{ counts }},
    output = output,
    ...
  )

  # combining the list of plots into a single plot
  if (output == "plot") p_ls <- combine_plots(p_ls, plotgrid.args, annotation.args)

  # return the object
  p_ls
}
