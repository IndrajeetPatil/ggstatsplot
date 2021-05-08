#' @title Grouped histograms for distribution of a labeled numeric variable
#' @name grouped_ggdotplotstats
#'
#' @description
#'
#' Helper function for `ggstatsplot::ggdotplotstats` to apply this function
#' across multiple levels of a given factor and combining the resulting plots
#' using `ggstatsplot::combine_plots`.
#'
#' @inheritParams ggdotplotstats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggdotplotstats -title
#'
#' @importFrom dplyr select
#' @importFrom rlang as_name ensym
#' @importFrom purrr pmap
#'
#' @seealso \code{\link{grouped_gghistostats}}, \code{\link{ggdotplotstats}},
#'  \code{\link{gghistostats}}
#'
#' @inherit ggdotplotstats return references
#' @inherit ggdotplotstats return details
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # removing factor level with very few no. of observations
#' df <- dplyr::filter(ggplot2::mpg, cyl %in% c("4", "6", "8"))
#'
#' # plot
#' grouped_ggdotplotstats(
#'   data = df,
#'   x = cty,
#'   y = manufacturer,
#'   grouping.var = cyl,
#'   test.value = 15.5
#' )
#' }
#' @export

# defining the function
grouped_ggdotplotstats <- function(data,
                                   x,
                                   y,
                                   grouping.var,
                                   output = "plot",
                                   plotgrid.args = list(),
                                   annotation.args = list(),
                                   ...) {

  # ======================== preparing dataframe ============================

  # creating a dataframe
  df <- dplyr::select(data, {{ grouping.var }}, {{ x }}, {{ y }}) %>%
    grouped_list(grouping.var = {{ grouping.var }})

  # creating a list of plots
  p_ls <- purrr::pmap(
    .l = list(data = df, title = names(df)),
    .f = ggstatsplot::ggdotplotstats,
    x = {{ x }},
    y = {{ y }},
    output = output,
    ...
  )

  # combining the list of plots into a single plot
  if (output == "plot") p_ls <- combine_plots(p_ls, plotgrid.args, annotation.args)

  # return the object
  p_ls
}
