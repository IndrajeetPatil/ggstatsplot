#' @title Grouped pie charts with statistical tests
#' @name grouped_ggpiestats
#' @description Helper function for `ggstatsplot::ggpiestats` to apply this
#'   function across multiple levels of a given factor and combining the
#'   resulting plots using `ggstatsplot::combine_plots`.
#'
#' @inheritParams ggpiestats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggpiestats -title
#'
#' @importFrom dplyr select
#' @importFrom rlang as_name ensym
#' @importFrom purrr pmap
#'
#' @seealso \code{\link{ggbarstats}}, \code{\link{ggpiestats}},
#'  \code{\link{grouped_ggbarstats}}
#'
#' @inherit ggpiestats return references
#' @inherit ggpiestats return details
#' @inherit ggpiestats return return
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # grouped one-sample proportion test
#' grouped_ggpiestats(
#'   data = mtcars,
#'   grouping.var = am,
#'   x = cyl
#' )
#' }
#' @export

# defining the function
grouped_ggpiestats <- function(data,
                               x,
                               y = NULL,
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
    .f = ggstatsplot::ggpiestats,
    # put common parameters here
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
