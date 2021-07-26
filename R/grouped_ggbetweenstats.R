#' @title Violin plots for group or condition comparisons in between-subjects
#'   designs repeated across all levels of a grouping variable.
#' @name grouped_ggbetweenstats
#'
#' @description
#'
#' Helper function for `ggstatsplot::ggbetweenstats` to apply this function
#' across multiple levels of a given factor and combining the resulting plots
#' using `ggstatsplot::combine_plots`.
#'
#' @inheritParams ggbetweenstats
#' @inheritParams grouped_list
#' @inheritParams combine_plots
#' @inheritDotParams ggbetweenstats -title
#'
#' @import ggplot2
#'
#' @importFrom purrr pmap
#'
#' @seealso \code{\link{ggbetweenstats}}, \code{\link{ggwithinstats}},
#'  \code{\link{grouped_ggwithinstats}}
#'
#' @inherit ggbetweenstats return references
#'
#' @examples
#' \donttest{
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # the most basic function call
#' grouped_ggbetweenstats(
#'   data = dplyr::filter(ggplot2::mpg, drv != "4"),
#'   x = year,
#'   y = hwy,
#'   grouping.var = drv
#' )
#'
#' # modifying individual plots using `ggplot.component` argument
#' grouped_ggbetweenstats(
#'   data = dplyr::filter(
#'     movies_long,
#'     genre %in% c("Action", "Comedy"),
#'     mpaa %in% c("R", "PG")
#'   ),
#'   x = genre,
#'   y = rating,
#'   grouping.var = mpaa,
#'   results.subtitle = FALSE,
#'   ggplot.component = ggplot2::scale_y_continuous(
#'     breaks = seq(1, 9, 1),
#'     limits = (c(1, 9))
#'   )
#' )
#' }
#' @export

# defining the function
grouped_ggbetweenstats <- function(data,
                                   grouping.var,
                                   output = "plot",
                                   plotgrid.args = list(),
                                   annotation.args = list(),
                                   ...) {

  # creating a dataframe
  data %<>% grouped_list(grouping.var = {{ grouping.var }})

  # creating a list of return objects
  p_ls <- purrr::pmap(
    .l = list(data = data, title = names(data)),
    .f = ggstatsplot::ggbetweenstats,
    output = output,
    ...
  )

  # combining the list of plots into a single plot
  if (output == "plot") p_ls <- combine_plots(p_ls, plotgrid.args, annotation.args)

  # return the object
  p_ls
}
