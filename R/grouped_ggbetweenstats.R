#' @title Violin plots for group or condition comparisons in between-subjects
#'   designs repeated across all levels of a grouping variable.
#' @name grouped_ggbetweenstats
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("maturing")}
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
#' @importFrom dplyr select
#' @importFrom rlang as_name ensym
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
#'   grouping.var = drv,
#'   conf.level = 0.99
#' )
#'
#' # modifying individual plots using `ggplot.component` argument
#' grouped_ggbetweenstats(
#'   data = dplyr::filter(
#'     ggstatsplot::movies_long,
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
                                   x,
                                   y,
                                   grouping.var,
                                   outlier.label = NULL,
                                   output = "plot",
                                   plotgrid.args = list(),
                                   annotation.args = list(),
                                   ...) {

  # ======================== preparing dataframe ==========================

  # creating a dataframe
  df <-
    data %>%
    dplyr::select({{ grouping.var }}, {{ x }}, {{ y }}, {{ outlier.label }}) %>%
    grouped_list(grouping.var = {{ grouping.var }})

  # ============== creating a list of plots using `pmap`=======================

  plotlist_purrr <-
    purrr::pmap(
      .l = list(data = df, title = names(df)),
      .f = ggstatsplot::ggbetweenstats,
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
    return(plotlist_purrr) # subtitle list
  }
}
