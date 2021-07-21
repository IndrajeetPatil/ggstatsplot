#' @title Scatterplot with marginal distributions for all levels of a grouping
#'   variable
#' @name grouped_ggscatterstats
#'
#' @description
#'
#' Grouped scatterplots from `ggplot2` combined with marginal distribution plots
#' with statistical details added as a subtitle.
#'
#' @inheritParams ggscatterstats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggscatterstats -title
#'
#' @import ggplot2
#'
#' @importFrom dplyr select
#' @importFrom rlang as_name enexpr ensym
#' @importFrom purrr pmap
#'
#' @seealso \code{\link{ggscatterstats}}, \code{\link{ggcorrmat}},
#' \code{\link{grouped_ggcorrmat}}
#'
#' @inherit ggscatterstats return references
#' @inherit ggscatterstats return details
#'
#' @examples
#' # to ensure reproducibility
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # skipping marginal distributions so that the examples run fast
#'
#' # basic function call
#' grouped_ggscatterstats(
#'   data = dplyr::filter(movies_long, genre == "Comedy" | genre == "Drama"),
#'   x = length,
#'   y = rating,
#'   type = "robust",
#'   grouping.var = genre,
#'   ggplot.component = list(ggplot2::geom_rug(sides = "b"))
#' )
#'
#' # using labeling
#' # (also show how to modify basic plot from within function call)
#' grouped_ggscatterstats(
#'   data = dplyr::filter(ggplot2::mpg, cyl != 5),
#'   x = displ,
#'   y = hwy,
#'   grouping.var = cyl,
#'   type = "robust",
#'   label.var = manufacturer,
#'   label.expression = hwy > 25 & displ > 2.5,
#'   ggplot.component = ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis())
#' )
#'
#' # labeling without expression
#'
#' grouped_ggscatterstats(
#'   data = dplyr::filter(
#'     movies_long,
#'     rating == 7,
#'     genre %in% c("Drama", "Comedy")
#'   ),
#'   x = budget,
#'   y = length,
#'   grouping.var = genre,
#'   bf.message = FALSE,
#'   label.var = "title",
#'   annotation.args = list(tag_levels = "a")
#' )
#' @export

# defining the function
grouped_ggscatterstats <- function(data,
                                   x,
                                   y,
                                   grouping.var,
                                   label.var = NULL,
                                   label.expression = NULL,
                                   output = "plot",
                                   plotgrid.args = list(),
                                   annotation.args = list(),
                                   ...) {
  # getting the dataframe ready
  df <- grouped_list(data, {{ grouping.var }})

  # creating a list of plots using `pmap`
  p_ls <- purrr::pmap(
    .l = list(data = df, title = names(df)),
    .f = ggstatsplot::ggscatterstats,
    # put common parameters here
    x = {{ x }},
    y = {{ y }},
    label.var = {{ label.var }},
    label.expression = !!rlang::enexpr(label.expression),
    output = output,
    ...
  )

  # combining the list of plots into a single plot
  if (output == "plot") p_ls <- combine_plots(p_ls, plotgrid.args, annotation.args)

  # return the object
  p_ls
}
