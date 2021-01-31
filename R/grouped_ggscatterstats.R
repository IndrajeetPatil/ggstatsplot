#' @title Scatterplot with marginal distributions for all levels of a grouping
#'   variable
#' @name grouped_ggscatterstats
#' @description Grouped scatterplots from `ggplot2` combined with marginal
#'   histograms/boxplots/density plots with statistical details added as a
#'   subtitle.
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
#' # basic function call
#' ggstatsplot::grouped_ggscatterstats(
#'   data = dplyr::filter(movies_long, genre == "Comedy" | genre == "Drama"),
#'   x = length,
#'   y = rating,
#'   method = "lm",
#'   formula = y ~ x + I(x^3),
#'   grouping.var = genre
#' )
#'
#' # using labeling
#' # (also show how to modify basic plot from within function call)
#' grouped_ggscatterstats(
#'   data = dplyr::filter(ggplot2::mpg, cyl != 5),
#'   x = displ,
#'   y = hwy,
#'   grouping.var = cyl,
#'   title.prefix = "Cylinder count",
#'   type = "robust",
#'   label.var = manufacturer,
#'   label.expression = hwy > 25 & displ > 2.5,
#'   ggplot.component = ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis())
#' )
#'
#' # labeling without expression
#'
#' ggstatsplot::grouped_ggscatterstats(
#'   data = dplyr::filter(
#'     .data = movies_long,
#'     rating == 7,
#'     genre %in% c("Drama", "Comedy")
#'   ),
#'   x = budget,
#'   y = length,
#'   grouping.var = genre,
#'   bf.message = FALSE,
#'   label.var = "title",
#'   marginal = FALSE,
#'   title.prefix = "Genre",
#'   caption.text = "All movies have IMDB rating equal to 7."
#' )
#' @export

# defining the function
grouped_ggscatterstats <- function(data,
                                   x,
                                   y,
                                   grouping.var,
                                   label.var = NULL,
                                   label.expression = NULL,
                                   title.prefix = NULL,
                                   output = "plot",
                                   ...,
                                   plotgrid.args = list(),
                                   annotation.args = list()) {

  # ======================== preparing dataframe =============================

  # ensure the grouping variable works quoted or unquoted
  label.var <- if (!rlang::quo_is_null(rlang::enquo(label.var))) rlang::ensym(label.var)

  # if `title.prefix` is not provided, use the variable `grouping.var` name
  if (is.null(title.prefix)) title.prefix <- rlang::as_name(rlang::ensym(grouping.var))

  # getting the dataframe ready
  df <- grouped_list(data = data, grouping.var = {{ grouping.var }})

  # ==================== creating a list of plots =======================

  # creating a list of plots using `pmap`
  plotlist_purrr <-
    purrr::pmap(
      .l = list(data = df, title = paste0(title.prefix, ": ", names(df))),
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
  if (output == "plot") {
    return(combine_plots(
      plotlist = plotlist_purrr,
      plotgrid.args = plotgrid.args,
      annotation.args = annotation.args
    ))
  } else {
    return(plotlist_purrr) # subtitle list
  }
}
