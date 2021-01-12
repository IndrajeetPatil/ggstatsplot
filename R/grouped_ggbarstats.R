#' @title Grouped bar (column) charts with statistical tests
#' @name grouped_ggbarstats
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("maturing")}
#'
#' Helper function for `ggstatsplot::ggbarstats` to apply this function across
#' multiple levels of a given factor and combining the resulting plots using
#' `ggstatsplot::combine_plots2`.
#'
#' @inheritParams ggbarstats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggbarstats -title
#'
#' @import ggplot2
#'
#' @importFrom dplyr select
#' @importFrom rlang enquo quo_name ensym
#' @importFrom purrr map
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
#'
#' # let's create a smaller dataframe
#' diamonds_short <- ggplot2::diamonds %>%
#'   dplyr::filter(.data = ., cut %in% c("Very Good", "Ideal")) %>%
#'   dplyr::filter(.data = ., clarity %in% c("SI1", "SI2", "VS1", "VS2")) %>%
#'   dplyr::sample_frac(tbl = ., size = 0.05)
#'
#' # plot
#' # let's skip statistical analysis
#' ggstatsplot::grouped_ggbarstats(
#'   data = diamonds_short,
#'   x = color,
#'   y = clarity,
#'   grouping.var = cut,
#'   title.prefix = "Quality",
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

  # ======================== preparing dataframe =============================

  # if `title.prefix` is not provided, use the variable `grouping.var` name
  if (is.null(title.prefix)) title.prefix <- rlang::as_name(rlang::ensym(grouping.var))

  # creating a dataframe
  df <-
    dplyr::select(.data = data, {{ grouping.var }}, {{ x }}, {{ y }}, {{ counts }}) %>%
    grouped_list(data = ., grouping.var = {{ grouping.var }})

  # ================ creating a list of return objects ========================

  # creating a list of plots using `pmap`
  plotlist_purrr <-
    purrr::pmap(
      .l = list(data = df, title = paste0(title.prefix, ": ", names(df))),
      .f = ggstatsplot::ggbarstats,
      # put common parameters here
      x = {{ x }},
      y = {{ y }},
      counts = {{ counts }},
      output = output,
      ...
    )

  # combining the list of plots into a single plot
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
    return(plotlist_purrr) # subtitle list
  }
}
