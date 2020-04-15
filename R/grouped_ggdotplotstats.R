#' @title Grouped histograms for distribution of a labeled numeric variable
#' @name grouped_ggdotplotstats
#' @description Helper function for `ggstatsplot::ggdotplotstats` to apply this
#'   function across multiple levels of a given factor and combining the
#'   resulting plots using `ggstatsplot::combine_plots`.
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#'
#' @inheritParams ggdotplotstats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggdotplotstats -title
#'
#' @importFrom dplyr select bind_rows summarize mutate mutate_at mutate_if group_by
#' @importFrom rlang !! enquo quo_name ensym
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
#'
#' # removing factor level with very few no. of observations
#' df <- dplyr::filter(.data = ggplot2::mpg, cyl %in% c("4", "6", "8"))
#'
#' # plot
#' ggstatsplot::grouped_ggdotplotstats(
#'   data = df,
#'   x = "cty",
#'   y = "manufacturer",
#'   grouping.var = "cyl",
#'   test.value = 15.5,
#'   title.prefix = "cylinder count",
#'   ggplot.component = ggplot2::scale_x_continuous(
#'     sec.axis = ggplot2::dup_axis(),
#'     limits = c(12, 24),
#'     breaks = seq(12, 24, 2)
#'   ),
#'   messages = FALSE
#' )
#' }
#' @export

# defining the function
grouped_ggdotplotstats <- function(data,
                                   x,
                                   y,
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

  # ======================== preparing dataframe ============================

  # ensure the grouping variable works quoted or unquoted
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  grouping.var <- rlang::ensym(grouping.var)

  # if `title.prefix` is not provided, use the variable `grouping.var` name
  if (is.null(title.prefix)) title.prefix <- rlang::as_name(grouping.var)

  # creating a dataframe
  df <-
    dplyr::select(.data = data, {{ grouping.var }}, {{ x }}, {{ y }}) %>%
    grouped_list(data = ., grouping.var = {{ grouping.var }})

  # creating a list of plots
  plotlist_purrr <-
    purrr::pmap(
      .l = list(data = df, title = paste(title.prefix, ": ", names(df), sep = "")),
      .f = ggstatsplot::ggdotplotstats,
      x = {{ x }},
      y = {{ y }},
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
