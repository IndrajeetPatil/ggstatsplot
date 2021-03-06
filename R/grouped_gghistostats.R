#' @title Grouped histograms for distribution of a numeric variable
#' @name grouped_gghistostats
#'
#'
#' @description
#'
#' Helper function for `ggstatsplot::gghistostats` to apply this function
#' across multiple levels of a given factor and combining the resulting plots
#' using `ggstatsplot::combine_plots`.
#'
#' @inheritParams gghistostats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams gghistostats -title
#'
#' @importFrom dplyr select
#' @importFrom rlang ensym as_name
#' @importFrom purrr pmap
#'
#' @seealso \code{\link{gghistostats}}, \code{\link{ggdotplotstats}},
#'  \code{\link{grouped_ggdotplotstats}}
#'
#' @inherit gghistostats return references
#' @inherit gghistostats return details
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # plot
#' grouped_gghistostats(
#'   data = iris,
#'   x = Sepal.Length,
#'   test.value = 5,
#'   grouping.var = Species,
#'   plotgrid.args = list(nrow = 1),
#'   annotation.args = list(tag_levels = "i"),
#' )
#' }
#' @export
#'

# defining the function
grouped_gghistostats <- function(data,
                                 x,
                                 grouping.var,
                                 binwidth = NULL,
                                 output = "plot",
                                 plotgrid.args = list(),
                                 annotation.args = list(),
                                 ...) {

  # binwidth ------------------------------------------

  # maximum value for x
  binmax <- max(dplyr::select(data, {{ x }}), na.rm = TRUE)

  # minimum value for x
  binmin <- min(dplyr::select(data, {{ x }}), na.rm = TRUE)

  # number of datapoints
  bincount <- as.integer(data %>% dplyr::count(.))

  # dataframe ------------------------------------------

  # getting the dataframe ready
  df <- dplyr::select(data, {{ grouping.var }}, {{ x }}) %>%
    grouped_list(grouping.var = {{ grouping.var }})

  # creating a list of plots
  p_ls <- purrr::pmap(
    .l = list(data = df, title = names(df)),
    .f = ggstatsplot::gghistostats,
    # common parameters
    x = {{ x }},
    binwidth = binwidth %||% ((binmax - binmin) / sqrt(bincount)),
    output = output,
    ...
  )

  # combining the list of plots into a single plot
  if (output == "plot") p_ls <- combine_plots(p_ls, plotgrid.args, annotation.args)

  # return the object
  p_ls
}
