#' @title Grouped histograms for distribution of a numeric variable
#' @name grouped_gghistostats
#' @aliases grouped_gghistostats
#' @description Helper function for `ggstatsplot::gghistostats` to apply this
#'   function across multiple levels of a given factor and combining the
#'   resulting plots using `ggstatsplot::combine_plots`.
#' @author Indrajeet Patil
#'
#' @param grouping.var Grouping variable.
#' @inheritDotParams combine_plots
#' @inheritParams `gghistostats` -title
#'
#' @import dplyr
#' @import rlang
#'
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @importFrom purrr map
#' @importFrom tidyr nest
#'
#' @examples
#'
#' library(gapminder)
#'
#' ggstatsplot::grouped_gghistostats(
#' data = gapminder::gapminder,
#' x = lifeExp,
#' xlab = "Life expectancy",
#' test.value = 50,
#' grouping.var = continent,
#' nrow = 3,
#' messages = FALSE
#' )
#'
#' @export
#'

# defining global variables and functions to quient the R CMD check notes
utils::globalVariables(
  c(
    "%<>%",
    "U",
    "V",
    "Z",
    "chi",
    "counts",
    "df",
    "df1",
    "df2",
    "effsize",
    "estimate",
    "eta",
    "omega",
    "perc",
    "cramer",
    "pvalue",
    "r",
    "rho",
    "xi",
    "y",
    "z_value",
    "italic",
    "rsubtitle",
    "stats_subtitle",
    "chi_subtitle",
    "proptest_subtitle",
    "LL",
    "UL",
    "..count..",
    "dnorm",
    "mean",
    "median",
    "sd",
    "bf",
    "bf_error"
  )
)

# defining the function
grouped_gghistostats <- function(data,
                                 x,
                                 grouping.var,
                                 xlab = NULL,
                                 subtitle = NULL,
                                 caption = NULL,
                                 type = "parametric",
                                 test.value = 0,
                                 bf.prior = 0.707,
                                 bf.message = TRUE,
                                 k = 3,
                                 results.subtitle = TRUE,
                                 centrality.para = NULL,
                                 centrality.colour = "blue",
                                 test.value.line = FALSE,
                                 test.value.colour = "black",
                                 binwidth.adjust = FALSE,
                                 binwidth = NULL,
                                 messages = TRUE,
                                 ...) {
  # ================== preparing dataframe ==================

  # getting the dataframe ready
  df <- dplyr::select(.data = data,
                      !!rlang::enquo(grouping.var),
                      !!rlang::enquo(x)) %>%
    dplyr::mutate(.data = .,
                  title.text = !!rlang::enquo(grouping.var))

  # creating a nested dataframe
  df %<>%
    dplyr::group_by(.data = ., !!rlang::enquo(grouping.var)) %>%
    tidyr::nest(data = .)

  # creating a list of plots
  plotlist_purrr <- df %>%
    dplyr::mutate(
      .data = .,
      plots = data %>%
        purrr::set_names(!!rlang::enquo(grouping.var)) %>%
        purrr::map(
          .x = .,
          .f = ~ ggstatsplot::gghistostats(
            data = .,
            x = !!rlang::enquo(x),
            xlab = xlab,
            title = as.character(.$title.text),
            subtitle = subtitle,
            caption = caption,
            type = type,
            test.value = test.value,
            bf.prior = bf.prior,
            bf.message = bf.message,
            k = k,
            results.subtitle = results.subtitle,
            centrality.para = centrality.para,
            centrality.colour = centrality.colour,
            test.value.line = test.value.line,
            test.value.colour = test.value.colour,
            binwidth.adjust = binwidth.adjust,
            binwidth = binwidth,
            messages = messages
          )
        )
    )

  # combining the list of plots into a single plot
  combined_plot <-
    ggstatsplot::combine_plots(plotlist = plotlist_purrr$plots,
                               ...)

  # return the combined plot
  return(combined_plot)
}
