#' @title Grouped histograms for distribution of a numeric variable
#' @name grouped_gghistostats
#' @aliases grouped_gghistostats
#' @description Helper function for `ggstatsplot::gghistostats` to apply this
#'   function across multiple levels of a given factor and combining the
#'   resulting plots using `ggstatsplot::combine_plots`.
#' @author Indrajeet Patil
#'
#' @inheritParams ggstatsplot::gghistostats
#' @param grouping.vars List of grouping variables.
#' @inheritDotParams combine_plots
#'
#' @import dplyr
#' @import rlang
#'
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
#' grouping.vars = continent,
#' nrow = 3
#' )
#'
#' @export
#'

# defining global variables and functions to quient the R CMD check notes
utils::globalVariables(
  c(
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
                                 grouping.vars,
                                 xlab = NULL,
                                 title = NULL,
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
                                 binwidth.adjust = FALSE,
                                 binwidth = NULL,
                                 messages = TRUE,
                                 ...) {
  # ================== preparing dataframe ==================
  #
  # check how many variables were entered for criterion variables vector
  x <-
    as.list(rlang::quo_squash(rlang::enquo(x)))
  x <-
    if (length(x) == 1) {
      x
    } else {
      x[-1]
    }

  # check how many variables were entered for grouping variable vector
  grouping.vars <-
    as.list(rlang::quo_squash(rlang::enquo(grouping.vars)))
  grouping.vars <-
    if (length(grouping.vars) == 1) {
      grouping.vars
    } else {
      grouping.vars[-1]
    }

  # getting the dataframe ready
  df <- dplyr::select(
    .data = data,
    !!!grouping.vars,
    !!!x
  ) %>%
    dplyr::group_by(.data = ., !!!grouping.vars) %>%
    tidyr::nest(data = .)

  # creating a list of plots
  plotlist_purrr <- df %>%
    dplyr::mutate(
      .data = .,
      plots = data %>%
        purrr::set_names(!!!grouping.vars) %>%
        purrr::map(
          .x = .,
          .f = ~ggstatsplot::gghistostats(
            data = .,
            x = !!!x,
            xlab = xlab,
            title = title,
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
            binwidth.adjust = binwidth.adjust,
            binwidth = binwidth,
            messages = messages
          )
        )
    )

  # combining the list of plots into a single plot
  combined_plot <-
    ggstatsplot::combine_plots(
      plotlist = plotlist_purrr$plots,
      ...
    )

  # return the combined plot
  return(combined_plot)
}
