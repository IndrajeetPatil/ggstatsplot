#'
#' @title Grouped histograms for distribution of a numeric variable
#' @name grouped_gghistostats
#' @aliases grouped_gghistostats
#' @description Helper function for `ggstatsplot::gghistostats` to apply this
#'   function across multiple levels of a given factor and combining the
#'   resulting plots using `ggstatsplot::combine_plots`.
#' @author Indrajeet Patil
#'
#' @param grouping.var Grouping variable.
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param x A numeric variable.
#' @param xlab Label for `x` axis variable.
#' @param title The text for the plot title.
#' @param subtitle The text for the plot subtitle *if* you don't want results
#'   from one sample test to be displayed.
#' @param caption The text for the plot caption.
#' @param type Type of statistic expected (`"parametric"` or `"nonparametric"`
#'   or `"bayes"`). Abbreviations accepted are `"p"` or `"np"` or `"bf"`,
#'   respectively.
#' @param test.value A number specifying the value of the null hypothesis.
#' @param bf.prior A number between 0.5 and 2 (default 0.707), the prior width
#'   to use in calculating Bayes factors.
#' @param bf.message Logical. Decides whether to display Bayes Factor in favor
#'   of null hypothesis for parametric test if the null hypothesis can't be
#'   rejected (Default: `bf.message = TRUE`).
#' @param k Number of decimal places expected for results.
#' @param low.color,high.color Colors for low and high ends of the gradient.
#'   Defaults are colorblind-friendly.
#' @param results.subtitle Decides whether the results of statistical tests are
#'   to be displayed as subtitle (Default: `results.subtitle = TRUE`). If set to
#'   `FALSE`, no statistical tests will be run.
#' @param legend.title.margin Adjusting the margin between legend title and the
#'   colorbar.
#' @param t.margin,b.margin Margins in grid units. For more details, see
#'   `?grid::unit()`.
#' @param centrality.para Decides *which* measure of central tendency (`"mean"`
#'   or `"median"`) is to be displayed as a vertical line.
#' @param centrality.color Decides color for the vertical line for centrality
#'   parameter (Default: `"blue"`).
#' @param test.value.line Decides whether test value is to be displayed as a
#'   vertical line (Default: `FALSE`).
#' @param test.value.color Decides color for the vertical line denoting test
#'   value (Default: `"black"`).
#' @param line.labeller A logical that decides whether line labels should be
#'   displayed (Default: `FALSE`).
#' @param binwidth The width of the bins. Can be specified as a numeric value,
#'   or a function that calculates width from `x`. The default is to use bins
#'   bins that cover the range of the data. You should always override this
#'   value, exploring multiple widths to find the best to illustrate the stories
#'   in your data.
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @inheritDotParams combine_plots
#'
#' @import dplyr
#' @import rlang
#'
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @importFrom purrr map
#' @importFrom tidyr nest
#'
#' @seealso \code{\link{gghistostats}}
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

# defining the function
grouped_gghistostats <- function(grouping.var,
                                 data,
                                 x,
                                 xlab = NULL,
                                 subtitle = NULL,
                                 caption = NULL,
                                 type = "parametric",
                                 test.value = 0,
                                 bf.prior = 0.707,
                                 bf.message = TRUE,
                                 k = 3,
                                 results.subtitle = TRUE,
                                 legend.title.margin = TRUE,
                                 t.margin = unit(0, "mm"),
                                 b.margin = unit(3, "mm"),
                                 centrality.para = NULL,
                                 centrality.color = "blue",
                                 test.value.line = FALSE,
                                 test.value.color = "black",
                                 line.labeller = FALSE,
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
            centrality.color = centrality.color,
            test.value.line = test.value.line,
            test.value.color = test.value.color,
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
