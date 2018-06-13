#'
#' @title Grouped pie charts with statistical tests
#' @name grouped_ggpiestats
#' @aliases grouped_ggpiestats
#' @description Helper function for `ggstatsplot::ggpiestats` to apply this
#'   function across multiple levels of a given factor and combining the
#'   resulting plots using `ggstatsplot::combine_plots`.
#' @author Indrajeet Patil
#'
#' @param grouping.var Grouping variable.
#' @param title.prefix Character specifying the prefix text for the fixed plot
#'   title (name of each factor level) (Default: `"Group"`).
#' @param data The data as a data frame.
#' @param main A string naming the variable to use as the rows in the
#'   contingency table.
#' @param condition A string naming the variable to use as the columns in the
#'   contingency table.
#' @param factor.levels A character vector with labels for factor levels of
#'   `main` variable.
#' @param stat.title Title for the effect being investigated with the chi-square
#'   test.
#' @param caption The text for the plot caption.
#' @param k Number of decimal places expected for results.
#' @param legend.title Title of legend.
#' @param facet.wrap.name The text for the facet_wrap variable label.
#' @param facet.proptest Decides whether proportion test for `main` variable is
#'   to be carried out for each level of `condition` (Default: `TRUE`).
#' @param ggtheme A function, `ggplot2` theme name. Default value is
#'   `ggplot2::theme_grey()`. Allowed values are the official `ggplot2` themes,
#'   including `theme_bw()`, `theme_minimal()`, `theme_classic()`,
#'   `theme_void()`, etc.
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @inheritDotParams combine_plots
#'
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr mutate_if
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom tidyr nest
#'
#' @seealso \code{\link{ggpiestats}}
#'
#' @examples
#'
#' # grouped one-sample proportion tests
#' ggstatsplot::grouped_ggpiestats(
#' data = mtcars,
#' grouping.var = am,
#' main = cyl
#' )
#'
#' @export
#'

# defining the function
grouped_ggpiestats <- function(grouping.var,
                               title.prefix = "Group",
                               data,
                               main,
                               condition = NULL,
                               factor.levels = NULL,
                               stat.title = NULL,
                               caption = NULL,
                               legend.title = NULL,
                               facet.wrap.name = NULL,
                               k = 3,
                               facet.proptest = TRUE,
                               ggtheme = ggplot2::theme_grey(),
                               messages = TRUE,
                               ...) {
  # ================================ preparing dataframe ======================================

  if (!base::missing(condition)) {
    # if condition variable *is* provided
    df <- dplyr::select(
      .data = data,
      !!rlang::enquo(grouping.var),
      !!rlang::enquo(main),
      !!rlang::enquo(condition)
    ) %>%
      dplyr::mutate(
        .data = .,
        title.text = !!rlang::enquo(grouping.var)
      )
  } else {
    # if condition variable is *not* provided
    df <- dplyr::select(
      .data = data,
      !!rlang::enquo(grouping.var),
      !!rlang::enquo(main)
    ) %>%
      dplyr::mutate(
        .data = .,
        title.text = !!rlang::enquo(grouping.var)
      )
  }

  # creating a nested dataframe
  df %<>%
    dplyr::mutate_if(
      .tbl = .,
      .predicate = purrr::is_bare_character,
      .funs = ~as.factor(.)
    ) %>%
    dplyr::mutate_if(
      .tbl = .,
      .predicate = is.factor,
      .funs = ~base::droplevels(.)
    ) %>%
    dplyr::arrange(.data = ., !!rlang::enquo(grouping.var)) %>%
    dplyr::group_by(.data = ., !!rlang::enquo(grouping.var)) %>%
    tidyr::nest(data = .)

  if (!base::missing(condition)) {
    # creating a list of plots
    plotlist_purrr <- df %>%
      dplyr::mutate(
        .data = .,
        plots = data %>%
          purrr::set_names(!!rlang::enquo(grouping.var)) %>%
          purrr::map(
            .x = .,
            .f = ~ggstatsplot::ggpiestats(
              data = .,
              main = !!rlang::enquo(main),
              condition = !!rlang::enquo(condition),
              title = glue::glue("{title.prefix}: {as.character(.$title.text)}"),
              factor.levels = factor.levels,
              stat.title = stat.title,
              caption = caption,
              legend.title = legend.title,
              facet.wrap.name = facet.wrap.name,
              k = k,
              facet.proptest = facet.proptest,
              ggtheme = ggtheme,
              messages = messages
            )
          )
      )
  } else {
    # creating a list of plots
    plotlist_purrr <- df %>%
      dplyr::mutate(
        .data = .,
        plots = data %>%
          purrr::set_names(!!rlang::enquo(grouping.var)) %>%
          purrr::map(
            .x = .,
            .f = ~ggstatsplot::ggpiestats(
              data = .,
              main = !!rlang::enquo(main),
              title = glue::glue("{title.prefix}: {as.character(.$title.text)}"),
              factor.levels = factor.levels,
              stat.title = stat.title,
              caption = caption,
              legend.title = legend.title,
              facet.wrap.name = facet.wrap.name,
              k = k,
              facet.proptest = facet.proptest,
              ggtheme = ggtheme,
              messages = messages
            )
          )
      )
  }

  # combining the list of plots into a single plot
  combined_plot <-
    ggstatsplot::combine_plots(
      plotlist = plotlist_purrr$plots,
      ...
    )

  # return the combined plot
  return(combined_plot)
}
