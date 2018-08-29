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
#' @param title.prefix Character specifying the prefix text for the fixed plot
#'   title (name of each factor level) (Default: `"Group"`).
#' @inheritParams gghistostats
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
#' @seealso \code{\link{gghistostats}}
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/gghistostats.html}
#'
#' @examples
#' 
#' ggstatsplot::grouped_gghistostats(
#'   data = iris,
#'   x = Sepal.Length,
#'   test.value = 5,
#'   grouping.var = Species,
#'   bar.fill = "orange",
#'   nrow = 1,
#'   messages = FALSE
#' )
#' @export
#'

# defining the function
grouped_gghistostats <- function(data,
                                 x,
                                 grouping.var,
                                 title.prefix = "Group",
                                 binwidth = NULL,
                                 bar.measure = "count",
                                 xlab = NULL,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL,
                                 type = "parametric",
                                 test.value = 0,
                                 bf.prior = 0.707,
                                 bf.message = FALSE,
                                 robust.estimator = "onestep",
                                 nboot = 500,
                                 k = 3,
                                 ggtheme = ggplot2::theme_bw(),
                                 ggstatsplot.layer = TRUE,
                                 fill.gradient = FALSE,
                                 low.color = "#0072B2",
                                 high.color = "#D55E00",
                                 bar.fill = "grey50",
                                 results.subtitle = TRUE,
                                 centrality.para = "mean",
                                 centrality.color = "blue",
                                 centrality.size = 1.2,
                                 centrality.linetype = "dashed",
                                 centrality.line.labeller = TRUE,
                                 centrality.k = 2,
                                 test.value.line = FALSE,
                                 test.value.color = "black",
                                 test.value.size = 1.2,
                                 test.value.linetype = "dashed",
                                 test.line.labeller = TRUE,
                                 test.k = 0,
                                 messages = TRUE,
                                 ...) {
  # ================== preparing dataframe ==================

  # getting the dataframe ready
  df <- dplyr::select(
    .data = data,
    !!rlang::enquo(grouping.var),
    !!rlang::enquo(x)
  ) %>%
    dplyr::mutate(
      .data = .,
      title.text = !!rlang::enquo(grouping.var)
    )

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

  # creating a list of plots
  plotlist_purrr <- df %>%
    dplyr::mutate(
      .data = .,
      plots = data %>%
        purrr::set_names(!!rlang::enquo(grouping.var)) %>%
        purrr::map(
          .x = .,
          .f = ~ggstatsplot::gghistostats(
            data = .,
            x = !!rlang::enquo(x),
            bar.measure = bar.measure,
            xlab = xlab,
            title = glue::glue("{title.prefix}: {as.character(.$title.text)}"),
            subtitle = subtitle,
            caption = caption,
            type = type,
            test.value = test.value,
            bf.prior = bf.prior,
            bf.message = bf.message,
            robust.estimator = robust.estimator,
            nboot = nboot,
            low.color = low.color,
            high.color = high.color,
            bar.fill = bar.fill,
            k = k,
            results.subtitle = results.subtitle,
            centrality.para = centrality.para,
            centrality.color = centrality.color,
            centrality.size = centrality.size,
            centrality.linetype = centrality.linetype,
            centrality.line.labeller = centrality.line.labeller,
            centrality.k = centrality.k,
            test.value.line = test.value.line,
            test.value.color = test.value.color,
            test.value.size = test.value.size,
            test.value.linetype = test.value.linetype,
            test.line.labeller = test.line.labeller,
            test.k = test.k,
            binwidth = binwidth,
            ggtheme = ggtheme,
            ggstatsplot.layer = ggstatsplot.layer,
            fill.gradient = fill.gradient,
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
