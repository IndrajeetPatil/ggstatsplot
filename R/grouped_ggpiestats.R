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
#' @inheritParams ggpiestats
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
#' @inherit ggpiestats return references
#' @inherit ggpiestats return details
#'
#' @examples
#'
#' # grouped one-sample proportion tests
#' ggstatsplot::grouped_ggpiestats(
#'   data = mtcars,
#'   grouping.var = am,
#'   main = cyl
#' )
#' @export
#'

# defining the function
grouped_ggpiestats <- function(data,
                               main,
                               condition = NULL,
                               counts = NULL,
                               grouping.var,
                               title.prefix = "Group",
                               ratio = NULL,
                               paired = FALSE,
                               factor.levels = NULL,
                               stat.title = NULL,
                               sample.size.label = TRUE,
                               caption = NULL,
                               nboot = 25,
                               legend.title = NULL,
                               facet.wrap.name = NULL,
                               k = 3,
                               facet.proptest = TRUE,
                               ggtheme = ggplot2::theme_bw(),
                               ggstatsplot.layer = TRUE,
                               package = "RColorBrewer",
                               palette = "Dark2",
                               direction = 1,
                               messages = TRUE,
                               ...) {
  # ================================ preparing dataframe ======================================

  if (!base::missing(condition)) {
    if (base::missing(counts)) {
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
      df <- dplyr::select(
        .data = data,
        !!rlang::enquo(grouping.var),
        !!rlang::enquo(main),
        !!rlang::enquo(condition),
        !!rlang::enquo(counts)
      ) %>%
        dplyr::mutate(
          .data = .,
          title.text = !!rlang::enquo(grouping.var)
        )
    }
  } else {
    # if condition variable is *not* provided
    if (base::missing(counts)) {
      df <- dplyr::select(
        .data = data,
        !!rlang::enquo(grouping.var),
        !!rlang::enquo(main)
      ) %>%
        dplyr::mutate(
          .data = .,
          title.text = !!rlang::enquo(grouping.var)
        )
    } else {
      df <- dplyr::select(
        .data = data,
        !!rlang::enquo(grouping.var),
        !!rlang::enquo(main),
        !!rlang::enquo(counts)
      ) %>%
        dplyr::mutate(
          .data = .,
          title.text = !!rlang::enquo(grouping.var)
        )
    }
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
    if (base::missing(counts)) {
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
                ratio = ratio,
                paired = paired,
                factor.levels = factor.levels,
                stat.title = stat.title,
                sample.size.label = sample.size.label,
                caption = caption,
                nboot = nboot,
                legend.title = legend.title,
                facet.wrap.name = facet.wrap.name,
                k = k,
                facet.proptest = facet.proptest,
                ggtheme = ggtheme,
                ggstatsplot.layer = ggstatsplot.layer,
                package = package,
                palette = palette,
                direction = direction,
                messages = messages
              )
            )
        )
    } else {
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
                counts = !!rlang::enquo(counts),
                title = glue::glue("{title.prefix}: {as.character(.$title.text)}"),
                ratio = ratio,
                paired = paired,
                factor.levels = factor.levels,
                stat.title = stat.title,
                sample.size.label = sample.size.label,
                caption = caption,
                nboot = nboot,
                legend.title = legend.title,
                facet.wrap.name = facet.wrap.name,
                k = k,
                facet.proptest = facet.proptest,
                ggtheme = ggtheme,
                ggstatsplot.layer = ggstatsplot.layer,
                package = package,
                palette = palette,
                direction = direction,
                messages = messages
              )
            )
        )
    }
  } else {
    if (base::missing(counts)) {
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
                ratio = ratio,
                paired = paired,
                factor.levels = factor.levels,
                stat.title = stat.title,
                caption = caption,
                nboot = nboot,
                legend.title = legend.title,
                facet.wrap.name = facet.wrap.name,
                k = k,
                facet.proptest = facet.proptest,
                ggtheme = ggtheme,
                ggstatsplot.layer = ggstatsplot.layer,
                package = package,
                palette = palette,
                direction = direction,
                messages = messages
              )
            )
        )
    } else {
      dplyr::mutate(
        .data = .,
        plots = data %>%
          purrr::set_names(!!rlang::enquo(grouping.var)) %>%
          purrr::map(
            .x = .,
            .f = ~ggstatsplot::ggpiestats(
              data = .,
              main = !!rlang::enquo(main),
              counts = !!rlang::enquo(counts),
              title = glue::glue("{title.prefix}: {as.character(.$title.text)}"),
              ratio = ratio,
              paired = paired,
              factor.levels = factor.levels,
              stat.title = stat.title,
              caption = caption,
              nboot = nboot,
              legend.title = legend.title,
              facet.wrap.name = facet.wrap.name,
              k = k,
              facet.proptest = facet.proptest,
              ggtheme = ggtheme,
              ggstatsplot.layer = ggstatsplot.layer,
              package = package,
              palette = palette,
              direction = direction,
              messages = messages
            )
          )
      )
    }
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
