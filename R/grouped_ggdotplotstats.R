#' @title Grouped histograms for distribution of a labelled numeric variable
#' @name grouped_ggdotplotstats
#' @aliases grouped_ggdotplotstats
#' @description Helper function for `ggstatsplot::ggdotplotstats` to apply this
#'   function across multiple levels of a given factor and combining the
#'   resulting plots using `ggstatsplot::combine_plots`.
#' @author Indrajeet Patil
#'
#' @param grouping.var Grouping variable.
#' @param title.prefix Character specifying the prefix text for the fixed plot
#'   title (name of each factor level) (Default: `"Group"`).
#' @inheritParams ggdotplotstats
#' @inheritDotParams combine_plots
#'
#' @importFrom dplyr select bind_rows summarize mutate mutate_at mutate_if
#' @importFrom dplyr group_by n arrange
#' @importFrom rlang !! enquo quo_name ensym
#' @importFrom glue glue
#' @importFrom purrr map set_names
#' @importFrom tidyr nest
#'
#' @seealso \code{\link{ggdotplotstats}}
#'
#' @inherit ggdotplotstats return references
#' @inherit ggdotplotstats return details
#'
#' @examples
#' 
#' set.seed(123)
#' 
#' # creating a dataframe with summary
#' (df_summarized <-
#'   groupedstats::grouped_summary(
#'     data = ggstatsplot::intent_morality,
#'     grouping.vars = c(condition, harm),
#'     measures = rating
#'   ))
#' 
#' # plot
#' ggstatsplot::grouped_ggdotplotstats(
#'   data = df_summarized,
#'   x = mean,
#'   y = harm,
#'   grouping.var = condition,
#'   xlab = "moral judgment (average)",
#'   ylab = "type of harm",
#'   title.prefix = "intentional status",
#'   point.color = "red",
#'   point.shape = 18,
#'   point.size = 5,
#'   centrality.para = FALSE
#' )
#' @export
#'

# defining the function
grouped_ggdotplotstats <- function(data,
                                   x,
                                   y,
                                   grouping.var,
                                   title.prefix = "Group",
                                   xlab = NULL,
                                   ylab = NULL,
                                   subtitle = NULL,
                                   caption = NULL,
                                   type = "parametric",
                                   test.value = 0,
                                   bf.prior = 0.707,
                                   bf.message = FALSE,
                                   robust.estimator = "onestep",
                                   conf.level = 0.95,
                                   nboot = 500,
                                   k = 2,
                                   ggtheme = ggplot2::theme_bw(),
                                   ggstatsplot.layer = TRUE,
                                   point.color = "black",
                                   point.size = 3,
                                   point.shape = 16,
                                   results.subtitle = TRUE,
                                   centrality.para = "mean",
                                   centrality.color = "blue",
                                   centrality.size = 1.0,
                                   centrality.linetype = "dashed",
                                   centrality.line.labeller = TRUE,
                                   centrality.k = 2,
                                   test.value.line = FALSE,
                                   test.value.color = "black",
                                   test.value.size = 1.0,
                                   test.value.linetype = "dashed",
                                   test.line.labeller = TRUE,
                                   test.k = 0,
                                   messages = TRUE,
                                   ...) {

  # ======================== preparing dataframe ============================

  # ensure the grouping variable works quoted or unquoted
  grouping.var <- rlang::ensym(grouping.var)

  # getting the dataframe ready
  df <-
    dplyr::select(
      .data = data,
      !!rlang::enquo(grouping.var),
      !!rlang::enquo(x),
      !!rlang::enquo(y)
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
      .funs = ~ as.factor(.)
    ) %>%
    dplyr::mutate_if(
      .tbl = .,
      .predicate = is.factor,
      .funs = ~ base::droplevels(.)
    ) %>%
    dplyr::filter(.data = ., !is.na(!!rlang::enquo(grouping.var))) %>%
    dplyr::arrange(.data = ., !!rlang::enquo(grouping.var)) %>%
    dplyr::group_by(.data = ., !!rlang::enquo(grouping.var)) %>%
    tidyr::nest(data = .)

  # creating a list of plots
  plotlist_purrr <-
    df %>%
    dplyr::mutate(
      .data = .,
      plots = data %>%
        purrr::set_names(x = ., nm = !!rlang::enquo(grouping.var)) %>%
        purrr::map(
          .x = .,
          .f = ~ ggstatsplot::ggdotplotstats(
            data = .,
            x = !!rlang::enquo(x),
            y = !!rlang::enquo(y),
            xlab = xlab,
            ylab = ylab,
            title = glue::glue("{title.prefix}: {as.character(.$title.text)}"),
            subtitle = subtitle,
            caption = caption,
            type = type,
            test.value = test.value,
            bf.prior = bf.prior,
            bf.message = bf.message,
            robust.estimator = robust.estimator,
            conf.level = conf.level,
            nboot = nboot,
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
            ggtheme = ggtheme,
            ggstatsplot.layer = ggstatsplot.layer,
            point.color = point.color,
            point.size = point.size,
            point.shape = point.shape,
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

  # show the note about grouped_ variant producing object which is not of
  # class ggplot
  if (isTRUE(messages)) {
    grouped_message()
  }

  # return the combined plot
  return(combined_plot)
}
