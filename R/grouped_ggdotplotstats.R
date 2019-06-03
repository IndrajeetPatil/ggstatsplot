#' @title Grouped histograms for distribution of a labeled numeric variable
#' @name grouped_ggdotplotstats
#' @description Helper function for `ggstatsplot::ggdotplotstats` to apply this
#'   function across multiple levels of a given factor and combining the
#'   resulting plots using `ggstatsplot::combine_plots`.
#' @author Indrajeet Patil
#'
#' @inheritParams ggdotplotstats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams combine_plots
#'
#' @importFrom dplyr select bind_rows summarize mutate mutate_at mutate_if
#' @importFrom dplyr group_by n arrange
#' @importFrom rlang !! enquo quo_name ensym
#' @importFrom glue glue
#' @importFrom purrr pmap
#'
#' @seealso \code{\link{grouped_gghistostats}}, \code{\link{ggdotplotstats}},
#'  \code{\link{gghistostats}}
#'
#' @inherit ggdotplotstats return references
#' @inherit ggdotplotstats return details
#'
#' @examples
#'
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
#' @export

# defining the function
grouped_ggdotplotstats <- function(data,
                                   x,
                                   y,
                                   grouping.var,
                                   title.prefix = NULL,
                                   xlab = NULL,
                                   ylab = NULL,
                                   subtitle = NULL,
                                   caption = NULL,
                                   type = "parametric",
                                   test.value = 0,
                                   bf.prior = 0.707,
                                   bf.message = TRUE,
                                   robust.estimator = "onestep",
                                   conf.level = 0.95,
                                   effsize.type = "g",
                                   effsize.noncentral = TRUE,
                                   nboot = 100,
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
                                   ggplot.component = NULL,
                                   messages = TRUE,
                                   ...) {

  # ======================== preparing dataframe ============================

  # ensure the grouping variable works quoted or unquoted
  grouping.var <- rlang::ensym(grouping.var)

  # if `title.prefix` is not provided, use the variable `grouping.var` name
  if (is.null(title.prefix)) {
    title.prefix <- rlang::as_name(grouping.var)
  }

  # getting the dataframe ready
  df <-
    dplyr::select(
      .data = data,
      !!rlang::enquo(grouping.var),
      !!rlang::enquo(x),
      !!rlang::enquo(y)
    ) %>%
    tidyr::drop_na(data = .)

  # creating a list for grouped analysis
  df %<>%
    grouped_list(data = ., grouping.var = !!rlang::enquo(grouping.var))

  # list with basic arguments
  flexiblelist <- list(
    data = df,
    x = rlang::quo_text(rlang::ensym(x)),
    y = rlang::quo_text(rlang::ensym(y)),
    title = glue::glue("{title.prefix}: {names(df)}")
  )

  # creating a list of plots
  plotlist_purrr <-
    purrr::pmap(
      .l = flexiblelist,
      .f = ggstatsplot::ggdotplotstats,
      xlab = xlab,
      ylab = ylab,
      subtitle = subtitle,
      caption = caption,
      type = type,
      test.value = test.value,
      bf.prior = bf.prior,
      bf.message = bf.message,
      robust.estimator = robust.estimator,
      conf.level = conf.level,
      effsize.type = effsize.type,
      effsize.noncentral = effsize.noncentral,
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
      ggplot.component = ggplot.component,
      messages = messages
    )

  # combining the list of plots into a single plot
  combined_plot <-
    ggstatsplot::combine_plots(
      plotlist = plotlist_purrr,
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
