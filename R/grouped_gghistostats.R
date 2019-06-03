#' @title Grouped histograms for distribution of a numeric variable
#' @name grouped_gghistostats
#' @description Helper function for `ggstatsplot::gghistostats` to apply this
#'   function across multiple levels of a given factor and combining the
#'   resulting plots using `ggstatsplot::combine_plots`.
#' @author Indrajeet Patil, Chuck Powell
#'
#' @inheritParams gghistostats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams combine_plots
#'
#' @importFrom dplyr select bind_rows summarize mutate mutate_at mutate_if
#' @importFrom dplyr group_by n arrange
#' @importFrom rlang !! enquo quo_name ensym
#' @importFrom glue glue
#' @importFrom purrr pmap
#' @importFrom tidyr drop_na
#'
#' @seealso \code{\link{gghistostats}}, \code{\link{ggdotplotstats}},
#'  \code{\link{grouped_ggdotplotstats}}
#'
#' @inherit gghistostats return references
#' @inherit gghistostats return details
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
#'   ggplot.component = list(
#'     ggplot2::scale_x_continuous(breaks = seq(3, 9, 1), limits = (c(3, 9))),
#'     ggplot2::scale_y_continuous(breaks = seq(0, 25, 5), limits = (c(0, 25)))
#'   ),
#'   messages = FALSE
#' )
#' @export
#'

# defining the function
grouped_gghistostats <- function(data,
                                 x,
                                 grouping.var,
                                 title.prefix = NULL,
                                 binwidth = NULL,
                                 bar.measure = "count",
                                 xlab = NULL,
                                 subtitle = NULL,
                                 caption = NULL,
                                 type = "parametric",
                                 test.value = 0,
                                 bf.prior = 0.707,
                                 bf.message = TRUE,
                                 robust.estimator = "onestep",
                                 effsize.type = "g",
                                 effsize.noncentral = TRUE,
                                 conf.level = 0.95,
                                 nboot = 100,
                                 k = 2,
                                 ggtheme = ggplot2::theme_bw(),
                                 ggstatsplot.layer = TRUE,
                                 fill.gradient = FALSE,
                                 low.color = "#0072B2",
                                 high.color = "#D55E00",
                                 bar.fill = "grey50",
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
                                 normal.curve = FALSE,
                                 normal.curve.color = "black",
                                 normal.curve.linetype = "solid",
                                 normal.curve.size = 1.0,
                                 ggplot.component = NULL,
                                 messages = TRUE,
                                 ...) {

  # ======================== computing binwidth ============================

  # ensure the grouping variable works quoted or unquoted
  grouping.var <- rlang::ensym(grouping.var)

  # if `title.prefix` is not provided, use the variable `grouping.var` name
  if (is.null(title.prefix)) {
    title.prefix <- rlang::as_name(grouping.var)
  }

  # maximum value for x
  binmax <-
    dplyr::select(.data = data, !!rlang::enquo(x)) %>%
    max(x = ., na.rm = TRUE)

  # minimum value for x
  binmin <-
    dplyr::select(.data = data, !!rlang::enquo(x)) %>%
    min(x = ., na.rm = TRUE)

  # number of datapoints
  bincount <- as.integer(data %>% dplyr::count(x = .))

  # adding some binwidth sanity checking
  if (is.null(binwidth)) {
    binwidth <- (binmax - binmin) / sqrt(bincount)
  }

  # ======================== preparing dataframe ============================

  # getting the dataframe ready
  df <-
    dplyr::select(
      .data = data,
      !!rlang::enquo(grouping.var),
      !!rlang::enquo(x)
    ) %>%
    tidyr::drop_na(data = .)

  # creating a list for grouped analysis
  df %<>%
    grouped_list(data = ., grouping.var = !!rlang::enquo(grouping.var))

  # list with basic arguments
  flexiblelist <- list(
    data = df,
    x = rlang::quo_text(rlang::ensym(x)),
    title = glue::glue("{title.prefix}: {names(df)}")
  )

  # creating a list of plots
  plotlist_purrr <-
    purrr::pmap(
      .l = flexiblelist,
      .f = ggstatsplot::gghistostats,
      bar.measure = bar.measure,
      xlab = xlab,
      subtitle = subtitle,
      caption = caption,
      type = type,
      test.value = test.value,
      bf.prior = bf.prior,
      bf.message = bf.message,
      robust.estimator = robust.estimator,
      effsize.type = effsize.type,
      effsize.noncentral = effsize.noncentral,
      conf.level = conf.level,
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
      normal.curve = normal.curve,
      normal.curve.color = normal.curve.color,
      normal.curve.linetype = normal.curve.linetype,
      normal.curve.size = normal.curve.size,
      binwidth = binwidth,
      ggtheme = ggtheme,
      ggstatsplot.layer = ggstatsplot.layer,
      fill.gradient = fill.gradient,
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
