#' @title Dot plot/chart for labeled numeric data.
#' @name ggdotplotstats
#' @aliases ggdotchartstats
#' @description A dot chart with statistical details from one-sample test
#'   included in the plot as a subtitle.
#' @author Indrajeet Patil
#'
#' @param y Label or grouping variable.
#' @param ylab Label for `y` axis variable.
#' @param point.color Character describing color for the point (Default:
#'   `"black"`).
#' @inheritParams histo_labeller
#' @inheritParams gghistostats
#' @inheritParams ggcoefstats
#'
#' @importFrom dplyr row_number percent_rank
#'
#' @seealso \code{\link{grouped_gghistostats}}, \code{\link{gghistostats}},
#'  \code{\link{grouped_ggdotplotstats}}
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#'
#' # plot
#' ggdotplotstats(
#'   data = ggplot2::mpg,
#'   x = cty,
#'   y = manufacturer,
#'   conf.level = 0.99,
#'   test.value = 15,
#'   test.value.line = TRUE,
#'   test.line.labeller = TRUE,
#'   test.value.color = "red",
#'   centrality.para = "median",
#'   centrality.k = 0,
#'   title = "Fuel economy data",
#'   xlab = "city miles per gallon",
#'   caption = substitute(
#'     paste(italic("Source"), ": EPA dataset on http://fueleconomy.gov")
#'   )
#' )
#' @export

# function body
ggdotplotstats <- function(data,
                           x,
                           y,
                           xlab = NULL,
                           ylab = NULL,
                           title = NULL,
                           stat.title = NULL,
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
                           results.subtitle = TRUE,
                           ggtheme = ggplot2::theme_bw(),
                           ggstatsplot.layer = TRUE,
                           point.color = "black",
                           point.size = 3,
                           point.shape = 16,
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
                           return = "plot",
                           messages = TRUE) {

  # ------------------------------ variable names ----------------------------

  # if `xlab` is not provided, use the variable `x` name
  if (is.null(xlab)) {
    xlab <- rlang::as_name(rlang::ensym(x))
  }

  # if `ylab` is not provided, use the variable `y` name
  if (is.null(ylab)) {
    ylab <- rlang::as_name(rlang::ensym(y))
  }

  # --------------------------- data preparation ----------------------------

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., x = {{ x }}, y = {{ y }}) %>%
    tidyr::drop_na(data = .) %>%
    dplyr::mutate(.data = ., y = droplevels(as.factor(y))) %>%
    dplyr::group_by(.data = ., y) %>%
    dplyr::summarise(.data = ., x = mean(x, na.rm = TRUE)) %>%
    dplyr::ungroup(x = .) %>% # rank ordering the data
    dplyr::arrange(.data = ., x) %>%
    dplyr::mutate(.data = ., y = factor(y, levels = .$y)) %>%
    dplyr::mutate(
      .data = .,
      percent_rank = dplyr::percent_rank(x),
      rank = dplyr::row_number()
    ) %>%
    tibble::as_tibble(x = .)

  # ================ stats labels ==========================================

  if (isTRUE(results.subtitle)) {
    # preparing the BF message for NULL
    if (isTRUE(bf.message)) {
      bf.caption.text <-
        bf_one_sample_ttest(
          data = data,
          x = x,
          test.value = test.value,
          bf.prior = bf.prior,
          caption = caption,
          output = "caption",
          k = k
        )
    }

    # preparing the subtitle with statistical results
    subtitle <-
      subtitle_t_onesample(
        data = data,
        x = x,
        type = type,
        test.value = test.value,
        bf.prior = bf.prior,
        robust.estimator = robust.estimator,
        effsize.type = effsize.type,
        effsize.noncentral = effsize.noncentral,
        conf.type = "norm",
        conf.level = conf.level,
        nboot = nboot,
        k = k,
        stat.title = stat.title,
        messages = messages
      )
  }

  # ------------------------------ basic plot ----------------------------

  # if bayes factor message needs to be displayed
  if (isTRUE(results.subtitle) &&
    type %in% c("parametric", "p") && isTRUE(bf.message)) {
    caption <- bf.caption.text
  }

  # creating the basic plot
  plot <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(x = x, y = rank)
  ) +
    ggplot2::geom_point(
      color = point.color,
      size = point.size,
      shape = point.shape,
      na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(
      name = ylab,
      labels = data$y,
      breaks = data$rank,
      sec.axis = ggplot2::dup_axis(
        name = "percentile",
        breaks = seq(
          from = 1,
          to = nrow(data),
          by = (nrow(data) - 1) / 4
        ),
        labels = 25 * 0:4
      )
    ) +
    ggplot2::scale_x_continuous(
      name = xlab,
      sec.axis = ggplot2::dup_axis(name = ggplot2::element_blank())
    )

  # ====================== centrality line and label ========================

  # computing statistics needed for displaying labels
  y_label_pos <- median(
    x = ggplot2::layer_scales(plot)$y$range$range,
    na.rm = TRUE
  )

  # using custom function for adding labels
  plot <- histo_labeller(
    plot = plot,
    x = data$x,
    y.label.position = y_label_pos,
    centrality.para = centrality.para,
    centrality.color = centrality.color,
    centrality.size = centrality.size,
    centrality.linetype = centrality.linetype,
    centrality.line.labeller = centrality.line.labeller,
    centrality.k = centrality.k,
    test.value = test.value,
    test.value.line = test.value.line,
    test.value.color = test.value.color,
    test.value.size = test.value.size,
    test.value.linetype = test.value.linetype,
    test.line.labeller = test.line.labeller,
    test.k = test.k
  )

  # ------------------------ annotations and themes -------------------------

  # specifying theme and labels for the final plot
  plot <- plot +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    ggstatsplot::theme_ggstatsplot(
      ggtheme = ggtheme,
      ggstatsplot.layer = ggstatsplot.layer
    ) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_line(
        color = "black",
        size = 0.1,
        linetype = "dashed"
      )
    )

  # ---------------- adding ggplot component ---------------------------------

  # if any additional modification needs to be made to the plot
  # this is primarily useful for grouped_ variant of this function
  plot <- plot + ggplot.component

  # ============================= messages =================================

  # display normality test result as a message
  if (isTRUE(messages)) {
    normality_message(
      x = data$x,
      lab = xlab,
      k = k,
      output = "message"
    )
  }

  # return the plot
  return(switch(
    EXPR = return,
    "plot" = plot,
    "subtitle" = subtitle,
    "caption" = caption,
    plot
  ))
}
