#' @title Dot plot/chart for labeled numeric data.
#' @name ggdotplotstats
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("maturing")}
#'
#' A dot chart (as described by William S. Cleveland) with statistical details
#' from one-sample test included in the plot as a subtitle.
#'
#' @param ... Currently ignored.
#' @param y Label or grouping variable.
#' @param point.args A list of additional aesthetic arguments passed to
#'   `geom_point`.
#' @inheritParams histo_labeller
#' @inheritParams gghistostats
#' @inheritParams ggcoefstats
#'
#' @importFrom dplyr row_number percent_rank pull
#' @importFrom statsExpressions expr_t_onesample bf_ttest
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggdotplotstats.html}
#'
#' @seealso \code{\link{grouped_gghistostats}}, \code{\link{gghistostats}},
#'  \code{\link{grouped_ggdotplotstats}}
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#'
#' # plot
#' ggdotplotstats(
#'   data = ggplot2::mpg,
#'   x = cty,
#'   y = manufacturer,
#'   test.value = 15,
#'   test.value.line = TRUE,
#'   test.line.labeller = TRUE,
#'   centrality.parameter = "median",
#'   centrality.k = 0,
#'   title = "Fuel economy data",
#'   xlab = "city miles per gallon",
#'   caption = substitute(
#'     paste(italic("Source"), ": EPA dataset on http://fueleconomy.gov")
#'   )
#' )
#' }
#' @export

# function body
ggdotplotstats <- function(data,
                           x,
                           y,
                           xlab = NULL,
                           ylab = NULL,
                           title = NULL,
                           subtitle = NULL,
                           caption = NULL,
                           type = "parametric",
                           test.value = 0,
                           bf.prior = 0.707,
                           bf.message = TRUE,
                           effsize.type = "g",
                           conf.level = 0.95,
                           nboot = 100,
                           tr = 0.1,
                           k = 2,
                           results.subtitle = TRUE,
                           point.args = list(color = "black", size = 3, shape = 16),
                           centrality.plotting = TRUE,
                           centrality.k = 2,
                           centrality.line.args = list(color = "blue", size = 1),
                           centrality.label.args = list(color = "blue", size = 3),
                           ggplot.component = NULL,
                           ggtheme = ggplot2::theme_bw(),
                           ggstatsplot.layer = TRUE,
                           output = "plot",
                           ...) {

  # convert entered stats type to a standard notation
  type <- ipmisc::stats_type_switch(type)

  # ------------------------------ variable names ----------------------------

  # ensure the variables work quoted or unquoted
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)

  # if `xlab` and `ylab` is not provided, use the variable `x` and `y` name
  if (is.null(xlab)) xlab <- rlang::as_name(x)
  if (is.null(ylab)) ylab <- rlang::as_name(y)

  # --------------------------- data preparation ----------------------------

  # creating a dataframe
  data %<>%
    dplyr::select({{ x }}, {{ y }}) %>%
    tidyr::drop_na(.) %>%
    dplyr::mutate({{ y }} := droplevels(as.factor({{ y }}))) %>%
    dplyr::group_by({{ y }}) %>%
    dplyr::summarise({{ x }} := mean({{ x }}, na.rm = TRUE)) %>%
    dplyr::ungroup(x = .) %>%
    # rank ordering the data
    dplyr::arrange({{ x }}) %>%
    dplyr::mutate(
      percent_rank = dplyr::percent_rank({{ x }}),
      rank = dplyr::row_number()
    )

  # ================ stats labels ==========================================

  if (isTRUE(results.subtitle)) {
    # preparing the BF message for NULL
    if (isTRUE(bf.message) && type == "parametric") {
      caption <-
        statsExpressions::bf_ttest(
          data = data,
          x = {{ x }},
          test.value = test.value,
          bf.prior = bf.prior,
          top.text = caption,
          output = "expression",
          k = k
        )
    }

    # preparing the subtitle with statistical results
    subtitle <-
      statsExpressions::expr_t_onesample(
        data = data,
        x = {{ x }},
        type = type,
        test.value = test.value,
        bf.prior = bf.prior,
        effsize.type = effsize.type,
        conf.level = conf.level,
        nboot = nboot,
        tr = tr,
        k = k
      )
  }

  # return early if anything other than plot
  if (output != "plot") {
    return(switch(EXPR = output, "caption" = caption, subtitle))
  }

  # ------------------------------ basic plot ----------------------------

  # creating the basic plot
  plot <-
    ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = {{ x }}, y = rank)) +
    rlang::exec(
      .fn = ggplot2::geom_point,
      na.rm = TRUE,
      !!!point.args
    ) +
    ggplot2::scale_y_continuous(
      name = ylab,
      labels = data %>% dplyr::pull({{ y }}),
      breaks = data$rank,
      sec.axis = ggplot2::dup_axis(
        name = "percentile",
        breaks = seq(1, nrow(data), (nrow(data) - 1) / 4),
        labels = 25 * 0:4
      )
    ) +
    ggplot2::scale_x_continuous(
      name = xlab,
      sec.axis = ggplot2::dup_axis(name = ggplot2::element_blank())
    )

  # ---------------- centrality tagging -------------------------------------

  # computing statistics needed for displaying labels
  y_label_pos <- median(ggplot2::layer_scales(plot)$y$range$range, na.rm = TRUE)

  # using custom function for adding labels
  if (isTRUE(centrality.plotting)) {
    plot <-
      histo_labeller(
        plot = plot,
        x = data %>% dplyr::pull({{ x }}),
        y.label.position = y_label_pos,
        type = type,
        tr = tr,
        test.value = test.value,
        centrality.k = centrality.k,
        centrality.line.args = centrality.line.args,
        centrality.label.args = centrality.label.args
      )
  }

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
    theme_ggstatsplot(ggtheme = ggtheme, ggstatsplot.layer = ggstatsplot.layer) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_line(
        color = "black",
        size = 0.1,
        linetype = "dashed"
      )
    )

  # adding ggplot component
  plot + ggplot.component
}
