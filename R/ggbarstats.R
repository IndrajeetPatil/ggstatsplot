#' @title Bar (column) charts with statistical tests
#' @name ggbarstats
#' @description Bar charts for categorical data with statistical details
#'   included in the plot as a subtitle.
#' @author Chuck Powell, Indrajeet Patil
#'
#' @param labels.legend A character vector with custom labels for levels of
#'   the `main` variable displayed in the legend.
#' @param xlab Custom text for the `x` axis label (Default: `NULL`, which
#'   will cause the `x` axis label to be the `main` variable).
#' @param ylab Custom text for the `y` axis label (Default: `"percent"`).
#' @param bar.proptest Decides whether proportion test for `main` variable is
#'   to be carried out for each level of `condition` (Default: `TRUE`).
#' @param bar.label,data.label Character decides what information needs to be
#'   displayed on the label in each pie slice. Possible options are
#'   `"percentage"` (default), `"counts"`, `"both"`.
#' @param legend.position The position of the legend
#'   `"none"`, `"left"`, `"right"`, `"bottom"`, `"top"` (Default: `"right"`).
#' @param x.axis.orientation The orientation of the `x` axis labels one of
#'   "slant" or "vertical" to change from the default horizontal
#'   orientation (Default: `NULL` which is horizontal).
#' @param bar.outline.color Character specifying color for bars (default:
#'   `"black"`).
#' @inheritParams ggpiestats
#'
#' @seealso \code{\link{grouped_ggbarstats}}, \code{\link{ggpiestats}},
#'  \code{\link{grouped_ggpiestats}}
#'
#' @import ggplot2
#'
#' @importFrom dplyr select group_by summarize n arrange if_else desc
#' @importFrom dplyr mutate mutate_at mutate_if
#' @importFrom rlang !! enquo quo_name
#' @importFrom crayon green blue yellow red
#' @importFrom paletteer scale_fill_paletteer_d
#' @importFrom groupedstats grouped_proptest
#' @importFrom tidyr uncount
#' @importFrom tibble as_tibble
#' @importFrom scales percent
#'
#' @inherit ggpiestats return details
#'
#' @examples
#'
#' # for reproducibility
#' set.seed(123)
#'
#' # simple function call with the defaults (with condition)
#' ggstatsplot::ggbarstats(
#'   data = datasets::mtcars,
#'   main = vs,
#'   condition = cyl,
#'   nboot = 10,
#'   labels.legend = c("0 = V-shaped", "1 = straight"),
#'   legend.title = "Engine"
#' )
#' \donttest{
#' # simple function call with the defaults (with count data)
#' library(jmv)
#'
#' ggstatsplot::ggbarstats(
#'   data = as.data.frame(HairEyeColor),
#'   main = Eye,
#'   condition = Hair,
#'   counts = Freq
#' )
#' }
#' @export

# defining the function
ggbarstats <- function(data,
                       main,
                       condition = NULL,
                       counts = NULL,
                       ratio = NULL,
                       paired = FALSE,
                       labels.legend = NULL,
                       results.subtitle = TRUE,
                       stat.title = NULL,
                       sample.size.label = TRUE,
                       label.separator = " ",
                       label.text.size = 4,
                       label.fill.color = "white",
                       label.fill.alpha = 1,
                       bar.outline.color = "black",
                       bf.message = TRUE,
                       sampling.plan = "indepMulti",
                       fixed.margin = "rows",
                       prior.concentration = 1,
                       title = NULL,
                       subtitle = NULL,
                       caption = NULL,
                       legend.position = "right",
                       x.axis.orientation = NULL,
                       conf.level = 0.95,
                       nboot = 100,
                       simulate.p.value = FALSE,
                       B = 2000,
                       legend.title = NULL,
                       xlab = NULL,
                       ylab = "Percent",
                       k = 2,
                       perc.k = 0,
                       bar.label = "percentage",
                       data.label = NULL,
                       bar.proptest = TRUE,
                       ggtheme = ggplot2::theme_bw(),
                       ggstatsplot.layer = TRUE,
                       package = "RColorBrewer",
                       palette = "Dark2",
                       direction = 1,
                       ggplot.component = NULL,
                       return = "plot",
                       messages = TRUE) {
  bar.label <- data.label %||% bar.label

  # ================= extracting column names as labels  =====================

  if (missing(condition)) {
    stop("You must specify a condition variable")
  }
  # if legend title is not provided, use the variable name for 'main'
  # argument
  if (is.null(legend.title)) {
    legend.title <- rlang::as_name(rlang::ensym(main))
  }

  # if alternate variable label is not specified, use the variable name for
  # 'condition' argument
  if (is.null(xlab)) {
    xlab <- rlang::as_name(rlang::ensym(condition))
  }

  # =============================== dataframe ================================

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      main = !!rlang::enquo(main),
      condition = !!rlang::enquo(condition),
      counts = !!rlang::enquo(counts)
    ) %>%
    tidyr::drop_na(data = .) %>%
    tibble::as_tibble(x = .)

  # =========================== converting counts ============================

  # untable the dataframe based on the count for each obervation
  if (!missing(counts)) {
    data %<>%
      tidyr::uncount(
        data = .,
        weights = counts,
        .remove = TRUE,
        .id = "id"
      )
  }

  # ============================ percentage dataframe ========================

  # main and condition need to be a factor for this analysis
  # also drop the unused levels of the factors
  data %<>%
    dplyr::mutate(.data = ., main = droplevels(as.factor(main))) %>%
    dplyr::mutate(.data = ., condition = droplevels(as.factor(condition)))

  # convert the data into percentages; group by conditional variable
  df <- cat_counter(data, main, condition)

  # dataframe with summary labels
  df %<>%
    cat_label_df(
      data = .,
      label.col.name = "bar.label",
      label.content = bar.label,
      label.separator = label.separator,
      perc.k = perc.k
    )

  # ============================ sample size label ==========================

  # if labels are to be displayed on the bottom of the charts
  # for each bar/column
  if (isTRUE(sample.size.label)) {
    df_n_label <- data %>%
      dplyr::group_by(.data = ., condition) %>%
      dplyr::summarize(.data = ., N = n()) %>%
      dplyr::mutate(.data = ., N = paste0("(n = ", N, ")", sep = ""))
  }

  # ====================== preparing names for legend  ======================

  # reorder the category factor levels to order the legend
  df$main <- factor(
    x = df$main,
    levels = unique(df$main)
  )

  # getting labels for all levels of the 'main' variable factor
  if (is.null(labels.legend)) {
    legend.labels <- as.character(df$main)
  } else if (!missing(labels.legend)) {
    legend.labels <- labels.legend
  }

  # =================================== plot =================================

  # if no. of factor levels is greater than the default palette color count
  palette_message(
    package = package,
    palette = palette,
    min_length = length(unique(levels(data$main)))[[1]]
  )

  # plot
  p <- ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(fill = main, y = perc, x = condition)
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "fill",
      color = bar.outline.color,
      na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      breaks = seq(from = 0, to = 1, by = 0.10),
      minor_breaks = seq(from = 0.05, to = 0.95, by = 0.10)
    ) +
    ggplot2::geom_label(
      mapping = ggplot2::aes(label = bar.label, group = main),
      show.legend = FALSE,
      position = ggplot2::position_fill(vjust = 0.5),
      color = "black",
      size = label.text.size,
      fill = label.fill.color,
      alpha = label.fill.alpha,
      na.rm = TRUE
    ) +
    ggstatsplot::theme_mprl(
      ggtheme = ggtheme,
      ggstatsplot.layer = ggstatsplot.layer
    ) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = legend.position
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend.title))

  # color palette
  p <- p +
    paletteer::scale_fill_paletteer_d(
      package = !!package,
      palette = !!palette,
      direction = direction,
      name = "",
      labels = unique(legend.labels)
    )

  # =============== chi-square test (either Pearson or McNemar) =============

  # if testing by condition is happening
  if (isTRUE(bar.proptest)) {
    # merging dataframe containing results from the proportion test with
    # counts and percentage dataframe
    df2 <-
      dplyr::full_join(
        x = df,
        # running grouped proportion test with helper functions
        y = groupedstats::grouped_proptest(
          data = data,
          grouping.vars = condition,
          measure = main
        ),
        by = "condition"
      ) %>%
      dplyr::mutate(
        .data = .,
        significance = dplyr::if_else(
          condition = duplicated(condition),
          true = NA_character_,
          false = significance
        )
      ) %>%
      dplyr::filter(.data = ., !is.na(significance))

    # display grouped proportion test results
    if (isTRUE(messages)) {
      # tell the user what these results are
      proptest_message(
        main = rlang::as_name(rlang::ensym(main)),
        condition = rlang::as_name(rlang::ensym(condition))
      )

      # print the tibble and leave out unnecessary columns
      print(tibble::as_tibble(df2) %>%
        dplyr::select(.data = ., -c(main:bar.label)))
    }
  }

  # running approprate statistical test
  # unpaired: Pearson's Chi-square test of independence
  if (isTRUE(results.subtitle)) {
    subtitle <-
      subtitle_contingency_tab(
        data = data,
        main = main,
        condition = condition,
        nboot = nboot,
        paired = paired,
        stat.title = stat.title,
        conf.level = conf.level,
        conf.type = "norm",
        simulate.p.value = simulate.p.value,
        B = B,
        messages = messages,
        k = k
      )

    # preparing the BF message for null hypothesis support
    if (isTRUE(bf.message) && !is.null(subtitle)) {
      bf.caption.text <-
        bf_contingency_tab(
          data = data,
          main = main,
          condition = condition,
          sampling.plan = sampling.plan,
          fixed.margin = fixed.margin,
          prior.concentration = prior.concentration,
          caption = caption,
          output = "caption",
          k = k
        )

      caption <- bf.caption.text
    }
  }

  # ====================== proportion test =======================

  # adding significance labels to bars for proportion tests
  if (isTRUE(bar.proptest)) {
    p <-
      p +
      ggplot2::geom_text(
        data = df2,
        mapping = ggplot2::aes(
          x = condition,
          y = 1.05,
          label = significance,
          fill = NULL
        ),
        size = 5,
        na.rm = TRUE
      )
  }

  # adding sample size info
  if (isTRUE(sample.size.label)) {
    p <-
      p +
      ggplot2::geom_text(
        data = df_n_label,
        mapping = ggplot2::aes(
          x = condition,
          y = -0.05,
          label = N,
          fill = NULL
        ),
        size = 4,
        na.rm = TRUE
      )
  }

  # =========================== putting all together ========================

  # if we need to modify `x`-axis orientation
  if (!is.null(x.axis.orientation)) {
    if (x.axis.orientation == "slant") {
      p <-
        p + ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 45,
            vjust = 1,
            hjust = 1,
            face = "bold"
          )
        )
    } else if (x.axis.orientation == "vertical") {
      p <-
        p + ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1,
            face = "bold"
          )
        )
    }
  }

  # preparing the plot
  p <-
    p +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      subtitle = subtitle,
      title = title,
      caption = caption
    )

  # ---------------- adding ggplot component ---------------------------------

  # if any additional modification needs to be made to the plot
  # this is primarily useful for grouped_ variant of this function
  p <- p + ggplot.component

  # return the final plot
  return(switch(
    EXPR = return,
    "plot" = p,
    "subtitle" = subtitle,
    "caption" = caption,
    p
  ))
}
