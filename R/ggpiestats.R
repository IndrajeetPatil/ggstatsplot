#' @title Pie charts with statistical tests
#' @name ggpiestats
#' @description Pie charts for categorical data with statistical details
#'   included in the plot as a subtitle.
#' @author Indrajeet Patil
#'
#' @param factor.levels A character vector with labels for factor levels of
#'   `main` variable.
#' @param title The text for the plot title.
#' @param caption The text for the plot caption.
#' @param sample.size.label Logical that decides whether sample size information
#'   should be displayed for each level of the grouping variable `condition`
#'   (Default: `TRUE`).
#' @param palette If a character string (e.g., `"Set1"`), will use that named
#'   palette. If a number, will index into the list of palettes of appropriate
#'   type. Default palette is `"Dark2"`.
#' @param facet.wrap.name The text for the facet_wrap variable label.
#' @param facet.proptest Decides whether proportion test for `main` variable is
#'   to be carried out for each level of `condition` (Default: `TRUE`).
#' @param perc.k Numeric that decides number of decimal places for percentage
#'   labels (Default: `0`).
#' @param slice.label Character decides what information needs to be displayed
#'   on the label in each pie slice. Possible options are `"percentage"`
#'   (default), `"counts"`, `"both"`.
#' @param label.text.size Numeric that decides text size for slice/bar labels
#'   (Default: `4`).
#' @param label.fill.color Character that specifies fill color for slice/bar
#'   labels (Default: `white`).
#' @param label.fill.alpha Numeric that specifies fill color transparency or
#'   `"alpha"` for slice/bar labels (Default: `1` range `0` to `1`).
#' @param bf.message Logical that decides whether to display a caption with
#'   results from bayes factor test in favor of the null hypothesis (default:
#'   `FALSE`).
#' @inheritParams bf_contingency_tab
#' @inheritParams subtitle_contingency_tab
#' @inheritParams subtitle_onesample_proptest
#' @inheritParams paletteer::scale_fill_paletteer_d
#' @inheritParams theme_ggstatsplot
#' @inheritParams gghistostats
#' @inheritParams cat_label_df
#'
#' @seealso \code{\link{grouped_ggpiestats}}, \code{\link{ggbarstats}},
#'  \code{\link{grouped_ggbarstats}}
#'
#' @import ggplot2
#'
#' @importFrom dplyr select group_by summarize n mutate mutate_at mutate_if
#' @importFrom rlang !! enquo quo_name as_name ensym
#' @importFrom crayon green blue yellow red
#' @importFrom paletteer scale_fill_paletteer_d
#' @importFrom groupedstats grouped_proptest
#' @importFrom tidyr uncount drop_na
#' @importFrom tibble as_tibble
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggpiestats.html}
#'
#' @return Unlike a number of statistical softwares, `ggstatsplot` doesn't
#'   provide the option for Yates' correction for the Pearson's chi-squared
#'   statistic. This is due to compelling amount of Monte-Carlo simulation
#'   research which suggests that the Yates' correction is overly conservative,
#'   even in small sample sizes. As such it is recommended that it should not
#'   ever be applied in practice (Camilli & Hopkins, 1978, 1979; Feinberg, 1980;
#'   Larntz, 1978; Thompson, 1988).
#'
#'   For more about how the effect size measures and their confidence intervals
#'   are computed, see `?rcompanion::cohenG`, `?rcompanion::cramerV`, and
#'   `?rcompanion::cramerVFit`.
#'
#' @examples
#'
#' # for reproducibility
#' set.seed(123)
#'
#' # simple function call with the defaults (without condition)
#' ggstatsplot::ggpiestats(
#'   data = ggplot2::msleep,
#'   main = vore,
#'   perc.k = 1,
#'   k = 3
#' )
#'
#' # simple function call with the defaults (with condition)
#' ggstatsplot::ggpiestats(
#'   data = datasets::mtcars,
#'   main = vs,
#'   condition = cyl,
#'   bf.message = TRUE,
#'   nboot = 10,
#'   factor.levels = c("0 = V-shaped", "1 = straight"),
#'   legend.title = "Engine"
#' )
#'
#' # simple function call with the defaults (without condition; with count data)
#' library(jmv, warn.conflicts = FALSE)
#'
#' ggstatsplot::ggpiestats(
#'   data = as.data.frame(HairEyeColor),
#'   main = Eye,
#'   counts = Freq
#' )
#' @export

# defining the function
ggpiestats <- function(data,
                       main,
                       condition = NULL,
                       counts = NULL,
                       ratio = NULL,
                       paired = FALSE,
                       results.subtitle = TRUE,
                       factor.levels = NULL,
                       stat.title = NULL,
                       sample.size.label = TRUE,
                       label.separator = "\n",
                       label.text.size = 4,
                       label.fill.color = "white",
                       label.fill.alpha = 1,
                       bf.message = TRUE,
                       sampling.plan = "indepMulti",
                       fixed.margin = "rows",
                       prior.concentration = 1,
                       title = NULL,
                       subtitle = NULL,
                       caption = NULL,
                       conf.level = 0.95,
                       bf.prior = 0.707,
                       nboot = 100,
                       simulate.p.value = FALSE,
                       B = 2000,
                       bias.correct = FALSE,
                       legend.title = NULL,
                       facet.wrap.name = NULL,
                       k = 2,
                       perc.k = 0,
                       slice.label = "percentage",
                       facet.proptest = TRUE,
                       ggtheme = ggplot2::theme_bw(),
                       ggstatsplot.layer = TRUE,
                       package = "RColorBrewer",
                       palette = "Dark2",
                       direction = 1,
                       ggplot.component = NULL,
                       return = "plot",
                       messages = TRUE) {

  # ================= extracting column names as labels  =====================

  # saving the column label for the 'main' variables
  if (is.null(legend.title)) {
    legend.title <- rlang::as_name(rlang::ensym(main))
  }

  # if facetting variable name is not specified, use the variable name for
  # 'condition' argument
  if (!missing(condition) && rlang::is_null(facet.wrap.name)) {
    facet.wrap.name <- rlang::as_name(rlang::ensym(condition))
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
  if ("counts" %in% names(data)) {
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

  # main
  data %<>%
    dplyr::mutate(.data = ., main = droplevels(as.factor(main)))

  # condition
  if ("condition" %in% names(data)) {
    data %<>%
      dplyr::mutate(.data = ., condition = droplevels(as.factor(condition)))
  }

  # convert the data into percentages; group by conditional variable if needed
  # dataframe with summary labels
  df <-
    cat_label_df(
      data = cat_counter(data, main, condition),
      label.col.name = "slice.label",
      label.content = slice.label,
      label.separator = label.separator,
      perc.k = perc.k
    )

  # ============ preparing label dataframe and other annotations  =============

  # dataframe containing all details needed for sample size and prop test
  if ("condition" %in% names(data)) {
    df_labels <- df_facet_label(data = data, x = main, y = condition)
  }

  # reorder the category factor levels to order the legend
  df$main <- factor(x = df$main, levels = unique(df$main))

  # getting labels for all levels of the 'main' variable factor
  if (is.null(factor.levels)) {
    legend.labels <- as.character(df$main)
  } else {
    legend.labels <- factor.levels
  }

  # custom function for facet_wrap variable label
  label_facet <- function(original_var, custom_name) {
    lev <- levels(as.factor(original_var))
    lab <- paste0(custom_name, ": ", lev)
    names(lab) <- lev
    return(lab)
  }

  # =================================== plot =================================

  # if no. of factor levels is greater than the default palette color count
  palette_message(
    package = package,
    palette = palette,
    min_length = length(unique(levels(data$main)))[[1]]
  )

  # creating the basic plot
  p <- ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(x = "", y = counts)
  ) +
    ggplot2::geom_col(
      position = "fill",
      color = "black",
      width = 1,
      ggplot2::aes(fill = factor(get("main"))),
      na.rm = TRUE
    ) +
    ggplot2::geom_label(
      ggplot2::aes(label = slice.label, group = factor(get("main"))),
      position = ggplot2::position_fill(vjust = 0.5),
      color = "black",
      size = label.text.size,
      fill = label.fill.color,
      alpha = label.fill.alpha,
      show.legend = FALSE,
      na.rm = TRUE
    )

  # if facet_wrap is *not* happening
  if (missing(condition)) {
    p <- p +
      ggplot2::coord_polar(theta = "y")
  } else {
    # if facet_wrap *is* happening
    p <- p +
      ggplot2::facet_wrap(
        facets = ~condition,
        labeller = ggplot2::labeller(
          condition = label_facet(
            original_var = df$condition,
            custom_name = facet.wrap.name
          )
        )
      ) +
      ggplot2::coord_polar(theta = "y")
  }

  # formatting
  p <- p +
    ggplot2::scale_y_continuous(breaks = NULL) +
    paletteer::scale_fill_paletteer_d(
      package = !!package,
      palette = !!palette,
      direction = direction,
      name = "",
      labels = unique(legend.labels)
    ) +
    theme_pie(
      ggtheme = ggtheme,
      ggstatsplot.layer = ggstatsplot.layer
    ) +
    # remove black diagonal line from legend
    ggplot2::guides(
      fill = ggplot2::guide_legend(override.aes = list(color = NA))
    )

  # ========================= statistical analysis ==========================

  # if subtitle with results is to be displayed
  if (isTRUE(results.subtitle)) {
    subtitle <-
      subtitle_contingency_tab(
        data = data,
        main = main,
        condition = condition,
        ratio = ratio,
        nboot = nboot,
        paired = paired,
        stat.title = stat.title,
        legend.title = legend.title,
        conf.level = conf.level,
        conf.type = "norm",
        bias.correct = bias.correct,
        simulate.p.value = simulate.p.value,
        B = B,
        k = k,
        messages = messages
      )

    # preparing Bayes Factor caption
    if (isTRUE(bf.message) && !is.null(subtitle)) {
      caption <- bf_contingency_tab(
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
    }
  }

  # if faceting by condition is happening
  if ("condition" %in% names(data)) {

    # ================ sample size and proportion test labels =================

    # adding significance labels to pie charts for grouped proportion tests
    if (isTRUE(facet.proptest)) {

      # display grouped proportion test results
      if (isTRUE(messages)) {
        # tell the user what these results are
        proptest_message(
          main = rlang::as_name(rlang::ensym(main)),
          condition = rlang::as_name(rlang::ensym(condition))
        )

        # print the tibble and leave out unnecessary columns
        print(df_labels)
      }

      # adding labels
      p <-
        p +
        ggplot2::geom_text(
          data = df_labels,
          mapping = ggplot2::aes(label = significance, x = 1.65, y = 0.5),
          position = ggplot2::position_fill(vjust = 1),
          size = 5,
          na.rm = TRUE
        )
    }

    # adding sample size info
    if (isTRUE(sample.size.label)) {
      p <-
        p +
        ggplot2::geom_text(
          data = df_labels,
          mapping = ggplot2::aes(label = N, x = 1.65, y = 1),
          position = ggplot2::position_fill(vjust = 0.5),
          size = 4,
          na.rm = TRUE
        )
    }
  }

  # =========================== putting all together ========================

  # preparing the plot
  p <-
    p +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      subtitle = subtitle,
      title = title,
      caption = caption
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend.title)) +
    # adding ggplot component
    ggplot.component

  # return the final plot
  return(switch(
    EXPR = return,
    "plot" = p,
    "subtitle" = subtitle,
    "caption" = caption,
    p
  ))
}
