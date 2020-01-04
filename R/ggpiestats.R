#' @title Pie charts with statistical tests
#' @name ggpiestats
#' @description Pie charts for categorical data with statistical details
#'   included in the plot as a subtitle.
#'
#' @param x,main The variable to use as the **rows** in the contingency table.
#' @param y,condition The variable to use as the **columns** in the contingency
#'   table. Default is `NULL`. If `NULL`, one-sample proportion test (a goodness
#'   of fit test) will be run for the `x` variable. Otherwise an appropriate
#'   association test will be run. This argument can not be `NULL` for
#'   `ggbarstats` function.
#' @param factor.levels A character vector with labels for factor levels of
#'   `main` variable.
#' @param title The text for the plot title.
#' @param caption The text for the plot caption.
#' @param sample.size.label Logical that decides whether sample size information
#'   should be displayed for each level of the grouping variable `y`
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
#' @param package Name of package from which the palette is desired as string
#' or symbol.
#' @param palette Name of palette as string or symbol.
#' @param direction Either `1` or `-1`. If `-1` the palette will be reversed.
#' @param bf.message Logical that decides whether to display a caption with
#'   results from Bayes Factor test in favor of the null hypothesis (default:
#'   `FALSE`).
#' @inheritParams statsExpressions::bf_contingency_tab
#' @inheritParams statsExpressions::expr_contingency_tab
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
#' # for reproducibility
#' set.seed(123)
#'
#' # one sample goodness of fit proportion test
#' ggstatsplot::ggpiestats(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   perc.k = 1,
#'   bf.message = FALSE,
#'   k = 3
#' )
#'
#' # association test (or contingency table analysis)
#' ggstatsplot::ggpiestats(
#'   data = mtcars,
#'   x = vs,
#'   y = cyl,
#'   bf.message = TRUE,
#'   nboot = 10,
#'   factor.levels = c("0 = V-shaped", "1 = straight"),
#'   legend.title = "Engine"
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
                       bias.correct = TRUE,
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
                       messages = TRUE,
                       x = NULL,
                       y = NULL) {

  # ensure the variables work quoted or unquoted
  main <- rlang::ensym(main)
  condition <- if (!rlang::quo_is_null(rlang::enquo(condition))) rlang::ensym(condition)
  x <- if (!rlang::quo_is_null(rlang::enquo(x))) rlang::ensym(x)
  y <- if (!rlang::quo_is_null(rlang::enquo(y))) rlang::ensym(y)
  x <- x %||% main
  y <- y %||% condition
  counts <- if (!rlang::quo_is_null(rlang::enquo(counts))) rlang::ensym(counts)

  # ================= extracting column names as labels  =====================

  # saving the column label for the 'x' variables
  if (rlang::is_null(legend.title)) legend.title <- rlang::as_name(x)

  # if facetting variable name is not specified, use 'y' variable name
  if (!rlang::quo_is_null(rlang::enquo(y)) && rlang::is_null(facet.wrap.name)) {
    facet.wrap.name <- rlang::as_name(y)
  }

  # =============================== dataframe ================================

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}, {{ counts }}) %>%
    tidyr::drop_na(data = .) %>%
    tibble::as_tibble(x = .)

  # =========================== converting counts ============================

  # untable the dataframe based on the count for each observation
  if (!rlang::quo_is_null(rlang::enquo(counts))) {
    data %<>%
      tidyr::uncount(
        data = .,
        weights = {{ counts }},
        .remove = TRUE,
        .id = "id"
      )
  }

  # ============================ percentage dataframe ========================

  # x and y need to be a factor for this analysis
  # also drop the unused levels of the factors

  # x
  data %<>% dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }})))

  # y
  if (!rlang::quo_is_null(rlang::enquo(y))) {
    data %<>% dplyr::mutate(.data = ., {{ y }} := droplevels(as.factor({{ y }})))
  }

  # convert the data into percentages; group by yal variable if needed
  # dataframe with summary labels
  df <-
    cat_label_df(
      data = cat_counter(data = data, x = {{ x }}, y = {{ y }}),
      label.col.name = "slice.label",
      label.content = slice.label,
      label.separator = label.separator,
      perc.k = perc.k
    )

  # ============ preparing label dataframe and other annotations  =============

  # dataframe containing all details needed for sample size and prop test
  if (!rlang::quo_is_null(rlang::enquo(y))) {
    df_labels <-
      df_facet_label(
        data = data,
        x = {{ x }},
        y = {{ y }},
        k = k
      )
  }

  # reorder the category factor levels to order the legend
  df %<>% dplyr::mutate(.data = ., {{ x }} := factor({{ x }}, unique({{ x }})))

  # getting labels for all levels of the 'x' variable factor
  if (is.null(factor.levels)) {
    legend.labels <- as.character(df %>% dplyr::pull({{ x }}))
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
    min_length = nlevels(data %>% dplyr::pull({{ x }}))[[1]]
  )

  # creating the basic plot
  p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = "", y = counts)) +
    ggplot2::geom_col(
      position = "fill",
      color = "black",
      width = 1,
      ggplot2::aes(fill = {{ x }}),
      na.rm = TRUE
    ) +
    ggplot2::geom_label(
      ggplot2::aes(label = slice.label, group = {{ x }}),
      position = ggplot2::position_fill(vjust = 0.5),
      color = "black",
      size = label.text.size,
      fill = label.fill.color,
      alpha = label.fill.alpha,
      show.legend = FALSE,
      na.rm = TRUE
    )

  # if facet_wrap is *not* happening
  if (rlang::quo_is_null(rlang::enquo(y))) {
    p <- p + ggplot2::coord_polar(theta = "y")
  } else {
    # if facet_wrap *is* happening
    p <- p +
      ggplot2::facet_wrap(
        facets = dplyr::vars({{ y }}),
        labeller = ggplot2::labeller(
          y = label_facet(
            original_var = df %>% dplyr::pull({{ y }}),
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
      palette = paste0(package, "::", palette),
      direction = direction,
      name = "",
      labels = unique(legend.labels)
    ) +
    theme_pie(
      ggtheme = ggtheme,
      ggstatsplot.layer = ggstatsplot.layer
    ) + # remove black diagonal line from legend
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(color = NA)))

  # ========================= statistical analysis ==========================

  # if subtitle with results is to be displayed
  if (isTRUE(results.subtitle)) {
    subtitle <-
      tryCatch(
        expr = statsExpressions::expr_contingency_tab(
          data = data,
          x = {{ x }},
          y = {{ y }},
          ratio = ratio,
          nboot = nboot,
          paired = paired,
          stat.title = stat.title,
          legend.title = legend.title,
          conf.level = conf.level,
          conf.type = "norm",
          bias.correct = bias.correct,
          k = k,
          messages = messages
        ),
        error = function(e) NULL
      )

    # preparing Bayes Factor caption
    if (isTRUE(bf.message) && !is.null(subtitle)) {
      caption <-
        bf_contingency_tab(
          data = data,
          x = {{ x }},
          y = {{ y }},
          sampling.plan = sampling.plan,
          fixed.margin = fixed.margin,
          prior.concentration = prior.concentration,
          caption = caption,
          output = "caption",
          k = k
        )
    }
  }

  # if faceting by y is happening
  if (!rlang::quo_is_null(rlang::enquo(y))) {

    # ================ sample size and proportion test labels =================

    # adding significance labels to pie charts for grouped proportion tests
    if (isTRUE(facet.proptest)) {
      # display grouped proportion test results
      if (isTRUE(messages)) print(dplyr::select(df_labels, -label))

      # adding labels
      p <- p +
        ggplot2::geom_text(
          data = df_labels,
          mapping = ggplot2::aes(label = label, x = 1.65, y = 0.5),
          position = ggplot2::position_fill(vjust = 1),
          size = 2.8,
          na.rm = TRUE,
          parse = TRUE
        )
    }

    # adding sample size info
    if (isTRUE(sample.size.label)) {
      p <- p +
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
  p <- p +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      subtitle = subtitle,
      title = title,
      caption = caption
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend.title)) +
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
