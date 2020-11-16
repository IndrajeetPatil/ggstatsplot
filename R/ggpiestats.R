#' @title Pie charts with statistical tests
#' @name ggpiestats
#' @description Pie charts for categorical data with statistical details
#'   included in the plot as a subtitle.
#'
#' @param x The variable to use as the **rows** in the contingency table.
#' @param y The variable to use as the **columns** in the contingency
#'   table. Default is `NULL`. If `NULL`, one-sample proportion test (a goodness
#'   of fit test) will be run for the `x` variable. Otherwise an appropriate
#'   association test will be run. This argument can not be `NULL` for
#'   `ggbarstats` function.
#' @param proportion.test Decides whether proportion test for `x` variable is
#'   to be carried out for each level of `y` (Default: `TRUE`).
#' @param perc.k Numeric that decides number of decimal places for percentage
#'   labels (Default: `0`).
#' @param label Character decides what information needs to be displayed
#'   on the label in each pie slice. Possible options are `"percentage"`
#'   (default), `"counts"`, `"both"`.
#' @param label.args Additional aesthetic arguments that will be passed to
#'   `geom_label`.
#' @param label.repel Whether labels should be repelled using `ggrepel` package.
#'   This can be helpful in case the labels are overlapping.
#' @param legend.title Title text for the legend.
#' @inheritParams ggbetweenstats
#' @inheritParams tidyBF::bf_contingency_tab
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
#' @importFrom dplyr select mutate vars pull
#' @importFrom rlang !! enquo quo_name as_name ensym
#' @importFrom ggrepel geom_label_repel
#' @importFrom paletteer scale_fill_paletteer_d
#' @importFrom tidyr uncount drop_na
#' @importFrom statsExpressions bf_contingency_tab expr_contingency_tab
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggpiestats.html}
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#'
#' # one sample goodness of fit proportion test
#' ggstatsplot::ggpiestats(ggplot2::msleep, vore)
#'
#' # association test (or contingency table analysis)
#' ggstatsplot::ggpiestats(
#'   data = mtcars,
#'   x = vs,
#'   y = cyl
#' )
#' }
#' @export

# defining the function
ggpiestats <- function(data,
                       x,
                       y = NULL,
                       counts = NULL,
                       ratio = NULL,
                       paired = FALSE,
                       results.subtitle = TRUE,
                       label = "percentage",
                       label.args = list(direction = "both"),
                       label.repel = FALSE,
                       conf.level = 0.95,
                       k = 2L,
                       proportion.test = TRUE,
                       perc.k = 0,
                       bf.message = TRUE,
                       sampling.plan = "indepMulti",
                       fixed.margin = "rows",
                       prior.concentration = 1,
                       title = NULL,
                       subtitle = NULL,
                       caption = NULL,
                       legend.title = NULL,
                       ggtheme = ggplot2::theme_bw(),
                       ggstatsplot.layer = TRUE,
                       package = "RColorBrewer",
                       palette = "Dark2",
                       ggplot.component = NULL,
                       output = "plot",
                       ...) {

  # ensure the variables work quoted or unquoted
  x <- rlang::ensym(x)
  y <- if (!rlang::quo_is_null(rlang::enquo(y))) rlang::ensym(y)
  counts <- if (!rlang::quo_is_null(rlang::enquo(counts))) rlang::ensym(counts)

  # saving the column label for the 'x' variables
  if (rlang::is_null(legend.title)) legend.title <- rlang::as_name(x)

  # =============================== dataframe ================================

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}, {{ counts }}) %>%
    tidyr::drop_na(data = .) %>%
    as_tibble(.)

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

  # x and y need to be a factor for this analysis
  # also drop the unused levels of the factors

  # x
  data %<>% dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }})))
  x_levels <- nlevels(data %>% dplyr::pull({{ x }}))[[1]]

  # y
  if (!rlang::quo_is_null(rlang::enquo(y))) {
    data %<>% dplyr::mutate(.data = ., {{ y }} := droplevels(as.factor({{ y }})))
    y_levels <- nlevels(data %>% dplyr::pull({{ y }}))[[1]]

    # TO DO: until one-way table is supported by `BayesFactor`
    if (y_levels == 1L) bf.message <- FALSE
  } else {
    y_levels <- 0L
  }

  # facting is happening only if both vars have more than one levels
  facet <- ifelse(y_levels > 1L, TRUE, FALSE)
  if (x_levels == 1L && isTRUE(facet)) proportion.test <- FALSE

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
          paired = paired,
          conf.level = conf.level,
          k = k
        ),
        error = function(e) NULL
      )

    # preparing Bayes Factor caption
    if (isTRUE(bf.message) && !is.null(subtitle)) {
      caption <-
        tryCatch(
          expr = bf_contingency_tab(
            data = data,
            x = {{ x }},
            y = {{ y }},
            sampling.plan = sampling.plan,
            fixed.margin = fixed.margin,
            prior.concentration = prior.concentration,
            top.text = caption,
            output = "caption",
            k = k
          ),
          error = function(e) NULL
        )
    }
  }

  # convert the data into percentages and add labels
  df <-
    cat_label_df(
      data = cat_counter(data = data, x = {{ x }}, y = {{ y }}),
      label.content = label,
      perc.k = perc.k
    )

  # dataframe containing all details needed for sample size and prop test
  if (!rlang::quo_is_null(rlang::enquo(y))) {
    df_labels <- df_facet_label(data, {{ x }}, {{ y }}, k)
  } else {
    df_labels <- NULL
  }

  # reorder the category factor levels to order the legend
  df %<>% dplyr::mutate(.data = ., {{ x }} := factor({{ x }}, unique({{ x }})))

  # return early if anything other than plot
  if (output != "plot") {
    return(switch(
      EXPR = output,
      "subtitle" = subtitle,
      "caption" = caption,
      "proptest" = df_labels
    ))
  }

  # =================================== plot =================================

  # if no. of factor levels is greater than the default palette color count
  palette_message(
    package = package,
    palette = palette,
    min_length = x_levels
  )

  # creating the basic plot
  p <-
    ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = "", y = perc)) +
    ggplot2::geom_col(
      mapping = ggplot2::aes(fill = {{ x }}),
      position = "fill",
      color = "black",
      width = 1,
      na.rm = TRUE
    )

  # whether labels need to be repelled
  if (isTRUE(label.repel)) {
    .fn <- ggrepel::geom_label_repel
  } else {
    .fn <- ggplot2::geom_label
  }

  # adding label with percentages and/or counts
  suppressWarnings(suppressMessages(p <- p +
    rlang::exec(
      .fn = .fn,
      mapping = ggplot2::aes(label = label, group = {{ x }}),
      position = ggplot2::position_fill(vjust = 0.5),
      min.segment.length = 0,
      fill = "white",
      alpha = 1,
      na.rm = TRUE,
      !!!label.args
    )))

  # if facet_wrap *is* happening
  if (isTRUE(facet)) p <- p + ggplot2::facet_wrap(facets = dplyr::vars({{ y }}))

  # polar coordinates plus formatting
  p <- p +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    paletteer::scale_fill_paletteer_d(palette = paste0(package, "::", palette), name = "") +
    theme_pie(ggtheme = ggtheme, ggstatsplot.layer = ggstatsplot.layer) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(color = NA)))

  # ================ sample size + proportion test labels =================

  # adding labels with proportion tests
  if (isTRUE(facet) && isTRUE(proportion.test)) {
    p <- p +
      rlang::exec(
        .fn = ggplot2::geom_text,
        data = df_labels,
        mapping = ggplot2::aes(label = label, x = 1.65, y = 0.5),
        position = ggplot2::position_fill(vjust = 1),
        size = 2.8,
        na.rm = TRUE,
        parse = TRUE
      )
  }

  # =========================== putting all together ========================

  # preparing the plot
  p +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      subtitle = subtitle,
      title = title,
      caption = caption
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend.title)) +
    ggplot.component
}
