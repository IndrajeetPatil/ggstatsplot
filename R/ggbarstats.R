#' @title Bar (column) charts with statistical tests
#' @name ggbarstats
#' @description Bar charts for categorical data with statistical details
#'   included in the plot as a subtitle.
#'
#' @param xlab Custom text for the `x` axis label (Default: `NULL`, which
#'   will cause the `x` axis label to be the `x` variable).
#' @param ylab Custom text for the `y` axis label (Default: `NULL`).
#' @param sample.size.label Logical that decides whether sample size information
#'   should be displayed for each level of the grouping variable `y`
#'   (Default: `TRUE`).
#' @inheritParams ggpiestats
#'
#' @seealso \code{\link{grouped_ggbarstats}}, \code{\link{ggpiestats}},
#'  \code{\link{grouped_ggpiestats}}
#'
#' @import ggplot2
#'
#' @importFrom dplyr select group_by summarize n mutate mutate_at mutate_if
#' @importFrom rlang !! enquo quo_name as_name ensym
#' @importFrom paletteer scale_fill_paletteer_d
#' @importFrom tidyr uncount drop_na
#' @importFrom statsExpressions expr_contingency_tab bf_contingency_tab
#'
#' @inherit ggpiestats return details
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#'
#' # association test (or contingency table analysis)
#' ggstatsplot::ggbarstats(
#'   data = mtcars,
#'   x = vs,
#'   y = cyl
#' )
#' @export

# defining the function
ggbarstats <- function(data,
                       x,
                       y,
                       counts = NULL,
                       ratio = NULL,
                       paired = FALSE,
                       results.subtitle = TRUE,
                       sample.size.label = TRUE,
                       label = "percentage",
                       label.args = list(alpha = 1, fill = "white"),
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
                       xlab = NULL,
                       ylab = NULL,
                       ggtheme = ggplot2::theme_bw(),
                       ggstatsplot.layer = TRUE,
                       package = "RColorBrewer",
                       palette = "Dark2",
                       ggplot.component = NULL,
                       output = "plot",
                       ...) {

  # ensure the variables work quoted or unquoted
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  counts <- if (!rlang::quo_is_null(rlang::enquo(counts))) rlang::ensym(counts)

  # ================= extracting column names as labels  =====================

  # if legend title is not provided, use the 'x' variable name
  if (rlang::is_null(legend.title)) legend.title <- rlang::as_name(x)

  # if x-axis label is not specified, use the 'y' variable
  if (is.null(xlab)) xlab <- rlang::as_name(y)

  # =============================== dataframe ================================

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}, {{ counts }}) %>%
    tidyr::drop_na(data = .) %>%
    as_tibble(x = .)

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

  # x and y need to be a factor for this analysis
  # also drop the unused levels of the factors
  data %<>%
    dplyr::mutate(
      {{ x }} := droplevels(as.factor({{ x }})),
      {{ y }} := droplevels(as.factor({{ y }}))
    )

  # TO DO: until one-way table is supported by `BayesFactor`
  if (nlevels(data %>% dplyr::pull({{ y }})) == 1L) {
    bf.message <- FALSE
    proportion.test <- FALSE
  }

  # ========================= statistical analysis ===========================

  # running appropriate statistical test
  # unpaired: Pearson's Chi-square test of independence
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

    # preparing the BF message for null hypothesis support
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

  # return early if anything other than plot
  if (output != "plot") {
    return(switch(
      EXPR = output,
      "subtitle" = subtitle,
      "caption" = caption
    ))
  }

  # =================================== plot =================================

  # dataframe with summary labels
  df_descriptive <- df_descriptive(data, {{ x }}, {{ y }}, label, perc.k)

  # dataframe containing all details needed for prop test
  df_proptest <- df_proptest(data, {{ x }}, {{ y }}, k)

  # if no. of factor levels is greater than the default palette color count
  palette_message(
    package = package,
    palette = palette,
    min_length = nlevels(data %>% dplyr::pull({{ x }}))[[1]]
  )

  # plot
  p <-
    ggplot2::ggplot(
      data = df_descriptive,
      mapping = ggplot2::aes(x = {{ y }}, y = perc, fill = {{ x }})
    ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "fill",
      color = "black",
      na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x * 100, "%"),
      breaks = seq(from = 0, to = 1, by = 0.10),
      minor_breaks = seq(from = 0.05, to = 0.95, by = 0.10)
    ) +
    rlang::exec(
      .fn = ggplot2::geom_label,
      mapping = ggplot2::aes(label = label, group = {{ x }}),
      show.legend = FALSE,
      position = ggplot2::position_fill(vjust = 0.5),
      na.rm = TRUE,
      !!!label.args
    ) +
    theme_ggstatsplot(ggtheme = ggtheme, ggstatsplot.layer = ggstatsplot.layer) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend.title)) +
    paletteer::scale_fill_paletteer_d(palette = paste0(package, "::", palette), name = "")

  # ================ sample size and proportion test labels ===================

  # adding significance labels to bars for proportion tests
  if (isTRUE(proportion.test)) {
    # modify plot
    p <- p +
      ggplot2::geom_text(
        data = df_proptest,
        mapping = ggplot2::aes(
          x = {{ y }},
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
    p <- p +
      ggplot2::geom_text(
        data = df_proptest,
        mapping = ggplot2::aes(
          x = {{ y }},
          y = -0.05,
          label = N,
          fill = NULL
        ),
        size = 4,
        na.rm = TRUE
      )
  }

  # =========================== putting all together ========================

  # preparing the plot
  p +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      subtitle = subtitle,
      title = title,
      caption = caption
    ) +
    ggplot.component
}
