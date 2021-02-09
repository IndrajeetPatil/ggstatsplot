#' @title Bar (column) charts with statistical tests
#' @name ggbarstats
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("maturing")}
#'
#' Bar charts for categorical data with statistical details included in the plot
#' as a subtitle.
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
#' @importFrom dplyr select mutate
#' @importFrom rlang !!! as_name ensym exec
#' @importFrom paletteer scale_fill_paletteer_d
#' @importFrom tidyr uncount drop_na
#' @importFrom statsExpressions expr_contingency_tab
#'
#' @inherit ggpiestats return details
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#'
#' # association test (or contingency table analysis)
#' ggstatsplot::ggbarstats(
#'   data = mtcars,
#'   x = vs,
#'   y = cyl
#' )
#' }
#' @export

# defining the function
ggbarstats <- function(data,
                       x,
                       y,
                       counts = NULL,
                       type = "parametric",
                       paired = FALSE,
                       results.subtitle = TRUE,
                       sample.size.label = TRUE,
                       label = "percentage",
                       label.args = list(alpha = 1, fill = "white"),
                       k = 2L,
                       proportion.test = TRUE,
                       perc.k = 0,
                       bf.message = TRUE,
                       ratio = NULL,
                       conf.level = 0.95,
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
  # convert entered stats type to a standard notation
  type <- ipmisc::stats_type_switch(type)

  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # =============================== dataframe ================================

  # creating a dataframe
  data %<>%
    dplyr::select({{ x }}, {{ y }}, .counts = {{ counts }}) %>%
    tidyr::drop_na(.)

  # untable the dataframe based on the count for each observation
  if (".counts" %in% names(data)) data %<>% tidyr::uncount(weights = .counts)

  # x and y need to be a factor; also drop the unused levels of the factors
  data %<>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ droplevels(as.factor(.x))))

  # TO DO: until one-way table is supported by `BayesFactor`
  if (nlevels(data %>% dplyr::pull({{ y }})) == 1L) c(bf.message, proportion.test) %<-% c(FALSE, FALSE)

  # -------------------------- statistical analysis --------------------------

  # if subtitle with results is to be displayed
  if (isTRUE(results.subtitle)) {
    subtitle <-
      tryCatch(
        expr = statsExpressions::expr_contingency_tab(
          data = data,
          x = {{ x }},
          y = {{ y }},
          type = type,
          k = k,
          paired = paired,
          ratio = ratio,
          conf.level = conf.level
        ),
        error = function(e) NULL
      )

    # preparing Bayes Factor caption
    if (type != "bayes" && isTRUE(bf.message) && isFALSE(paired)) {
      caption <-
        tryCatch(
          expr = statsExpressions::expr_contingency_tab(
            data = data,
            x = {{ x }},
            y = {{ y }},
            type = "bayes",
            k = k,
            top.text = caption,
            sampling.plan = sampling.plan,
            fixed.margin = fixed.margin,
            prior.concentration = prior.concentration
          ),
          error = function(e) NULL
        )
    }
  }

  # return early if anything other than plot
  if (output != "plot") {
    return(switch(EXPR = output,
      "caption" = caption,
      subtitle
    ))
  }

  # =================================== plot =================================

  # dataframe with summary labels
  df_descriptive <- df_descriptive(data, {{ x }}, {{ y }}, label, perc.k)

  # dataframe containing all details needed for prop test
  df_proptest <- df_proptest(data, {{ x }}, {{ y }}, k)

  # if no. of factor levels is greater than the default palette color count
  palette_message(package, palette, min_length = nlevels(data %>% dplyr::pull({{ x }}))[[1]])

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
      mapping = ggplot2::aes(label = .label, group = {{ x }}),
      show.legend = FALSE,
      position = ggplot2::position_fill(vjust = 0.5),
      na.rm = TRUE,
      !!!label.args
    ) +
    theme_ggstatsplot(ggtheme, ggstatsplot.layer) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend.title %||% rlang::as_name(x))) +
    paletteer::scale_fill_paletteer_d(palette = paste0(package, "::", palette), name = "")

  # ================ sample size and proportion test labels ===================

  # adding significance labels to bars for proportion tests
  if (isTRUE(proportion.test)) {
    # modify plot
    p <- p +
      ggplot2::geom_text(
        data = df_proptest,
        mapping = ggplot2::aes(x = {{ y }}, y = 1.05, label = .p.label, fill = NULL),
        size = 2.8,
        na.rm = TRUE,
        parse = TRUE
      )
  }

  # adding sample size info
  if (isTRUE(sample.size.label)) {
    p <- p +
      ggplot2::geom_text(
        data = df_proptest,
        mapping = ggplot2::aes(x = {{ y }}, y = -0.05, label = N, fill = NULL),
        size = 4,
        na.rm = TRUE
      )
  }

  # =========================== putting all together ========================

  # preparing the plot
  p +
    ggplot2::labs(
      x = xlab %||% rlang::as_name(y),
      y = ylab,
      subtitle = subtitle,
      title = title,
      caption = caption
    ) +
    ggplot.component
}
