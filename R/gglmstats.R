#'
#' @title Regression coefficients for fitted linear models with the model
#'   summary as a caption.
#' @name gglmstats
#' @aliases gglmstats
#' @author Indrajeet Patil
#' @return Plot with the regression coefficients' point estimates as dots with
#'   confidence interval whiskers.
#'
#' @param formula An object of class "formula" (or one that can be coerced to
#'   that class): a symbolic description of the model to be fitted. The details
#'   of model specification are given under 'Details'.
#' @param data A data frame containing the variables in the model.
#' @param xlab Label for `x` axis variable (Default: `"estimate"`).
#' @param ylab Label for `y` axis variable (Default: `"term"`).
#' @param title The text for the plot title.
#' @param subtitle The text for the plot subtitle.
#' @param k Number of decimal places expected for results.
#' @param dot.color Character describing color for the dot (Default: `"blue"`).
#' @param stats.labels Logical. Decides whether the statistic and p-values for
#'   each coefficient are to be attached to each dot as a text label using
#'   `ggrepel`.
#'
#' @import ggplot2
#'
#' @importFrom broom glance
#' @importFrom broom tidy
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate_at
#' @importFrom glue glue
#' @importFrom purrrlyr by_row
#' @importFrom dotwhisker dwplot
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @importFrom ggrepel geom_label_repel
#' @importFrom grid unit
#' @importFrom magrittr "%>%"
#'
#' @examples
#'
#' set.seed(123)
#' gglmstats(formula = mpg ~ cyl * am, data = mtcars)
#'
#' @export
#'

# function body
gglmstats <- function(data,
                      formula,
                      k = 3,
                      dot.color = "blue",
                      xlab = "estimate",
                      ylab = "term",
                      title = NULL,
                      subtitle = NULL,
                      stats.labels = TRUE) {
  #================================================== model and its summary ===========================================================

  # linear model object
  lm.object <-
    stats::lm(
      formula = stats::as.formula(formula),
      data = data,
      na.action = na.omit
    )

  # glance object from broom
  glance_df <- broom::glance(x = lm.object)

  # tidy dataframe of results from the linear model
  model_df <- broom::tidy(x = lm.object) %>%
    dplyr::filter(.data = ., term != "(Intercept)") %>%
    dplyr::select(.data = ., term, estimate, statistic, p.value) %>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "statistic",
      .funs = ~ ggstatsplot::specify_decimal_p(x = ., k = k)
    ) %>%
    signif_column(data = ., p = p.value) %>%
    purrrlyr::by_row(
      .d = .,
      ..f = ~ ggstatsplot::specify_decimal_p(
        x = .$p.value,
        k = k,
        p.value = TRUE
      ),
      .collate = "rows",
      .to = "p.value.formatted",
      .labels = TRUE
    )

  #================================================== summary caption ===========================================================

  # extracting the elements of the statistical object to prepare the caption
  caption.text <-
    base::substitute(
      expr =
        paste(
          italic("F"),
          "(",
          df1,
          ",",
          df2,
          ") = ",
          estimate,
          ", ",
          italic("p"),
          " = ",
          pvalue,
          ", AIC = ",
          AIC,
          ", BIC = ",
          BIC,
          ", Adjusted ",
          R ^ {
            2
          },
          " = ",
          adj.r.squared
        ),
      env = base::list(
        estimate = ggstatsplot::specify_decimal_p(x = glance_df$statistic[[1]], k),
        df1 = glance_df$df[[1]],
        # degrees of freedom are always integer
        df2 = glance_df$df.residual[[1]],
        pvalue = ggstatsplot::specify_decimal_p(x = glance_df$p.value[[1]],
                                                k,
                                                p.value = TRUE),
        AIC = ggstatsplot::specify_decimal_p(x = glance_df$AIC[[1]], k),
        BIC = ggstatsplot::specify_decimal_p(x = glance_df$BIC[[1]], k),
        adj.r.squared = ggstatsplot::specify_decimal_p(x = glance_df$adj.r.squared[[1]], k)
      )
    )

  #================================================== basic plot ===========================================================

  # creating the dot-whisker plot
  plot <- dotwhisker::dwplot(
    x = lm.object,
    show_intercept = FALSE,
    dodge_size = 0.2,
    tyle = "dotwhisker",
    dot_args = list(size = 0.8, color = dot.color),
    line_args = list(alpha = 0.75, size = 1)
  )  +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::geom_vline(
      xintercept = 0,
      colour = "grey60",
      linetype = 2,
      size = 1,
      na.rm = TRUE
    )

  #================================================== ggrepel labels ===========================================================

  if (isTRUE(stats.labels)) {
    # creating the labels as a column
    model_df$label <- glue::glue(
      "t({glance_df$df.residual}) = {model_df$statistic}, p = {model_df$p.value.formatted}"
    )

    # adding the labels
    plot <- plot +
      ggrepel::geom_label_repel(
        mapping = ggplot2::aes(label = model_df$label),
        size = 5,
        box.padding = grid::unit(x = 0.75, units = "lines"),
        fontface = "bold",
        direction = "both",
        color = "black",
        label.size = 0.25,
        max.iter = 3e2,
        point.padding = 0.5,
        segment.size = 0.2,
        segment.color = "grey50",
        force = 2,
        na.rm = TRUE
      )
  } else {

  }

  #================================================== other text labels ===========================================================
  #
  plot <- plot +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      caption = caption.text,
      subtitle = subtitle,
      title = title
    ) +
    ggstatsplot::theme_mprl() +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 12))

  # return the final plot
  return(plot)
}
