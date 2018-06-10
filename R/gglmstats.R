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
#' @param dot.size Numeric specifying size for the dot (Default: `3`).
#' @param conf.int Logical. Decides whether to display confidence intervals as
#'   error bars (Default: `TRUE`).
#' @param conf.level Numeric deciding level of confidence intervals (Default:
#'   `0.95`).
#' @param exponentiate If `TRUE`, the x-axis will be logarithmic (Default:
#'   `FALSE`).
#' @param errorbar.color Character deciding color of the error bars (Default:
#'   `"black"`).
#' @param errorbar.height Numeric specifying the height of the error bars
#'   (Default: `0`).
#' @param errorbar.linetype Line type of the error bars (Default: `"solid"`).
#' @param errorbar.size Numeric speifying the size of the error bars (Default:
#'   `0.5`).
#' @param vline Decides whether to display a vertical line (Default: `"TRUE"`).
#' @param vline.intercept The xintercept for the vertical line. "auto" for x =
#'   `0` (or x = `1` if `exponentiate` is TRUE)
#' @param vline.color Character specifying color of the vertical line (Default:
#'   `"black"`).
#' @param vline.linetype Character specifying line type of the vertical line
#'   (Default: `"dashed"`).
#' @param vline.size Numeric specifying the size of the vertical line (Default:
#'   `1`).
#' @param sort `"none"` (default) do not sort, `"ascending"` sort by increasing
#'   coefficient value, or `"decending"` sort by decreasing coefficient value.
#' @param stats.labels Logical. Decides whether the statistic and p-values for
#'   each coefficient are to be attached to each dot as a text label using
#'   `ggrepel`.
#' @param label.direction Character (`"both"`, `"x"`, or `"y"`) -- direction in
#'   which to adjust position of labels (Default: `"y"`).
#'
#'
#' @import ggplot2
#'
#' @importFrom broom glance
#' @importFrom broom tidy
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate_at
#' @importFrom purrrlyr by_row
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @importFrom ggrepel geom_label_repel
#' @importFrom grid unit
#' @importFrom magrittr "%>%"
#' @importFrom GGally ggcoef
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
                      dot.size = 3,
                      conf.int = TRUE,
                      conf.level = 0.95,
                      exponentiate = FALSE,
                      errorbar.color = "black",
                      errorbar.height = 0,
                      errorbar.linetype = "solid",
                      errorbar.size = 0.5,
                      vline = TRUE,
                      vline.intercept = "auto",
                      vline.color = "black",
                      vline.linetype = "dashed",
                      vline.size = 1,
                      sort = c("none", "ascending", "decending"),
                      xlab = "estimate",
                      ylab = "term",
                      title = NULL,
                      subtitle = NULL,
                      stats.labels = TRUE,
                      label.direction = "y") {
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
    ) %>%
    purrrlyr::by_row(
      .d = .,
      ..f = ~ paste(
        "t(",
        glance_df$df.residual,
        ") = ",
        .$statistic,
        ", p = ",
        .$p.value.formatted,
        sep = ""
      ),
      .collate = "rows",
      .to = "label",
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
  plot <- GGally::ggcoef(
    x = lm.object,
    mapping = ggplot2::aes_string(y = "term", x = "estimate"),
    color = dot.color,
    size = dot.size,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    exclude_intercept = TRUE,
    vline = vline,
    vline_intercept = vline.intercept,
    vline_color = vline.color,
    vline_linetype = vline.linetype,
    vline_size = vline.size,
    errorbar_color = errorbar.color,
    errorbar_height = errorbar.height,
    errorbar_linetype = errorbar.linetype,
    errorbar_size = errorbar.size,
    sort = sort
  )

  #================================================== ggrepel labels ===========================================================

  if (isTRUE(stats.labels)) {
    # adding the labels
    plot <- plot +
      ggrepel::geom_label_repel(
        data = model_df,
        mapping = ggplot2::aes(x = estimate, y = term, label = label),
        size = 5,
        box.padding = grid::unit(x = 0.75, units = "lines"),
        fontface = "bold",
        direction = label.direction,
        color = "black",
        label.size = 0.25,
        max.iter = 3e2,
        point.padding = 0.5,
        segment.size = 0.2,
        segment.color = "grey50",
        force = 2,
        na.rm = TRUE
      )
  }

  #================================================== other text labels ===========================================================
  #
  # adding other labels to the plot
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
