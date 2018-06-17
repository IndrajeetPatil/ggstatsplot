#'
#' @title Model coefficients for fitted models with the model summary as a
#'   caption.
#' @name ggcoefstats
#' @aliases ggcoefstats
#' @author Indrajeet Patil
#' @return Plot with the regression coefficients' point estimates as dots with
#'   confidence interval whiskers.
#'
#' @param x A model object to be tidied with `broom::tidy`.
#' @param output Character descrbing the expected output from this function:
#'   `"plot"` (visualization of regression coefficients) or `"tidy"` (tidy
#'   dataframe of results from `broom::tidy`) or `"glance"` (object from
#'   `broom::glance`) or `"augment"` (object from `broom::augment`).
#' @param xlab Label for `x` axis variable (Default: `"estimate"`).
#' @param ylab Label for `y` axis variable (Default: `"term"`).
#' @param title The text for the plot title.
#' @param subtitle The text for the plot subtitle.
#' @param effects.mermod,group.mermod In case the object is of class `merMod`
#'   (`lmerMod`, `glmerMod`, `nlmerMod`), these arguments determine which
#'   effects are to be displayed. By default, only the `"fixed"` effects will be
#'   shown.
#' @param point.color Character describing color for the point (Default:
#'   `"blue"`).
#' @param point.size Numeric specifying size for the point (Default: `3`).
#' @param point.shape Numeric specifying shape to draw the points (Default: `16`
#'   (a dot)).
#' @param conf.int Logical. Decides whether to display confidence intervals as
#'   error bars (Default: `TRUE`).
#' @param conf.level Numeric deciding level of confidence intervals (Default:
#'   `0.95`).
#' @param k Number of decimal places expected for results.
#' @param exclude.intercept Logical that decides whether the intercept should be
#'   excluded from the plot (Default: `TRUE`).
#' @param exponentiate If `TRUE`, the x-axis will be logarithmic (Default:
#'   `FALSE`).
#' @param errorbar.color Character deciding color of the error bars (Default:
#'   `"black"`).
#' @param errorbar.height Numeric specifying the height of the error bars
#'   (Default: `0`).
#' @param errorbar.linetype Line type of the error bars (Default: `"solid"`).
#' @param errorbar.size Numeric specifying the size of the error bars (Default:
#'   `0.5`).
#' @param vline Decides whether to display a vertical line (Default: `"TRUE"`).
#' @param vline.color Character specifying color of the vertical line (Default:
#'   `"black"`).
#' @param vline.linetype Character specifying line type of the vertical line
#'   (Default: `"dashed"`).
#' @param vline.size Numeric specifying the size of the vertical line (Default:
#'   `1`).
#' @param sort `"none"` (default) do not sort, `"ascending"` sort by increasing
#'   coefficient value, or `"descending"` sort by decreasing coefficient value.
#' @param stats.labels Logical. Decides whether the statistic and p-values for
#'   each coefficient are to be attached to each dot as a text label using
#'   `ggrepel` (Default: `TRUE`).
#' @param caption.summary Logical. Decides whether the model summary should be
#'   displayed as a cation to the plot (Default: `TRUE`).
#' @param label.direction Character (`"both"`, `"x"`, or `"y"`) -- direction in
#'   which to adjust position of labels (Default: `"y"`).
#' @param ggtheme A function, `ggplot2` theme name. Default value is
#'   `ggplot2::theme_grey()`. Allowed values are the official `ggplot2` themes,
#'   including `theme_bw()`, `theme_minimal()`, `theme_classic()`,
#'   `theme_void()`, etc.
#' @param \dots Extra arguments to pass to \code{\link[broom]{tidy}}.
#'
#' @import ggplot2
#'
#' @importFrom broom glance
#' @importFrom broom tidy
#' @importFrom broom augment
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate_at
#' @importFrom dplyr full_join
#' @importFrom dplyr everything
#' @importFrom dplyr desc
#' @importFrom purrrlyr by_row
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @importFrom ggrepel geom_label_repel
#' @importFrom grid unit
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom lmerTest as_lmerModLmerTest
#' @importFrom tibble rownames_to_column
#' @importFrom tibble as_data_frame
#'
#' @examples
#'
#' set.seed(123)
#' ggcoefstats(x = lm(formula = mpg ~ cyl * am, data = mtcars))
#'
#' @export
#'

# function body
ggcoefstats <- function(x,
                        output = "plot",
                        effects.mermod = "fixed",
                        group.mermod = "fixed",
                        point.color = "blue",
                        point.size = 3,
                        point.shape = 16,
                        conf.int = TRUE,
                        conf.level = 0.95,
                        k = 3,
                        exclude.intercept = TRUE,
                        exponentiate = FALSE,
                        errorbar.color = "black",
                        errorbar.height = 0,
                        errorbar.linetype = "solid",
                        errorbar.size = 0.5,
                        vline = TRUE,
                        vline.color = "black",
                        vline.linetype = "dashed",
                        vline.size = 1,
                        sort = "none",
                        xlab = "estimate",
                        ylab = "term",
                        title = NULL,
                        subtitle = NULL,
                        stats.labels = TRUE,
                        caption.summary = TRUE,
                        label.direction = "y",
                        ggtheme = ggplot2::theme_grey(),
                        ...) {
  #====================================== checking if object is supported =============================================================
  if (class(x)[[1]] == "aov") {
    base::message(cat(
      crayon::green("Note:"),
      crayon::blue(
        "The objects of class",
        crayon::yellow(class(x)[[1]]),
        "aren't currently supported."
      )
    ))
  } else if (class(x)[[1]] == "nlmerMod" ||
             class(x)[[1]] == "rlm") {
    stats.labels <- FALSE
    base::message(cat(
      crayon::green("Note:"),
      crayon::blue(
        "No p-values available for regression coefficients from",
        crayon::yellow(class(x)[[1]]),
        ", so skipping labels."
      )
    ))
  }
  #================================================== model and its summary ===========================================================

  # glance object from broom
  glance_df <- broom::glance(x = x) %>%
    tibble::as_data_frame(x = .)

  # tidy dataframe of results from the model
  # if these are merMod objects, choose whether the random effects are to be displayed
  tidy_df <-
    broom::tidy(
      x = x,
      conf.int = TRUE,
      conf.level = conf.level,
      effects = effects.mermod,
      group = group.mermod,
      ...
    )

  # p-values won't be computed by default for the lmer models
  if (class(x)[[1]] == "lmerMod") {
    # computing p-values
    lmer_p <-
      coef(summary(lmerTest::as_lmerModLmerTest(model = x, tol = 1e-08))) %>%
      base::as.data.frame(.) %>%
      tibble::rownames_to_column(df = ., var = "term") %>%
      dplyr::select(.data = ., term, p.value = `Pr(>|t|)`) %>%
      tibble::as_data_frame(x = .)

    # merging the two dataframes
    tidy_df <-
      dplyr::full_join(x = tidy_df, y = lmer_p, by = "term")
  }

  # ordering the dataframe
  tidy_df %<>%
    dplyr::select(.data = .,
                  term,
                  estimate,
                  conf.low,
                  conf.high,
                  dplyr::everything())

  # whether to show model intercept; if not, remove the corresponding terms from the dataframe
  if (isTRUE(exclude.intercept)) {
    tidy_df %<>% dplyr::filter(.data = .,
                               !base::grepl(
                                 pattern = "(Intercept)",
                                 x = term,
                                 ignore.case = TRUE
                               ))
  }

  # if the coefficients are to be exponentiated, the label positions will also have to be adjusted
  if (isTRUE(exponentiate)) {
    tidy_df$estimate <- base::exp(tidy_df$estimate)
  }

  # formatting the numbers for display and preparing labels
  if (isTRUE(stats.labels)) {
    tidy_df %<>%
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
          "estimate = ",
          ggstatsplot::specify_decimal_p(x = .$estimate, k = k),
          ", statistic = ",
          .$statistic,
          ", p = ",
          .$p.value.formatted,
          sep = ""
        ),
        .collate = "rows",
        .to = "label",
        .labels = TRUE
      )
  }
  #================================================== summary caption ===========================================================

  if (isTRUE(caption.summary)) {
    caption.text <-
      base::substitute(
        expr =
          paste("AIC = ",
                AIC,
                ", BIC = ",
                BIC,
                ", log-likelihood = ",
                loglik),
        env = base::list(
          AIC = ggstatsplot::specify_decimal_p(x = glance_df$AIC[[1]], k),
          BIC = ggstatsplot::specify_decimal_p(x = glance_df$BIC[[1]], k),
          loglik = ggstatsplot::specify_decimal_p(x = glance_df$logLik[[1]], k)
        )
      )
  } else {
    caption.text <- NULL
  }
  #================================================== basic plot ===========================================================

  # whether the term need to be arranged in any specified order
  if (sort != "none") {
    tidy_df$term <- base::as.factor(tidy_df$term)
    if (sort == "ascending") {
      new_order <- base::order(tidy_df$estimate, decreasing = FALSE)
    } else {
      new_order <- base::order(tidy_df$estimate, decreasing = TRUE)
    }
    tidy_df$term <- as.character(tidy_df$term)
    tidy_df$term <- base::factor(x = tidy_df$term, levels = tidy_df$term[new_order])
  } else {
    tidy_df$term <- base::as.factor(tidy_df$term)
    tidy_df %<>% tibble::rownames_to_column(df = ., var = "rowid")
    new_order <- base::order(tidy_df$rowid, decreasing = FALSE)
    tidy_df$term <- as.character(tidy_df$term)
    tidy_df$term <- base::factor(x = tidy_df$term, levels = tidy_df$term[new_order])
    tidy_df %<>% dplyr::select(.data = ., -rowid)
  }

  # setting up the basic architecture
  plot <-
    ggplot2::ggplot(data = tidy_df,
                    mapping = ggplot2::aes(x = estimate, y = factor(term)))

  # adding the vertical line, either at 1 if coefficients are exponentiated or to 0 if not
  if (isTRUE(vline)) {
    if (isTRUE(exponentiate)) {
      plot <- plot +
        ggplot2::geom_vline(
          xintercept = 1,
          color = vline.color,
          linetype = vline.linetype,
          size = vline.size,
          na.rm = TRUE
        ) +
        ggplot2::scale_x_log10()
    } else {
      plot <- plot +
        ggplot2::geom_vline(
          xintercept = 0,
          color = vline.color,
          linetype = vline.linetype,
          size = vline.size,
          na.rm = TRUE
        )
    }
  }

  # if the confidence intervals are to be displayed on the plot
  if (isTRUE(conf.int))
    plot <- plot +
    ggplot2::geom_errorbarh(
      ggplot2::aes_string(xmin = "conf.low", xmax = "conf.high"),
      color = errorbar.color,
      height = errorbar.height,
      linetype = errorbar.linetype,
      size = errorbar.size,
      na.rm = TRUE
    )

  # changing the point aesthetics
  plot <- plot +
    ggplot2::geom_point(
      color = point.color,
      size = point.size,
      shape = point.shape,
      na.rm = TRUE
    )

  #================================================== ggrepel labels ===========================================================

  if (isTRUE(stats.labels)) {
    # adding the labels
    plot <- plot +
      ggrepel::geom_label_repel(
        data = tidy_df,
        mapping = ggplot2::aes(x = estimate, y = term, label = label),
        size = 3,
        box.padding = grid::unit(x = 1, units = "lines"),
        fontface = "bold",
        direction = label.direction,
        color = "black",
        label.size = 0.25,
        segment.color = "black",
        segment.size = 0.5,
        segment.alpha = NULL,
        min.segment.length = 0.5,
        max.iter = 2000,
        point.padding = 0.5,
        force = 2,
        na.rm = TRUE
      )
  }

  #================================================== other plot labels ===========================================================
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
    ggstatsplot::theme_mprl(ggtheme = ggtheme) +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 10))

  #================================================== output ===========================================================
  #
  # what needs to be returned?
  if (output == "plot") {
    # return the final plot
    return(plot)
  } else if (output == "tidy") {
    return(tidy_df)
  } else if (output == "glance") {
    return(glance_df)
  } else if (output == "augment") {
    return(broom::augment(x = x) %>%
             tibble::as_data_frame(x = .))
  }
}
