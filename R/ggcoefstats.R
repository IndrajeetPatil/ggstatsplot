#' @title Dot-and-whisker plots for regression analyses
#' @name ggcoefstats
#'
#' @description
#'
#' Plot with the regression coefficients' point estimates as dots with
#' confidence interval whiskers and other statistical details included as
#' labels.
#'
#'  Although the statistical models displayed in the plot may differ based on the
#'  class of models being investigated, there are few aspects of the plot that will
#'  be invariant across models:
#'
#'   - The dot-whisker plot contains a dot representing the **estimate** and their
#'     **confidence intervals** (`95%` is the default). The estimate can either be
#'     effect sizes (for tests that depend on the `F`-statistic) or regression
#'     coefficients (for tests with `t`-, $\chi^{2}$-, and `z`-statistic), etc. The
#'     function will, by default, display a helpful `x`-axis label that should
#'     clear up what estimates are being displayed. The confidence intervals can
#'     sometimes be asymmetric if bootstrapping was used.
#'
#'   - The label attached to dot will provide more details from the statistical
#'     test carried out and it will typically contain estimate, statistic, and
#'     *p*-value.
#'
#'   - The caption will contain diagnostic information, if available, about
#'     models that can be useful for model selection: The smaller the Akaike's
#'     Information Criterion (**AIC**) and the Bayesian Information Criterion
#'     (**BIC**) values, the "better" the model is.
#'
#'   - The output of this function will be a `{ggplot2}` object and, thus,
#'     it can be further modified (e.g. change themes) with `{ggplot2}`.
#'
#' @section Summary of graphics:
#'
#' ```{r child="man/rmd-fragments/ggcoefstats_graphics.Rmd"}
#' ```
#'
#' @param x A model object to be tidied, or a tidy data frame from a regression
#'   model. Function internally uses [`parameters::model_parameters()`] to get a
#'   tidy data frame. If a data frame, it *must* contain at the minimum two
#'   columns named `term` (names of predictors) and `estimate` (corresponding
#'   estimates of coefficients or other quantities of interest).
#' @param statistic Relevant statistic for the model (`"t"`, `"f"`, `"z"`, or
#'   `"chi"`) in the label. Relevant only if `x` is a *data frame*.
#' @param effectsize.type This is the same as `es_type` argument of
#'   [`parameters::model_parameters()`]. Defaults to `"eta"`, and relevant for
#'   ANOVA-like objects.
#' @param bf.message Logical that decides whether results from running a
#'   Bayesian meta-analysis assuming that the effect size *d* varies across
#'   studies with standard deviation *t* (i.e., a random-effects analysis)
#'   should be displayed in caption. Defaults to `TRUE`.
#' @param subtitle The text for the plot subtitle. The input to this argument
#'   will be ignored if `meta.analytic.effect` is set to `TRUE`.
#' @param conf.int Logical. Decides whether to display confidence intervals as
#'   error bars (Default: `TRUE`).
#' @param conf.level Numeric deciding level of confidence or credible intervals
#'   (Default: `0.95`).
#' @param meta.analytic.effect Logical that decides whether subtitle for
#'   meta-analysis via linear (mixed-effects) models (default: `FALSE`). If
#'   `TRUE`, input to argument `subtitle` will be ignored. This will be mostly
#'   relevant if a data frame with estimates and their standard errors is
#'   entered.
#' @param meta.type Type of statistics used to carry out random-effects
#'   meta-analysis. If `"parametric"` (default), [`metafor::rma()`] will be
#'   used. If `"robust"`, [`metaplus::metaplus()`] will be used. If `"bayes"`,
#'   [`metaBMA::meta_random()`] will be used.
#' @param exclude.intercept Logical that decides whether the intercept should be
#'   excluded from the plot (Default: `FALSE`).
#' @param errorbar.args Additional arguments that will be passed to
#'   `geom_errorbarh()` geom. Please see documentation for that function
#'   to know more about these arguments.
#' @param vline Decides whether to display a vertical line (Default: `"TRUE"`).
#' @param vline.args Additional arguments that will be passed to
#'   `geom_vline` geom. Please see documentation for that function to
#'   know more about these arguments.
#' @param sort If `"none"` (default) do not sort, `"ascending"` sort by
#'   increasing coefficient value, or `"descending"` sort by decreasing
#'   coefficient value.
#' @param stats.labels Logical. Decides whether the statistic and *p*-values for
#'   each coefficient are to be attached to each dot as a text label using
#'   `{ggrepel}` (Default: `TRUE`).
#' @param stats.label.color Color for the labels. If set to `NULL`, colors will
#'   be chosen from the specified `package` (Default: `"RColorBrewer"`) and
#'   `palette` (Default: `"Dark2"`).
#' @param stats.label.args Additional arguments that will be passed to
#'   [`ggrepel::geom_label_repel()`].
#' @param only.significant If `TRUE`, only stats labels for significant effects
#'   is shown (Default: `FALSE`). This can be helpful when a large number of
#'   regression coefficients are to be displayed in a single plot.
#' @param ... Additional arguments to tidying method. For more, see
#'   [`parameters::model_parameters()`].
#' @inheritParams parameters::model_parameters
#' @inheritParams theme_ggstatsplot
#' @inheritParams statsExpressions::meta_analysis
#' @inheritParams ggbetweenstats
#'
#' @inheritSection statsExpressions::meta_analysis Random-effects meta-analysis
#'
#' @autoglobal
#'
#' @note
#'
#' 1. In case you want to carry out meta-analysis, you will be asked to install
#'   the needed packages (`{metafor}`, `{metaplus}`, or `{metaBMA}`) if they are
#'   unavailable.
#'
#' 2. All rows of regression estimates where either of the following
#'   quantities is `NA` will be removed if labels are requested:
#'   `estimate`, `statistic`, `p.value`.
#'
#' 3. Given the rapid pace at which new methods are added to these packages, it
#'   is recommended that you install development versions of `{easystats}`
#'   packages using the `install_latest()` function from `{easystats}`.
#'
#' @details For details, see:
#' <https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcoefstats.html>
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("lme4", quietly = TRUE)
#' # for reproducibility
#' set.seed(123)
#' library(lme4)
#'
#' # model object
#' mod <- lm(formula = mpg ~ cyl * am, data = mtcars)
#'
#' # creating a plot
#' p <- ggcoefstats(mod)
#'
#' # looking at the plot
#' p
#'
#' # extracting details from statistical tests
#' extract_stats(p)
#'
#' # further arguments can be passed to `parameters::model_parameters()`
#' ggcoefstats(lmer(Reaction ~ Days + (Days | Subject), sleepstudy), effects = "fixed")
#' @export
ggcoefstats <- function(
    x,
    statistic = NULL,
    conf.int = TRUE,
    conf.level = 0.95,
    digits = 2L,
    exclude.intercept = FALSE,
    effectsize.type = "eta",
    meta.analytic.effect = FALSE,
    meta.type = "parametric",
    bf.message = TRUE,
    sort = "none",
    xlab = NULL,
    ylab = NULL,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    only.significant = FALSE,
    point.args = list(size = 3.0, color = "blue", na.rm = TRUE),
    errorbar.args = list(height = 0, na.rm = TRUE),
    vline = TRUE,
    vline.args = list(linewidth = 1.0, linetype = "dashed"),
    stats.labels = TRUE,
    stats.label.color = NULL,
    stats.label.args = list(
      size = 3.0,
      direction = "y",
      min.segment.length = 0,
      na.rm = TRUE
    ),
    package = "RColorBrewer",
    palette = "Dark2",
    ggtheme = ggstatsplot::theme_ggstatsplot(),
    ...) {
  # model check -------------------------

  # if a data frame is entered then `statistic` is necessary to create labels
  if (!insight::is_model(x)) {
    tidy_df <- as_tibble(x)
    if (is.null(statistic)) stats.labels <- FALSE
  }

  # tidy data frame -------------------------

  if (insight::is_model(x)) {
    statistic <- insight::find_statistic(x)

    # converting model object to a tidy data frame
    tidy_df <- tidy_model_parameters(
      model      = x,
      es_type    = effectsize.type,
      ci         = conf.level,
      table_wide = TRUE,
      ...
    )

    # anova objects need further cleaning
    # nolint next: line_length_linter.
    if (all(c("df", "df.error") %in% names(tidy_df))) tidy_df %<>% mutate(effectsize = paste0("partial ", effectsize.type, "-squared"))
  }

  tidy_df <- .preprocess_tidy_data(tidy_df, sort)

  # if tidy data frame doesn't contain p-value or statistic column, no label
  if (!(all(c("p.value", "statistic") %in% names(tidy_df)))) stats.labels <- FALSE

  # CIs and intercepts -------------------------

  # if tidy data frame doesn't contain CIs, show only the estimate dots
  if (!"conf.low" %in% names(tidy_df)) {
    tidy_df %<>% mutate(conf.low = NA, conf.high = NA)
    conf.int <- FALSE
  }

  if (exclude.intercept) tidy_df %<>% filter(!grepl("(Intercept)", term, TRUE))

  # label -------------------------

  if (stats.labels) {
    tidy_df %<>% tidy_model_expressions(statistic, digits, effectsize.type)

    # only significant p-value labels are shown
    if (only.significant && ("p.value" %in% names(tidy_df))) {
      tidy_df %<>% mutate(expression = ifelse(p.value >= 0.05, list(NULL), expression))
    }
  }

  # summary caption -------------------------

  glance_df <- performance::model_performance(x, verbose = FALSE) %>% as_tibble()

  if (!is.null(glance_df) && all(c("AIC", "BIC") %in% names(glance_df))) {
    # nolint next: line_length_linter.
    glance_df %<>% mutate(expression = list(parse(text = glue("list(AIC=='{format_value(AIC, 0L)}', BIC=='{format_value(BIC, 0L)}')"))))
    caption <- .extract_expression(glance_df)
  }

  # meta analysis -------------------------

  if (meta.analytic.effect) {
    meta.type <- stats_type_switch(meta.type)

    subtitle_df <- meta_analysis(tidy_df, type = meta.type, digits = digits)
    subtitle <- .extract_expression(subtitle_df)

    if (meta.type == "parametric" && bf.message) {
      caption_df <- suppressWarnings(meta_analysis(tidy_df, type = "bayes", digits = digits))
      caption <- .extract_expression(caption_df)
    }
  }

  # basic plot -------------------------

  plot_coef <- ggplot(tidy_df, mapping = aes(estimate, term)) +
    exec(geom_point, !!!point.args)

  # adding confidence intervals
  if (conf.int) {
    plot_coef <- plot_coef +
      exec(
        geom_errorbarh,
        data    = tidy_df,
        mapping = aes(xmin = conf.low, xmax = conf.high),
        !!!errorbar.args
      )
  }

  if (vline) plot_coef <- plot_coef + exec(geom_vline, xintercept = 0, !!!vline.args)

  # ggrepel labels -------------------------

  if (stats.labels) {
    if (is.null(stats.label.color) && .is_palette_sufficient(package, palette, length(tidy_df$term))) {
      stats.label.color <- paletteer::paletteer_d(paste0(package, "::", palette), length(tidy_df$term))
    }

    plot_coef <- plot_coef +
      exec(
        ggrepel::geom_label_repel,
        data    = tidy_df,
        mapping = aes(x = estimate, y = term, label = expression),
        parse   = TRUE,
        color   = stats.label.color %||% "black",
        !!!stats.label.args
      )
  }

  # annotations ---------------------------------------------

  plot_coef +
    labs(
      x        = xlab %||% "estimate",
      y        = ylab %||% "term",
      caption  = caption,
      subtitle = subtitle,
      title    = title
    ) +
    ggtheme +
    theme(plot.caption = element_text(size = 10))
}


#' @noRd
.preprocess_tidy_data <- function(data, sort) {
  if (is.null(data) || !"estimate" %in% names(data)) {
    rlang::abort("The tidy data frame *must* contain 'estimate' column.")
  }

  # create a new term column if it's not present
  if (!"term" %in% names(data)) data %<>% mutate(term = paste0("term_", row_number()))

  # check if there are repeated terms (relevant for `maov`, `lqm`, etc.)
  if (anyDuplicated(data$term)) {
    data %<>%
      tidyr::unite(
        col    = "term",
        matches("term|variable|parameter|method|curve|response|component|contrast|group"),
        remove = TRUE,
        sep    = "_"
      )
  }

  # halt if there are still repeated terms
  if (anyDuplicated(data$term)) rlang::abort("Elements in `term` column must be unique.")

  data %<>% parameters::sort_parameters(sort = sort, column = "estimate")

  # `term` needs to be a factor column; otherwise, ggplot2 will sort the `x`-axis
  # labels alphabetically and terms won't appear in the expected order
  data %>% dplyr::mutate(term = factor(term, data$term))
}
