#' @title Dot-and-whisker plots for regression analyses
#' @name ggcoefstats
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("maturing")}
#'
#' Plot with the regression coefficients' point estimates as dots with
#' confidence interval whiskers and other statistical details included as
#' labels.
#'
#' @param x A model object to be tidied, or a tidy data frame containing results
#'   from a regression model. Function internally uses
#'   `parameters::model_parameters` to get a tidy dataframe. If
#'   a data frame is used, it *must* contain columns named `term` (names of
#'   predictors) and `estimate` (corresponding estimates of coefficients or
#'   other quantities of interest).
#' @param output Character describing the expected output from this function:
#'   `"plot"` (visualization of regression coefficients) or `"tidy"` (tidy
#'   dataframe of results `parameters::model_parameters`) or `"glance"` (object
#'   from `performance::model_performance`).
#' @param statistic Which statistic is to be displayed (either `"t"` or `"f"`or
#'   `"z"` or `"chi"`) in the label. This is relevant if the `x` argument is a
#'   *dataframe*.
#' @param bf.message Logical that decides whether results from running a
#'   Bayesian meta-analysis assuming that the effect size *d* varies across
#'   studies with standard deviation *t* (i.e., a random-effects analysis)
#'   should be displayed in caption. Defaults to `TRUE`.
#' @param xlab,ylab Labels for `x`- and `y`- axis variables, respectively
#'   (Defaults: `"regression coefficient"` and `"term"`).
#' @param subtitle The text for the plot subtitle. The input to this argument
#'   will be ignored if `meta.analytic.effect` is set to `TRUE`.
#' @param point.args Additional arguments that will be passed to
#'   `ggplot2::geom_point` geom. Please see documentation for that function to
#'   know more about these arguments.
#' @param conf.int Logical. Decides whether to display confidence intervals as
#'   error bars (Default: `TRUE`).
#' @param conf.level Numeric deciding level of confidence or credible intervals
#'   (Default: `0.95`).
#' @param effsize Character describing the effect size to be displayed: `"eta"`
#'   (default) or `"omega"`. This argument is relevant only for models objects
#'   of class `aov`, `anova`, `aovlist`, `"Gam"`, and `"manova"`.
#' @param meta.analytic.effect Logical that decides whether subtitle for
#'   meta-analysis via linear (mixed-effects) models (default: `FALSE`). If
#'   `TRUE`, input to argument `subtitle` will be ignored. This will be mostly
#'   relevant if a data frame with estimates and their standard errors is
#'   entered.
#' @param meta.type Type of statistics used to carry out random-effects
#'   meta-analysis. If `"parametric"` (default), `metafor::rma` function will be
#'   used. If `"robust"`, `metaplus::metaplus` function will be used. If
#'   `"bayes"`, `metaBMA::meta_random` function will be used.
#' @param exclude.intercept Logical that decides whether the intercept should be
#'   excluded from the plot (Default: `FALSE`).
#' @param errorbar.args Additional arguments that will be passed to
#'   `ggplot2::geom_errorbarh` geom. Please see documentation for that function
#'   to know more about these arguments.
#' @param vline Decides whether to display a vertical line (Default: `"TRUE"`).
#' @param vline.args Additional arguments that will be passed to
#'   `ggplot2::geom_vline` geom. Please see documentation for that function to
#'   know more about these arguments.
#' @param sort If `"none"` (default) do not sort, `"ascending"` sort by
#'   increasing coefficient value, or `"descending"` sort by decreasing
#'   coefficient value.
#' @param stats.labels Logical. Decides whether the statistic and *p*-values for
#'   each coefficient are to be attached to each dot as a text label using
#'   `ggrepel` (Default: `TRUE`).
#' @param stats.label.color Color for the labels. If set to `NULL`, colors will
#'   be chosen from the specified `package` (Default: `"RColorBrewer"`) and
#'   `palette` (Default: `"Dark2"`).
#' @param stats.label.args Additional arguments that will be passed to
#'   `ggrepel::geom_label_repel` geom. Please see documentation for that
#'   function to know more about these arguments.
#' @param only.significant If `TRUE`, only stats labels for significant effects
#'   is shown (Default: `FALSE`). This can be helpful when a large number of
#'   regression coefficients are to be displayed in a single plot. Relevant only
#'   when the `output` is a plot.
#' @param ... Additional arguments to tidying method. For more, see
#'   `parameters::model_parameters`.
#' @inheritParams parameters::model_parameters
#' @inheritParams theme_ggstatsplot
#' @inheritParams statsExpressions::expr_meta_random
#' @inheritParams ggbetweenstats
#'
#' @note **Important**: The function assumes that you have already downloaded
#'   the needed package (`metafor`, `metaplus`, or `metaBMA`) for meta-analysis.
#'
#' @import ggplot2
#' @importFrom rlang exec !!! !!
#' @importFrom dplyr select mutate matches across row_number last group_by ungroup
#' @importFrom ggrepel geom_label_repel
#' @importFrom tidyr unite
#' @importFrom insight is_model find_statistic
#' @importFrom statsExpressions expr_meta_random
#' @importFrom parameters model_parameters standardize_names
#' @importFrom performance model_performance
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcoefstats.html}
#'
#' @note
#' 1. All rows of regression estimates where either of the following
#'   quantities is `NA` will be removed if labels are requested: `estimate`,
#'   `statistic`, `p.value`.
#'
#' 2. Given the rapid pace at which new methods are added to these packages, it
#'   is recommended that you install the GitHub versions of `parameters` and
#'   `performance` in order to make most of this function.
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#'
#' # -------------- with model object --------------------------------------
#'
#' # model object
#' mod <- lm(formula = mpg ~ cyl * am, data = mtcars)
#'
#' # to get a plot
#' ggstatsplot::ggcoefstats(x = mod, output = "plot")
#'
#' # to get a tidy dataframe
#' ggstatsplot::ggcoefstats(x = mod, output = "tidy")
#'
#' # to get a glance summary
#' ggstatsplot::ggcoefstats(x = mod, output = "glance")
#'
#' # -------------- with custom dataframe -----------------------------------
#'
#' # creating a dataframe
#' df <-
#'   structure(
#'     list(
#'       term = structure(
#'         c(3L, 4L, 1L, 2L, 5L),
#'         .Label = c(
#'           "Africa",
#'           "Americas", "Asia", "Europe", "Oceania"
#'         ),
#'         class = "factor"
#'       ),
#'       estimate = c(
#'         0.382047603321706,
#'         0.780783111514665,
#'         0.425607573765058,
#'         0.558365541235078,
#'         0.956473848429961
#'       ),
#'       std.error = c(
#'         0.0465576338644502,
#'         0.0330218199731529,
#'         0.0362834986178494,
#'         0.0480571500648261,
#'         0.062215818388157
#'       ),
#'       statistic = c(
#'         8.20590677855356,
#'         23.6444603038067,
#'         11.7300588415607,
#'         11.6187818146078,
#'         15.3734833553524
#'       ),
#'       conf.low = c(
#'         0.290515146096969,
#'         0.715841986960399,
#'         0.354354575031406,
#'         0.46379116008131,
#'         0.827446138277154
#'       ),
#'       conf.high = c(
#'         0.473580060546444,
#'         0.845724236068931,
#'         0.496860572498711,
#'         0.652939922388847,
#'         1.08550155858277
#'       ),
#'       p.value = c(
#'         3.28679518728519e-15,
#'         4.04778497135963e-75,
#'         7.59757330804449e-29,
#'         5.45155840151592e-26,
#'         2.99171217913312e-13
#'       ),
#'       df.error = c(
#'         394L, 358L, 622L,
#'         298L, 22L
#'       )
#'     ),
#'     row.names = c(NA, -5L),
#'     class = c(
#'       "tbl_df",
#'       "tbl", "data.frame"
#'     )
#'   )
#'
#' # plotting the dataframe
#' ggstatsplot::ggcoefstats(
#'   x = df,
#'   statistic = "t",
#'   meta.analytic.effect = TRUE,
#'   k = 3
#' )
#' }
#' @export

# function body
ggcoefstats <- function(x,
                        output = "plot",
                        statistic = NULL,
                        conf.int = TRUE,
                        conf.level = 0.95,
                        k = 2L,
                        exclude.intercept = FALSE,
                        effsize = "eta",
                        meta.analytic.effect = FALSE,
                        meta.type = "parametric",
                        bf.message = TRUE,
                        sort = "none",
                        xlab = "regression coefficient",
                        ylab = "term",
                        title = NULL,
                        subtitle = NULL,
                        caption = NULL,
                        only.significant = FALSE,
                        point.args = list(size = 3, color = "blue"),
                        errorbar.args = list(height = 0),
                        vline = TRUE,
                        vline.args = list(size = 1, linetype = "dashed"),
                        stats.labels = TRUE,
                        stats.label.color = NULL,
                        stats.label.args = list(size = 3, direction = "y"),
                        package = "RColorBrewer",
                        palette = "Dark2",
                        ggtheme = ggplot2::theme_bw(),
                        ggstatsplot.layer = TRUE,
                        ...) {
  # ============================= dataframe ===============================

  if (isFALSE(insight::is_model(x))) {
    # set tidy_df to entered dataframe
    tidy_df <- as_tibble(x)

    # check that `statistic` is specified
    if (rlang::is_null(statistic)) {
      # inform the user
      if (output == "plot" && isTRUE(stats.labels)) {
        message(cat(
          "Note: The argument `statistic` must be specified.\n",
          "Skipping labels with statistical details.\n"
        ))
      }

      # skip labels
      stats.labels <- FALSE
    }
  }

  # =========================== tidy it ====================================

  if (isTRUE(insight::is_model(x))) {
    # which effect size?
    eta_squared <- omega_squared <- NULL
    if (effsize == "eta") eta_squared <- "partial"
    if (effsize == "omega") omega_squared <- "partial"

    # converting model object to a tidy dataframe
    tidy_df <-
      parameters::model_parameters(
        model = x,
        eta_squared = eta_squared,
        omega_squared = omega_squared,
        ci = conf.level,
        verbose = FALSE,
        ...
      ) %>%
      parameters::standardize_names(style = "broom") %>%
      dplyr::rename_all(~ gsub("omega2.|eta2.", "", .x))

    # anova objects need further cleaning
    if (class(x)[[1]] %in% c("aov", "aovlist", "anova", "Gam", "manova", "maov")) {
      if (dim(dplyr::filter(tidy_df, term == "Residuals"))[[1]] > 0L) {
        if ("group" %in% names(tidy_df)) tidy_df %<>% group_by(group)
        # creating a new column for residual degrees of freedom
        tidy_df %<>% dplyr::mutate(df.error = dplyr::last(df))
      }

      # renaming the `xlab` according to the estimate chosen
      xlab <- paste("partial", " ", effsize, "-squared", sep = "")

      # final cleanup
      tidy_df %<>%
        dplyr::filter(!is.na(statistic)) %>%
        dplyr::select(-dplyr::matches("sq$")) %>%
        dplyr::mutate(estimate.type = xlab) %>%
        dplyr::ungroup()
    }
  }

  # =================== tidy dataframe cleanup ================================

  # check for the one necessary column
  if (rlang::is_null(tidy_df) || !"estimate" %in% names(tidy_df)) {
    stop(message(cat(
      "Error: The tidy dataframe *must* contain column called 'estimate'.\n",
      "Check the tidy output using argument `output = 'tidy'`."
    )),
    call. = FALSE
    )
  }

  # remove NAs
  if (isTRUE(stats.labels)) {
    tidy_df %<>%
      dplyr::filter(dplyr::across(
        .cols = c(dplyr::matches("estimate|statistic|std.error|p.value")),
        .fns = ~ !is.na(.)
      ))
  }

  # create a new term column if it's not present
  if (!"term" %in% names(tidy_df)) {
    tidy_df %<>% dplyr::mutate(term = paste("term", dplyr::row_number(), sep = "_"))
  }

  # ================ check for duplicate terms and columns ===================

  # a check if there are repeated terms
  # needed for maov, lqm, lqmm, etc. kind of objects
  if (any(duplicated(dplyr::select(tidy_df, term)))) {
    tidy_df %<>%
      tidyr::unite(
        data = .,
        col = "term",
        dplyr::matches("term|variable|parameter|method|curve|response|component|contrast|group"),
        remove = TRUE,
        sep = "_"
      )
  }

  # halt if there are still repeated terms
  if (any(duplicated(dplyr::select(tidy_df, term)))) {
    message("Error: All elements in the column `term` should be unique.")
    return(invisible(tidy_df))
  }

  # if `parameters` output doesn't contain p-value or statistic column
  if (sum(c("p.value", "statistic") %in% names(tidy_df)) != 2L) stats.labels <- FALSE

  # =========================== CIs and intercepts ===========================

  # if `parameters` output doesn't contain CI
  if (!"conf.low" %in% names(tidy_df)) {
    # add NAs so that only dots will be shown
    tidy_df %<>% dplyr::mutate(conf.low = NA_character_, conf.high = NA_character_)

    # stop displaying whiskers
    conf.int <- FALSE
  }

  # whether to show model intercept
  if (isTRUE(exclude.intercept)) tidy_df %<>% dplyr::filter(!grepl("(Intercept)", term, TRUE))

  # ========================== preparing label ================================

  # adding a column with labels to be used with `ggrepel`
  if (isTRUE(stats.labels)) {
    # in case a dataframe was entered, `x` and `tidy_df` are going to be same
    if (isTRUE(insight::is_model(x))) statistic <- insight::find_statistic(x)

    # adding a column with labels using custom function
    tidy_df %<>%
      ggcoefstats_label_maker(
        tidy_df = .,
        statistic = substring(tolower(statistic), 1, 1),
        k = k,
        effsize = effsize
      )
  }

  # ========================== summary caption ================================

  # for non-dataframe objects
  if (isTRUE(insight::is_model(x))) {
    # creating glance dataframe
    suppressWarnings(glance_df <-
      performance::model_performance(x, verbose = FALSE) %>%
      parameters::standardize_names(style = "broom"))

    # no meta-analysis in this context
    meta.analytic.effect <- FALSE

    # if glance is not available, inform the user
    if (!is.null(glance_df) && all(c("aic", "bic") %in% names(glance_df))) {
      # preparing caption with model diagnostics
      caption <-
        substitute(
          expr = atop(displaystyle(top.text), expr = paste("AIC = ", AIC, ", BIC = ", BIC)),
          env = list(
            top.text = caption,
            AIC = format_num(glance_df$aic[[1]], k = 0L),
            BIC = format_num(glance_df$bic[[1]], k = 0L)
          )
        )
    }
  }

  # running meta-analysis
  if (isTRUE(meta.analytic.effect)) {
    # standardizing type of statistics name
    meta.type <- ipmisc::stats_type_switch(meta.type)

    # results from frequentist random-effects meta-analysis
    subtitle_df <- statsExpressions::expr_meta_random(tidy_df, type = meta.type, k = k)

    subtitle <- subtitle_df$expression[[1]]

    # results from Bayesian random-effects meta-analysis (only for parametric)
    if (meta.type == "parametric" && isTRUE(bf.message)) {
      caption_df <-
        statsExpressions::expr_meta_random(
          top.text = caption,
          type = "bayes",
          data = tidy_df,
          k = k
        )

      caption <- caption_df$expression[[1]]
    }
  }

  # ========================== sorting ===================================

  # whether the term need to be arranged in any specified order
  tidy_df %<>% dplyr::mutate(term = as.factor(term), .rowid = dplyr::row_number())

  # sorting factor levels
  new_order <-
    switch(sort,
      "none" = order(tidy_df$.rowid, decreasing = FALSE),
      "ascending" = order(tidy_df$estimate, decreasing = FALSE),
      "descending" = order(tidy_df$estimate, decreasing = TRUE),
      order(tidy_df$.rowid, decreasing = FALSE)
    )

  # sorting `term` factor levels according to new sorting order
  tidy_df %<>%
    dplyr::mutate(term = as.character(term)) %>%
    dplyr::mutate(term = factor(x = term, levels = term[new_order])) %>%
    dplyr::select(-.rowid)

  # ========================== basic plot ===================================

  # palette check is necessary only if output is a plot
  if (output == "plot") {
    # setting up the basic architecture
    plot <- ggplot2::ggplot(data = tidy_df, mapping = ggplot2::aes(x = estimate, y = term))

    # if needed, adding the vertical line
    if (isTRUE(vline)) {
      # adding the line geom
      plot <- plot +
        rlang::exec(
          .fn = ggplot2::geom_vline,
          xintercept = 0,
          na.rm = TRUE,
          !!!vline.args
        )
    }

    # if the confidence intervals are to be displayed on the plot
    if (isTRUE(conf.int)) {
      plot <- plot +
        rlang::exec(
          .fn = ggplot2::geom_errorbarh,
          data = tidy_df,
          mapping = ggplot2::aes(xmin = conf.low, xmax = conf.high),
          na.rm = TRUE,
          !!!errorbar.args
        )
    }

    # changing the point aesthetics
    plot <- plot +
      rlang::exec(
        .fn = ggplot2::geom_point,
        na.rm = TRUE,
        !!!point.args
      )

    # ========================= ggrepel labels ================================

    # adding the labels
    if (isTRUE(stats.labels)) {
      # only significant p-value labels are shown
      if (isTRUE(only.significant) && "p.value" %in% names(tidy_df)) {
        tidy_df %<>% dplyr::mutate(label = dplyr::case_when(
          p.value >= 0.05 ~ NA_character_,
          TRUE ~ label
        ))
      }

      # ========================== palette check =================================

      # if no. of factor levels is greater than the default palette color count
      palette_message(package, palette, length(tidy_df$term))

      # computing the number of colors in a given palette
      palette_df <-
        as_tibble(paletteer::palettes_d_names) %>%
        dplyr::filter(package == !!package, palette == !!palette) %>%
        dplyr::select(length)

      # if insufficient number of colors are available in a given palette
      if (palette_df$length[[1]] < length(tidy_df$term)) stats.label.color <- "black"

      # if user has not specified colors, then use a color palette
      if (is.null(stats.label.color)) {
        stats.label.color <-
          paletteer::paletteer_d(
            palette = paste0(package, "::", palette),
            n = length(tidy_df$term),
            type = "discrete"
          )
      }

      # adding labels
      plot <- plot +
        rlang::exec(
          .fn = ggrepel::geom_label_repel,
          data = tidy_df,
          mapping = ggplot2::aes(x = estimate, y = term, label = label),
          na.rm = TRUE,
          show.legend = FALSE,
          parse = TRUE,
          min.segment.length = 0,
          color = stats.label.color,
          !!!stats.label.args
        )
    }

    # ========================== annotations =============================

    # adding other labels to the plot
    plot <- plot +
      ggplot2::labs(
        x = xlab,
        y = ylab,
        caption = caption,
        subtitle = subtitle,
        title = title
      ) +
      theme_ggstatsplot(ggtheme, ggstatsplot.layer) +
      ggplot2::theme(plot.caption = ggplot2::element_text(size = 10))
  }

  # =========================== output =====================================

  # what needs to be returned?
  switch(output,
    "subtitle" = subtitle,
    "caption" = caption,
    "tidy" = as_tibble(tidy_df),
    "glance" = as_tibble(glance_df),
    plot
  )
}
