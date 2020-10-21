#' @title Dot-and-whisker plots for regression analyses
#' @name ggcoefstats
#' @return Plot with the regression coefficients' point estimates as dots with
#'   confidence interval whiskers and other statistical details included as
#'   labels.
#'
#' @param x A model object to be tidied, or a tidy data frame containing results
#'   from a regression model. Function internally uses
#'   `parameters::model_parameters` or `broom::tidy` to get a tidy dataframe. If
#'   a data frame is used, it *must* contain columns named `term` (names of
#'   predictors) and `estimate` (corresponding estimates of coefficients or
#'   other quantities of interest).
#' @param output Character describing the expected output from this function:
#'   `"plot"` (visualization of regression coefficients) or `"tidy"` (tidy
#'   dataframe of results from `broom::tidy`) or `"glance"` (object from
#'   `broom::glance`) or `"augment"` (object from `broom::augment`).
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
#'   excluded from the plot (Default: `TRUE`).
#' @param exponentiate If `TRUE`, the `x`-axis will be logarithmic (Default:
#'   `FALSE`). Note that exponents for the coefficient estimates and associated
#'   standard errors plus confidence intervals are computed by the underlying
#'   tidying packages (`broom`/`parameters`) and not done by `ggcoefstats`. So
#'   this might not work if the underlying packages don't support
#'   exponentiation.
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
#'   `parameters::model_parameters` and `broom::tidy`.
#' @inheritParams statsExpressions::bf_meta
#' @inheritParams parameters::model_parameters
#' @inheritParams theme_ggstatsplot
#' @inheritParams statsExpressions::expr_meta_random
#' @inheritParams ggbetweenstats
#'
#' @import ggplot2
#' @importFrom rlang exec !!!
#' @importFrom broomExtra tidy glance augment
#' @importFrom dplyr select mutate matches vars all_vars filter_at row_number
#' @importFrom stats qnorm lm
#' @importFrom ggrepel geom_label_repel
#' @importFrom tidyr unite
#' @importFrom insight is_model find_statistic
#' @importFrom statsExpressions expr_meta_random bf_meta
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcoefstats.html}
#'
#' @note All rows of regression estimates where either of the following
#'   quantities is `NA` will be removed if labels are requested: `estimate`,
#'   `statistic`, `p.value`.
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
#' # to get augmented dataframe
#' ggstatsplot::ggcoefstats(x = mod, output = "augment")
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
                        exclude.intercept = TRUE,
                        exponentiate = FALSE,
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
      # skip labels
      stats.labels <- FALSE

      # inform the user
      if (output == "plot") {
        message(cat(
          ipmisc::red("Note: "),
          ipmisc::blue("The argument `statistic` must be specified.\n"),
          ipmisc::blue("Skipping labels with statistical details.\n"),
          sep = ""
        ))
      }
    }
  }

  # =========================== tidy it ====================================

  if (isTRUE(insight::is_model(x))) {
    if (class(x)[[1]] %in% c("aov", "aovlist", "anova", "Gam", "manova", "maov")) {
      # creating dataframe
      tidy_df <-
        lm_effsize_standardizer(
          object = x,
          effsize = effsize,
          conf.level = conf.level
        )

      # renaming the `xlab` according to the estimate chosen
      xlab <- paste("partial", " ", effsize, "-squared", sep = "")
    } else {
      tidy_df <-
        broomExtra::tidy_parameters(
          x = x,
          conf.int = conf.int, # remove when only `parameters`
          conf.level = conf.level, # remove when only `parameters`
          ci = conf.level,
          exponentiate = exponentiate,
          effects = "fixed",
          verbose = FALSE,
          parametric = TRUE, # for `gam` objects
          ...
        )
    }
  }

  # =================== tidy dataframe cleanup ================================

  # check for the one necessary column
  if (rlang::is_null(tidy_df) || !"estimate" %in% names(tidy_df)) {
    stop(message(cat(
      ipmisc::red("Error: "),
      ipmisc::blue("The tidy dataframe *must* contain column called 'estimate'.\n"),
      ipmisc::blue("Check the tidy output using argument `output = 'tidy'`."),
      sep = ""
    )),
    call. = FALSE
    )
  }

  # remove NAs
  if (isTRUE(stats.labels)) {
    tidy_df %<>%
      dplyr::filter_at(
        .tbl = .,
        .vars = dplyr::vars(dplyr::matches("estimate|statistic|std.error|p.value")),
        .vars_predicate = dplyr::all_vars(!is.na(.))
      )
  }

  # create a new term column if it's not present
  if (!"term" %in% names(tidy_df)) {
    tidy_df %<>%
      dplyr::mutate(.data = ., term = dplyr::row_number()) %>%
      dplyr::mutate(.data = ., term = paste("term", term, sep = "_"))
  }

  # ================ check for duplicate terms and columns ===================

  # a check if there are repeated terms
  if (any(duplicated(dplyr::select(tidy_df, term)))) {
    tidy_df %<>%
      tidyr::unite(
        data = .,
        col = "term",
        dplyr::matches("term|variable|parameter|method|curve|response|component|contrast"),
        remove = TRUE,
        sep = "_"
      )
  }

  # halt if there are still repeated terms
  if (any(duplicated(dplyr::select(tidy_df, term)))) {
    message(cat(
      ipmisc::red("Error: "),
      ipmisc::blue("All elements in the column `term` should be unique."),
      sep = ""
    ))
    return(invisible(tidy_df))
  }

  # if broom output doesn't contain p-value or statistic column
  if (sum(c("p.value", "statistic") %in% names(tidy_df)) != 2L) stats.labels <- FALSE

  # ==================== confidence intervals check ===========================

  # if broom output doesn't contain CI
  if (!"conf.low" %in% names(tidy_df)) {

    # if standard error is present, create confidence intervals
    if ("std.error" %in% names(tidy_df)) {
      # probability for computing confidence intervals
      prob <- 1 - ((1 - conf.level) / 2)

      # computing confidence intervals
      tidy_df %<>%
        dplyr::mutate(
          .data = .,
          conf.low = estimate - stats::qnorm(prob) * std.error,
          conf.high = estimate + stats::qnorm(prob) * std.error
        )
    } else {
      # add NAs so that only dots will be shown
      tidy_df %<>% dplyr::mutate(conf.low = NA_character_, conf.high = NA_character_)

      # stop displaying whiskers
      conf.int <- FALSE

      # inform the user that skipping labels for the same reason
      message(cat(
        ipmisc::green("Note: "),
        ipmisc::blue("No confidence intervals available for regression coefficients,"),
        ipmisc::blue("so whiskers in the plot will be skipped.\n"),
        sep = ""
      ))
    }
  }

  # ============= intercept, exponentiation, and final tidy dataframe =========

  # whether to show model intercept
  # if not, remove the corresponding terms from the dataframe
  if (isTRUE(exclude.intercept)) {
    tidy_df %<>% dplyr::filter(!grepl(pattern = "(Intercept)", x = term, ignore.case = TRUE))
  }

  # ========================== preparing label ================================

  # adding a column with labels to be used with `ggrepel`
  if (isTRUE(stats.labels)) {
    # in case a dataframe was entered, `x` and `tidy_df` are going to be same
    if (isTRUE(insight::is_model(x))) {
      statistic <- substring(tolower(insight::find_statistic(x)), 1, 1)
    } else {
      statistic <- substring(tolower(statistic), 1, 1)
    }

    # adding a column with labels using custom function
    tidy_df %<>%
      ggcoefstats_label_maker(
        tidy_df = .,
        statistic = statistic,
        k = k,
        effsize = effsize
      )
  }

  # ========================== summary caption ================================

  # for non-dataframe objects
  if (isTRUE(insight::is_model(x))) {
    # creating glance dataframe
    glance_df <- broomExtra::glance_performance(x, verbose = FALSE)
    meta.analytic.effect <- FALSE

    # if glance is not available, inform the user
    if (!is.null(glance_df) && all(c("aic", "bic") %in% names(glance_df))) {
      # preparing caption with model diagnostics
      caption <-
        substitute(
          atop(
            displaystyle(top.text),
            expr = paste("AIC = ", AIC, ", BIC = ", BIC)
          ),
          env = list(
            top.text = caption,
            AIC = specify_decimal_p(x = glance_df$aic[[1]], k = 0L),
            BIC = specify_decimal_p(x = glance_df$bic[[1]], k = 0L)
          )
        )
    }
  }

  # running meta-analysis
  if (isTRUE(meta.analytic.effect)) {
    # standardizing type of statistics name
    meta.type <- ipmisc::stats_type_switch(meta.type)

    # results from frequentist random-effects meta-analysis
    subtitle <-
      expr_meta_random(
        data = tidy_df,
        type = meta.type,
        k = k
      )

    # model summary (detailed only for parametric statistics)
    if (meta.type == "parametric") {
      # results from Bayesian random-effects meta-analysis
      if (isTRUE(bf.message)) {
        caption <-
          statsExpressions::bf_meta(
            top.text = caption,
            output = "caption",
            data = tidy_df,
            k = k
          )
      }

      # caption with heterogeneity test results
      caption <-
        statsExpressions::expr_meta_random(
          data = tidy_df,
          k = k,
          caption = caption,
          output = "caption"
        )
    }
  }

  # ========================== sorting ===================================

  # whether the term need to be arranged in any specified order
  tidy_df %<>%
    dplyr::mutate(.data = ., term = as.factor(term)) %>%
    dplyr::mutate(.data = ., rowid = dplyr::row_number())

  # sorting factor levels
  new_order <-
    switch(
      sort,
      "none" = order(tidy_df$rowid, decreasing = FALSE),
      "ascending" = order(tidy_df$estimate, decreasing = FALSE),
      "descending" = order(tidy_df$estimate, decreasing = TRUE),
      order(tidy_df$rowid, decreasing = FALSE)
    )

  # sorting `term` factor levels according to new sorting order
  tidy_df %<>%
    dplyr::mutate(.data = ., term = as.character(term)) %>%
    dplyr::mutate(.data = ., term = factor(x = term, levels = term[new_order])) %>%
    dplyr::select(.data = ., -rowid)

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
          xintercept = ifelse(exponentiate, 1, 0),
          na.rm = TRUE,
          !!!vline.args
        )

      # logarithmic scale for exponent of coefficients
      if (isTRUE(exponentiate)) plot <- plot + ggplot2::scale_x_log10()
    }

    # if the confidence intervals are to be displayed on the plot
    if (isTRUE(conf.int)) {
      plot <- plot +
        rlang::exec(
          .fn = ggplot2::geom_errorbarh,
          data = tidy_df,
          mapping = ggplot2::aes_string(xmin = "conf.low", xmax = "conf.high"),
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
      if (isTRUE(only.significant) && "significance" %in% names(tidy_df)) {
        tidy_df %<>%
          dplyr::mutate(
            .data = .,
            label = dplyr::case_when(
              significance == "ns" ~ NA_character_,
              TRUE ~ label
            )
          )
      }

      # ========================== palette check =================================

      # if no. of factor levels is greater than the default palette color count
      palette_message(package, palette, length(tidy_df$term))

      # computing the number of colors in a given palette
      palette_df <-
        as_tibble(paletteer::palettes_d_names) %>%
        dplyr::filter(.data = ., package == !!package, palette == !!palette) %>%
        dplyr::select(.data = ., length)

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
      ggstatsplot::theme_ggstatsplot(
        ggtheme = ggtheme,
        ggstatsplot.layer = ggstatsplot.layer
      ) +
      ggplot2::theme(plot.caption = ggplot2::element_text(size = 10))
  }

  # =========================== output =====================================

  # what needs to be returned?
  return(switch(
    EXPR = output,
    "plot" = plot,
    "subtitle" = subtitle,
    "caption" = caption,
    "tidy" = tidy_df,
    "glance" = glance_df,
    "augment" = as_tibble(broomExtra::augment(x, ...)),
    "plot"
  ))
}
