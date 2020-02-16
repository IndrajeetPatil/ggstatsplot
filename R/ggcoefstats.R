#' @title Dot-and-whisker plots for regression analyses
#' @name ggcoefstats
#' @return Plot with the regression coefficients' point estimates as dots with
#'   confidence interval whiskers and other statistical details included as
#'   labels.
#'
#' @param x A model object to be tidied, or a tidy data frame containing
#'   results. If a data frame is to be plotted, it *must* contain columns named
#'   `term` (names of predictors), or `estimate` (corresponding estimates of
#'   coefficients or other quantities of interest). Other optional columns are
#'   `conf.low` and `conf.high` (for confidence intervals); `p.value`. It is
#'   important that all `term` names should be unique. Function internally uses
#'   `broom::tidy` or `parameters::model_parameters` to get a tidy dataframe.
#' @param output Character describing the expected output from this function:
#'   `"plot"` (visualization of regression coefficients) or `"tidy"` (tidy
#'   dataframe of results from `broom::tidy`) or `"glance"` (object from
#'   `broom::glance`) or `"augment"` (object from `broom::augment`).
#' @param statistic Which statistic is to be displayed (either `"t"` or `"f"`or
#'   `"z"`) in the label. This is especially important if the `x` argument in
#'   `ggcoefstats` is a dataframe in which case the function wouldn't know what
#'   kind of model it is dealing with.
#' @param bf.message Logical that decides whether results from running a
#'   Bayesian meta-analysis assuming that the effect size *d* varies across
#'   studies with standard deviation *t* (i.e., a random-effects analysis)
#'   should be displayed in caption. Defaults to `TRUE`.
#' @param xlab,ylab Labels for `x` axis variable (Defaults: `"regression
#'   coefficient"` and `"term"`, resp.).
#' @param subtitle The text for the plot subtitle. The input to this argument
#'   will be ignored if `meta.analytic.effect` is set to `TRUE`.
#' @param p.adjust.method Adjustment method for *p*-values for multiple
#'   comparisons. Possible methods are: `"holm"`, `"hochberg"`, `"hommel"`,
#'   `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`. Default is no correction
#'   (`"none"`). This argument is relevant for multiplicity correction for
#'   multiway ANOVA designs (see,
#'   \href{https://link.springer.com/article/10.3758/s13423-015-0913-5}{Cramer
#'   et al., 2015}).
#' @param point.color Character describing color for the point (Default:
#'   `"blue"`).
#' @param point.size Numeric specifying size for the point (Default: `3`).
#' @param point.shape Numeric specifying shape to draw the points (Default: `16`
#'   (**a dot**)).
#' @param conf.int Logical. Decides whether to display confidence intervals as
#'   error bars (Default: `TRUE`).
#' @param conf.level Numeric deciding level of confidence intervals (Default:
#'   `0.95`). For `MCMC` model objects (`Stan`, `JAGS`, etc.), this will be
#'   probability level for CI.
#' @param coefficient.type Relevant only for ordinal regression models (`clm` ,
#'   `clmm`, `"svyolr"`, and `polr`), this argument decides which parameters are
#'   display in the plot. Available parameters are: parameter that measures the
#'   **intercept**, i.e. the log-odds distance between response values
#'   (`"alpha"`); effects on the **location** (`"beta"`); or effects on the
#'   **scale** (`"zeta"`). For `clm` and `clmm` models, by default, only
#'   `"beta"` (a vector of regression parameters) parameters will be show. Other
#'   options are `"alpha"` (a vector of threshold parameters) or `"both"`. For
#'   `polr` models, by default, only `"coefficient"` will be shown. Other option
#'   is to show `"zeta"` parameters. Note that, from `broom 0.7.0` onward,
#'   coefficients will be renamed and `"intercept"` type coefficients will
#'   correspond to `"alpha"` parameters, `"location"` type coefficients will
#'   correspond to `"beta"` parameters, and `"scale"` type coefficients will
#'   correspond to `"zeta"` parameters.
#' @param nboot Number of bootstrap samples for confidence intervals for partial
#'   eta-squared and omega-squared (Default: `500`). This argument is relevant
#'   only for models objects of class `aov`, `anova`, and `aovlist`.
#' @param effsize Character describing the effect size to be displayed: `"eta"`
#'   (default) or `"omega"`. This argument is relevant
#'   only for models objects of class `aov`, `anova`, and `aovlist`.
#' @param partial Logical that decides if partial eta-squared or omega-squared
#'   are returned (Default: `TRUE`). If `FALSE`, eta-squared or omega-squared
#'   will be returned. Valid only for objects of class `aov`, `anova`, or
#'   `aovlist`.
#' @param meta.analytic.effect Logical that decides whether subtitle for
#'   meta-analysis via linear (mixed-effects) models (default: `FALSE`). If
#'   `TRUE`, input to argument `subtitle` will be ignored. This will be mostly
#'   relevant if a data frame with estimates and their standard errors is
#'   entered as input to `x` argument.
#' @param meta.type Type of statistics used to carry out random-effects
#'   meta-analysis. If `"parametric"` (default), `metafor::rma` function will be
#'   used. If `"robust"`, `metaplus::metaplus` function will be used. If
#'   `"bayes"`, `metaBMA::meta_random` function will be used.
#' @param k Number of decimal places expected for results displayed in labels
#'   (Default : `k = 2`).
#' @param k.caption.summary Number of decimal places expected for results
#'   displayed in captions (Default : `k.caption.summary = 0`).
#' @param exclude.intercept Logical that decides whether the intercept should be
#'   excluded from the plot (Default: `TRUE`).
#' @param exponentiate If `TRUE`, the `x`-axis will be logarithmic (Default:
#'   `FALSE`).
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
#' @param stats.label.color Color for the labels. If `stats.label.color` is
#'   `NULL`, colors will be chosen from the specified `package` (Default:
#'   `"RColorBrewer"`) and `palette` (Default: `"Dark2"`).
#' @param stats.label.args Additional arguments that will be passed to
#'   `ggrepel::geom_label_repel` geom. Please see documentation for that
#'   function to know more about these arguments.
#' @param only.significant If `TRUE`, only stats labels for significant effects
#'   is shown (Default: `FALSE`). This can be helpful when a large number of
#'   regression coefficients are to be displayed in a single plot. Relevant only
#'   when the `output` is a plot.
#' @param caption.summary Logical. Decides whether the model summary should be
#'   displayed as a cation to the plot (Default: `TRUE`). Color of the line
#'   segment. Defaults to the same color as the text.
#' @param ... Additional arguments to tidying method.
#' @inheritParams statsExpressions::bf_meta
#' @inheritParams broom::tidy.clm
#' @inheritParams broom::tidy.polr
#' @inheritParams theme_ggstatsplot
#' @inheritParams statsExpressions::expr_meta_parametric
#' @inheritParams ggbetweenstats
#'
#' @import ggplot2
#' @importFrom rlang exec
#' @importFrom ipmisc tidy glance augment
#' @importFrom dplyr select bind_rows summarize mutate mutate_at mutate_if n
#' @importFrom dplyr group_by arrange full_join vars matches desc everything
#' @importFrom dplyr vars all_vars filter_at starts_with row_number
#' @importFrom stats as.formula lm confint qnorm p.adjust
#' @importFrom ggrepel geom_label_repel
#' @importFrom parameters p_value
#' @importFrom tidyr unite
#' @importFrom groupedstats lm_effsize_standardizer
#' @importFrom insight is_model
#' @importFrom performance model_performance
#' @importFrom statsExpressions expr_meta_parametric bf_meta
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcoefstats.html}
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
#'       df.residual = c(
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
#'
#' # -------------- getting model summary ------------------------------
#'
#' # model
#' library(lme4)
#' lmm1 <- lme4::lmer(
#'   formula = Reaction ~ Days + (Days | Subject),
#'   data = sleepstudy
#' )
#'
#' # dataframe with model summary
#' ggstatsplot::ggcoefstats(x = lmm1, output = "glance")
#'
#' # -------------- getting augmented dataframe ------------------------------
#'
#' # setup
#' set.seed(123)
#' library(survival)
#'
#' # fit
#' cfit <-
#'   survival::coxph(formula = Surv(time, status) ~ age + sex, data = lung)
#'
#' # augmented dataframe
#' ggstatsplot::ggcoefstats(
#'   x = cfit,
#'   data = lung,
#'   output = "augment",
#'   type.predict = "risk"
#' )
#' }
#' @export

# function body
ggcoefstats <- function(x,
                        output = "plot",
                        statistic = NULL,
                        bf.message = TRUE,
                        p.adjust.method = "none",
                        coefficient.type = c("beta", "location", "coefficient"),
                        effsize = "eta",
                        partial = TRUE,
                        nboot = 500,
                        meta.analytic.effect = FALSE,
                        meta.type = "parametric",
                        conf.int = TRUE,
                        conf.level = 0.95,
                        k = 2,
                        k.caption.summary = 0,
                        exclude.intercept = TRUE,
                        exponentiate = FALSE,
                        sort = "none",
                        xlab = "regression coefficient",
                        ylab = "term",
                        title = NULL,
                        subtitle = NULL,
                        only.significant = FALSE,
                        caption = NULL,
                        caption.summary = TRUE,
                        point.color = "blue",
                        point.size = 3,
                        point.shape = 16,
                        errorbar.args = list(height = 0),
                        vline = TRUE,
                        vline.args = list(size = 1, linetype = "dashed"),
                        stats.labels = TRUE,
                        stats.label.color = NULL,
                        stats.label.args = list(
                          size = 3,
                          segment.color = "grey50",
                          direction = "y"
                        ),
                        package = "RColorBrewer",
                        palette = "Dark2",
                        direction = 1,
                        ggtheme = ggplot2::theme_bw(),
                        ggstatsplot.layer = TRUE,
                        messages = FALSE,
                        ...) {

  # =================== list of objects (for tidy and glance) ================

  # creating a list of objects which will have fixed and random "effects"
  # only fixed effects will be selected
  mixed.mods <-
    c(
      "glmmadmb", "glmerMod", "glmmPQL", "glmmTMB",
      "bglmerMod", "blmerMod", "lme", "lmerMod", "merMod", "nlmerMod", "rlmerMod", "TMB",
      "brmsfit", "brmsfit_multiple", "mcmc", "MCMCglmm", "rjags", "stanreg", "stanmvreg"
    )

  # =================== types of models =====================================

  # models for which statistic is F-value
  f.mods <- c("aov", "aovlist", "anova", "Gam", "manova")

  # model for which the output names are going to be slightly weird
  weird_name_mods <-
    c("gmm", "lmodel2", "gamlss", "drc", "mlm", "DirichletRegModel")

  # ============================= model summary ============================

  # creating glance dataframe
  glance_df <- ipmisc::glance(x)

  # if `NULL`, try with `performance`
  if (is.null(glance_df)) {
    glance_df <-
      tryCatch(
        expr = as_tibble(performance::model_performance(x)),
        error = function(e) NULL
      )
  }

  # if the object is not a dataframe, check if summary caption is to be displayed
  if (isTRUE(insight::is_model(x))) {
    # if glance is not available, inform the user
    if (is.null(glance_df) ||
      !all(c("aic", "bic") %in% tolower(names(glance_df)))) {
      # inform the user
      message(cat(
        ipmisc::green("Note: "),
        ipmisc::blue("No model diagnostics information available, so skipping caption.\n"),
        sep = ""
      ))

      # and skip the caption
      caption.summary <- FALSE
    }
  }

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
          ipmisc::red("Note"),
          ipmisc::blue(": For the object of class"),
          ipmisc::yellow(class(x)[[1]]),
          ipmisc::blue(", the argument `statistic` must be specified ('t', 'z', or 'f').\n"),
          ipmisc::blue("Statistical labels will therefore be skipped.\n"),
          sep = ""
        ))
      }
    }
  }

  # =========================== broom.mixed tidiers =======================

  if (isTRUE(insight::is_model(x))) {
    if (class(x)[[1]] %in% mixed.mods) {
      # getting tidy output using `broom.mixed`
      tidy_df <-
        ipmisc::tidy(
          x = x,
          conf.int = conf.int,
          # exponentiate = exponentiate,
          conf.level = conf.level,
          effects = "fixed",
          ...
        )

      # ====================== tidying F-statistic objects ===================
    } else if (class(x)[[1]] %in% f.mods) {
      # creating dataframe
      tidy_df <-
        groupedstats::lm_effsize_standardizer(
          object = x,
          effsize = effsize,
          partial = partial,
          conf.level = conf.level,
          nboot = nboot
        ) %>%
        dplyr::rename(.data = ., statistic = F.value)

      # prefix for effect size
      if (isTRUE(partial)) {
        effsize.prefix <- "partial"
      } else {
        effsize.prefix <- NULL
      }

      # renaming the `xlab` according to the estimate chosen
      xlab <- paste(effsize.prefix, " ", effsize, "-squared", sep = "")

      # ==================== tidying everything else ===========================
    } else {
      tidy_df <-
        ipmisc::tidy(
          x = x,
          conf.int = conf.int,
          conf.level = conf.level,
          effects = "fixed",
          # exponentiate = exponentiate,
          parametric = TRUE, # relevant for `gam` objects
          ...
        )
    }
  }

  # try again with `broomExtra` and `easystats`
  if (rlang::is_null(tidy_df)) tidy_df <- ipmisc::tidy(x, ...)
  if (rlang::is_null(tidy_df)) tidy_df <- parameters_tidy(x, ci = conf.level, ...)

  # =================== tidy dataframe cleanup ================================

  # check for the one necessary column
  if (rlang::is_null(tidy_df) || !"estimate" %in% names(tidy_df)) {
    stop(message(cat(
      ipmisc::red("Error: "),
      ipmisc::blue("The object of class "),
      ipmisc::yellow(class(x)[[1]]),
      ipmisc::blue(" *must* contain column called 'estimate' in tidy output.\n"),
      ipmisc::blue("Check the tidy output using argument `output = 'tidy'`."),
      sep = ""
    )),
    call. = FALSE
    )
  }

  # create a new term column if it's not present
  if (!"term" %in% names(tidy_df)) {
    tidy_df %<>%
      dplyr::mutate(.data = ., term = dplyr::row_number()) %>%
      dplyr::mutate(.data = ., term = paste("term", term, sep = "_"))
  }

  # selecting needed coefficients/parameters for ordinal regression models
  if (any(names(tidy_df) %in% c("coefficient_type", "coef.type"))) {
    if (any(coefficient.type %in%
      c("alpha", "beta", "zeta", "intercept", "location", "scale", "coefficient"))) {
      # subset the dataframe, only if not all coefficients are to be retained
      tidy_df %<>%
        dplyr::filter_at(
          .tbl = .,
          .vars = dplyr::vars(dplyr::starts_with("coef")),
          .vars_predicate = dplyr::all_vars(. %in% coefficient.type)
        )
    }
  }

  # changing names of columns to the required format for `aareg` objects
  if (class(x)[[1]] == "aareg") {
    tidy_df %<>%
      dplyr::rename(
        .data = .,
        coefficient = statistic,
        statistic = statistic.z
      )
  }

  # =================== check for duplicate terms ============================

  # for some class of objects, there are going to be duplicate terms
  # create a new column by collapsing original `variable` and `term` columns
  if (class(x)[[1]] %in% weird_name_mods) {
    tidy_df %<>%
      tidyr::unite(
        data = .,
        col = "term",
        dplyr::matches("term|variable|parameter|method|curve|response"),
        remove = TRUE,
        sep = "_"
      )
  }

  # halt if there are repeated terms
  if (any(duplicated(dplyr::select(tidy_df, term)))) {
    message(cat(
      ipmisc::red("Error: "),
      ipmisc::blue("All elements in the column `term` should be unique.\n"),
      sep = ""
    ))
    return(invisible(tidy_df))
  }

  # =================== p-value computation ==================================

  # p-values won't be computed by default for some of the models
  if (isTRUE(insight::is_model(x)) && !"p.value" %in% names(tidy_df)) {
    # use `sjstats` S3 methods to add them to the tidy dataframe
    tryCatch(
      expr = tidy_df %<>%
        dplyr::full_join(
          x = .,
          y = parameters::p_value(model = x, method = "wald", component = "all") %>%
            dplyr::rename(.data = ., p.value = p),
          by = c("term" = "Parameter")
        ) %>%
        dplyr::filter(.data = ., !is.na(estimate)) %>%
        as_tibble(x = .),
      error = function(e) tidy_df
    )
  }

  # ================== statistic and p-value check ===========================

  # if broom output doesn't contain p-value or statistic column
  if (sum(c("p.value", "statistic") %in% names(tidy_df)) != 2) {
    # skip the labels
    stats.labels <- FALSE

    # inform the user that skipping labels for the same reason
    # (relevant only in case of a plot)
    if (output == "plot") {
      message(cat(
        ipmisc::green("Note: "),
        ipmisc::blue("No p-values and/or statistic available for the model object;"),
        ipmisc::blue("\nskipping labels with statistical details.\n"),
        sep = ""
      ))
    }
  }

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
      tidy_df %<>%
        dplyr::mutate(.data = ., conf.low = NA_character_, conf.high = NA_character_)

      # stop displaying whiskers
      conf.int <- FALSE

      # inform the user that skipping labels for the same reason
      message(cat(
        ipmisc::green("Note: "),
        ipmisc::blue("No confidence intervals available for regression coefficients"),
        ipmisc::blue("object, so skipping whiskers in the plot.\n"),
        sep = ""
      ))
    }
  }

  # ============= intercept, exponentiation, and final tidy dataframe =========

  # ordering the dataframe
  tidy_df %<>%
    dplyr::select(
      .data = .,
      term,
      estimate,
      conf.low,
      conf.high,
      dplyr::everything()
    )

  # whether to show model intercept
  # if not, remove the corresponding terms from the dataframe
  if (isTRUE(exclude.intercept)) {
    tidy_df %<>%
      dplyr::filter(
        .data = ., !grepl(pattern = "(Intercept)", x = term, ignore.case = TRUE)
      )
  }

  # if the coefficients are to be exponentiated, the label positions will also
  # have to be adjusted
  if (isTRUE(exponentiate)) {
    tidy_df %<>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = dplyr::vars(dplyr::matches(match = "estimate|conf", ignore.case = TRUE)),
        .funs = exp
      )
  }

  # # adjust the p-values based on the adjustment used
  if ("p.value" %in% names(tidy_df)) {
    # adjust the p-values based on the adjustment used
    tidy_df %<>% dplyr::mutate(p.value = stats::p.adjust(p.value, p.adjust.method))
  }

  # ========================== preparing label ================================

  # adding a column with labels to be used with `ggrepel`
  if (isTRUE(stats.labels)) {
    # in case a dataframe was entered, `x` and `tidy_df` are going to be same
    if (isFALSE(insight::is_model(x))) x <- tidy_df

    # adding a column with labels using custom function
    tidy_df %<>%
      ggcoefstats_label_maker(
        x = x,
        statistic = statistic,
        tidy_df = .,
        glance_df = glance_df,
        k = k,
        effsize = effsize,
        partial = partial
      )
  }

  # ============== meta-analysis plus Bayes factor =========================

  # check if meta-analysis is to be run
  if (isTRUE(meta.analytic.effect) && "std.error" %in% names(tidy_df)) {
    if (dim(dplyr::filter(.data = tidy_df, is.na(std.error)))[[1]] > 0L) {
      # inform the user that skipping labels for the same reason
      message(cat(
        ipmisc::red("Error: "),
        ipmisc::blue("At least one of the `std.error` column values is `NA`.\n"),
        ipmisc::blue("No meta-analysis will be carried out.\n"),
        sep = ""
      ))

      # turn off meta-analysis
      meta.analytic.effect <- FALSE
    }
  }

  # running meta-analysis
  if (isTRUE(meta.analytic.effect)) {
    # standardizing type of statistics name
    meta.type <- stats_type_switch(meta.type)

    # results from frequentist random-effects meta-analysis
    subtitle <-
      subtitle_function_switch(
        test = "meta",
        type = meta.type,
        data = tidy_df,
        k = k,
        messages = messages
      )

    # results from Bayesian random-effects meta-analysis
    if (isTRUE(bf.message) && meta.type == "parametric") {
      caption <-
        statsExpressions::bf_meta(
          caption = caption,
          data = tidy_df,
          k = k,
          messages = messages
        )
    }

    # model summary (detailed only for parametric statistics)
    if (meta.type == "parametric") {
      caption.meta <-
        statsExpressions::expr_meta_parametric(
          data = tidy_df,
          k = k,
          caption = caption,
          messages = FALSE,
          output = "caption"
        )
    } else {
      caption.meta <- caption
    }
  }

  # ========================== summary caption ================================

  # caption containing model diagnostics
  if (isTRUE(caption.summary)) {
    # for dataframe objects
    if (isFALSE(insight::is_model(x)) && isTRUE(meta.analytic.effect)) {
      caption <- caption.meta
    }

    # for non-dataframe objects
    if (isTRUE(insight::is_model(x))) {
      # lowercase names to account for tidiers from `jtools`
      g_df <- dplyr::rename_all(glance_df, tolower)

      # preparing caption with model diagnostics
      caption <-
        substitute(
          atop(
            displaystyle(top.text),
            expr = paste("AIC = ", AIC, ", BIC = ", BIC)
          ),
          env = list(
            top.text = caption,
            AIC = specify_decimal_p(x = g_df$aic[[1]], k = k.caption.summary),
            BIC = specify_decimal_p(x = g_df$bic[[1]], k = k.caption.summary)
          )
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
    plot <-
      ggplot2::ggplot(data = tidy_df, mapping = ggplot2::aes(x = estimate, y = term))

    # if needed, adding the vertical line
    if (isTRUE(vline)) {
      # either at 1 - if coefficients are to be exponentiated - or at 0
      xintercept <- ifelse(exponentiate, 1, 0)

      # adding the line geom
      plot <- plot +
        rlang::exec(
          .fn = ggplot2::geom_vline,
          xintercept = xintercept,
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
      ggplot2::geom_point(
        color = point.color,
        size = point.size,
        shape = point.shape,
        na.rm = TRUE
      )

    # ========================= ggrepel labels ================================

    # adding the labels
    if (isTRUE(stats.labels)) {
      # removing all rows that have NAs anywhere in the columns of interest
      tidy_df %<>%
        dplyr::filter_at(
          .tbl = .,
          .vars = dplyr::vars(dplyr::matches("estimate|statistic|std.error|p.value")),
          .vars_predicate = dplyr::all_vars(!is.na(.))
        )

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
        as_tibble(x = paletteer::palettes_d_names) %>%
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
            direction = direction,
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
    "tidy" = tidy_df,
    "glance" = glance_df,
    "summary" = glance_df,
    "augment" = as_tibble(ipmisc::augment(x = x, ...)),
    "plot"
  ))
}
