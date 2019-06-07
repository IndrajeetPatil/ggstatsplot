#' @title Model coefficients for fitted models with the model summary as a
#'   caption.
#' @name ggcoefstats
#' @author Indrajeet Patil
#' @return Plot with the regression coefficients' point estimates as dots with
#'   confidence interval whiskers.
#'
#' @param x A model object to be tidied with `broom::tidy`, or a tidy data frame
#'   containing results. If a data frame is to be plotted, it *must* contain
#'   columns named `term` (names of predictors), or `estimate` (corresponding
#'   estimates of coefficients or other quantities of interest). Other optional
#'   columns are `conf.low` and `conf.high` (for confidence intervals);
#'   `p.value`. It is important that all `term` names should be unique.
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
#'   should be displayed in caption. Defaults to `FALSE`.
#' @param xlab Label for `x` axis variable (Default: `"estimate"`).
#' @param ylab Label for `y` axis variable (Default: `"term"`).
#' @param subtitle The text for the plot subtitle. The input to this argument
#'   will be ignored if `meta.analytic.effect` is set to `TRUE`.
#' @param conf.method Character describing method for computing confidence
#'   intervals (for more, see `?lme4::confint.merMod` and
#'   `?broom.mixed::tidy.brmsfit`). This argument has different defaults
#'   depending on the model object. For the `merMod` class model objects
#'   (`lmer`, `glmer`, `nlmer`, etc.), the default is `"Wald"` (other options
#'   are: `"profile"`, `"boot"`). For MCMC or brms fit model objects (Stan,
#'   JAGS, etc.), the default is `"quantile"`, while the only other options is
#'   `"HPDinterval"`.
#' @param p.kr Logical, if `TRUE`, the computation of *p*-values for `lmer` is
#'   based on conditional F-tests with Kenward-Roger approximation for the df.
#'   For details, see `?sjstats::p_value`.
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
#'   `0.95`). For MCMC model objects (Stan, JAGS, etc.), this will be
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
#' @param by.class A logical indicating whether or not to show performance
#'   measures broken down by class. Defaults to `FALSE`. When `by.class = FALSE`
#'   only returns a tibble with accuracy and kappa statistics. Mostly relevant
#'   for an object of class `"confusionMatrix"`.
#' @param se.type Character specifying the method used to compute standard
#'   standard errors for quantile regression (Default: `"nid"`). To see all
#'   available methods, see `quantreg::summary.rq()`.
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
#'   meta-analysis via linear (mixed-effects) models - as implemented in the
#'   `metafor` package - is to be displayed (default: `FALSE`). If `TRUE`, input
#'   to argument `subtitle` will be ignored. This will be mostly relevant if a
#'   data frame with estimates and their standard errors is entered as input to
#'   `x` argument.
#' @param k Number of decimal places expected for results displayed in labels
#'   (Default : `k = 2`).
#' @param k.caption.summary Number of decimal places expected for results
#'   displayed in captions (Default : `k.caption.summary = 0`).
#' @param exclude.intercept Logical that decides whether the intercept should be
#'   excluded from the plot (Default: `TRUE`).
#' @param exponentiate If `TRUE`, the `x`-axis will be logarithmic (Default:
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
#' @param sort If `"none"` (default) do not sort, `"ascending"` sort by
#'   increasing coefficient value, or `"descending"` sort by decreasing
#'   coefficient value.
#' @param stats.labels Logical. Decides whether the statistic and p-values for
#'   each coefficient are to be attached to each dot as a text label using
#'   `ggrepel` (Default: `TRUE`).
#' @param caption.summary Logical. Decides whether the model summary should be
#'   displayed as a cation to the plot (Default: `TRUE`). Color of the line
#'   segment. Defaults to the same color as the text.
#' @param stats.label.size,stats.label.fontface,stats.label.color Aesthetics for
#'   the labels. Defaults: `3`, `"bold"`,`NULL`, resp. If `stats.label.color` is
#'   `NULL`, colors will be chosen from the specified `package` (Default:
#'   `"RColorBrewer"`) and `palette` (Default: `"Dark2"`).
#' @param label.r, Radius of rounded corners, as unit or number. Defaults to
#'   `0.15`. (Default unit is lines).
#' @param label.size Size of label border, in mm. Defaults to `0.25`.
#' @param label.box.padding	 Amount of padding around bounding box, as number.
#'   Defaults to `1`. (Default unit is lines).
#' @param label.label.padding	 Amount of padding around label, as number.
#'   Defaults to `0.25`. (Default unit is lines).
#' @param label.point.padding	 Amount of padding around labeled point, as
#'   number. Defaults to `0`. (Default unit is lines).
#' @param label.segment.color Color of the line segment (Default: `"grey50"`).
#' @param label.segment.size Width of line segment connecting the data point to
#'   the text label, in mm. Defaults to `0.5`.
#' @param label.segment.alpha Transparency of the line segment. Defaults to the
#'   same transparency as the text.
#' @param label.min.segment.length Skip drawing segments shorter than this.
#'   Defaults to `0.5`. (Default unit is lines).
#' @param label.force Force of repulsion between overlapping text labels.
#'   Defaults to `1`.
#' @param label.max.iter Maximum number of iterations to try to resolve
#'   overlaps. Defaults to `2000`.
#' @param label.nudge.x,label.nudge.y Horizontal and vertical adjustments to
#'   nudge the starting position of each text label. Defaults to `0`.
#' @param label.xlim,label.ylim Limits for the x and y axes. Text labels will be
#'   constrained to these limits. By default, text labels are constrained to the
#'   entire plot area. Defaults to `c(NA, NA)`.
#' @param label.direction Character (`"both"`, `"x"`, or `"y"`) -- direction in
#'   which to adjust position of labels (Default: `"y"`).
#' @param ... Additional arguments to tidying method.
#' @inheritParams bf_meta_message
#' @inheritParams broom.mixed::tidy.merMod
#' @inheritParams broom::tidy.clm
#' @inheritParams broom::tidy.polr
#' @inheritParams broom::tidy.mjoint
#' @inheritParams theme_ggstatsplot
#' @inheritParams paletteer::paletteer_d
#' @inheritParams subtitle_meta_ggcoefstats
#' @inheritParams ggbetweenstats
#'
#' @import ggplot2
#' @importFrom broomExtra tidy glance augment
#' @importFrom dplyr select bind_rows summarize mutate mutate_at mutate_if n
#' @importFrom dplyr group_by arrange full_join vars matches desc everything
#' @importFrom dplyr vars all_vars filter_at starts_with
#' @importFrom purrrlyr by_row
#' @importFrom stats as.formula lm confint qnorm
#' @importFrom ggrepel geom_label_repel
#' @importFrom grid unit
#' @importFrom sjstats p_value
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom tidyr unite
#' @importFrom groupedstats lm_effsize_standardizer
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcoefstats.html}
#'
#' @examples
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
#' \dontrun{
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
#'   bf.message = TRUE,
#'   k = 3
#' )
#' }
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
#' @export

# function body
ggcoefstats <- function(x,
                        output = "plot",
                        statistic = NULL,
                        scales = NULL,
                        conf.method = "Wald",
                        conf.type = "Wald",
                        component = "survival",
                        bf.message = FALSE,
                        d = "norm",
                        d.par = c(0, 0.3),
                        tau = "halfcauchy",
                        tau.par = 0.5,
                        sample = 10000,
                        summarize = "integrate",
                        p.kr = TRUE,
                        p.adjust.method = "none",
                        coefficient.type = c("beta", "location", "coefficient"),
                        by.class = FALSE,
                        effsize = "eta",
                        partial = TRUE,
                        nboot = 500,
                        meta.analytic.effect = FALSE,
                        point.color = "blue",
                        point.size = 3,
                        point.shape = 16,
                        conf.int = TRUE,
                        conf.level = 0.95,
                        se.type = "nid",
                        k = 2,
                        k.caption.summary = 0,
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
                        xlab = "regression coefficient",
                        ylab = "term",
                        title = NULL,
                        subtitle = NULL,
                        stats.labels = TRUE,
                        caption = NULL,
                        caption.summary = TRUE,
                        stats.label.size = 3,
                        stats.label.fontface = "bold",
                        stats.label.color = NULL,
                        label.r = 0.15,
                        label.size = 0.25,
                        label.box.padding = 1,
                        label.label.padding = 0.25,
                        label.point.padding = 0.5,
                        label.segment.color = "grey50",
                        label.segment.size = 0.5,
                        label.segment.alpha = NULL,
                        label.min.segment.length = 0.5,
                        label.force = 1,
                        label.max.iter = 2000,
                        label.nudge.x = 0,
                        label.nudge.y = 0,
                        label.xlim = c(NA, NA),
                        label.ylim = c(NA, NA),
                        label.direction = "y",
                        package = "RColorBrewer",
                        palette = "Dark2",
                        direction = 1,
                        ggtheme = ggplot2::theme_bw(),
                        ggstatsplot.layer = TRUE,
                        messages = FALSE,
                        ...) {

  # =================== list of objects (for tidy and glance) ================

  # dataframe objects
  df.mods <- c(
    "data.frame",
    "grouped_df",
    "tbl",
    "tbl_df",
    "spec_tbl_df"
  )

  # creating a list of objects which will have fixed and random "effects"
  # only fixed effects will be selected
  mixed.mods <-
    c(
      "brmsfit",
      "gamlss",
      "glmmadmb",
      "glmerMod",
      "glmmTMB",
      "gls",
      "lme",
      "lmerMod",
      "mcmc",
      "MCMCglmm",
      "merMod",
      "nlmerMod",
      "rjags",
      "rlmerMod",
      "stanfit",
      "stanreg",
      "TMB"
    )

  # models which are currently not supported
  unsupported.mods <-
    c(
      "acf",
      "AUC",
      "cv.glmnet",
      "density",
      "dist",
      "durbinWatsonTest",
      "elnet",
      "emmGrid",
      "ftable",
      "glht",
      "glmnet",
      "kde",
      "Kendall",
      "kmeans",
      "list",
      "map",
      "Mclust",
      "mts",
      "muhaz",
      "optim",
      "pam",
      "poLCA",
      "power.htest",
      "prcomp",
      "spec",
      "survdiff",
      "survexp",
      "survfit",
      "ts",
      "zoo"
    )

  # objects for which p-value needs to be computed using `sjstats` package
  p.mods <- c(
    "lmerMod",
    "nlmerMod",
    "polr",
    "rlm",
    "svyolr"
  )

  # =================== types of models =====================================

  # bayesian models (default `conf.method` won't work for these)
  bayes.mods <- c(
    "brmsfit",
    "mcmc",
    "MCMCglmm",
    "rjags",
    "stanreg"
  )

  # models for which statistic is F-value
  f.mods <- c(
    "aov",
    "aovlist",
    "anova",
    "Gam",
    "manova"
  )

  # changing conf.method to something suitable for Bayesian models
  if (class(x)[[1]] %in% bayes.mods && conf.method == "Wald") {
    conf.method <- "quantile"
  }

  # =========================== checking if object is supported ==============

  # glace is not supported for all models
  if (class(x)[[1]] %in% unsupported.mods) {
    stop(message(cat(
      crayon::red("Note: "),
      crayon::blue(
        "The object of class",
        crayon::yellow(class(x)[[1]]),
        "aren't currently supported.\n"
      ),
      sep = ""
    )),
    call. = FALSE
    )
  }

  # ============================= model and its summary ======================

  # creating glance dataframe
  glance_df <- broomExtra::glance(x)

  # if the object is not a dataframe, check if summary caption is to be displayed
  if (!class(x)[[1]] %in% df.mods) {
    # if glance is not available, inform the user
    if (is.null(glance_df) && output == "plot") {
      message(cat(
        crayon::green("Note: "),
        crayon::blue(
          "No model diagnostics information available for the object of class",
          crayon::yellow(class(x)[[1]]),
          ".\n"
        ),
        sep = ""
      ))

      # and skip the caption
      caption.summary <- FALSE
    } else {
      # if glance is not null, but the needed metric are not available, skip caption
      if (!all(c("logLik", "AIC", "BIC") %in% names(glance_df))) {
        caption.summary <- FALSE
      }
    }
  }

  # ============================= dataframe ===============================

  if (class(x)[[1]] %in% df.mods) {
    # set tidy_df to entered dataframe
    tidy_df <- tibble::as_tibble(x)

    # check for the two necessary columns
    if (!"estimate" %in% names(tidy_df)) {
      stop(message(cat(
        crayon::red("Error: "),
        crayon::blue(
          "The object of class",
          crayon::yellow(class(x)[[1]]),
          "*must* contain the following column: 'estimate'.\n"
        ),
        sep = ""
      )),
      call. = FALSE
      )
    }

    # create a new term column if it's not present
    if (!"term" %in% names(tidy_df)) {
      tidy_df %<>%
        dplyr::mutate(.data = ., term = 1:nrow(.)) %>%
        dplyr::mutate(.data = ., term = as.character(term))
    }

    # check that statistic is specified
    if (purrr::is_null(statistic)) {
      message(cat(
        crayon::red("Note"),
        crayon::blue(
          ": For the object of class",
          crayon::yellow(class(x)[[1]]),
          ", the argument `statistic` is not specified ('t', 'z', or 'f').\n",
          "Statistical labels will therefore be skipped.\n"
        ),
        sep = ""
      ))

      # skip labels
      stats.labels <- FALSE
    }

    # =========================== broom.mixed tidiers =======================
  } else if (class(x)[[1]] %in% mixed.mods) {

    # getting tidy output using `broom.mixed`
    tidy_df <-
      broomExtra::tidy(
        x = x,
        conf.int = TRUE,
        # exponentiate = exponentiate,
        conf.level = conf.level,
        effects = "fixed",
        scales = scales,
        conf.method = conf.method,
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

    # renaming the `xlab` according to the estimate chosen
    if (effsize == "eta") {
      if (isTRUE(partial)) {
        xlab <- "partial eta-squared"
      } else {
        xlab <- "eta-squared"
      }
    } else if (effsize == "omega") {
      if (isTRUE(partial)) {
        xlab <- "partial omega-squared"
      } else {
        xlab <- "omega-squared"
      }
    }
    # ============ tidying robust models =====================================
  } else if (class(x)[[1]] %in% c("lmRob", "glmRob")) {
    tidy_df <-
      broomExtra::tidy(
        x = x,
        ...
      )
    # ==================== tidying everything else ===========================
  } else {
    tidy_df <-
      broomExtra::tidy(
        x = x,
        conf.int = TRUE,
        conf.level = conf.level,
        se.type = se.type,
        by_class = by.class,
        conf.type = conf.type,
        component = component,
        # exponentiate = exponentiate,
        parametric = TRUE,
        ...
      )
  }

  # =================== tidy dataframe cleanup ================================

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
  # create a new column by collapsing orignal `variable` and `term` columns
  if (class(x)[[1]] %in% c("gmm", "lmodel2", "gamlss", "drc", "mlm")) {
    tidy_df %<>%
      tidyr::unite(
        data = .,
        col = "term",
        dplyr::matches("term|variable|parameter|method|curveid|response"),
        remove = TRUE,
        sep = "_"
      )
  }

  # halt if there are repeated terms
  if (any(duplicated(dplyr::select(tidy_df, term)))) {
    message(cat(
      crayon::red("Error: "),
      crayon::blue(
        "All elements in the column `term` should be unique.\n"
      ),
      sep = ""
    ))
    return(invisible(tidy_df))
  }

  # =================== p-value computation ==================================

  # p-values won't be computed by default for the lmer models
  if (class(x)[[1]] %in% p.mods) {
    # computing p-values
    tidy_df %<>%
      dplyr::full_join(
        x = dplyr::mutate_at(
          .tbl = .,
          .vars = "term",
          .funs = ~ as.character(x = .)
        ),
        y = sjstats::p_value(fit = x, p.kr = p.kr) %>%
          dplyr::select(.data = ., -std.error) %>%
          dplyr::mutate_at(
            .tbl = .,
            .vars = "term",
            .funs = ~ as.character(x = .)
          ),
        by = "term"
      ) %>%
      dplyr::filter(.data = ., !is.na(estimate)) %>%
      tibble::as_tibble(x = .)
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
        crayon::green("Note: "),
        crayon::blue(
          "No p-values and/or statistic available for regression coefficients from",
          crayon::yellow(class(x)[[1]]),
          "object; \nskipping labels with stats.\n"
        ),
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
        dplyr::mutate(
          .data = .,
          conf.low = NA_character_,
          conf.high = NA_character_
        )

      # stop displaying whiskers
      conf.int <- FALSE

      # inform the user that skipping labels for the same reason
      message(cat(
        crayon::green("Note: "),
        crayon::blue(
          "No confidence intervals available for regression coefficients from",
          crayon::yellow(class(x)[[1]]),
          "object, so skipping whiskers in the plot.\n"
        ),
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
        .data = .,
        !grepl(
          pattern = "(Intercept)",
          x = term,
          ignore.case = TRUE
        )
      )
  }

  # if the coefficients are to be exponentiated, the label positions will also
  # have to be adjusted
  if (isTRUE(exponentiate)) {
    tidy_df %<>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = dplyr::vars(dplyr::matches(
          match = "estimate|conf", ignore.case = TRUE
        )),
        .funs = ~ exp(x = .)
      )
  }

  # ========================== p-value adjustment ===========================

  # clean up the p-value column
  if ("p.value" %in% names(tidy_df)) {
    # if p-value column is not numeric
    if (!purrr::is_bare_numeric(tidy_df$p.value)) {
      tidy_df %<>%
        dplyr::mutate(.data = ., p.value = as.numeric(as.character(p.value)))
    }

    # adjust the p-values based on the adjustment used
    tidy_df %<>%
      dplyr::mutate(
        .data = .,
        p.value = stats::p.adjust(p = p.value, method = p.adjust.method)
      )
  }

  # ========================== preparing label ================================

  # adding a column with labels to be used with `ggrepel`
  if (isTRUE(stats.labels)) {
    if (class(x)[[1]] %in% df.mods) {
      # in case a dataframe was entered, `x` and `tidy_df` are going to be same
      x <- tidy_df
    }

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
    if (dim(dplyr::filter(.data = tidy_df, is.na(std.error)))[[1]] > 0) {
      # inform the user that skipping labels for the same reason
      message(cat(
        crayon::red("Error: "),
        crayon::blue(
          "At least one of the values in the `std.error` column is NA.\n",
          "No meta-analysis will be carried out.\n"
        ),
        sep = ""
      ))

      # turn off meta-analysis
      meta.analytic.effect <- FALSE
    }
  }

  # running meta-analysis
  if (isTRUE(meta.analytic.effect)) {
    # result
    subtitle <-
      subtitle_meta_ggcoefstats(
        data = tidy_df,
        k = k,
        messages = messages,
        output = "subtitle"
      )

    # add Bayes factor caption
    if (isTRUE(bf.message)) {
      caption <-
        bf_meta_message(
          caption = caption,
          data = tidy_df,
          k = k,
          messages = messages,
          d = d,
          d.par = d.par,
          tau = tau,
          tau.par = tau.par,
          sample = sample,
          summarize = summarize
        )
    }

    # model summary
    caption.meta <-
      subtitle_meta_ggcoefstats(
        data = tidy_df,
        k = k,
        caption = caption,
        messages = FALSE,
        output = "caption"
      )
  }

  # ========================== summary caption ================================

  # caption containing model diagnostics
  if (isTRUE(caption.summary)) {
    # for dataframe objects
    if (class(x)[[1]] %in% df.mods && isTRUE(meta.analytic.effect)) {
      caption <- caption.meta
    }

    # for non-dataframe objects
    if (!class(x)[[1]] %in% df.mods) {
      if (!is.na(glance_df$AIC[[1]])) {
        # preparing caption with model diagnostics
        caption <-
          substitute(
            atop(displaystyle(top.text),
              expr =
                paste(
                  "AIC = ",
                  AIC,
                  ", BIC = ",
                  BIC,
                  ", log-likelihood = ",
                  LL
                )
            ),
            env = list(
              top.text = caption,
              AIC = specify_decimal_p(x = glance_df$AIC[[1]], k = k.caption.summary),
              BIC = specify_decimal_p(x = glance_df$BIC[[1]], k = k.caption.summary),
              LL = specify_decimal_p(x = glance_df$logLik[[1]], k = k.caption.summary)
            )
          )
      }
    }
  }

  # ========================== sorting ===================================

  # whether the term need to be arranged in any specified order
  tidy_df$term <- as.factor(tidy_df$term)
  tidy_df %<>% tibble::rownames_to_column(.data = ., var = "rowid")

  # sorting factor levels
  if (sort != "none") {
    if (sort == "ascending") {
      new_order <- order(tidy_df$estimate, decreasing = FALSE)
    } else {
      new_order <- order(tidy_df$estimate, decreasing = TRUE)
    }
  } else {
    new_order <- order(tidy_df$rowid, decreasing = FALSE)
  }

  # sorting `term` factor levels according to new sorting order
  tidy_df$term <- as.character(tidy_df$term)
  tidy_df$term <- factor(x = tidy_df$term, levels = tidy_df$term[new_order])
  tidy_df %<>% dplyr::select(.data = ., -rowid)

  # palette check is necessary only if output is a plot
  if (output == "plot") {

    # ========================== basic plot ===================================

    # setting up the basic architecture
    plot <-
      ggplot2::ggplot(
        data = tidy_df,
        mapping = ggplot2::aes(x = estimate, y = factor(term))
      )

    # if needed, adding the vertical line
    if (isTRUE(vline)) {
      # either at 1 - if coefficients are to be exponentiated - or at 0
      if (isTRUE(exponentiate)) {
        xintercept <- 1
      } else {
        xintercept <- 0
      }

      # adding the line geom
      plot <- plot +
        ggplot2::geom_vline(
          xintercept = xintercept,
          color = vline.color,
          linetype = vline.linetype,
          size = vline.size,
          na.rm = TRUE
        )

      # logarithmic scale for exponent of coefficients
      if (isTRUE(exponentiate)) {
        plot <- plot +
          ggplot2::scale_x_log10()
      }
    }

    # if the confidence intervals are to be displayed on the plot
    if (isTRUE(conf.int)) {
      plot <- plot +
        ggplot2::geom_errorbarh(
          ggplot2::aes_string(xmin = "conf.low", xmax = "conf.high"),
          color = errorbar.color,
          height = errorbar.height,
          linetype = errorbar.linetype,
          size = errorbar.size,
          na.rm = TRUE
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

      # ========================== palette check =================================

      # counting the number of terms in the tidy dataframe
      count_term <- length(tidy_df$term)

      # if no. of factor levels is greater than the default palette color count
      palette_message(
        package = package,
        palette = palette,
        min_length = count_term
      )

      # computing the number of colors in a given palette
      palette_df <-
        tibble::as_tibble(x = paletteer::palettes_d_names) %>%
        dplyr::filter(.data = ., package == !!package, palette == !!palette) %>%
        dplyr::select(.data = ., length)

      # if insufficient number of colors are available in a given palette
      if (palette_df$length[[1]] < count_term) {
        stats.label.color <- "black"
      }

      # if user has not specified colors, then use a color palette
      if (is.null(stats.label.color)) {
        stats.label.color <-
          paletteer::paletteer_d(
            package = !!package,
            palette = !!palette,
            n = count_term,
            direction = direction,
            type = "discrete"
          )
      }

      # adding labels
      plot <- plot +
        ggrepel::geom_label_repel(
          data = tidy_df,
          mapping = ggplot2::aes(x = estimate, y = term, label = label),
          size = stats.label.size,
          fontface = stats.label.fontface,
          color = stats.label.color,
          box.padding = grid::unit(x = label.box.padding, units = "lines"),
          label.padding = grid::unit(x = label.label.padding, units = "lines"),
          point.padding = grid::unit(x = label.point.padding, units = "lines"),
          label.r = grid::unit(x = label.r, units = "lines"),
          label.size = label.size,
          segment.color = label.segment.color,
          segment.size = label.segment.size,
          segment.alpha = label.segment.alpha,
          min.segment.length = label.min.segment.length,
          force = label.force,
          max.iter = label.max.iter,
          nudge_x = label.nudge.x,
          nudge_y = label.nudge.y,
          xlim = label.xlim,
          ylim = label.ylim,
          na.rm = TRUE,
          show.legend = FALSE,
          direction = label.direction,
          parse = TRUE,
          seed = 123
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
      ggstatsplot::theme_mprl(
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
    "dataframe" = tidy_df,
    "df" = tidy_df,
    "glance" = glance_df,
    "summary" = glance_df,
    "augment" = tibble::as_tibble(broomExtra::augment(x = x, ...)),
    "plot"
  ))
}
