#' @title Model coefficients for fitted models with the model summary as a
#'   caption.
#' @name ggcoefstats
#' @aliases ggcoefstats
#' @author Indrajeet Patil
#' @return Plot with the regression coefficients' point estimates as dots with
#'   confidence interval whiskers.
#'
#' @param x A model object to be tidied with `broom::tidy`.
#' @param output Character describing the expected output from this function:
#'   `"plot"` (visualization of regression coefficients) or `"tidy"` (tidy
#'   dataframe of results from `broom::tidy`) or `"glance"` (object from
#'   `broom::glance`) or `"augment"` (object from `broom::augment`).
#' @param xlab Label for `x` axis variable (Default: `"estimate"`).
#' @param ylab Label for `y` axis variable (Default: `"term"`).
#' @param title The text for the plot title.
#' @param subtitle The text for the plot subtitle.
#' @param conf.method Character describing method for computing confidence
#'   intervals (for more, see `lme4::confint.merMod`).
#' @param p.kr Logical, if `TRUE`, the computation of p-values for `lmer` is
#'   based on conditional F-tests with Kenward-Roger approximation for the df.
#'   For details, see `?sjstats::p_value`.
#' @param point.color Character describing color for the point (Default:
#'   `"blue"`).
#' @param point.size Numeric specifying size for the point (Default: `3`).
#' @param point.shape Numeric specifying shape to draw the points (Default: `16`
#'   (**a dot**)).
#' @param conf.int Logical. Decides whether to display confidence intervals as
#'   error bars (Default: `TRUE`).
#' @param conf.level Numeric deciding level of confidence intervals (Default:
#'   `0.95`).
#' @param coefficient.type For ordinal regression models, which parameters to
#'   display in the plot. By default only `"beta"` (a vector of regression
#'   parameters) parameters will be show. Other options are `"alpha"` (a vector
#'   of threshold parameters) or `"both"`.
#' @param se.type Character specifying the method used to compute standard
#'   standard errors for quantile regression (Default: `"nid"`). To see all
#'   available methods, see `quantreg::summary.rq()`.
#' @param k Number of decimal places expected for results displayed in labels.
#' @param k.caption.summary Number of decimal places expected for results
#'   displayed in captions.
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
#' @param sort If `"none"` (default) do not sort, `"ascending"` sort by increasing
#'   coefficient value, or `"descending"` sort by decreasing coefficient value.
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
#' @inheritParams lm_effsize_ci
#' @inheritParams broom::tidy.merMod
#' @inheritParams broom::tidy.clm
#' @inheritParams theme_ggstatsplot
#' @inheritParams paletteer::paletteer_d
#' @param \dots Extra arguments to pass to \code{\link[broom]{tidy}}.
#'
#' @import ggplot2
#'
#' @importFrom broom glance
#' @importFrom broom tidy
#' @importFrom broom augment
#' @importFrom broom.mixed glance
#' @importFrom broom.mixed tidy
#' @importFrom broom.mixed augment
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate_at
#' @importFrom dplyr full_join
#' @importFrom dplyr everything
#' @importFrom dplyr desc
#' @importFrom dplyr vars
#' @importFrom dplyr matches
#' @importFrom purrrlyr by_row
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @importFrom ggrepel geom_label_repel
#' @importFrom grid unit
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom sjstats p_value
#' @importFrom tibble rownames_to_column
#' @importFrom tibble as_data_frame
#'
#' @references
#' \url{https://cran.r-project.org/package=ggstatsplot/vignettes/ggcoefstats.html}
#'
#' @examples
#'
#' set.seed(123)
#' ggcoefstats(x = lm(formula = mpg ~ cyl * am, data = mtcars))
#' @export
#'

# function body
ggcoefstats <- function(x,
                        output = "plot",
                        scales = NULL,
                        conf.method = "Wald",
                        p.kr = TRUE,
                        coefficient.type = "beta",
                        effsize = "eta",
                        nboot = 1000,
                        point.color = "blue",
                        point.size = 3,
                        point.shape = 16,
                        conf.int = TRUE,
                        conf.level = 0.95,
                        se.type = "nid",
                        k = 3,
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
                        ...) {
  # ================================== list of objects (for tidy and glance) ==========================================================

  # creating a list of objects which will have fixed and random "effects"
  # only fixed effects will be selected
  lmm.mods <- c("lmerMod", "glmerMod", "nlmerMod")

  # models which are currently not supported
  unsupported.mods <- c("glht", "kmeans")

  # models for which glance is not supported
  noglance.mods <- c("aovlist")

  # models for which the diagnostics is not available (AIC, BIC, loglik)
  nodiagnostics.mods <- c("lmRob", "glmRob")

  # ================================== list of objects (for statistic) ================================================================

  # models for which statistic is t-value
  t.mods <- c("lmerMod", "lm", "nls", "lmRob", "rq", "rlm")

  # models for which statistic is z-value
  z.mods <- c("clm", "clmm")

  # models for which statistic is F-value
  f.mods <- c("aov", "aovlist")

  # models for which there is no clear t-or z-statistic
  # which statistic to use will be decided based on the family used
  g.mods <- c("glm", "glmerMod", "glmRob")

  # ====================================== checking if object is supported =============================================================
  # glace is not supported for all models
  if (class(x)[[1]] %in% unsupported.mods) {
    base::stop(base::message(cat(
      crayon::green("Note:"),
      crayon::blue(
        "The object of class",
        crayon::yellow(class(x)[[1]]),
        "aren't currently supported."
      )
    )))
  }
  # ================================================== model and its summary ===========================================================

  # glance object from broom
  if (!(class(x)[[1]] %in% noglance.mods)) {
    if (class(x)[[1]] %in% lmm.mods) {
      glance_df <- broom.mixed::glance(x = x) %>%
        tibble::as_data_frame(x = .)
    } else {
      glance_df <- broom::glance(x = x) %>%
        tibble::as_data_frame(x = .)
    }
  } else {
    base::message(cat(
      crayon::green("Note:"),
      crayon::blue(
        "No model diagnostics information available for the object of class",
        crayon::yellow(class(x)[[1]]),
        ". Future release might support this."
      )
    ))
  }

  # tidy dataframe of results from the model
  # ===================================== lmm tidying =======================================================================
  if (class(x)[[1]] %in% lmm.mods) {
    tidy_df <-
      broom.mixed::tidy(
        x = x,
        conf.int = TRUE,
        conf.level = conf.level,
        effects = "fixed",
        scales = scales,
        conf.method = conf.method,
        ...
      )
  } else if (class(x)[[1]] %in% f.mods) {

    # ===================================== aov tidying =======================================================================
    #
    tidy_df <- lm_effsize_ci(
      object = x,
      effsize = effsize,
      partial = TRUE,
      conf.level = conf.level,
      nboot = nboot
    ) %>%
      dplyr::rename(.data = ., statistic = F.value)

    # renaming the effect size to estimate
    if (effsize == "eta") {
      tidy_df %<>%
        dplyr::rename(.data = ., estimate = partial.etasq)
      xlab <- "partial eta-squared"
    } else if (effsize == "omega") {
      tidy_df %<>%
        dplyr::rename(.data = ., estimate = partial.omegasq)
      xlab <- "partial omega-squared"
    }
    # ===================================== clm and clmm tidying =======================================================================
  } else if (class(x)[[1]] == "clm" || class(x)[[1]] == "clmm") {
    tidy_df <-
      broom::tidy(
        x = x,
        conf.int = TRUE,
        conf.level = conf.level,
        quick = FALSE,
        conf.type = "Wald",
        ...
      )

    # selecting which coeffiecients to display
    if (coefficient.type == "alpha") {
      tidy_df %<>%
        dplyr::filter(.data = ., coefficient_type == "alpha")
    } else if (coefficient.type == "beta") {
      tidy_df %<>%
        dplyr::filter(.data = ., coefficient_type == "beta")
    }

    # ===================================== tidying robust models =======================================================================
  } else if (class(x)[[1]] == "lmRob" || class(x)[[1]] == "glmRob") {
    tidy_df <-
      broom::tidy(
        x = x,
        ...
      )
    # ===================================== quantile regression ==========================================================================
  } else if (class(x)[[1]] == "rq" || class(x)[[1]] == "rqs") {
    tidy_df <-
      broom::tidy(
        x = x,
        conf.int = TRUE,
        conf.level = conf.level,
        se.type = se.type,
        ...
      )
    # ===================================== tidying everything else =======================================================================
  } else {
    tidy_df <-
      broom::tidy(
        x = x,
        conf.int = TRUE,
        conf.level = conf.level,
        ...
      )
  }

  # ===================================== p-value computation =======================================================================
  #
  # p-values won't be computed by default for the lmer models
  if (class(x)[[1]] == "lmerMod" || class(x)[[1]] == "rlm") {
    # computing p-values
    tidy_df %<>%
      tibble::as_data_frame(x = .) %>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "term",
        .funs = ~as.character(x = .)
      ) %>%
      dplyr::full_join(
        x = .,
        y = sjstats::p_value(fit = x, p.kr = p.kr) %>%
          tibble::as_data_frame(x = .) %>%
          dplyr::select(.data = ., -std.error) %>%
          dplyr::mutate_at(
            .tbl = .,
            .vars = "term",
            .funs = ~as.character(x = .)
          ),
        by = "term"
      )
  }

  # ===================================== p-value and CI check =======================================================================
  #
  # if broom output doesn't contain p-value
  if (!"p.value" %in% names(tidy_df)) {
    # skip the labels
    stats.labels <- FALSE

    # inform the user that skipping labels for the same reason
    base::message(cat(
      crayon::green("Note:"),
      crayon::blue(
        "No p-values available for regression coefficients from",
        crayon::yellow(class(x)[[1]]),
        "object, so skipping labels."
      )
    ))
  }

  # if broom output doesn't contain CI
  if (!"conf.low" %in% names(tidy_df)) {
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
    base::message(cat(
      crayon::green("Note:"),
      crayon::blue(
        "No 95% confidence intervals available for regression coefficients from",
        crayon::yellow(class(x)[[1]]),
        "object, so skipping whiskers in the plot."
      )
    ))
  }

  # =============================  intercept, exponentiation, and final tidy dataframe ================================================

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

  # whether to show model intercept; if not, remove the corresponding terms from the dataframe
  if (isTRUE(exclude.intercept)) {
    tidy_df %<>%
      dplyr::filter(
        .data = .,
        !base::grepl(
          pattern = "(Intercept)",
          x = term,
          ignore.case = TRUE
        )
      )
  }

  # if the coefficients are to be exponentiated, the label positions will also have to be adjusted
  if (isTRUE(exponentiate)) {
    # tidy_df$estimate <- base::exp(tidy_df$estimate)
    tidy_df %<>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = dplyr::vars(dplyr::matches(
          match = "estimate|conf", ignore.case = TRUE
        )),
        .funs = ~base::exp(x = .)
      )
  }

  # ========================================================= p-value formatting =========================================================
  #
  # formatting the numbers for display and preparing labels
  if (isTRUE(stats.labels)) {
    tidy_df %<>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "statistic",
        .funs = ~ggstatsplot::specify_decimal_p(x = ., k = k)
      ) %>%
      signif_column(data = ., p = p.value) %>%
      purrrlyr::by_row(
        .d = .,
        ..f = ~ggstatsplot::specify_decimal_p(
          x = .$p.value,
          k = k,
          p.value = TRUE
        ),
        .collate = "rows",
        .to = "p.value.formatted",
        .labels = TRUE
      ) %>%
      dplyr::mutate(
        .data = .,
        p.value.formatted2 = dplyr::case_when(
          p.value.formatted == "< 0.001" ~ "<= 0.001",
          p.value.formatted != "< 0.001" ~ paste("==", p.value.formatted, sep = "")
        )
      )

    # ========================================================= t-statistic labels =========================================================
    if (class(x)[[1]] %in% t.mods) {
      tidy_df %<>%
        tz_labeller(
          tidy_df = .,
          glance_df = glance_df,
          statistic = "t",
          k = k
        )
      # ========================================================= z-statistic labels =========================================================
    } else if (class(x)[[1]] %in% z.mods) {
      tidy_df %<>%
        tz_labeller(
          tidy_df = .,
          glance_df = glance_df,
          statistic = "z",
          k = k
        )

      # ========================================================= t/z-statistic labels =========================================================
    } else if (class(x)[[1]] %in% g.mods) {
      if (class(x)[[1]] == "glm") {
        if (summary(x)$family$family[[1]] %in% c(
          "quasi",
          "gaussian",
          "quasibinomial",
          "quasipoisson",
          "Gamma",
          "inverse.gaussian"
        )) {
          tidy_df %<>%
            tz_labeller(
              tidy_df = .,
              glance_df = glance_df,
              statistic = "t",
              k = k
            )
        } else if (summary(x)$family$family[[1]] %in% c("binomial", "poisson")) {
          tidy_df %<>%
            tz_labeller(
              tidy_df = .,
              glance_df = glance_df,
              statistic = "z",
              k = k
            )
        }
      } else if (class(x)[[1]] == "glmerMod") {
        if (summary(x)$family[[1]] %in% c(
          "quasi",
          "gaussian",
          "quasibinomial",
          "quasipoisson",
          "Gamma",
          "inverse.gaussian"
        )) {
          tidy_df %<>%
            tz_labeller(
              tidy_df = .,
              glance_df = glance_df,
              statistic = "t",
              k = k
            )
        } else if (summary(x)$family[[1]] %in% c("binomial", "poisson")) {
          tidy_df %<>%
            tz_labeller(
              tidy_df = .,
              glance_df = glance_df,
              statistic = "z",
              k = k
            )
        }
      } else if (class(x)[[1]] == "glmRob") {
        if (x$family[[1]] %in% c(
          "quasi",
          "gaussian",
          "quasibinomial",
          "quasipoisson",
          "Gamma",
          "inverse.gaussian"
        )) {
          tidy_df %<>%
            tz_labeller(
              tidy_df = .,
              glance_df = glance_df,
              statistic = "t",
              k = k
            )
        } else if (x$family[[1]] %in% c("binomial", "poisson")) {
          tidy_df %<>%
            tz_labeller(
              tidy_df = .,
              glance_df = glance_df,
              statistic = "z",
              k = k
            )
        }
      }
      # ========================================================= F-statistic =========================================================
    } else if (class(x)[[1]] %in% f.mods) {
      # which effect size is needed?
      if (effsize == "eta") {
        tidy_df %<>%
          purrrlyr::by_row(
            .d = .,
            ..f = ~paste(
              "list(~italic(F)",
              "(",
              .$df1,
              ",",
              .$df2,
              ")==",
              .$statistic,
              ", ~italic(p)",
              .$p.value.formatted2,
              ", ~italic(eta)[p]^2==",
              ggstatsplot::specify_decimal_p(x = .$estimate, k = k),
              ")",
              sep = ""
            ),
            .collate = "rows",
            .to = "label",
            .labels = TRUE
          )
      } else if (effsize == "omega") {
        tidy_df %<>%
          purrrlyr::by_row(
            .d = .,
            ..f = ~paste(
              "list(~italic(F)",
              "(",
              .$df1,
              ",",
              .$df2,
              ")==",
              .$statistic,
              ", ~italic(p)",
              .$p.value.formatted2,
              ", ~italic(omega)[p]^2==",
              ggstatsplot::specify_decimal_p(x = .$estimate, k = k),
              ")",
              sep = ""
            ),
            .collate = "rows",
            .to = "label",
            .labels = TRUE
          )
      }
    }
  }

  # ================================================== summary caption ===========================================================

  # caption containing model diagnostics
  if (isTRUE(caption.summary)) {
    if (!(class(x)[[1]] %in% noglance.mods) && !(class(x)[[1]] %in% nodiagnostics.mods)) {
      if (!is.na(glance_df$AIC[[1]])) {
        # preparing caption with model diagnostics
        caption <-
          base::substitute(
            expr =
              paste(
                "AIC = ",
                AIC,
                ", BIC = ",
                BIC,
                ", log-likelihood = ",
                loglik
              ),
            env = base::list(
              AIC = ggstatsplot::specify_decimal_p(x = glance_df$AIC[[1]], k = k.caption.summary),
              BIC = ggstatsplot::specify_decimal_p(x = glance_df$BIC[[1]], k = k.caption.summary),
              loglik = ggstatsplot::specify_decimal_p(x = glance_df$logLik[[1]], k = k.caption.summary)
            )
          )
      } else {
        caption <- NULL
      }
    } else {
      caption <- NULL
    }
  } else {
    caption <- NULL
  }
  # ================================================== basic plot ===========================================================

  # whether the term need to be arranged in any specified order
  if (sort != "none") {
    tidy_df$term <- base::as.factor(tidy_df$term)
    if (sort == "ascending") {
      new_order <- base::order(tidy_df$estimate, decreasing = FALSE)
    } else {
      new_order <- base::order(tidy_df$estimate, decreasing = TRUE)
    }
    tidy_df$term <- as.character(tidy_df$term)
    tidy_df$term <-
      base::factor(x = tidy_df$term, levels = tidy_df$term[new_order])
  } else {
    tidy_df$term <- base::as.factor(tidy_df$term)
    tidy_df %<>%
      tibble::rownames_to_column(., var = "rowid")
    new_order <- base::order(tidy_df$rowid, decreasing = FALSE)
    tidy_df$term <- as.character(tidy_df$term)
    tidy_df$term <-
      base::factor(x = tidy_df$term, levels = tidy_df$term[new_order])
    tidy_df %<>% dplyr::select(.data = ., -rowid)
  }

  # counting the number of terms in the tidy dataframe
  count_term <- length(tidy_df$term)

  # setting up the basic architecture
  plot <-
    ggplot2::ggplot(
      data = tidy_df,
      mapping = ggplot2::aes(x = estimate, y = factor(term))
    )

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

  # ================================================== ggrepel labels ===========================================================

  # if user has not specified colors, then use a color palette
  if (is.null(stats.label.color)) {
    stats.label.color <- paletteer::paletteer_d(
      package = !!package,
      palette = !!palette,
      n = count_term,
      direction = direction,
      type = "discrete"
    )
  }

  # adding the labels
  if (isTRUE(stats.labels)) {
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

  # ================================================== other plot labels ===========================================================
  #
  # adding other labels to the plot
  plot <- plot +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      caption = caption,
      subtitle = subtitle,
      title = title
    ) +
    ggstatsplot::theme_mprl(ggtheme = ggtheme, ggstatsplot.layer = ggstatsplot.layer) +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 10))

  # ================================================== output ===========================================================
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
    if (class(x)[[1]] %in% lmm.mods) {
      return(broom::augment(x = x) %>%
        tibble::as_data_frame(x = .))
    } else {
      return(broom.mixed::augment(x = x) %>%
        tibble::as_data_frame(x = .))
    }
  }
}
