#' @title Create labels with statistical details for `ggcoefstats`.
#' @name ggcoefstats_label_maker
#' @author Indrajeet Patil
#'
#' @param ... Currently ignored.
#' @inheritParams ggcoefstats
#' @inheritParams tfz_labeller
#'
#' @examples
#'
#' # show all columns in output tibble
#' options(tibble.width = Inf)
#'
#' # for reproducibility
#' set.seed(123)
#'
#' #------------------------- models with *t*-statistic ------------------
#' # model with t-statistic
#' ggstatsplot:::ggcoefstats_label_maker(x = broomExtra::tidy(stats::lm(
#'   data = mtcars, formula = wt ~ cyl * mpg
#' )), statistic = "t")
#'
#' # (in case `x` is not a dataframe, no need to specify `statistic` argument;
#' # this will be figured out by the function itself)
#'
#' #------------------------- models with *t*-statistic ------------------
#'
#' # dataframe
#' clotting <- data.frame(
#'   u = c(5, 10, 15, 20, 30, 40, 60, 80, 100),
#'   lot1 = c(118, 58, 42, 35, 27, 25, 21, 19, 18),
#'   lot2 = c(69, 35, 26, 21, 18, 16, 13, 12, 12)
#' )
#'
#' # model
#' mod <-
#'   stats::glm(
#'     formula = lot1 ~ log(u),
#'     data = clotting,
#'     family = Gamma
#'   )
#'
#' # model with t-statistic
#' ggstatsplot:::ggcoefstats_label_maker(
#'   x = mod,
#'   tidy_df = broomExtra::tidy(
#'     x = mod,
#'     conf.int = TRUE,
#'     conf.level = 0.95
#'   )
#' )
#'
#' #------------------------- models with *z*-statistic --------------------
#'
#' # preparing dataframe
#' counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
#' outcome <- gl(3, 1, 9)
#' treatment <- gl(3, 3)
#' d.AD <- data.frame(treatment, outcome, counts)
#'
#' # model
#' mod <- stats::glm(
#'   formula = counts ~ outcome + treatment,
#'   family = poisson(),
#'   data = d.AD
#' )
#'
#' # creating tidy dataframe with label column
#' ggstatsplot:::ggcoefstats_label_maker(x = mod, tidy_df = broomExtra::tidy(mod))
#'
#' #------------------------- models with *f*-statistic --------------------
#' # creating a model object
#' op <- options(contrasts = c("contr.helmert", "contr.poly"))
#' npk.aov <- stats::aov(formula = yield ~ block + N * P * K, data = npk)
#'
#' # extracting a tidy dataframe with effect size estimate and their CIs
#' tidy_df <-
#'   ggstatsplot::lm_effsize_ci(
#'     object = npk.aov,
#'     effsize = "omega",
#'     partial = FALSE,
#'     nboot = 50
#'   ) %>%
#'   dplyr::rename(.data = ., estimate = omegasq, statistic = F.value)
#'
#' # including a new column with a label
#' ggstatsplot:::ggcoefstats_label_maker(
#'   x = npk.aov,
#'   tidy_df = tidy_df,
#'   effsize = "omega",
#'   partial = FALSE
#' )
#' @keywords internal

# function body
ggcoefstats_label_maker <- function(x,
                                    tidy_df = NULL,
                                    glance_df = NULL,
                                    statistic = NULL,
                                    k = 2,
                                    effsize = "eta",
                                    partial = TRUE,
                                    ...) {

  # =========================== list of objects ===============================

  # dataframe objects
  df.mods <- c(
    "data.frame",
    "data.table",
    "grouped_df",
    "tbl",
    "tbl_df",
    "spec_tbl_df",
    "resampled_df"
  )

  # models for which statistic is t-value
  t.mods <-
    c(
      "biglm",
      "cch",
      "coeftest",
      "drc",
      "felm",
      "gam",
      "gamlss",
      "garch",
      "glmmPQL",
      "gls",
      "gmm",
      "ivreg",
      "lm",
      "lm.beta",
      "lmerMod",
      "lmRob",
      "lmrob",
      "mlm",
      "multinom",
      "nlmerMod",
      "nlrq",
      "nls",
      "orcutt",
      "plm",
      "polr",
      "rlm",
      "rlmerMod",
      "rq",
      "speedglm",
      "speedlm",
      "svyglm",
      "svyolr"
    )

  # models for which statistic is z-value
  z.mods <-
    c(
      "aareg",
      "clm",
      "clmm",
      "coxph",
      "ergm",
      "glmmadmb",
      "glmmTMB",
      "lavaan",
      "mjoint",
      "mle2",
      "mclogit",
      "mmclogit",
      "negbin",
      "survreg"
    )

  # models for which statistic is F-value
  f.mods <- c(
    "aov",
    "aovlist",
    "anova",
    "Gam",
    "manova"
  )

  # models for which there is no clear t-or z-statistic
  # which statistic to use will be decided based on the family used
  g.mods <- c(
    "glm",
    "glmerMod",
    "glmRob",
    "glmrob"
  )

  # t-statistic
  g.t.mods <- c(
    "quasi",
    "gaussian",
    "quasibinomial",
    "quasipoisson",
    "Gamma",
    "inverse.gaussian"
  )

  # for z-statistic, the families are going to be "binomial" and "poisson"
  # but package-dependent; `robustbase` gives z for "Gamma" family, e.g.

  # ==================== dataframe, t-statistic, z-statistic ================

  if (class(x)[[1]] %in% df.mods) tidy_df <- x
  if (class(x)[[1]] %in% t.mods) statistic <- "t"
  if (class(x)[[1]] %in% z.mods) statistic <- "z"

  # ======================= t/z-statistic labels ============================

  if (class(x)[[1]] %in% g.mods) {
    if (class(x)[[1]] == "glm" && summary(x)$family$family[[1]] %in% g.t.mods) {
      statistic <- "t"
    } else if (class(x)[[1]] == "glmerMod" && summary(x)$family[[1]] %in% g.t.mods) {
      statistic <- "t"
    } else {
      statistic <- "z"
    }
  }

  # ====================== F-statistic ====================================

  if (class(x)[[1]] %in% f.mods) {
    statistic <- "f"
    glance_df <- NULL
  }

  # ====================== output ===========================================

  # creating a dataframe with labels
  return(tidy_df %<>%
    tfz_labeller(
      tidy_df = .,
      glance_df = glance_df,
      statistic = statistic,
      effsize = effsize,
      partial = partial,
      k = k
    ))
}


#' @title Prepare labels with statistic for `ggcoefstats` function.
#' @description Creates text labels for `ggcoefstats` plot with the appropriate
#'   statistic (*t*, *z*, or *f*) displayed in the label.
#' @name tfz_labeller
#'
#' @param tidy_df Tidy dataframe from `broomExtra::tidy`.
#' @param glance_df Glance model summary dataframe from `broom::glance`
#'   (default: `NULL`). This is optional argument. If provide, the `glance`
#'   summary will be used to write `caption` for the final plot.
#' @param ... Currently ignored.
#' @inheritParams ggcoefstats
#'
#' @keywords internal

# function body
tfz_labeller <- function(tidy_df,
                         glance_df = NULL,
                         statistic,
                         effsize = "eta",
                         partial = TRUE,
                         k = 2,
                         ...) {

  #----------------------- p-value cleanup ------------------------------------

  # formatting the p-values
  tidy_df %<>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "statistic",
      .funs = ~ specify_decimal_p(x = ., k = k)
    ) %>%
    signif_column(data = ., p = p.value) %>%
    purrrlyr::by_row(
      .d = .,
      ..f = ~ specify_decimal_p(
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
        TRUE ~ paste("==", p.value.formatted, sep = "")
      )
    )

  #--------------------------- t-statistic ------------------------------------

  # if the statistic is t-value
  if (statistic %in% c("t", "t.value", "t-value", "T")) {
    # if `df` column is in the tidy dataframe, rename it to `df.residual`
    if ("df" %in% names(tidy_df)) {
      tidy_df %<>% dplyr::mutate(.data = ., df.residual = df)
    }

    # check if df info is available somewhere
    if ("df.residual" %in% names(glance_df) ||
      "df.residual" %in% names(tidy_df)) {

      # if glance object is available, use that `df.residual`
      if ("df.residual" %in% names(glance_df)) {
        tidy_df$df.residual <- glance_df$df.residual
      }

      # adding a new column with residual df
      tidy_df %<>%
        purrrlyr::by_row(
          .d = .,
          ..f = ~ paste(
            "list(~italic(beta)==",
            specify_decimal_p(x = .$estimate, k = k),
            ", ~italic(t)",
            "(",
            specify_decimal_p(x = .$df.residual, k = 0L),
            ")==",
            .$statistic,
            ", ~italic(p)",
            .$p.value.formatted2,
            ")",
            sep = ""
          ),
          .collate = "rows",
          .to = "label",
          .labels = TRUE
        )
    } else {
      # for objects like `rlm` there will be no parameter
      tidy_df %<>%
        purrrlyr::by_row(
          .d = .,
          ..f = ~ paste(
            "list(~italic(beta)==",
            specify_decimal_p(x = .$estimate, k = k),
            ", ~italic(t)",
            "==",
            .$statistic,
            ", ~italic(p)",
            .$p.value.formatted2,
            ")",
            sep = ""
          ),
          .collate = "rows",
          .to = "label",
          .labels = TRUE
        )
    }
  }

  #--------------------------- z-statistic ---------------------------------

  if (statistic %in% c("z", "z.value", "z-value", "Z")) {
    # if the statistic is z-value
    tidy_df %<>%
      purrrlyr::by_row(
        .d = .,
        ..f = ~ paste(
          "list(~italic(beta)==",
          specify_decimal_p(x = .$estimate, k = k),
          ", ~italic(z)==",
          .$statistic,
          ", ~italic(p)",
          .$p.value.formatted2,
          ")",
          sep = ""
        ),
        .collate = "rows",
        .to = "label",
        .labels = TRUE
      )
  }

  #--------------------------- f-statistic ---------------------------------

  if (statistic %in% c("f", "f.value", "f-value", "F-value", "F")) {

    # which effect size is needed?
    if (effsize == "eta") {
      if (isTRUE(partial)) {
        tidy_df$effsize.text <- list(quote(italic(eta)[p]^2))
      } else {
        tidy_df$effsize.text <- list(quote(italic(eta)^2))
      }
    }

    if (effsize == "omega") {
      if (isTRUE(partial)) {
        tidy_df$effsize.text <- list(quote(italic(omega)[p]^2))
      } else {
        tidy_df$effsize.text <- list(quote(italic(omega)^2))
      }
    }

    # which effect size is needed?
    tidy_df %<>%
      purrrlyr::by_row(
        .d = .,
        ..f = ~ paste(
          "list(~italic(F)",
          "(",
          .$df1,
          "*\",\"*",
          .$df2,
          ")==",
          .$statistic,
          ", ~italic(p)",
          .$p.value.formatted2,
          ", ~", .$effsize.text, "==",
          specify_decimal_p(x = .$estimate, k = k),
          ")",
          sep = ""
        ),
        .collate = "rows",
        .to = "label",
        .labels = TRUE
      )
  }

  # return the final dataframe
  return(tibble::as_tibble(tidy_df))
}
