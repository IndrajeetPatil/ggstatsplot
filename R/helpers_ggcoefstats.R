#' @title Create labels with statistical details for `ggcoefstats`.
#' @name ggcoefstats_label_maker
#'
#' @inheritParams ggcoefstats
#' @inheritParams tfz_labeller
#'
#' @examples
#' \donttest{
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
#' }
#'
#' @keywords internal

# function body
ggcoefstats_label_maker <- function(x,
                                    tidy_df = NULL,
                                    glance_df = NULL,
                                    statistic = NULL,
                                    k = 2,
                                    effsize = "eta",
                                    partial = TRUE) {
  # ================================ list of objects ==========================

  # dataframe objects
  df.mods <- c(
    "data.frame",
    "grouped_df",
    "tbl",
    "tbl_df"
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
      "gls",
      "gmm",
      "ivreg",
      "lm",
      "lm.beta",
      "lmerMod",
      "lmRob",
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
    "glmRob"
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

  # z-statistic
  g.z.mods <- c(
    "binomial",
    "poisson"
  )

  # ================================ dataframe ================================

  if (class(x)[[1]] %in% df.mods) {
    tidy_df <-
      tfz_labeller(
        tidy_df = x,
        glance_df = glance_df,
        statistic = statistic,
        effsize = effsize,
        partial = partial,
        k = k
      )
  }

  # ================================ t-statistic labels =====================

  if (class(x)[[1]] %in% t.mods) {
    tidy_df %<>%
      tfz_labeller(
        tidy_df = .,
        glance_df = glance_df,
        statistic = "t",
        k = k
      )
  }

  # ======================= z-statistic labels ==============================

  if (class(x)[[1]] %in% z.mods) {
    tidy_df %<>%
      tfz_labeller(
        tidy_df = .,
        glance_df = glance_df,
        statistic = "z",
        k = k
      )
  }

  # ======================= t/z-statistic labels ==============================

  if (class(x)[[1]] %in% g.mods) {
    if (class(x)[[1]] == "glm") {
      if (summary(x)$family$family[[1]] %in% g.t.mods) {
        tidy_df %<>%
          tfz_labeller(
            tidy_df = .,
            glance_df = glance_df,
            statistic = "t",
            k = k
          )
      } else {
        tidy_df %<>%
          tfz_labeller(
            tidy_df = .,
            glance_df = glance_df,
            statistic = "z",
            k = k
          )
      }
    }

    if (class(x)[[1]] == "glmerMod") {
      # models with t-statistic
      if (summary(x)$family[[1]] %in% g.t.mods) {
        tidy_df %<>%
          tfz_labeller(
            tidy_df = .,
            glance_df = glance_df,
            statistic = "t",
            k = k
          )
      } else {
        # models with z-statistic
        tidy_df %<>%
          tfz_labeller(
            tidy_df = .,
            glance_df = glance_df,
            statistic = "z",
            k = k
          )
      }
    }

    if (class(x)[[1]] == "glmRob") {
      # only binomial and poisson families are implemented in `robust` package
      if (x$family[[1]] %in% g.z.mods) {
        tidy_df %<>%
          tfz_labeller(
            tidy_df = .,
            glance_df = glance_df,
            statistic = "z",
            k = k
          )
      }
    }
  }

  # ====================== F-statistic ====================================

  if (class(x)[[1]] %in% f.mods) {
    tidy_df %<>%
      tfz_labeller(
        tidy_df = .,
        glance_df = NULL,
        statistic = "f",
        effsize = effsize,
        partial = partial,
        k = k
      )
  }

  # return the dataframe with a column with labels
  return(tidy_df)
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
#' @inheritParams ggcoefstats
#'
#' @keywords internal

# function body
tfz_labeller <- function(tidy_df,
                         glance_df = NULL,
                         statistic,
                         effsize = "eta",
                         partial = TRUE,
                         k = 2) {

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
        p.value.formatted != "< 0.001" ~ paste("==", p.value.formatted,
          sep = ""
        )
      )
    )

  #--------------------------- t-statistic ------------------------------------

  # if the statistic is t-value
  if (statistic %in% c("t", "t.value", "t-value", "T")) {
    if ("df.residual" %in% names(glance_df) ||
      "df.residual" %in% names(tidy_df)) {

      # if glance object is available, insert df.residual as a new column
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
    } else if (effsize == "omega") {
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

  # convert to tibble
  tidy_df %<>% tibble::as_tibble(.)

  # return the final dataframe
  return(tidy_df)
}

#' @title Prepare subtitle with meta-analysis results
#' @description Making text subtitle for meta-analysis via linear (mixed-effects)
#'   models as implemented in the `metafor` package.
#' @name subtitle_meta_ggcoefstats
#' @author Indrajeet Patil
#'
#' @param data A dataframe. It **must** contain columns named `estimate`
#'   (corresponding estimates of coefficients or other quantities of interest)
#'   and `std.error` (the standard error of the regression term).
#' @param output  Character describing the desired output. If `"subtitle"`, a
#'   formatted subtitle with summary effect and statistical details will be
#'   returned, and if `"caption"`, expression containing details from model
#'   summary will be returned. The other option is to return `"tidy"` data frame
#'   with coefficients or `"glance"` dataframe with model summaries.
#' @inheritParams ggbetweenstats
#' @param ... Additional arguments (ignored).
#'
#' @importFrom metafor rma
#'
#' @examples
#' # let's create a dataframe
#' df_results <-
#'   structure(
#'     .Data = list(estimate = c(
#'       0.382047603321706, 0.780783111514665,
#'       0.425607573765058, 0.558365541235078, 0.956473848429961
#'     ), std.error = c(
#'       0.0465576338644502,
#'       0.0330218199731529, 0.0362834986178494, 0.0480571500648261, 0.062215818388157
#'     ), t.value = c(
#'       8.20590677855356, 23.6444603038067, 11.7300588415607,
#'       11.6187818146078, 15.3734833553524
#'     ), conf.low = c(
#'       0.290515146096969,
#'       0.715841986960399, 0.354354575031406, 0.46379116008131, 0.827446138277154
#'     ), conf.high = c(
#'       0.473580060546444, 0.845724236068931, 0.496860572498711,
#'       0.652939922388847, 1.08550155858277
#'     ), p.value = c(
#'       3.28679518728519e-15,
#'       4.04778497135963e-75, 7.59757330804449e-29, 5.45155840151592e-26,
#'       2.99171217913312e-13
#'     ), df.residual = c(
#'       394L, 358L, 622L, 298L,
#'       22L
#'     )),
#'     row.names = c(NA, -5L),
#'     class = c("tbl_df", "tbl", "data.frame")
#'   )
#'
#' # making subtitle
#' ggstatsplot::subtitle_meta_ggcoefstats(
#'   data = df_results,
#'   k = 3,
#'   messages = FALSE
#' )
#'
#' # getting tidy data frame with coefficients
#' ggstatsplot::subtitle_meta_ggcoefstats(
#'   data = df_results,
#'   messages = FALSE,
#'   output = "tidy"
#' )
#'
#' # making caption
#' ggstatsplot::subtitle_meta_ggcoefstats(
#'   data = df_results,
#'   k = 2,
#'   messages = FALSE,
#'   output = "caption"
#' )
#'
#' # getting dataframe with model summary
#' ggstatsplot::subtitle_meta_ggcoefstats(
#'   data = df_results,
#'   messages = FALSE,
#'   output = "glance"
#' )
#' @export

# function body
subtitle_meta_ggcoefstats <- function(data,
                                      k = 2,
                                      messages = TRUE,
                                      output = "subtitle",
                                      caption = NULL,
                                      ...) {

  #----------------------- input checking ------------------------------------

  # check if the two columns needed are present
  if (sum(c("estimate", "std.error") %in% names(data)) != 2) {
    # inform the user that skipping labels for the same reason
    stop(message(cat(
      crayon::red("Error"),
      crayon::blue(": The dataframe **must** contain the following two columns:\n"),
      crayon::blue("`estimate` and `std.error`."),
      sep = ""
    )),
    call. = FALSE
    )
  }

  #----------------------- meta-analysis ------------------------------------

  # object from meta-analysis
  meta_res <- metafor::rma(
    yi = estimate,
    sei = std.error,
    measure = "GEN",
    intercept = TRUE,
    data = data,
    vtype = "LS",
    method = "REML",
    weighted = TRUE,
    test = "z",
    level = 95,
    digits = 4,
    ...
  )

  # print the results
  if (isTRUE(messages)) {
    print(summary(meta_res))
  }

  #----------------------- tidy output and subtitle ---------------------------

  # create a dataframe with coeffcients
  df_tidy <- coef(summary(meta_res)) %>%
    tibble::as_tibble(x = .) %>%
    dplyr::rename(
      .data = .,
      std.error = se,
      z.value = zval,
      p.value = pval,
      conf.low = ci.lb,
      conf.high = ci.ub
    ) %>%
    dplyr::mutate(.data = ., term = "summary effect") %>%
    dplyr::select(
      .data = .,
      term,
      estimate,
      conf.low,
      conf.high,
      dplyr::everything()
    )

  # preparing the subtitle
  subtitle <-
    substitute(
      expr =
        paste(
          "Summary effect: ",
          beta,
          " = ",
          estimate,
          ", CI"["95%"],
          " [",
          LL,
          ", ",
          UL,
          "]",
          ", ",
          italic("z"),
          " = ",
          zvalue,
          ", ",
          "se = ",
          se,
          ", ",
          italic("p"),
          " = ",
          pvalue
        ),
      env = list(
        estimate = specify_decimal_p(x = df_tidy$estimate, k = k),
        LL = specify_decimal_p(x = df_tidy$conf.low, k = k),
        UL = specify_decimal_p(x = df_tidy$conf.high, k = k),
        zvalue = specify_decimal_p(x = df_tidy$z.value, k = k),
        se = specify_decimal_p(x = df_tidy$std.error, k = k),
        pvalue = specify_decimal_p(x = df_tidy$p.value, k = k, p.value = TRUE)
      )
    )

  #----------------------- model sumamry ------------------------------------

  df_glance <- with(
    data = meta_res,
    expr = tibble::tibble(
      tau2 = tau2,
      se.tau2 = se.tau2,
      k = k,
      p = p,
      m = m,
      QE = QE,
      QEp = QEp,
      QM = QM,
      QMp = QMp,
      I2 = I2,
      H2 = H2,
      int.only = int.only
    )
  )

  # preparing the subtitle
  caption <-
    substitute(
      atop(displaystyle(top.text),
        expr =
          paste(
            "Heterogeneity: ",
            italic("Q"),
            "(",
            df,
            ") = ",
            Q,
            ", ",
            italic("p"),
            " = ",
            pvalue,
            ", ",
            tau["REML"]^2,
            " = ",
            tau2,
            ", ",
            "I"^2,
            " = ",
            I2
          )
      ),
      env = list(
        top.text = caption,
        Q = specify_decimal_p(x = df_glance$QE, k = 0L),
        df = specify_decimal_p(x = (df_glance$k - 1), k = 0L),
        pvalue = specify_decimal_p(x = df_glance$QEp, k = k, p.value = TRUE),
        tau2 = specify_decimal_p(x = df_glance$tau2, k = k),
        I2 = paste(specify_decimal_p(x = df_glance$I2, k = 2L), "%", sep = "")
      )
    )

  #---------------------------- output ---------------------------------------

  # what needs to be returned?
  return(switch(
    EXPR = output,
    "subtitle" = subtitle,
    "tidy" = df_tidy,
    "caption" = caption,
    "glance" = df_glance,
    "subtitle"
  ))
}


#' @title Bayes factor message for random-effects meta-analysis
#' @name bf_meta_message
#' @importFrom metaBMA meta_random
#'
#' @inherit metaBMA::meta_random return Description
#'
#' @inheritParams subtitle_meta_ggcoefstats
#' @inheritParams metaBMA::meta_random
#'
#' @examples
#'
#' \donttest{
#' # setup
#' set.seed(123)
#' library(metaBMA)
#'
#' # creating a dataframe
#' (df <-
#'   structure(
#'     .Data = list(
#'       study = c("1", "2", "3", "4", "5"),
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
#'       )
#'     ),
#'     row.names = c(NA, -5L),
#'     class = c("tbl_df", "tbl", "data.frame")
#'   ))
#'
#' # getting bayes factor in favor of null hypothesis
#' ggstatsplot::bf_meta_message(
#'   data = df,
#'   k = 3,
#'   sample = 50,
#'   messages = FALSE
#' )
#' }
#'
#' @export

# function body
bf_meta_message <- function(data,
                            k = 2,
                            d = "norm",
                            d.par = c(0, 0.3),
                            tau = "halfcauchy",
                            tau.par = 0.5,
                            sample = 10000,
                            summarize = "integrate",
                            caption = NULL,
                            messages = TRUE,
                            ...) {

  #----------------------- input checking ------------------------------------

  # check if the two columns needed are present
  if (sum(c("estimate", "std.error") %in% names(data)) != 2) {
    # inform the user that skipping labels for the same reason
    stop(message(cat(
      crayon::red("Error"),
      crayon::blue(": The dataframe **must** contain the following two columns:\n"),
      crayon::blue("`estimate` and `std.error`.\n"),
      sep = ""
    )),
    call. = FALSE
    )
  }

  #----------------------- create labels column -------------------------------

  if (!"term" %in% names(data)) {
    data %<>%
      dplyr::mutate(.data = ., term = 1:nrow(.)) %>%
      dplyr::mutate(.data = ., term = as.character(term))
  }

  # extracting results from random-effects meta-analysis
  bf_meta <- metaBMA::meta_random(
    y = data$estimate,
    SE = data$std.error,
    labels = data$term,
    d = d,
    d.par = d.par,
    tau = tau,
    tau.par = tau.par,
    sample = sample,
    summarize = summarize,
    method = "parallel",
    ...
  )

  # print results from meta-analysis
  if (isTRUE(messages)) {
    print(bf_meta)
  }

  #----------------------- preparing caption -------------------------------

  # creating a dataframe with posterior estimates
  df_estimates <- as.data.frame(bf_meta$estimates) %>%
    tibble::rownames_to_column(.data = ., var = "term") %>%
    tibble::as_tibble(x = .) %>%
    dplyr::filter(.data = ., term == "d")

  # prepare the bayes factor message
  bf_text <-
    substitute(
      atop(displaystyle(top.text),
        expr =
          paste(
            "In favor of null: ",
            "log"["e"],
            "(BF"["01"],
            ") = ",
            bf,
            ", ",
            italic("d")["mean"]^"posterior",
            " = ",
            d.pmean,
            ", CI"["95%"],
            " [",
            d.pmean.LB,
            ", ",
            d.pmean.UB,
            "]"
          )
      ),
      env = list(
        top.text = caption,
        bf = specify_decimal_p(x = -log(bf_meta$BF[[1]]), k = k),
        d.pmean = specify_decimal_p(x = df_estimates$Mean[[1]], k = k),
        d.pmean.LB = specify_decimal_p(x = df_estimates$HPD95lower[[1]], k = k),
        d.pmean.UB = specify_decimal_p(x = df_estimates$HPD95upper[[1]], k = k)
      )
    )

  # return the caption
  return(bf_text)
}
