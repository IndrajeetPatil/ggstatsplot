#' @title Create labels with statistical details for `ggcoefstats`.
#' @name ggcoefstats_label_maker
#'
#' @inheritParams ggcoefstats
#' @inheritParams tfz_labeller
#'
#' @examples
#' \dontrun{
#' # show all columns in a tibble
#' options(tibble.width = Inf)
#' 
#' # for reproducibility
#' set.seed(123)
#' 
#' #------------------------- models with *t*-statistic ------------------
#' # model with t-statistic
#' ggstatsplot:::ggcoefstats_label_maker(x = broom::tidy(stats::lm(
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
#'   tidy_df = broom::tidy(
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
#' ggstatsplot:::ggcoefstats_label_maker(x = mod, tidy_df = broom::tidy(mod))
#' 
#' #------------------------- models with *f*-statistic --------------------
#' # creating a model object
#' op <- options(contrasts = c("contr.helmert", "contr.poly"))
#' npk.aov <- stats::aov(formula = yield ~ block + N * P * K, data = npk)
#' 
#' # converting to a dataframe using
#' tidy_df <- ggstatsplot::lm_effsize_ci(
#'   object = npk.aov,
#'   effsize = "omega",
#'   partial = FALSE,
#'   nboot = 50
#' ) %>%
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

  # dataframe objects
  df.mods <- c("tbl_df", "tbl", "data.frame", "grouped_df")

  # models for which statistic is t-value
  t.mods <-
    c(
      "cch",
      "garch",
      "gls",
      "lmerMod",
      "lm",
      "coeftest",
      "plm",
      "nlrq",
      "ivreg",
      "biglm",
      "nls",
      "lmRob",
      "rq",
      "rlm",
      "rlmerMod",
      "felm",
      "polr",
      "gamlss",
      "gmm",
      "svyolr"
    )

  # models for which statistic is z-value
  z.mods <-
    c("clm", "clmm", "glmmTMB", "coxph", "survreg", "aareg", "ergm")

  # models for which statistic is F-value
  f.mods <- c("aov", "aovlist", "anova", "Gam")

  # models for which there is no clear t-or z-statistic
  # which statistic to use will be decided based on the family used
  g.mods <- c("glm", "glmerMod", "glmRob")

  # ================================ dataframe ================================
  if (class(x)[[1]] %in% df.mods) {
    tidy_df <- tfz_labeller(
      tidy_df = x,
      glance_df = glance_df,
      statistic = statistic,
      effsize = effsize,
      partial = partial,
      k = k
    )
    # ================================ t-statistic labels =====================
  } else if (class(x)[[1]] %in% t.mods) {
    tidy_df %<>%
      tfz_labeller(
        tidy_df = .,
        glance_df = glance_df,
        statistic = "t",
        k = k
      )
    # ======================= z-statistic labels ==============================
  } else if (class(x)[[1]] %in% z.mods) {
    tidy_df %<>%
      tfz_labeller(
        tidy_df = .,
        glance_df = glance_df,
        statistic = "z",
        k = k
      )

    # ================ t/z-statistic labels ===================================
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
          tfz_labeller(
            tidy_df = .,
            glance_df = glance_df,
            statistic = "t",
            k = k
          )
      } else if (summary(x)$family$family[[1]] %in%
        c("binomial", "poisson")) {
        tidy_df %<>%
          tfz_labeller(
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
          tfz_labeller(
            tidy_df = .,
            glance_df = glance_df,
            statistic = "t",
            k = k
          )
      } else if (summary(x)$family[[1]] %in% c("binomial", "poisson")) {
        tidy_df %<>%
          tfz_labeller(
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
          tfz_labeller(
            tidy_df = .,
            glance_df = glance_df,
            statistic = "t",
            k = k
          )
      } else if (x$family[[1]] %in% c("binomial", "poisson")) {
        tidy_df %<>%
          tfz_labeller(
            tidy_df = .,
            glance_df = glance_df,
            statistic = "z",
            k = k
          )
      }
    }
    # ====================== F-statistic ====================================
  } else if (class(x)[[1]] %in% f.mods) {
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
#' @param tidy_df Tidy dataframe from `broom::tidy`.
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
  if (statistic == "t") {
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
      # for objects like rlm there will be no parameter
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
    #--------------------------- z-statistic ---------------------------------
  } else if (statistic == "z") {
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

    #--------------------------- f-statistic ---------------------------------
  } else if (statistic == "f") {

    # which effect size is needed?
    if (effsize == "eta") {
      if (isTRUE(partial)) {
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
              ", ~italic(eta)[p]^2==",
              specify_decimal_p(x = .$estimate, k = k),
              ")",
              sep = ""
            ),
            .collate = "rows",
            .to = "label",
            .labels = TRUE
          )
      } else {
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
              ", ~italic(eta)^2==",
              specify_decimal_p(x = .$estimate, k = k),
              ")",
              sep = ""
            ),
            .collate = "rows",
            .to = "label",
            .labels = TRUE
          )
      }
    } else if (effsize == "omega") {
      if (isTRUE(partial)) {
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
              ", ~italic(omega)[p]^2==",
              specify_decimal_p(x = .$estimate, k = k),
              ")",
              sep = ""
            ),
            .collate = "rows",
            .to = "label",
            .labels = TRUE
          )
      } else {
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
              ", ~italic(omega)^2==",
              specify_decimal_p(x = .$estimate, k = k),
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
#'   formatted subtitle will be returned. The other option is to return `"tidy"`
#'   data frame with coefficients.
#' @inheritParams ggbetweenstats
#' @param ... Additional arguments (ignored).
#'
#' @importFrom metafor rma
#'
#' @examples
#' # set up
#' set.seed(123)
#' library(ggstatsplot)
#' library(gapminder)
#' 
#' # saving results from regression
#' df_results <- purrr::pmap(
#'   .l = list(
#'     data = list(gapminder::gapminder),
#'     formula = list(scale(lifeExp) ~ scale(gdpPercap)),
#'     grouping.vars = alist(continent),
#'     output = list("tidy", "glance")
#'   ),
#'   .f = groupedstats::grouped_lm
#' ) %>%
#'   dplyr::full_join(x = .[[1]], y = .[[2]], by = "continent") %>%
#'   dplyr::filter(.data = ., term != "(Intercept)")
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
#' @export

# function body
subtitle_meta_ggcoefstats <- function(data,
                                      k = 2,
                                      messages = TRUE,
                                      output = "subtitle",
                                      ...) {

  # check if the two columns needed are present
  if (sum(c("estimate", "std.error") %in% names(data)) != 2) {
    # inform the user that skipping labels for the same reason
    base::stop(base::message(cat(
      crayon::red("Error"),
      crayon::blue(": The dataframe **must** contain the following two columns:\n"),
      crayon::blue("`estimate` and `std.error`."),
      sep = ""
    )),
    call. = FALSE
    )
  }

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

  # create a dataframe with coeffcients
  df_coef <- coef(summary(meta_res)) %>%
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
    base::substitute(
      expr =
        paste(
          "Meta-analytic effect: ",
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
      env = base::list(
        estimate = specify_decimal_p(x = df_coef$estimate, k = k),
        LL = specify_decimal_p(x = df_coef$conf.low, k = k),
        UL = specify_decimal_p(x = df_coef$conf.high, k = k),
        zvalue = specify_decimal_p(x = df_coef$z.value, k = k),
        se = specify_decimal_p(x = df_coef$std.error, k = k),
        pvalue = specify_decimal_p(x = df_coef$p.value, k = k, p.value = TRUE)
      )
    )

  if (output == "subtitle") {
    # return the subtitle
    return(subtitle)
  } else if (output == "tidy") {
    # tidy data frame with coefficients
    return(df_coef)
  }
}
