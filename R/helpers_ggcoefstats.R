#' @title Create text labels
#' @name tfz_labeller
#'
#' @param tidy_df Tidy dataframe from `broom::tidy`.
#' @param glance_df Glance model summary dataframe from `broom::glance`.
#' @param statistic Which statistic is to be displayed (either `"t"` or `"f"`or
#'   `"z"`).
#' @inheritParams ggcoefstats
#'
#' @keywords internal

# function body
tfz_labeller <- function(tidy_df,
                         glance_df = NULL,
                         statistic,
                         effsize = "eta",
                         partial = TRUE,
                         k = 3) {

  #----------------------- p-value cleanup ------------------------------------

  # formatting the p-values
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
    if ("df.residual" %in% names(glance_df)) {
      tidy_df %<>%
        purrrlyr::by_row(
          .d = .,
          ..f = ~ paste(
            "list(~italic(beta)==",
            ggstatsplot::specify_decimal_p(x = .$estimate, k = k),
            ", ~italic(t)",
            "(",
            glance_df$df.residual,
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
            ggstatsplot::specify_decimal_p(x = .$estimate, k = k),
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
          ggstatsplot::specify_decimal_p(x = .$estimate, k = k),
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
      } else {
        tidy_df %<>%
          purrrlyr::by_row(
            .d = .,
            ..f = ~ paste(
              "list(~italic(F)",
              "(",
              .$df1,
              ",",
              .$df2,
              ")==",
              .$statistic,
              ", ~italic(p)",
              .$p.value.formatted2,
              ", ~italic(eta)^2==",
              ggstatsplot::specify_decimal_p(x = .$estimate, k = k),
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
      } else {
        tidy_df %<>%
          purrrlyr::by_row(
            .d = .,
            ..f = ~ paste(
              "list(~italic(F)",
              "(",
              .$df1,
              ",",
              .$df2,
              ")==",
              .$statistic,
              ", ~italic(p)",
              .$p.value.formatted2,
              ", ~italic(omega)^2==",
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

  # return the final dataframe
  return(tidy_df)
}

#' @title Create labels with statistical details for `ggcoefstats`.
#' @name ggcoefstats_label_maker
#'
#' @inheritParams ggcoefstats
#' @inheritParams tfz_labeller
#'
#' @keywords internal

ggcoefstats_label_maker <- function(x,
                                    tidy_df,
                                    glance_df,
                                    k = 3,
                                    effsize = "eta",
                                    partial = TRUE) {

  # models for which statistic is t-value
  t.mods <- c("lmerMod", "lm", "nls", "lmRob", "rq", "rlm", "rlmerMod", "felm")

  # models for which statistic is z-value
  z.mods <- c("clm", "clmm")

  # models for which statistic is F-value
  f.mods <- c("aov", "aovlist", "anova")

  # models for which there is no clear t-or z-statistic
  # which statistic to use will be decided based on the family used
  g.mods <- c("glm", "glmerMod", "glmRob")

  # ================================ t-statistic labels =====================

  if (class(x)[[1]] %in% t.mods) {
    tidy_df %<>%
      tfz_labeller(
        tidy_df = .,
        glance_df = glance_df,
        statistic = "t",
        k = k
      )
    # ======================= z-statistic labels ============================
  } else if (class(x)[[1]] %in% z.mods) {
    tidy_df %<>%
      tfz_labeller(
        tidy_df = .,
        glance_df = glance_df,
        statistic = "z",
        k = k
      )

    # ================ t/z-statistic labels =================================
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
