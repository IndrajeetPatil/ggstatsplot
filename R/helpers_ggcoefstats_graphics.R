#' @title Create labels with statistical details for `ggcoefstats`
#' @name ggcoefstats_label_maker
#'
#' @param ... Currently ignored.
#' @param tidy_df Tidy dataframe from `broomExtra::tidy`.
#' @param glance_df Glance model summary dataframe from `broom::glance`
#'   (default: `NULL`). This is optional argument. If provide, the `glance`
#'   summary will be used to write `caption` for the final plot.
#' @param ... Currently ignored.
#' @inheritParams ggcoefstats
#'
#' @importFrom insight is_model find_statistic
#' @importFrom purrr pmap_dfc
#' @importFrom dplyr select_if
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
#'   groupedstats::lm_effsize_ci(
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

  #----------------------- statistic cleanup ----------------------------------

  # if a dataframe
  if (isFALSE(insight::is_model(x))) {
    tidy_df <- x
  } else {
    # if not a dataframe, figure out what's the relevant statistic
    statistic <- insight::find_statistic(x)

    # standardize statistic type symbol for regression models
    # checking entered strings to extract the statistic
    grep_stat <- function(x, pattern) {
      if (isTRUE(grepl(pattern, x, ignore.case = TRUE))) {
        return(tolower(substring(x, 1, 1)))
      } else {
        return(NA_character_)
      }
    }

    # extracting statistic value
    statistic <-
      purrr::pmap_dfc(
        .l =
          list(
            pattern = list("^t", "^f", "^z", "^chi"),
            x = list(statistic)
          ),
        .f = grep_stat
      ) %>%
      dplyr::select_if(.tbl = ., .predicate = ~ sum(!is.na(.)) > 0) %>%
      unlist(x = ., use.names = FALSE)
  }

  # No glance method is available for F-statistic
  if (statistic == "f") glance_df <- NULL

  #----------------------- p-value cleanup ------------------------------------

  # formatting the p-values
  tidy_df %<>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "statistic",
      .funs = ~ specify_decimal_p(x = ., k = k)
    ) %>%
    signif_column(data = ., p = p.value) %>%
    p_value_formatter(df = ., k = k) %>%
    dplyr::mutate(.data = ., rowid = dplyr::row_number())

  #--------------------------- t-statistic ------------------------------------

  # if the statistic is t-value
  if (statistic == "t") {
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
        dplyr::group_nest(.tbl = ., rowid) %>%
        dplyr::mutate(
          .data = .,
          label = data %>%
            purrr::map(
              .x = .,
              .f = ~ paste(
                "list(~widehat(italic(beta))==",
                specify_decimal_p(x = .$estimate, k = k),
                ", ~italic(t)",
                "(",
                specify_decimal_p(x = .$df.residual, k = 0L),
                ")==",
                .$statistic,
                ", ~italic(p)",
                .$p.value.formatted,
                ")",
                sep = ""
              )
            )
        )
    } else {
      # for objects like `rlm` there will be no parameter
      tidy_df %<>%
        dplyr::group_nest(.tbl = ., rowid) %>%
        dplyr::mutate(
          .data = .,
          label = data %>%
            purrr::map(
              .x = .,
              .f = ~ paste(
                "list(~widehat(italic(beta))==",
                specify_decimal_p(x = .$estimate, k = k),
                ", ~italic(t)",
                "==",
                .$statistic,
                ", ~italic(p)",
                .$p.value.formatted,
                ")",
                sep = ""
              )
            )
        )
    }
  }

  #--------------------------- z-statistic ---------------------------------

  # if the statistic is z-value
  if (statistic == "z") {
    tidy_df %<>%
      dplyr::group_nest(.tbl = ., rowid) %>%
      dplyr::mutate(
        .data = .,
        label = data %>%
          purrr::map(
            .x = .,
            .f = ~ paste(
              "list(~widehat(italic(beta))==",
              specify_decimal_p(x = .$estimate, k = k),
              ", ~italic(z)==",
              .$statistic,
              ", ~italic(p)",
              .$p.value.formatted,
              ")",
              sep = ""
            )
          )
      )
  }

  #--------------------------- f-statistic ---------------------------------

  if (statistic == "f") {
    # which effect size is needed?
    if (effsize == "eta") {
      if (isTRUE(partial)) {
        tidy_df$effsize.text <- list(quote(widehat(italic(eta)[p]^2)))
      } else {
        tidy_df$effsize.text <- list(quote(widehat(italic(eta)^2)))
      }
    }

    if (effsize == "omega") {
      if (isTRUE(partial)) {
        tidy_df$effsize.text <- list(quote(widehat(italic(omega)[p]^2)))
      } else {
        tidy_df$effsize.text <- list(quote(widehat(italic(omega)^2)))
      }
    }

    # which effect size is needed?
    tidy_df %<>%
      dplyr::group_nest(.tbl = ., rowid) %>%
      dplyr::mutate(
        .data = .,
        label = data %>%
          purrr::map(
            .x = .,
            .f = ~ paste(
              "list(~italic(F)",
              "(",
              .$df1,
              "*\",\"*",
              .$df2,
              ")==",
              .$statistic,
              ", ~italic(p)",
              .$p.value.formatted,
              ", ~",
              .$effsize.text,
              "==",
              specify_decimal_p(x = .$estimate, k = k),
              ")",
              sep = ""
            )
          )
      )
  }

  # unnest
  tidy_df %<>%
    tidyr::unnest(data = ., cols = c(label, data)) %>%
    dplyr::select(.data = ., -rowid)

  # return the final dataframe
  return(tibble::as_tibble(tidy_df))
}


#' @title Tidier for `parameters` package objects
#' @name parameters_tidy
#'
#' @inheritParams parameters::model_parameters
#'
#' @importFrom parameters model_parameters
#' @importFrom dplyr rename_all recode
#'
#' @examples
#' \donttest{
#' # setup
#' library(lme4)
#' library(parameters)
#' set.seed(123)
#'
#' # model
#' mm0 <-
#'   lme4::lmer(
#'     formula = scale(Reaction) ~ scale(Days) + (1 | Subject),
#'     data = sleepstudy
#'   )
#'
#' # model parameters
#' ggstatsplot:::parameters_tidy(mm0)
#' }
#'
#' @keywords internal

parameters_tidy <- function(x, ...) {
  tryCatch(
    expr = parameters::model_parameters(x, ...) %>%
      tibble::as_tibble(.) %>%
      dplyr::rename_all(
        .tbl = .,
        .funs = tolower
      ) %>%
      dplyr::rename_all(
        .tbl = .,
        .funs = ~ gsub(
          x = .,
          pattern = "_",
          replacement = "."
        )
      ) %>%
      dplyr::rename_all(
        .tbl = .,
        .funs = dplyr::recode,
        parameter = "term",
        coefficient = "estimate",
        median = "estimate",
        se = "std.error",
        ci.low = "conf.low",
        ci.high = "conf.high",
        f = "statistic",
        t = "statistic",
        z = "statistic",
        df_error = "df.residual",
        p = "p.value"
      ) %>%
      dplyr::filter(.data = ., !is.na(estimate)),
    error = function(e) NULL
  )
}
