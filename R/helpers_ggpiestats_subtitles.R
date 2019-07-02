#' @title Subtitle for categorical tests
#' @description Making text subtitle for contingency analysis (Pearson's
#'   chi-square test for independence for between-subjects design or McNemar's
#'   test for within-subjects design) or goodness of fit test for a single
#'   categorical variable.
#' @author Indrajeet Patil
#'
#' @param main The variable to use as the **rows** in the contingency table.
#' @param condition The variable to use as the **columns** in the contingency
#'   table. Default is `NULL`. If `NULL`, one-sample proportion test (a goodness
#'   of fit test) will be run for the `main` variable. Otherwise an appropriate
#'   association test will be run.
#' @param counts A string naming a variable in data containing counts, or `NULL`
#'   if each row represents a single observation (Default).
#' @param paired Logical indicating whether data came from a within-subjects or
#'   repeated measures design study (Default: `FALSE`). If `TRUE`, McNemar's
#'   test subtitle will be returned. If `FALSE`, Pearson's chi-square test will
#'   be returned.
#' @param stat.title Title for the effect being investigated with the chi-square
#'   test. The default is `NULL`, i.e. no title will be added to describe the
#'   effect being shown. An example of a `stat.title` argument will be something
#'   like `"main x condition"` or `"interaction"`.
#' @param bias.correct If `TRUE`, a bias correction will be applied to Cramer's
#'   *V*.
#' @param ratio A vector of proportions: the expected proportions for the
#'   proportion test (should sum to 1). Default is `NULL`, which means the null
#'   is equal theoretical proportions across the levels of the nominal variable.
#'   This means if there are two levels this will be `ratio = c(0.5,0.5)` or if
#'   there are four levels this will be `ratio = c(0.25,0.25,0.25,0.25)`, etc.
#' @param legend.title Title text for the legend.
#' @param ... Additional arguments (currently ignored).
#' @inheritParams t1way_ci
#' @inheritParams subtitle_t_parametric
#' @inheritParams stats::chisq.test
#' @inheritParams subtitle_anova_parametric
#'
#' @importFrom dplyr select mutate mutate_at union rename filter
#' @importFrom ellipsis check_dots_used
#' @importFrom rlang !! enquo as_name ensym
#' @importFrom tibble tribble as_tibble
#' @importFrom tidyr uncount drop_na
#' @importFrom stats mcnemar.test chisq.test
#' @importFrom rcompanion cramerV cohenG cramerVFit
#'
#' @seealso \code{\link{ggpiestats}}
#'
#' @details For more details about how the effect sizes and their confidence
#'   intervals were computed, see documentation in `?rcompanion::cramerV`,
#'   `?rcompanion::cramerVFit`, and `?rcompanion::cohenG`.
#'
#' @examples
#'
#' # ------------------------ association tests -----------------------------
#'
#' set.seed(123)
#'
#' # without counts data
#' ggstatsplot::subtitle_contingency_tab(
#'   data = mtcars,
#'   main = am,
#'   condition = cyl,
#'   nboot = 15
#' )
#'
#' # with counts data
#' # in case of no variation, a `NULL` will be returned.
#' library(jmv)
#'
#' as.data.frame(HairEyeColor) %>%
#'   dplyr::filter(.data = ., Sex == "Male") %>%
#'   subtitle_contingency_tab(
#'     data = .,
#'     main = Hair,
#'     condition = Sex,
#'     counts = Freq
#'   )
#'
#' # ------------------------ goodness of fit tests ---------------------------
#'
#' # for reproducibility
#' set.seed(123)
#'
#' # with counts
#' subtitle_contingency_tab(
#'   data = as.data.frame(HairEyeColor),
#'   main = Eye,
#'   counts = Freq,
#'   ratio = c(0.2, 0.2, 0.3, 0.3)
#' )
#'
#' # in case of no variation, only sample size will be shown
#' subtitle_contingency_tab(
#'   data = cbind.data.frame(x = rep("a", 10)),
#'   main = x
#' )
#' @export

# function body
subtitle_contingency_tab <- function(data,
                                     main,
                                     condition = NULL,
                                     counts = NULL,
                                     ratio = NULL,
                                     nboot = 100,
                                     paired = FALSE,
                                     stat.title = NULL,
                                     legend.title = NULL,
                                     conf.level = 0.95,
                                     conf.type = "norm",
                                     simulate.p.value = FALSE,
                                     B = 2000,
                                     bias.correct = FALSE,
                                     k = 2,
                                     messages = TRUE,
                                     ...) {
  ellipsis::check_dots_used()

  # =============================== dataframe ================================

  # creating a dataframe
  data %<>%
    dplyr::select(
      .data = .,
      main = {{ main }},
      condition = {{ condition }},
      counts = {{ counts }}
    ) %>%
    tidyr::drop_na(data = .)

  # main and condition need to be a factor for this analysis
  # also drop the unused levels of the factors

  # main
  data %<>%
    dplyr::mutate(.data = ., main = droplevels(as.factor(main))) %>%
    tibble::as_tibble(x = .)

  # condition
  if ("condition" %in% names(data)) {
    data %<>%
      dplyr::mutate(.data = ., condition = droplevels(as.factor(condition)))

    # in case there is no variation, no subtitle will be shown
    if (length(unique(levels(data$condition))) == 1L) {
      # display message
      message(cat(
        crayon::red("Error: "),
        crayon::blue("Row variable 'condition' contains less than 2 levels.\n"),
        crayon::blue("Chi-squared test can't be run; no subtitle displayed."),
        sep = ""
      ))

      # assigning NULL to subtitle
      subtitle <- NULL

      # return early
      return(subtitle)
    }
  }

  # ============================ converting counts ===========================

  # untable the dataframe based on the count for each observation
  if ("counts" %in% names(data)) {
    data %<>%
      tidyr::uncount(
        data = .,
        weights = counts,
        .remove = TRUE,
        .id = "id"
      ) %>%
      tibble::as_tibble(.)
  }

  # =============================== Pearson's chi-square =====================

  # sample size
  sample_size <- nrow(data)

  # ratio
  if (is.null(ratio)) {
    ratio <- rep(1 / length(table(data$main)), length(table(data$main)))
  }

  # association tests
  if ("condition" %in% names(data)) {

    # running Pearson's Chi-square test of independence
    if (!isTRUE(paired)) {
      # ======================== Pearson's test ==============================

      # object containing stats
      stats_df <-
        broomExtra::tidy(stats::chisq.test(
          x = data$main,
          y = data$condition,
          correct = FALSE,
          rescale.p = FALSE,
          simulate.p.value = simulate.p.value,
          B = B
        ))

      # computing Cramer's V and its confidence intervals
      effsize_df <-
        tryCatch(
          expr = rcompanion::cramerV(
            x = as.integer(data$main),
            y = as.integer(data$condition),
            ci = TRUE,
            conf = conf.level,
            type = conf.type,
            R = nboot,
            histogram = FALSE,
            digits = k,
            bias.correct = bias.correct
          ) %>%
            tibble::as_tibble(x = .) %>%
            dplyr::rename(
              .data = .,
              estimate = Cramer.V,
              conf.low = lower.ci,
              conf.high = upper.ci
            ),
          error = function(x) {
            tibble::tribble(
              ~estimate, ~conf.low, ~conf.high,
              NaN, NaN, NaN
            )
          }
        )

      # ======================== McNemar's test =================================
    } else {

      # figuring out all unique factor levels across two variables
      factor.levels <- dplyr::union(levels(data$main), levels(data$condition))

      # introducing dropped levels back into the variables
      data %<>%
        dplyr::mutate_at(
          .tbl = .,
          .vars = dplyr::vars(main, condition),
          .funs = factor,
          levels = factor.levels
        )

      # creating a matrix with frequencies and cleaning it up
      mat_df <- as.matrix(table(data$main, data$condition))

      # computing effect size + CI
      stats_df <- broomExtra::tidy(
        stats::mcnemar.test(
          x = mat_df,
          y = NULL,
          correct = FALSE
        )
      )

      # computing effect size + CI
      effsize_df <- rcompanion::cohenG(
        x = mat_df,
        ci = TRUE,
        conf = conf.level,
        type = conf.type,
        R = nboot,
        histogram = FALSE,
        digits = 5
      )$Global.statistics %>%
        tibble::as_tibble(x = .) %>%
        dplyr::rename(
          .data = .,
          estimate = Value,
          conf.low = lower.ci,
          conf.high = upper.ci
        ) %>%
        dplyr::filter(.data = ., Statistic == "g")
    }

    # effect size text
    if (isTRUE(paired)) {
      effsize.text <- quote(italic("g")["Cohen"])
      statistic.text <- quote(chi["McNemar"]^2)
    } else {
      effsize.text <- quote(italic("V")["Cramer"])
      statistic.text <- quote(chi["Pearson"]^2)
    }
  } else {
    # ======================== goodness of fit test ========================

    # checking if the chi-squared test can be run
    stats_df <-
      tryCatch(
        expr = stats::chisq.test(
          x = table(data$main),
          y = NULL,
          correct = FALSE,
          p = ratio,
          rescale.p = FALSE,
          simulate.p.value = simulate.p.value,
          B = B
        ),
        error = function(x) {
          tibble::tribble(
            ~statistic, ~parameter, ~p.value,
            NaN, NaN, NaN
          )
        }
      )

    # if there is no value corresponding to one of the levels of the 'main'
    # variable, then no subtitle is needed
    if (is.nan(stats_df$statistic[[1]])) {
      subtitle <-
        substitute(
          expr = paste(italic("n"), " = ", n),
          env = list(n = sample_size)
        )

      # display message
      message(cat(
        crayon::red("Warning: "),
        crayon::blue("Proportion test will not be run because it requires "),
        crayon::yellow(legend.title),
        crayon::blue(" to have at least \n2 levels with non-zero frequencies."),
        sep = ""
      ))

      # return early
      return(subtitle)
    } else {

      # tidying up the results
      stats_df <- broomExtra::tidy(stats_df)

      # dataframe with effect size and its confidence intervals
      effsize_df <- rcompanion::cramerVFit(
        x = as.vector(table(data$main)),
        p = ratio,
        ci = TRUE,
        conf = conf.level,
        type = conf.type,
        R = nboot,
        histogram = FALSE,
        digits = 5
      ) %>%
        tibble::as_tibble(x = .) %>%
        dplyr::rename(
          .data = .,
          estimate = Cramer.V,
          conf.low = lower.ci,
          conf.high = upper.ci
        )

      # effect size is Cramer's V
      effsize.text <- quote(italic("V")["Cramer"])
      statistic.text <- quote(chi["gof"]^2)
    }
  }

  # preparing subtitle
  subtitle <- subtitle_template(
    no.parameters = 1L,
    stat.title = stat.title,
    statistic.text = statistic.text,
    statistic = stats_df$statistic[[1]],
    parameter = stats_df$parameter[[1]],
    p.value = stats_df$p.value[[1]],
    effsize.text = effsize.text,
    effsize.estimate = effsize_df$estimate[[1]],
    effsize.LL = effsize_df$conf.low[[1]],
    effsize.UL = effsize_df$conf.high[[1]],
    n = sample_size,
    conf.level = conf.level,
    k = k,
    k.parameter = 0L
  )

  # message about effect size measure
  if (isTRUE(messages)) {
    effsize_ci_message(nboot = nboot, conf.level = conf.level)
  }

  # return the subtitle
  return(subtitle)
}

#' @rdname subtitle_contingency_tab
#' @aliases subtitle_contingency_tab
#' @export

subtitle_onesample_proptest <- subtitle_contingency_tab
