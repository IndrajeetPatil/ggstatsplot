#' @title Making text subtitle for contingency analysis (Pearson's chi-square
#'   test for independence for between-subjects design or McNemar's test for
#'   within-subjects design)
#' @name subtitle_contingency_tab
#' @author Indrajeet Patil
#'
#' @param main The variable to use as the **rows** in the
#'   contingency table.
#' @param condition The variable to use as the **columns** in the contingency
#'   table.
#' @param counts A string naming a variable in data containing counts, or `NULL`
#'   if each row represents a single observation (Default).
#' @param paired Logical indicating whether data came from a within-subjects
#'   design study (Default: `FALSE`). If `TRUE`, McNemar test subtitle will be
#'   returned. If `FALSE`, Pearson's chi-square test will be returned.
#' @param stat.title Title for the effect being investigated with the chi-square
#'   test. The default is `NULL`, i.e. no title will be added to describe the
#'   effect being shown. An example of a `stat.title` argument will be something
#'   like `"main x condition"` or `"interaction"`.
#' @param bias.correct If `TRUE`, a bias correction will be applied to Cramer's
#'   *V*.
#' @param ... Additional arguments (currently ignored).
#' @inheritParams t1way_ci
#' @inheritParams subtitle_t_parametric
#' @inheritParams stats::chisq.test
#' @inheritParams subtitle_anova_parametric
#'
#' @importFrom tibble tribble as_tibble
#' @importFrom tidyr uncount drop_na
#' @importFrom jmv propTestN
#' @importFrom stats mcnemar.test chisq.test
#' @importFrom rcompanion cramerV cohenG
#'
#' @seealso \code{\link{ggpiestats}}
#'
#' @details For more details about how the effect sizes and their confidence
#'   intervals were computed, see documentation in `?rcompanion::cramerV` and
#'   `?rcompanion::cohenG`.
#'
#' @examples
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
#' @export

# function body
subtitle_contingency_tab <- function(data,
                                     main,
                                     condition,
                                     counts = NULL,
                                     nboot = 100,
                                     paired = FALSE,
                                     stat.title = NULL,
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
  data <-
    dplyr::select(
      .data = data,
      main = !!rlang::enquo(main),
      condition = !!rlang::enquo(condition),
      counts = !!rlang::enquo(counts)
    ) %>%
    tidyr::drop_na(data = .)

  # main and condition need to be a factor for this analysis
  # also drop the unused levels of the factors

  # main
  data %<>%
    dplyr::mutate(.data = ., main = droplevels(as.factor(main))) %>%
    tibble::as_tibble(x = .)

  # condition
  if (!missing(condition)) {
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
  if (!missing(counts)) {
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

  # running Pearson's Chi-square test of independence
  if (!isTRUE(paired)) {

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

    # computing confidence interval for Cramer's V
    # if there was problem computing Cramer's V or its effect size, use NaN
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
    factor.levels <- union(levels(data$main), levels(data$condition))

    # introducing dropped levels back into the variabls
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
    stats_df <- broomExtra::tidy(stats::mcnemar.test(
      x = mat_df,
      y = NULL,
      correct = FALSE
    ))

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
  } else {
    effsize.text <- quote(italic("V")["Cramer"])
  }

  # preparing subtitle
  subtitle <- subtitle_template(
    no.parameters = 1L,
    stat.title = stat.title,
    statistic.text = quote(italic(chi)^2),
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


#' @title Making text subtitle for Proportion Test (N Outcomes)
#' @description This is going to be a chi-squared Goodness of fit test.
#' @name subtitle_onesample_proptest
#' @author Indrajeet Patil
#'
#' @param ratio A vector of numbers: the expected proportions for the proportion
#'   test. Default is `NULL`, which means if there are two levels `ratio =
#'   c(1,1)`, etc.
#' @param legend.title Title text for the legend.
#' @inheritParams subtitle_contingency_tab
#'
#' @importFrom ellipsis check_dots_used
#' @importFrom rlang !! enquo as_name ensym
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom tidyr uncount drop_na gather spread
#' @importFrom rcompanion cramerVFit
#' @importFrom jmv propTestN
#'
#' @details For more details about how the effect sizes and their confidence
#'   intervals were computed, see documentation in `?rcompanion::cramerVFit`.
#'
#' @examples
#'
#' # for reproducibility
#' set.seed(123)
#' library(jmv)
#'
#' # with counts
#' subtitle_onesample_proptest(
#'   data = as.data.frame(HairEyeColor),
#'   main = Eye,
#'   counts = Freq,
#'   ratio = c(0.2, 0.2, 0.3, 0.3)
#' )
#'
#' # in case of no variation, only sample size will be shown
#' subtitle_onesample_proptest(
#'   data = cbind.data.frame(x = rep("a", 10)),
#'   main = x
#' )
#' @export

# defining the function
subtitle_onesample_proptest <- function(data,
                                        main,
                                        counts = NULL,
                                        ratio = NULL,
                                        conf.level = 0.95,
                                        conf.type = "norm",
                                        nboot = 100,
                                        stat.title = NULL,
                                        legend.title = NULL,
                                        k = 2,
                                        messages = TRUE,
                                        ...) {
  ellipsis::check_dots_used()

  # saving the column label for the 'main' variables
  if (is.null(legend.title)) {
    legend.title <- rlang::as_name(rlang::ensym(main))
  }

  # ============================ dataframe ===============================

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      main = !!rlang::enquo(main),
      counts = !!rlang::enquo(counts)
    ) %>%
    tidyr::drop_na(data = .)

  # ====================== converting counts ================================

  # untable the dataframe based on the count for each observation
  if (!missing(counts)) {
    data %<>%
      tidyr::uncount(
        data = .,
        weights = counts,
        .remove = TRUE,
        .id = "id"
      ) %>%
      tibble::as_tibble(x = .)
  }

  # ============================= statistical test =========================

  # sample size
  sample_size <- nrow(data)

  # ratio
  if (is.null(ratio)) {
    ratio <- rep(1 / length(table(data$main)), length(table(data$main)))
  }

  # conducting proportion test with jmv::propTestN()
  stats_df <-
    jmv::propTestN(
      data = data,
      var = "main",
      ratio = ratio,
      expected = FALSE
    )

  # extracting the results
  stats_df <-
    tibble::as_tibble(as.data.frame(stats_df$tests)) %>%
    dplyr::rename(.data = ., statistic = chi, parameter = df, p.value = p)

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
  } else {

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

    # message about effect size measure
    if (isTRUE(messages)) {
      effsize_ci_message(nboot = nboot, conf.level = conf.level)
    }

    # preparing subtitle
    subtitle <- subtitle_template(
      no.parameters = 1L,
      stat.title = stat.title,
      statistic.text = quote(italic(chi)^2),
      statistic = stats_df$statistic[[1]],
      parameter = stats_df$parameter[[1]],
      p.value = stats_df$p.value[[1]],
      effsize.text = quote(italic("V")["Cramer"]),
      effsize.estimate = effsize_df$estimate[[1]],
      effsize.LL = effsize_df$conf.low[[1]],
      effsize.UL = effsize_df$conf.high[[1]],
      n = sample_size,
      conf.level = conf.level,
      k = k,
      k.parameter = 0L
    )
  }

  # return the subtitle text
  return(subtitle)
}
