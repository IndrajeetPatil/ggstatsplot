#' @title Making expression for contingency table and goodness of fit tests
#' @description Making expression for contingency analysis (Pearson's
#'   chi-square test for independence for between-subjects design or McNemar's
#'   test for within-subjects design) or goodness of fit test for a single
#'   categorical variable.
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#'
#' @param x The variable to use as the **rows** in the contingency table.
#' @param y The variable to use as the **columns** in the contingency
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
#' library(ggstatsplot)
#'
#' # without counts data
#' ggstatsplot::subtitle_contingency_tab(
#'   data = mtcars,
#'   x = am,
#'   y = cyl,
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
#'     x = Hair,
#'     y = Sex,
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
#'   x = Eye,
#'   counts = Freq,
#'   ratio = c(0.2, 0.2, 0.3, 0.3)
#' )
#'
#' # in case of no variation, only sample size will be shown
#' subtitle_contingency_tab(
#'   data = cbind.data.frame(x = rep("a", 10)),
#'   x = x
#' )
#' @export

# function body
subtitle_contingency_tab <- function(data,
                                     x,
                                     y = NULL,
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

  # ensure the variables work quoted or unquoted
  x <- rlang::ensym(x)
  y <- if (!rlang::quo_is_null(rlang::enquo(y))) rlang::ensym(y)
  counts <- if (!rlang::quo_is_null(rlang::enquo(counts))) rlang::ensym(counts)
  ellipsis::check_dots_used()

  # =============================== dataframe ================================

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}, {{ counts }}) %>%
    tidyr::drop_na(data = .) %>%
    tibble::as_tibble(x = .)

  # x and y need to be a factor for this analysis
  # also drop the unused levels of the factors

  # x
  data %<>% dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }})))

  # untable the dataframe based on the count for each observation
  if (!rlang::quo_is_null(rlang::enquo(counts))) {
    data %<>%
      tidyr::uncount(
        data = .,
        weights = {{ counts }},
        .remove = TRUE,
        .id = "id"
      )
  }

  # y
  if (!rlang::quo_is_null(rlang::enquo(y))) {
    # drop the unused levels of the column variable
    data %<>% dplyr::mutate(.data = ., {{ y }} := droplevels(as.factor({{ y }})))

    # in case there is no variation, no subtitle will be shown
    if (nlevels(data %>% dplyr::pull({{ y }}))[[1]] == 1L) {
      # display message
      message(cat(
        crayon::red("Error: "),
        crayon::blue("Row variable 'y' contains less than 2 levels.\n"),
        crayon::blue("Chi-squared test can't be run; no subtitle displayed."),
        sep = ""
      ))

      # return early
      return(NULL)
    }
  }

  # =============================== association tests ========================

  # sample size
  sample_size <- nrow(data)

  # ratio
  if (is.null(ratio)) {
    x_vec <- data %>% dplyr::pull({{ x }})
    ratio <- rep(1 / length(table(x_vec)), length(table(x_vec)))
  }

  # association tests
  if (!rlang::quo_is_null(rlang::enquo(y))) {

    # ======================== Pearson's test ================================

    if (isFALSE(paired)) {
      # creating a matrix with frequencies and cleaning it up
      x_arg <- as.matrix(table(
        data %>% dplyr::pull({{ x }}),
        data %>% dplyr::pull({{ y }})
      ))

      # object containing stats
      stats_df <-
        broomExtra::tidy(stats::chisq.test(
          x = x_arg,
          correct = FALSE,
          rescale.p = FALSE,
          simulate.p.value = simulate.p.value,
          B = B
        ))

      # computing Cramer's V and its confidence intervals
      effsize_df <-
        tryCatch(
          expr = rcompanion::cramerV(
            x = x_arg,
            ci = TRUE,
            conf = conf.level,
            type = conf.type,
            R = nboot,
            histogram = FALSE,
            digits = 5,
            bias.correct = bias.correct
          ) %>%
            rcompanion_cleaner(object = ., estimate.col = "Cramer.V"),
          error = function(x) {
            tibble::tribble(
              ~estimate, ~conf.low, ~conf.high,
              NaN, NaN, NaN
            )
          }
        )

      # effect size text
      effsize.text <- quote(italic("V")["Cramer"])
      statistic.text <- quote(chi["Pearson"]^2)
      n.text <- quote(italic("n")["obs"])
    }

    # ======================== McNemar's test ================================

    if (isTRUE(paired)) {
      # figuring out all unique factor levels across two variables
      factor.levels <- dplyr::union(
        levels(data %>% dplyr::pull({{ x }})),
        levels(data %>% dplyr::pull({{ y }}))
      )

      # introducing dropped levels back into the variables
      data %<>%
        dplyr::mutate_at(
          .tbl = .,
          .vars = dplyr::vars({{ x }}, {{ y }}),
          .funs = factor,
          levels = factor.levels
        )

      # creating a matrix with frequencies and cleaning it up
      x_arg <- as.matrix(table(
        data %>% dplyr::pull({{ x }}),
        data %>% dplyr::pull({{ y }})
      ))

      # computing effect size + CI
      stats_df <-
        broomExtra::tidy(stats::mcnemar.test(
          x = x_arg,
          correct = FALSE
        ))

      # computing effect size + CI
      effsize_df <-
        rcompanion::cohenG(
          x = x_arg,
          ci = TRUE,
          conf = conf.level,
          type = conf.type,
          R = nboot,
          histogram = FALSE,
          digits = 5
        )$Global.statistics %>%
        rcompanion_cleaner(object = ., estimate.col = "Value") %>%
        dplyr::filter(.data = ., Statistic == "g")

      # effect size text
      effsize.text <- quote(italic("g")["Cohen"])
      statistic.text <- quote(chi["McNemar"]^2)
      n.text <- quote(italic("n")["pairs"])
    }
  }

  # ======================== goodness of fit test ========================

  if (rlang::quo_is_null(rlang::enquo(y))) {
    # checking if the chi-squared test can be run
    stats_df <-
      tryCatch(
        expr = stats::chisq.test(
          x = table(data %>% dplyr::pull({{ x }})),
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

    # if there is no value corresponding to one of the levels of the 'x'
    # variable, then no subtitle is needed
    if (is.nan(stats_df$statistic[[1]])) {
      subtitle <-
        substitute(expr = paste(italic("n"), " = ", n), env = list(n = sample_size))

      # display message
      message(cat(
        crayon::red("Warning: "),
        crayon::blue("Proportion test could not be run. Only sample size returned."),
        sep = ""
      ))

      # return early
      return(subtitle)
    }

    # tidying up the results
    stats_df <- broomExtra::tidy(stats_df)

    # `x` argument for effect size function
    x_arg <- as.vector(table(data %>% dplyr::pull({{ x }})))

    # dataframe with effect size and its confidence intervals
    effsize_df <-
      rcompanion::cramerVFit(
        x = x_arg,
        p = ratio,
        ci = TRUE,
        conf = conf.level,
        type = conf.type,
        R = nboot,
        histogram = FALSE,
        digits = 5
      ) %>%
      rcompanion_cleaner(object = ., estimate.col = "Cramer.V")

    # effect size text
    effsize.text <- quote(italic("V")["Cramer"])
    statistic.text <- quote(chi["gof"]^2)
    n.text <- quote(italic("n")["obs"])
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
    n.text = n.text,
    conf.level = conf.level,
    k = k,
    k.parameter = 0L
  )

  # message about effect size measure
  if (isTRUE(messages)) effsize_ci_message(nboot, conf.level)

  # return the subtitle
  return(subtitle)
}

#' @rdname subtitle_contingency_tab
#' @aliases subtitle_contingency_tab
#' @export

subtitle_onesample_proptest <- subtitle_contingency_tab
