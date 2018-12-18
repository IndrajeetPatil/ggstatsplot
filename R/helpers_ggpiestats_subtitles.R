#' @title Making text subtitle for contingency analysis (Pearson's chi-square
#'   test for independence for between-subjects design or McNemar's test for
#'   within-subjects design)
#' @name subtitle_contingency_tab
#' @author Indrajeet Patil
#'
#' @param data The data as a data frame (matrix or tables will not be accepted).
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
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @inheritParams chisq_v_ci
#' @inheritParams subtitle_t_parametric
#' @inheritParams stats::chisq.test
#'
#' @importFrom tibble tribble as_tibble
#' @importFrom exact2x2 exact2x2
#' @importFrom tidyr uncount
#' @importFrom broom tidy
#' @importFrom jmv propTestN contTables contTablesPaired
#'
#' @seealso \code{\link{ggpiestats}}
#'
#' @examples
#'
#' # without counts data
#' subtitle_contingency_tab(
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
                                     nboot = 25,
                                     paired = FALSE,
                                     stat.title = NULL,
                                     conf.level = 0.95,
                                     conf.type = "norm",
                                     simulate.p.value = FALSE,
                                     B = 2000,
                                     k = 2,
                                     messages = TRUE) {

  # =============================== dataframe ================================

  # creating a dataframe based on which variables are provided
  if (base::missing(counts)) {
    data <-
      dplyr::select(
        .data = data,
        main = !!rlang::enquo(main),
        condition = !!rlang::quo_name(rlang::enquo(condition))
      ) %>%
      dplyr::filter(
        .data = .,
        !is.na(main), !is.na(condition)
      ) %>%
      tibble::as_tibble(x = .)
  } else {
    data <-
      dplyr::select(
        .data = data,
        main = !!rlang::enquo(main),
        condition = !!rlang::quo_name(rlang::enquo(condition)),
        counts = !!rlang::quo_name(rlang::enquo(counts))
      ) %>%
      dplyr::filter(
        .data = .,
        !is.na(main), !is.na(condition), !is.na(counts)
      ) %>%
      tibble::as_tibble(x = .)
  }

  # main and condition need to be a factor for this analysis
  # also drop the unused levels of the factors

  # main
  data %<>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "main",
      .funs = ~ base::droplevels(x = base::as.factor(x = .))
    )

  # condition
  if (!base::missing(condition)) {
    data %<>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "condition",
        .funs = ~ base::droplevels(x = base::as.factor(x = .))
      )

    # in case there is no variation, no subtitle will be shown
    if (length(unique(levels(data$condition))) == 1L) {
      # display message
      base::message(cat(
        crayon::red("Error: "),
        crayon::blue("Row variable 'condition' contains less than 2 levels.\n"),
        crayon::blue("Chi-squared test can't be run; no subtitle displayed.\n"),
        sep = ""
      ))

      # assigning NULL to subtitle
      subtitle <- NULL

      # return early
      return(subtitle)
    }
  }

  # ============================ converting counts ===========================

  # untable the dataframe based on the count for each obervation
  if (!base::missing(counts)) {
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

    # object contatining stats
    stats_df <-
      broom::tidy(stats::chisq.test(
        x = data$main,
        y = data$condition,
        correct = FALSE,
        rescale.p = FALSE,
        simulate.p.value = simulate.p.value,
        B = B
      ))

    # object with Cramer's V
    jmv_df <- jmv::contTables(
      data = data,
      rows = "condition",
      cols = "main",
      phiCra = TRUE,
      chiSq = TRUE,
      chiSqCorr = FALSE
    )

    # preparing Cramer's V object depending on whether V is NaN or not it will
    # be NaN in cases where there are no values of one categorial variable for
    # level of another categorial variable
    if (is.nan(as.data.frame(jmv_df$nom)[[4]])) {
      # in case Cramer's V is aNaN
      effsize_df <- tibble::tribble(
        ~`Cramer's V`, ~conf.low, ~conf.high,
        NaN, NaN, NaN
      )
    } else {
      # results for confidence interval of Cramer's V
      effsize_df <- chisq_v_ci(
        data = data,
        rows = main,
        cols = condition,
        nboot = nboot,
        conf.level = conf.level,
        conf.type = conf.type
      )

      # message about effect size measure
      if (isTRUE(messages)) {
        effsize_ci_message(nboot = nboot, conf.level = conf.level)
      }
    }

    # preparing subtitle
    subtitle <- subtitle_template_1(
      stat.title = stat.title,
      statistic.text = quote(italic(chi)^2),
      statistic = stats_df$statistic[[1]],
      parameter = stats_df$parameter[[1]],
      p.value = stats_df$p.value[[1]],
      effsize.text = quote(italic("V")),
      effsize.estimate = effsize_df$Cramer.V[[1]],
      effsize.LL = effsize_df$conf.low[[1]],
      effsize.UL = effsize_df$conf.high[[1]],
      n = sample_size,
      conf.level = conf.level,
      k = k,
      k.parameter = 0L
    )

    # ============== McNemar's test ===========================================
  } else if (isTRUE(paired)) {
    # carrying out McNemar's test
    stats_df <-
      jmv::contTablesPaired(
        data = data,
        rows = "condition",
        cols = "main",
        counts = NULL,
        chiSq = TRUE,
        chiSqCorr = FALSE,
        exact = FALSE,
        pcRow = FALSE,
        pcCol = FALSE
      )

    # extracting needed information from jamovi object
    stats_df <- as.data.frame(stats_df$test)

    # computing exact odds ratio as effect size and their confidence interval
    effsize_df <-
      exact2x2::exact2x2(
        x = data$main,
        y = data$condition,
        or = 1,
        alternative = "two.sided",
        tsmethod = NULL,
        conf.int = TRUE,
        conf.level = conf.level,
        tol = 0.00001,
        conditional = TRUE,
        paired = TRUE,
        plot = FALSE,
        midp = FALSE
      )

    # converting to log odds
    effsize_df <- tibble::tribble(
      ~`estimate`,
      ~conf.low,
      ~conf.high,
      log(effsize_df$estimate[[1]]),
      log(effsize_df$conf.int[1]),
      log(effsize_df$conf.int[2])
    )

    # preparing subtitle
    subtitle <- subtitle_template_1(
      stat.title = stat.title,
      statistic.text = quote(italic(chi)^2),
      statistic = stats_df$`value[mcn]`[[1]],
      parameter = stats_df$`df[mcn]`[[1]],
      p.value = stats_df$`p[mcn]`[[1]],
      effsize.text = quote("log"["e"](OR)),
      effsize.estimate = effsize_df$estimate[[1]],
      effsize.LL = effsize_df$conf.low[[1]],
      effsize.UL = effsize_df$conf.high[[1]],
      n = sample_size,
      conf.level = conf.level,
      k = k,
      k.parameter = 0L
    )
  }

  # return the subtitle
  return(subtitle)
}


#' @title Making text subtitle for Proportion Test (N Outcomes), a chi-squared
#'   Goodness of fit test.
#' @name subtitle_onesample_proptest
#' @author Indrajeet Patil
#'
#' @param ratio A vector of numbers: the expected proportions for the proportion
#'   test. Default is `NULL`, which means if there are two levels `ratio =
#'   c(1,1)`, etc.
#' @param legend.title Title text for the legend.
#' @inheritParams subtitle_contingency_tab
#'
#' @examples
#'
#' # with counts
#' library(jmv)
#'
#' subtitle_onesample_proptest(
#'   data = as.data.frame(HairEyeColor),
#'   main = Eye,
#'   counts = Freq
#' )
#'
#' # in case no variation, only sample size will be shown
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
                                        legend.title = NULL,
                                        k = 2) {

  # saving the column label for the 'main' variables
  if (is.null(legend.title)) {
    legend.title <-
      colnames(dplyr::select(
        .data = data,
        !!rlang::enquo(main)
      ))[1]
  }

  # ============================ dataframe ===============================

  if (base::missing(counts)) {
    data <-
      dplyr::select(
        .data = data,
        main = !!rlang::enquo(main)
      ) %>%
      dplyr::filter(.data = ., !is.na(main))
  } else {
    data <-
      dplyr::select(
        .data = data,
        main = !!rlang::enquo(main),
        counts = !!rlang::enquo(counts)
      ) %>%
      dplyr::filter(.data = ., !is.na(main), !is.na(counts))
  }

  # ====================== converting counts ================================

  # untable the dataframe based on the count for each obervation
  if (!base::missing(counts)) {
    data %<>%
      tidyr::uncount(
        data = .,
        weights = counts,
        .remove = TRUE,
        .id = "id"
      ) %>%
      tibble::as_tibble(.)
  }

  # ============================= statistical test =========================

  # sample size
  sample_size <- nrow(data)

  # conducting proportion test with jmv::propTestN()
  stats_df <-
    jmv::propTestN(
      data = data,
      var = "main",
      ratio = ratio
    )

  # if there is no value corresponding to one of the levels of the 'main'
  # variable, then no subtitle is needed
  if (is.nan(as.data.frame(stats_df$tests)$chi[[1]])) {
    subtitle <-
      base::substitute(
        expr =
          paste(
            italic("n"),
            " = ",
            n
          ),
        env = base::list(n = sample_size)
      )

    # display message
    base::message(cat(
      crayon::red("Warning: "),
      crayon::blue("Proportion test will not be run because it requires "),
      crayon::yellow(legend.title),
      crayon::blue(" to have at least \n2 levels with non-zero frequencies.\n"),
      sep = ""
    ))
  } else {
    # preparing proportion test subtitle for the plot
    subtitle <-
      base::substitute(
        expr =
          paste(
            italic(chi)^2,
            "(",
            df,
            ") = ",
            estimate,
            ", ",
            italic("p"),
            " = ",
            pvalue,
            ", ",
            italic("n"),
            " = ",
            n
          ),
        env = base::list(
          estimate = ggstatsplot::specify_decimal_p(
            x = as.data.frame(stats_df$tests)[[1]],
            k = k,
            p.value = FALSE
          ),
          df = base::as.data.frame(stats_df$tests)[[2]],
          pvalue = ggstatsplot::specify_decimal_p(
            x = as.data.frame(stats_df$tests)[[3]],
            k = k,
            p.value = TRUE
          ),
          n = sample_size
        )
      )
  }

  # return the subtitle text
  return(subtitle)
}
