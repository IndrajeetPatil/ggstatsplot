#' @title Making text subtitle for contingency analysis (Pearson's chi-square
#'   test for independence for between-subjects design or McNemar's test for
#'   within-subjects design)
#' @name subtitle_contigency_tab
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
#' @inheritParams groupedstats::specify_decimal_p
#'
#' @importFrom tibble tribble
#' @importFrom exact2x2 exact2x2
#' @importFrom tidyr uncount
#' @importFrom jmv propTestN contTables contTablesPaired
#'
#' @seealso \code{\link{ggpiestats}}
#'
#' @examples
#'
#' # without counts data
#' subtitle_contigency_tab(
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
#' dat <- as.data.frame(HairEyeColor) %>%
#'   dplyr::filter(.data = ., Sex == "Male")
#'
#' subtitle_contigency_tab(
#'   data = dat,
#'   main = Hair,
#'   condition = Sex,
#'   counts = Freq
#' )
#' @export

# function body
subtitle_contigency_tab <- function(data,
                                    main,
                                    condition,
                                    counts = NULL,
                                    nboot = 25,
                                    paired = FALSE,
                                    stat.title = NULL,
                                    conf.level = 0.95,
                                    conf.type = "norm",
                                    messages = TRUE,
                                    k = 3) {

  # ================================= dataframe ================================

  # creating a dataframe based on which variables are provided
  if (base::missing(counts)) {
    data <-
      dplyr::select(
        .data = data,
        main = !!rlang::enquo(main),
        condition = !!rlang::quo_name(rlang::enquo(condition))
      ) %>%
      stats::na.omit(.) %>%
      tibble::as_data_frame(x = .)
  } else {
    data <-
      dplyr::select(
        .data = data,
        main = !!rlang::enquo(main),
        condition = !!rlang::quo_name(rlang::enquo(condition)),
        counts = !!rlang::quo_name(rlang::enquo(counts))
      ) %>%
      stats::na.omit(.) %>%
      tibble::as_data_frame(x = .)
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
        crayon::blue("Chi-squared test can't be run and subtitle won't be displayed.\n"),
        sep = ""
      ))

      # assigning NULL to subtitle
      subtitle <- NULL

      # return early
      return(subtitle)
    }
  }

  # ============================== converting counts ===========================

  # untable the dataframe based on the count for each obervation
  if (!base::missing(counts)) {
    data %<>%
      tidyr::uncount(
        data = .,
        weights = counts,
        .remove = TRUE,
        .id = "id"
      ) %>%
      tibble::as_data_frame(.)
  }

  # ================================= Pearson's chi-square =====================

  # running Pearson's Chi-square test of independence using jmv::contTables
  if (!isTRUE(paired)) {
    jmv_chi <- jmv::contTables(
      data = data,
      rows = "condition",
      cols = "main",
      phiCra = TRUE # provides Phi and Cramer's V, the latter will be displayed
    )

    # preparing Cramer's V object depending on whether V is NaN or not it will
    # be NaN in cases where there are no values of one categorial variable for
    # level of another categorial variable
    if (is.nan(as.data.frame(jmv_chi$nom)[[4]])) {

      # in case Cramer's V is aNaN
      cramer_ci <- tibble::tribble(
        ~estimate, ~conf.low, ~conf.high,
        NaN, NaN, NaN
      )
    } else {

      # results for confidence interval of Cramer's V
      cramer_ci <- chisq_v_ci(
        data = data,
        rows = main,
        cols = condition,
        nboot = nboot,
        conf.level = conf.level,
        conf.type = conf.type
      )

      # displaying message about bootstrap
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::green("Note: "),
          crayon::blue(
            "95% CI for Cramer's V was computed with",
            crayon::yellow(nboot),
            "bootstrap samples.\n"
          ),
          sep = ""
        ))
      }
    }

    # preparing the subtitle
    subtitle <- base::substitute(
      expr =
        paste(
          y,
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
          italic(V),
          " = ",
          cramer,
          ", 95% CI [",
          LL,
          ", ",
          UL,
          "]",
          ", ",
          italic("n"),
          " = ",
          n
        ),
      env = base::list(
        y = stat.title,
        estimate = ggstatsplot::specify_decimal_p(
          x = as.data.frame(jmv_chi$chiSq)[[2]],
          k = k
        ),
        df = as.data.frame(jmv_chi$chiSq)[[3]],
        pvalue = ggstatsplot::specify_decimal_p(
          x = as.data.frame(jmv_chi$chiSq)[[4]],
          k,
          p.value = TRUE
        ),
        cramer = ggstatsplot::specify_decimal_p(
          x = as.data.frame(jmv_chi$nom)[[4]],
          k = k
        ),
        LL = ggstatsplot::specify_decimal_p(x = cramer_ci$conf.low[[1]], k = k),
        UL = ggstatsplot::specify_decimal_p(x = cramer_ci$conf.high[[1]], k = k),
        n = as.data.frame(jmv_chi$chiSq)$`value[N]`[[1]]
      )
    )

    # ============== McNemar's test ============================================
  } else if (isTRUE(paired)) {
    # carrying out McNemar's test
    jmv_chi <- jmv::contTablesPaired(
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

    # computing exact odds ratio as effect size and their confidence interval
    or_df <- exact2x2::exact2x2(
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

    # preparing the subtitle
    subtitle <- base::substitute(
      expr =
        paste(
          y,
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
          "log"["e"],
          "(OR) = ",
          or,
          ", 95% CI [",
          LL,
          ", ",
          UL,
          "]",
          ", ",
          italic("n"),
          " = ",
          n
        ),
      env = base::list(
        y = stat.title,
        estimate = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_chi$test)$`value[mcn]`[[1]], k),
        df = as.data.frame(jmv_chi$test)$`df[mcn]`[[1]],
        # df always an integer
        pvalue = ggstatsplot::specify_decimal_p(
          x = as.data.frame(jmv_chi$test)$`p[mcn]`[[1]],
          k,
          p.value = TRUE
        ),
        # select odds ratio as effect size
        or = ggstatsplot::specify_decimal_p(x = log(
          x = or_df$estimate, base = exp(1)
        ), k),
        LL = ggstatsplot::specify_decimal_p(x = log(
          x = or_df$conf.int[1], base = exp(1)
        ), k),
        UL = ggstatsplot::specify_decimal_p(x = log(
          x = or_df$conf.int[2], base = exp(1)
        ), k),
        n = as.data.frame(jmv_chi$test)$`value[n]`[[1]]
      )
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
#' @inheritParams groupedstats::specify_decimal_p
#' @inheritParams subtitle_contigency_tab
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
subtitle_onesample_proptest <-
  function(data,
             main,
             counts = NULL,
             ratio = NULL,
             legend.title = NULL,
             k = 3) {

    # saving the column label for the 'main' variables
    if (is.null(legend.title)) {
      legend.title <-
        colnames(dplyr::select(
          .data = data,
          !!rlang::enquo(main)
        ))[1]
    }

    # =================== dataframe =============================================

    if (base::missing(counts)) {
      data <-
        dplyr::select(
          .data = data,
          main = !!rlang::enquo(main)
        ) %>%
        tibble::as_data_frame(x = .)
    } else {
      data <-
        dplyr::select(
          .data = data,
          main = !!rlang::enquo(main),
          counts = !!rlang::enquo(counts)
        ) %>%
        tibble::as_data_frame(x = .)
    }

    # ========================== converting counts ================================

    # untable the dataframe based on the count for each obervation
    if (!base::missing(counts)) {
      data %<>%
        tidyr::uncount(
          data = .,
          weights = counts,
          .remove = TRUE,
          .id = "id"
        ) %>%
        tibble::as_data_frame(.)
    }

    # ================================= statistical test =========================

    # conducting proportion test with jmv::propTestN()
    jmv_prop <- jmv::propTestN(
      data = data,
      var = "main",
      ratio = ratio
    )

    # if there is no value corresponding to one of the levels of the 'main'
    # variable, then no subtitle is needed
    if (is.nan(as.data.frame(jmv_prop$tests)$chi[[1]])) {
      subtitle <-
        base::substitute(
          expr =
            paste(
              italic("n"),
              " = ",
              n
            ),
          env = base::list(n = nrow(x = data))
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
            estimate = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_prop$tests)[[1]], k),
            df = base::as.data.frame(jmv_prop$tests)[[2]],
            # df is always an integer
            pvalue = ggstatsplot::specify_decimal_p(
              x = as.data.frame(jmv_prop$tests)[[3]],
              k,
              p.value = TRUE
            ),
            n = nrow(x = data)
          )
        )
    }

    # return the subtitle text
    return(subtitle)
  }
