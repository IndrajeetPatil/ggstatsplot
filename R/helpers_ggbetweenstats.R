#' @title Create a dataframe with mean per group and a formatted label for
#'   display in `ggbetweenstats` plot.
#' @name mean_labeller
#'
#' @inheritParams ggbetweenstats
#'
#' @importFrom stats na.omit
#' @importFrom groupedstats grouped_summary
#' @importFrom dplyr select group_by vars contains mutate mutate_at
#' @importFrom rlang enquo
#' @importFrom tibble as_data_frame
#' @importFrom purrrlyr by_row
#'
#' @keywords internal

# function body
mean_labeller <- function(data,
                          x,
                          y,
                          mean.ci = FALSE,
                          k = 3) {

  # creating the dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    )

  # convert the grouping variable to factor and drop unused levels
  data %<>%
    stats::na.omit(.) %>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "x",
      .funs = ~ base::droplevels(x = base::as.factor(x = .))
    ) %>%
    tibble::as_data_frame(x = .)

  # computing mean and confidence interval for mean
  mean_dat <-
    groupedstats::grouped_summary(
      data = data,
      grouping.vars = x,
      measures = y
    ) %>%
    dplyr::mutate(.data = ., y = mean) %>%
    dplyr::select(
      .data = .,
      x,
      y,
      mean.y = mean,
      lower.ci.y = mean.low.conf,
      upper.ci.y = mean.high.conf,
      n
    ) %>% # format the numeric values
    dplyr::mutate_at(
      .tbl = .,
      .vars = dplyr::vars(dplyr::contains(".y")),
      .funs = ~ ggstatsplot::specify_decimal_p(x = ., k = k)
    )

  # adding confidence intervals to the label for mean
  if (isTRUE(mean.ci)) {
    mean_dat %<>%
      purrrlyr::by_row(
        .d = .,
        ..f = ~ paste(.$mean.y,
          ", 95% CI [",
          .$lower.ci.y,
          ", ",
          .$upper.ci.y,
          "]",
          sep = "",
          collapse = ""
        ),
        .collate = "rows",
        .to = "label",
        .labels = TRUE
      )
  } else {
    mean_dat %<>%
      dplyr::mutate(.data = ., label = mean.y)
  }

  # return the dataframe with mean information
  return(mean_dat)
}


#' @title Finding the outliers in the dataframe using Tukey's interquartile
#'   range rule
#' @name check_outlier
#' @author Indrajeet Patil
#' @description Returns a logical vector
#'
#' @param var A numeric vector.
#' @param coef Coefficient for outlier detection using Tukey's method.
#'   With Tukey's method, outliers are below (1st Quartile) or above (3rd
#'   Quartile) `coef` times the Inter-Quartile Range (IQR) (Default: `1.5`).
#'
#' @importFrom stats quantile
#'
#' @family helper_stats
#'
#' @keywords internal

# defining function to detect outliers
check_outlier <- function(var, coef = 1.5) {

  # compute the quantiles
  quantiles <- stats::quantile(
    x = var,
    probs = c(0.25, 0.75)
  )

  # compute the interquartile range
  IQR <- quantiles[2] - quantiles[1]

  # check for outlier and output a logical
  res <-
    ((var < (quantiles[1] - coef * IQR)) |
      (var > (quantiles[2] + coef * IQR)))

  # return the result
  return(res)
}


#' @title Converts long-format dataframe to wide-format dataframe
#' @name long_to_wide_converter
#' @author Indrajeet Patil
#'
#' @importFrom rlang !! enquo
#' @importFrom dplyr n row_number select mutate mutate_at group_by ungroup
#' @importFrom tidyr spread
#' @importFrom stats na.omit
#'
#' @keywords internal

long_to_wide_converter <- function(data, x, y) {

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    )

  # convert the grouping variable to factor and drop unused levels
  data %<>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "x",
      .funs = ~ base::droplevels(x = base::as.factor(x = .))
    )

  # wide format
  data_wide <-
    data %>%
    dplyr::group_by(.data = ., x) %>%
    dplyr::mutate(.data = ., rowid = dplyr::row_number()) %>%
    dplyr::ungroup(x = .) %>%
    stats::na.omit(.) %>%
    dplyr::group_by(.data = ., rowid) %>%
    dplyr::mutate(.data = ., n = dplyr::n()) %>%
    dplyr::ungroup(x = .) %>%
    dplyr::filter(.data = ., n == 2) %>%
    dplyr::select(.data = ., x, y, rowid) %>%
    tidyr::spread(
      data = .,
      key = x,
      value = y,
      convert = TRUE
    )

  # return the dataframe in wide format
  return(data_wide)
}

#' @title Pairwise comparison tests
#' @name pairwise_p
#' @aliases pairwise_p
#' @description Calculate pairwise comparisons between group levels with
#'   corrections for multiple testing.
#' @author Indrajeet Patil
#'
#' @param p.adjust.method Adjustment method for *p*-values for multiple
#'   comparisons. Possible methods are: `"holm"` (default), `"hochberg"`,
#'   `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.
#' @inheritParams ggbetweenstats
#' @inheritParams stats::p.adjust
#' @inheritParams stats::t.test
#'
#' @importFrom dplyr select rename mutate mutate_if everything full_join
#' @importFrom stats p.adjust pairwise.t.test pairwise.wilcox.test na.omit
#' @importFrom stats aov TukeyHSD
#' @importFrom WRS2 lincon
#' @importFrom tidyr gather spread separate
#' @importFrom rlang !! enquo
#' @importFrom tibble as.tibble rowid_to_column
#' @importFrom broom tidy
#' @importFrom jmv anovaNP
#'
#' @seealso \code{\link{ggbetweenstats}}
#'
#' @family helper_messages
#'
#' @examples
#' \dontrun{
#' 
#' # for reproducibility
#' set.seed(123)
#' 
#' # parametric
#' ggstatsplot::pairwise_p(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = brainwt,
#'   type = "p",
#'   p.adjust.method = "bonferroni"
#' )
#' 
#' # non-parametric
#' ggstatsplot::pairwise_p(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = brainwt,
#'   type = "np",
#'   p.adjust.method = "none"
#' )
#' 
#' # robust
#' ggstatsplot::pairwise_p(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = brainwt,
#'   type = "r",
#'   p.adjust.method = "fdr"
#' )
#' }
#' @export

# function body
pairwise_p <-
  function(data,
             x,
             y,
             type = "parametric",
             tr = 0.1,
             paired = FALSE,
             p.adjust.method = "holm",
             messages = TRUE,
             ...) {
    # ---------------------------- data cleanup -------------------------------
    # creating a dataframe
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(x),
        y = !!rlang::enquo(y)
      )

    # convert the grouping variable to factor and drop unused levels
    data %<>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "x",
        .funs = ~ base::droplevels(x = base::as.factor(x = .))
      ) %>%
      stats::na.omit(.) %>%
      tibble::as.tibble(x = .)

    # ---------------------------- parametric ---------------------------------
    #
    if (type %in% c("parametric", "p")) {
      df <-
        dplyr::full_join(
          # mean difference and its confidence intervals
          x = stats::aov(formula = y ~ x, data = data) %>%
            stats::TukeyHSD(x = .) %>%
            broom::tidy(x = .) %>%
            dplyr::select(
              .data = .,
              comparison, estimate, conf.low, conf.high
            ) %>%
            tidyr::separate(
              data = .,
              col = comparison,
              into = c("group1", "group2"),
              sep = "-"
            ) %>%
            dplyr::rename(.data = ., mean.difference = estimate),
          y = broom::tidy(
            stats::pairwise.t.test(
              x = data$y,
              g = data$x,
              p.adjust.method = p.adjust.method,
              paired = FALSE,
              alternative = "two.sided",
              na.action = na.omit,
              exact = FALSE,
              correct = TRUE,
              conf.int = TRUE,
              conf.level = 0.95
            )
          ) %>%
            ggstatsplot::signif_column(data = ., p = p.value),
          by = c("group1", "group2")
        )

      if (isTRUE(messages)) {
        base::message(cat(
          crayon::red("Note: "),
          crayon::blue(
            "The parametric pairwise multiple comparisons carried out-\n",
            "Student's t-test.\n",
            "Adjustment method for p-values: "
          ),
          crayon::yellow(p.adjust.method),
          sep = ""
        ))
      }

      # ---------------------------- nonparametric ----------------------------
      #
    } else if (type %in% c("nonparametric", "np")) {
      # running Dwass-Steel-Crichtlow-Fligner test using `jmv` package
      jmv_pairs <-
        jmv::anovaNP(
          data = data,
          deps = "y",
          group = "x",
          pairs = TRUE
        )

      # extracting the pairwise tests and formatting the output
      df <-
        as.data.frame(x = jmv_pairs$comparisons[[1]]) %>%
        tibble::as.tibble(x = .) %>%
        dplyr::rename(
          .data = .,
          group1 = p1,
          group2 = p2,
          p.value = p
        ) %>%
        dplyr::mutate(
          .data = .,
          p.value = stats::p.adjust(p = p.value, method = p.adjust.method)
        ) %>%
        ggstatsplot::signif_column(data = ., p = p.value)

      # letting the user know which test was run
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::red("Note: "),
          crayon::blue(
            "The nonparametric pairwise multiple comparisons carried out-\n",
            "Dwass-Steel-Crichtlow-Fligner test.\n",
            "Adjustment method for p-values: "
          ),
          crayon::yellow(p.adjust.method),
          sep = ""
        ))
      }
    } else if (type %in% c("robust", "r")) {
      if (!isTRUE(paired)) {

        # object with all details about pairwise comparisons
        rob_pairwise_df <-
          WRS2::lincon(
            formula = y ~ x,
            data = data,
            tr = tr
          )
      }

      # cleaning the raw object and getting it in the right format
      df <-
        dplyr::full_join(
          # dataframe comparing comparion details
          x = rob_pairwise_df$comp %>%
            tibble::as.tibble(x = .) %>%
            dplyr::rename(
              .data = .,
              group1 = Group,
              group2 = Group1
            ) %>%
            dplyr::mutate(
              .data = .,
              p.value = stats::p.adjust(p = p.value, method = p.adjust.method)
            ) %>%
            ggstatsplot::signif_column(data = ., p = p.value) %>%
            tidyr::gather(
              data = .,
              key = "key",
              value = "rowid",
              group1:group2
            ),
          # dataframe with factor level codings
          y = rob_pairwise_df$fnames %>%
            tibble::as.tibble(x = .) %>%
            tibble::rowid_to_column(.),
          by = "rowid"
        ) %>%
        dplyr::select(.data = ., -rowid) %>%
        tidyr::spread(data = ., key = "key", value = "value") %>%
        dplyr::select(.data = ., group1, group2, dplyr::everything())

      # message about which test was run
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::red("Note: "),
          crayon::blue(
            "The robust pairwise multiple comparisons carried out-\n",
            "Yuen's trimmed means comparisons test.\n",
            "Adjustment method for p-values: "
          ),
          crayon::yellow(p.adjust.method),
          sep = ""
        ))
      }
    }

    # return
    return(df)
  }
