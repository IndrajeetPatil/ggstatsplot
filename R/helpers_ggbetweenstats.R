#' @title Create a dataframe with mean per group and a formatted label for
#'   display in `ggbetweenstats` plot.
#' @name mean_labeller
#'
#' @inheritParams ggbetweenstats
#'
#' @importFrom stats na.omit
#' @importFrom groupedstats grouped_summary
#' @importFrom dplyr select group_by vars contains mutate mutate_at arrange
#' @importFrom rlang !! enquo
#' @importFrom tibble as_tibble
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
    dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "x",
      .funs = ~ base::droplevels(x = base::as.factor(x = .))
    ) %>%
    tibble::as_tibble(x = .)

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
      .funs = ~ specify_decimal_p(x = ., k = k)
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

  # adding sample size labels and arranging by original factor levels
  mean_dat %<>%
    dplyr::mutate(
      .data = .,
      n_label = paste0(x, "\n(n = ", n, ")", sep = "")
    ) %>%
    dplyr::arrange(.data = ., x)

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
    probs = c(0.25, 0.75),
    na.rm = TRUE
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
#' @description This conversion is helpful mostly for repeated measures design.
#'
#' @inheritParams ggbetweenstats
#' @param paired A logical that indicates whether the design was repeated
#'   measures (within-subjects) or between-subjects (default: `TRUE`).
#'
#' @importFrom rlang !! enquo
#' @importFrom dplyr n row_number select mutate mutate_at group_by ungroup
#' @importFrom tidyr spread
#' @importFrom stats na.omit
#'
#' @examples
#' ggstatsplot:::long_to_wide_converter(
#'   data = iris_long,
#'   x = condition,
#'   y = value,
#'   paired = TRUE
#' )
#' @keywords internal

long_to_wide_converter <- function(data,
                                   x,
                                   y,
                                   paired = TRUE) {

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    ) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x)))

  # figuring out number of levels in the grouping factor
  x_n_levels <- length(levels(data$x))[[1]]

  # wide format
  data_wide <-
    data %>%
    dplyr::filter(.data = ., !is.na(x)) %>%
    dplyr::group_by(.data = ., x) %>%
    dplyr::mutate(.data = ., rowid = dplyr::row_number()) %>%
    dplyr::ungroup(x = .) %>%
    dplyr::filter(.data = ., !is.na(y)) %>%
    tibble::as_tibble(x = .)

  # clean up for repeated measures design
  if (isTRUE(paired)) {
    data_wide %<>%
      dplyr::group_by(.data = ., rowid) %>%
      dplyr::mutate(.data = ., n = dplyr::n()) %>%
      dplyr::ungroup(x = .) %>%
      dplyr::filter(.data = ., n == x_n_levels) %>%
      dplyr::select(.data = ., x, y, rowid)
  }

  # spreading the columns of interest
  data_wide %<>%
    tidyr::spread(
      data = .,
      key = x,
      value = y,
      convert = TRUE
    )

  # return the dataframe in wide format
  return(data_wide)
}

#' @title Standardize a dataframe with effect sizes for `aov`, `lm`, `aovlist`,
#'   etc. objects.
#' @name lm_effsize_standardizer
#'
#' @inheritParams groupedstats::lm_effsize_ci
#'
#' @examples
#' \dontrun{
#' ggstatsplot:::lm_effsize_standardizer(
#'   object = stats::lm(formula = brainwt ~ vore, data = ggplot2::msleep),
#'   effsize = "eta",
#'   partial = FALSE,
#'   conf.level = 0.99,
#'   nboot = 50
#' )
#' }
#' @keywords internal

# function body
lm_effsize_standardizer <- function(object,
                                    effsize = "eta",
                                    partial = TRUE,
                                    conf.level = 0.95,
                                    nboot = 500) {

  # creating a dataframe with effect size and its CI
  df <- groupedstats::lm_effsize_ci(
    object = object,
    effsize = effsize,
    partial = partial,
    conf.level = conf.level,
    nboot = nboot
  )

  # renaming the particular effect size to standard term 'estimate'
  if (effsize == "eta") {
    # partial eta-squared
    if (isTRUE(partial)) {
      df %<>%
        dplyr::rename(.data = ., estimate = partial.etasq)
    } else {
      # eta-squared
      df %<>%
        dplyr::rename(.data = ., estimate = etasq)
    }
  } else if (effsize == "omega") {
    # partial omega-squared
    if (isTRUE(partial)) {
      df %<>%
        dplyr::rename(.data = ., estimate = partial.omegasq)
    } else {
      # omega-squared
      df %<>%
        dplyr::rename(.data = ., estimate = omegasq)
    }
  }

  # return the dataframe in standard format
  return(df)
}
