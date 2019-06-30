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
      x = {{ x }},
      y = {{ y }}
    ) %>%
    tidyr::drop_na(data = .) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
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
    mean_dat %<>% dplyr::mutate(.data = ., label = mean.y)
  }

  # adding sample size labels and arranging by original factor levels
  mean_dat %<>%
    dplyr::mutate(.data = ., n_label = paste0(x, "\n(n = ", n, ")", sep = "")) %>%
    dplyr::arrange(.data = ., x)

  # return the dataframe with mean information
  return(mean_dat)
}


#' @title Adding labels for mean values.
#' @name mean_ggrepel
#' @author Indrajeet Patil
#'
#' @param mean.data A dataframe containing means for each level of the factor.
#'   The columns should be titled `x`, `y`, and `label`.
#' @param plot A `ggplot` object for which means are to be displayed.
#' @param ... Additional arguments.
#' @inheritParams ggbetweenstats
#' @inheritParams ggrepel::geom_label_repel
#'
#' @importFrom ggrepel geom_label_repel
#' @importFrom rlang !! enquo
#' @importFrom ellipsis check_dots_used
#'
#' @examples
#'
#' \dontrun{
#' # this internal function may not have much utility outside of the package
#' set.seed(123)
#' library(ggplot2)
#'
#' # make a plot
#' p <- ggplot(data = iris, aes(x = Species, y = Sepal.Length)) +
#'   geom_boxplot()
#'
#' # get a dataframe with means
#' mean_dat <- ggstatsplot:::mean_labeller(
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   mean.ci = TRUE,
#'   k = 3
#' )
#'
#' # add means
#' ggstatsplot:::mean_ggrepel(
#'   plot = p,
#'   mean.data = mean_dat,
#'   mean.color = "darkgreen"
#' )
#' }
#' @keywords internal

# function body
mean_ggrepel <- function(plot,
                         mean.data,
                         mean.size = 5,
                         mean.color = "darkred",
                         mean.label.size = 3,
                         mean.label.fontface = "bold",
                         mean.label.color = "black",
                         inherit.aes = TRUE,
                         ...) {

  # check any misspecified argumenta
  ellipsis::check_dots_used()

  # highlight the mean of each group
  if (isTRUE(inherit.aes)) {
    plot <- plot +
      ggplot2::stat_summary(
        fun.y = mean,
        geom = "point",
        color = mean.color,
        size = mean.size,
        na.rm = TRUE
      )
  } else {
    plot <- plot +
      ggplot2::stat_summary(
        mapping = ggplot2::aes(
          x = x,
          y = y
        ),
        fun.y = mean,
        geom = "point",
        color = mean.color,
        size = mean.size,
        inherit.aes = FALSE,
        na.rm = TRUE
      )
  }

  # attach the labels with means to the plot
  plot <- plot +
    ggrepel::geom_label_repel(
      data = mean.data,
      mapping = ggplot2::aes(x = x, y = y, label = label),
      size = mean.label.size,
      fontface = mean.label.fontface,
      color = mean.label.color,
      direction = "both",
      max.iter = 3e2,
      box.padding = 0.35,
      point.padding = 0.5,
      segment.color = "black",
      force = 2,
      inherit.aes = FALSE,
      na.rm = TRUE,
      seed = 123
    )

  # return the plot with labels
  return(plot)
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


#' @title Adding a column to dataframe describing outlier status.
#' @name outlier_df
#' @author Indrajeet Patil
#' @description This function is mostly helpful for internal operations of some
#'   of the functions in this package.
#'
#' @inheritParams ggbetweenstats
#' @param ... Additional arguments.
#'
#' @importFrom rlang !! enquo
#' @importFrom dplyr group_by mutate ungroup
#'
#' @examples
#' # adding column for outlier and a label for that outlier
#' ggstatsplot::outlier_df(
#'   data = morley,
#'   x = Expt,
#'   y = Speed,
#'   outlier.label = Run,
#'   outlier.coef = 2
#' ) %>%
#'   dplyr::arrange(outlier)
#' @export

outlier_df <- function(data,
                       x,
                       y,
                       outlier.label,
                       outlier.coef = 1.5,
                       ...) {
  ellipsis::check_dots_used()

  # add a logical column indicating whether a point is or is not an outlier
  data %<>%
    dplyr::group_by(.data = ., {{ x }}) %>%
    dplyr::mutate(
      .data = .,
      isanoutlier = ifelse(
        test = check_outlier(
          var = {{ y }},
          coef = outlier.coef
        ),
        yes = TRUE,
        no = FALSE
      )
    ) %>%
    dplyr::mutate(
      .data = .,
      outlier = ifelse(
        test = isanoutlier,
        yes = {{ outlier.label }},
        no = NA
      )
    ) %>%
    dplyr::ungroup(x = .)

  # return the data frame with outlier
  return(data)
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
    dplyr::select(.data = data, {{ x }}, {{ y }}) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    tibble::as_tibble(x = .)

  # figuring out number of levels in the grouping factor
  x_n_levels <- length(levels(data[[rlang::as_name(rlang::enquo(x))]]))[[1]]

  # wide format
  data_wide <-
    data %>%
    dplyr::filter(.data = ., !is.na({{ x }})) %>%
    dplyr::group_by(.data = ., {{ x }}) %>%
    dplyr::mutate(.data = ., rowid = dplyr::row_number()) %>%
    dplyr::ungroup(x = .) %>%
    dplyr::filter(.data = ., !is.na({{ y }})) %>%
    tibble::as_tibble(x = .)

  # clean up for repeated measures design
  if (isTRUE(paired)) {
    data_wide %<>%
      dplyr::group_by(.data = ., rowid) %>%
      dplyr::mutate(.data = ., n = dplyr::n()) %>%
      dplyr::ungroup(x = .) %>%
      dplyr::filter(.data = ., n == x_n_levels) %>%
      dplyr::select(.data = ., {{ x }}, {{ y }}, rowid)
  }

  # spreading the columns of interest
  data_wide %<>%
    tidyr::spread(
      data = .,
      key = {{ x }},
      value = {{ y }},
      convert = TRUE
    )

  # return the dataframe in wide format
  return(data_wide)
}


#' @title Adding `geom_signif` to the plot.
#' @name ggsignif_adder
#' @param plot A `ggplot` object on which `geom_signif` needed to be added.
#' @inheritParams ggbetweenstats
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # data
#' df <- data.frame(x = iris$Species, y = iris$Sepal.Length)
#'
#' # plot
#' p <- ggplot(df, aes(x, y)) + geom_boxplot()
#'
#' # dataframe with pairwise comparison test results
#' df_pair <- ggstatsplot::pairwise_p(df, x, y)
#'
#' # adding plot with
#' ggstatsplot:::ggsignif_adder(
#'   plot = p,
#'   df_pairwise = df_pair,
#'   data = df
#' )
#' }
#'
#' @keywords internal

ggsignif_adder <- function(plot,
                           df_pairwise,
                           data,
                           pairwise.annotation = "asterisk",
                           pairwise.display = "significant") {
  # creating a column for group combinations
  df_pairwise %<>%
    purrrlyr::by_row(
      .d = .,
      ..f = ~ c(.$group1, .$group2),
      .collate = "list",
      .to = "groups"
    )

  # decide what needs to be displayed:
  # only significant or non-significant comparisons
  if (pairwise.display %in% c("s", "significant")) {
    df_pairwise %<>%
      dplyr::filter(.data = ., significance != "ns")
  } else if (pairwise.display %in% c("ns", "nonsignificant", "non-significant")) {
    df_pairwise %<>%
      dplyr::filter(.data = ., significance == "ns")
  }

  # proceed only if there are any significant comparisons to display
  if (dim(df_pairwise)[[1]] != 0L) {
    # deciding what needs to be displayed
    if (pairwise.annotation %in% c("p", "p-value", "p.value")) {
      # if p-values are to be displayed
      df_pairwise %<>% dplyr::rename(.data = ., label = p.value.label)

      # for ggsignif
      textsize <- 3
      vjust <- 0
    } else {
      # otherwise just show the asterisks
      df_pairwise %<>% dplyr::rename(.data = ., label = significance)

      # for ggsignif
      textsize <- 4
      vjust <- 0.2
    }

    # arrange the dataframe so that annotations are properly aligned
    df_pairwise %<>% dplyr::arrange(.data = ., group1)

    # computing y coordinates for ggsignif bars
    ggsignif_y_position <-
      ggsignif_position_calculator(x = data$x, y = data$y)

    # adding ggsignif comparisons to the plot
    plot <- plot +
      ggsignif::geom_signif(
        comparisons = df_pairwise$groups,
        map_signif_level = TRUE,
        textsize = textsize,
        tip_length = 0.01,
        vjust = vjust,
        y_position = ggsignif_y_position,
        annotations = df_pairwise$label,
        test = NULL,
        na.rm = TRUE
      )
  }

  # return the plot
  return(plot)
}

# function body
sort_xy <- function(data,
                    x,
                    y,
                    sort = "none",
                    sort.fun = mean,
                    ...) {
  ellipsis::check_dots_used()

  # decide the needed order
  if (sort == "ascending") {
    .desc <- FALSE
  } else {
    .desc <- TRUE
  }

  # reordering `x` based on its mean values
  data %<>%
    dplyr::mutate(
      .data = .,
      {{ x }} := forcats::fct_reorder(
        .f = {{ x }},
        .x = {{ y }},
        .fun = sort.fun,
        na.rm = TRUE,
        .desc = .desc
      )
    )

  # return the final dataframe
  return(data)
}
