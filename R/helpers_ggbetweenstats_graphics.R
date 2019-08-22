#'  @title Create a dataframe with mean per group and a formatted label for
#'   display in `ggbetweenstats` plot.
#' @name mean_labeller
#'
#' @inheritParams ggbetweenstats
#'
#' @importFrom groupedstats grouped_summary
#' @importFrom dplyr select group_by vars contains mutate mutate_at arrange
#' @importFrom rlang !! enquo
#' @importFrom tibble as_tibble
#' @importFrom purrrlyr by_row
#'
#' @examples
#' \donttest{
#' ggstatsplot:::mean_labeller(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = brainwt,
#'   mean.ci = TRUE,
#'   k = 3
#' )
#' }
#'
#' @keywords internal

# function body
mean_labeller <- function(data,
                          x,
                          y,
                          mean.ci = FALSE,
                          k = 3) {

  # creating the dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}) %>%
    tidyr::drop_na(data = .) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    tibble::as_tibble(x = .)

  # computing mean and confidence interval for mean
  mean_dat <-
    groupedstats::grouped_summary(
      data = data,
      grouping.vars = {{ x }},
      measures = {{ y }}
    ) %>%
    dplyr::mutate(.data = ., {{ y }} := mean) %>%
    dplyr::select(
      .data = .,
      {{ x }},
      {{ y }},
      dplyr::matches("^mean"),
      n
    ) %>% # format the numeric values
    dplyr::mutate_at(
      .tbl = .,
      .vars = dplyr::vars(dplyr::contains("mean")),
      .funs = ~ specify_decimal_p(x = ., k = k)
    )

  # adding confidence intervals to the label for mean
  if (isTRUE(mean.ci)) {
    mean_dat %<>%
      purrrlyr::by_row(
        .d = .,
        ..f = ~ paste(.$mean,
          ", 95% CI [",
          .$mean.low.conf,
          ", ",
          .$mean.high.conf,
          "]",
          sep = "",
          collapse = ""
        ),
        .collate = "rows",
        .to = "label",
        .labels = TRUE
      )
  } else {
    mean_dat %<>% dplyr::mutate(.data = ., label = mean)
  }

  # adding sample size labels and arranging by original factor levels
  mean_dat %<>%
    dplyr::mutate(.data = ., n_label = paste0({{ x }}, "\n(n = ", n, ")", sep = "")) %>%
    dplyr::arrange(.data = ., {{ x }})

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
#'   x = Species,
#'   y = Sepal.Length,
#'   mean.data = mean_dat,
#'   mean.color = "darkgreen"
#' )
#' @keywords internal

# function body
mean_ggrepel <- function(plot,
                         x,
                         y,
                         mean.data,
                         mean.size = 5,
                         mean.color = "darkred",
                         mean.label.size = 3,
                         mean.label.fontface = "bold",
                         mean.label.color = "black",
                         inherit.aes = TRUE,
                         ...) {

  # check any misspecified arguments
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
        mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}),
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
      mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}, label = label),
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
  quantiles <-
    stats::quantile(
      x = var,
      probs = c(0.25, 0.75),
      na.rm = TRUE
    )

  # compute the interquartile range
  IQR <- quantiles[2] - quantiles[1]

  # check for outlier and output a logical
  res <-
    ((var < (quantiles[1] - coef * IQR)) | (var > (quantiles[2] + coef * IQR)))

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
#' ggstatsplot:::outlier_df(
#'   data = morley,
#'   x = Expt,
#'   y = Speed,
#'   outlier.label = Run,
#'   outlier.coef = 2
#' ) %>%
#'   dplyr::arrange(outlier)
#' @keywords internal

# function body
outlier_df <- function(data,
                       x,
                       y,
                       outlier.label,
                       outlier.coef = 1.5,
                       ...) {
  # make sure both quoted and unquoted arguments are allowed
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  outlier.label <- rlang::ensym(outlier.label)
  ellipsis::check_dots_used()

  # add a logical column indicating whether a point is or is not an outlier
  data %<>%
    dplyr::group_by(.data = ., {{ x }}) %>%
    dplyr::mutate(
      .data = .,
      isanoutlier = ifelse(
        test = check_outlier(var = {{ y }}, coef = outlier.coef),
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


#' @title Adding `geom_signif` to the plot.
#' @name ggsignif_adder
#' @author Indrajeet Patil
#'
#' @param plot A `ggplot` object on which `geom_signif` needed to be added.
#' @param df_pairwise A dataframe containing results from pairwise comparisons
#'   (produced by `pairwiseComparisons::pairwise_comparisons()` function).
#' @inheritParams ggbetweenstats
#'
#' @importFrom purrrlyr by_row
#' @importFrom dplyr mutate rename filter arrange pull
#' @importFrom ggsignif geom_signif
#'
#' @examples
#' library(ggplot2)
#'
#' # plot
#' p <- ggplot(iris, aes(Species, Sepal.Length)) + geom_boxplot()
#'
#' # dataframe with pairwise comparison test results
#' df_pair <- pairwiseComparisons::pairwise_comparisons(data = iris, x = Species, y = Sepal.Length)
#'
#' # adding plot with
#' ggstatsplot:::ggsignif_adder(
#'   plot = p,
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   df_pairwise = df_pair
#' )
#' @keywords internal

ggsignif_adder <- function(plot,
                           df_pairwise,
                           data,
                           x,
                           y,
                           pairwise.annotation = "p.value",
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
  # only significant comparisons shown
  if (pairwise.display %in% c("s", "significant")) {
    df_pairwise %<>% dplyr::filter(.data = ., significance != "ns")
  }

  # only non-significant comparisons shown
  if (pairwise.display %in% c("ns", "nonsignificant", "non-significant")) {
    df_pairwise %<>% dplyr::filter(.data = ., significance == "ns")
  }

  # proceed only if there are any significant comparisons to display
  if (dim(df_pairwise)[[1]] != 0L) {
    # deciding what needs to be displayed
    if (pairwise.annotation %in% c("p", "p-value", "p.value")) {
      # if p-values are to be displayed
      df_pairwise %<>% dplyr::rename(.data = ., label = p.value.label)
      textsize <- 3
      vjust <- 0
      parse <- TRUE
    } else {
      # otherwise just show the asterisks
      df_pairwise %<>% dplyr::rename(.data = ., label = significance)
      textsize <- 4
      vjust <- 0.2
      parse <- FALSE
    }

    # arrange the dataframe so that annotations are properly aligned
    df_pairwise %<>% dplyr::arrange(.data = ., group1)

    # computing y coordinates for ggsignif bars
    ggsignif_y_position <-
      ggsignif_position_calculator(
        x = data %>% dplyr::pull({{ x }}),
        y = data %>% dplyr::pull({{ y }})
      )

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
        na.rm = TRUE,
        parse = parse
      )
  }

  # return the plot
  return(plot)
}

#' @title Calculating `y` coordinates for the `ggsignif` comparison bars.
#' @inheritParams ggbetweenstats
#'
#' @keywords internal

ggsignif_position_calculator <- function(x, y) {
  # number of comparisons
  n_comparions <-
    length(utils::combn(
      x = unique(x),
      m = 2,
      simplify = FALSE
    ))

  # start position on y-axis for the ggsignif lines
  y_start <- max(y, na.rm = TRUE) * (1 + 0.025)

  # steps in which the y values need to increase
  step_length <- (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)) / 20

  # end position on y-axis for the ggsignif lines
  y_end <- y_start + (step_length * n_comparions)

  # creating a vector of positions for the ggsignif lines
  return(
    seq(
      from = y_start,
      to = y_end,
      length.out = n_comparions
    )
  )
}


# function body
sort_xy <- function(data,
                    x,
                    y,
                    sort = "none",
                    sort.fun = mean,
                    ...) {

  # make sure both quoted and unquoted arguments are allowed
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  ellipsis::check_dots_used()

  # decide the needed order
  if (sort == "ascending") {
    .desc <- FALSE
  } else {
    .desc <- TRUE
  }

  # reordering `x` based on its mean values
  return(
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
  )
}


#' @title Making aesthetic modifications to the plot.
#' @name aesthetic_addon
#' @author Indrajeet Patil
#'
#' @param plot Plot to be aesthetically modified.
#' @param x A numeric vector for `x` axis.
#' @inheritParams ggbetweenstats
#' @param ... Additional arguments.
#'
#' @keywords internal

aesthetic_addon <- function(plot,
                            x,
                            xlab = NULL,
                            ylab = NULL,
                            title = NULL,
                            subtitle = NULL,
                            caption = NULL,
                            ggtheme = ggplot2::theme_bw(),
                            ggstatsplot.layer = TRUE,
                            package = "RColorBrewer",
                            palette = "Dark2",
                            direction = 1,
                            ggplot.component = NULL,
                            ...) {
  ellipsis::check_dots_used()

  # if no. of factor levels is greater than the default palette color count
  palette_message(
    package = package,
    palette = palette,
    min_length = length(unique(levels(x)))[[1]]
  )

  # modifying the plot
  plot <- plot +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      color = xlab
    ) +
    ggstatsplot::theme_ggstatsplot(
      ggtheme = ggtheme,
      ggstatsplot.layer = ggstatsplot.layer
    ) +
    ggplot2::theme(legend.position = "none") +
    paletteer::scale_color_paletteer_d(
      package = !!package,
      palette = !!palette,
      direction = direction
    ) +
    paletteer::scale_fill_paletteer_d(
      package = !!package,
      palette = !!palette,
      direction = direction
    )

  # ---------------- adding ggplot component ---------------------------------

  # if any additional modification needs to be made to the plot
  # this is primarily useful for grouped_ variant of this function
  plot <- plot + ggplot.component

  # return the changed plot
  return(plot)
}
