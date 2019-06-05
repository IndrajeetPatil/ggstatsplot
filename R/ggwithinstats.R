#' @title Box/Violin plots for group or condition comparisons in
#'   **within**-subjects (or repeated measures) designs.
#' @name ggwithinstats
#' @description A combination of box and violin plots along with raw
#'   (unjittered) data points for within-subjects designs with statistical
#'   details included in the plot as a subtitle.
#' @author Indrajeet Patil
#'
#' @inheritParams ggbetweenstats
#' @param path.point,path.mean Logical that decides whether individual data
#'   points and means, respectively, should be connected using `geom_path`. Both
#'   default to `TRUE`. Note that `path.point` argument is relevant only when
#'   there are two groups (i.e., in case of a *t*-test). In case of large number
#'   of data points, it is advisable to set `path.point = FALSE` as these lines
#'   can overwhelm the plot.
#' @inheritParams subtitle_anova_parametric
#'
#' @seealso \code{\link{grouped_ggbetweenstats}}, \code{\link{ggbetweenstats}},
#'  \code{\link{grouped_ggwithinstats}}, \code{\link{pairwise_p}}
#'
#' @importFrom forcats fct_reorder
#'
#' @details
#'
#'  For more about how the effect size measures (for nonparametric tests) and
#'  their confidence intervals are computed, see `?rcompanion::wilcoxonPairedR`.
#'
#'  For independent measures designs, use `ggbetweenstats`.
#'
#' @examples
#'
#' # setup
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # two groups (t-test)
#' ggstatsplot::ggwithinstats(
#'   data = VR_dilemma,
#'   x = modality,
#'   y = score,
#'   xlab = "Presentation modality",
#'   ylab = "Proportion of utilitarian decisions"
#' )
#'
#' # more than two groups (anova)
#' library(WRS2)
#'
#' ggstatsplot::ggwithinstats(
#'   data = tibble::as_tibble(WineTasting),
#'   x = Wine,
#'   y = Taste,
#'   type = "r",
#'   pairwise.comparisons = TRUE,
#'   outlier.tagging = TRUE,
#'   outlier.label = Taster
#' )
#' @export

# defining the function
ggwithinstats <- function(data,
                          x,
                          y,
                          type = "parametric",
                          pairwise.comparisons = FALSE,
                          pairwise.annotation = "asterisk",
                          pairwise.display = "significant",
                          p.adjust.method = "holm",
                          effsize.type = "unbiased",
                          partial = TRUE,
                          effsize.noncentral = TRUE,
                          bf.prior = 0.707,
                          bf.message = TRUE,
                          sphericity.correction = TRUE,
                          results.subtitle = TRUE,
                          xlab = NULL,
                          ylab = NULL,
                          caption = NULL,
                          title = NULL,
                          subtitle = NULL,
                          sample.size.label = TRUE,
                          k = 2,
                          conf.level = 0.95,
                          nboot = 100,
                          tr = 0.1,
                          path.point = TRUE,
                          path.mean = TRUE,
                          sort = "none",
                          sort.fun = mean,
                          axes.range.restrict = FALSE,
                          mean.label.size = 3,
                          mean.label.fontface = "bold",
                          mean.label.color = "black",
                          notch = FALSE,
                          notchwidth = 0.5,
                          linetype = "solid",
                          outlier.tagging = FALSE,
                          outlier.shape = 19,
                          outlier.label = NULL,
                          outlier.label.color = "black",
                          outlier.color = "black",
                          outlier.coef = 1.5,
                          mean.plotting = TRUE,
                          mean.ci = FALSE,
                          mean.size = 5,
                          mean.color = "darkred",
                          ggtheme = ggplot2::theme_bw(),
                          ggstatsplot.layer = TRUE,
                          package = "RColorBrewer",
                          palette = "Dark2",
                          direction = 1,
                          ggplot.component = NULL,
                          messages = TRUE) {

  # no pairwise comparisons are available for bayesian t-tests
  if (type %in% c("bf", "bayes") && isTRUE(pairwise.comparisons)) {
    # turn off pairwise comparisons
    pairwise.comparisons <- FALSE
  }

  # ------------------------------ variable names ----------------------------

  # if `xlab` is not provided, use the variable `x` name
  if (is.null(xlab)) {
    xlab <- rlang::as_name(rlang::ensym(x))
  }

  # if `ylab` is not provided, use the variable `y` name
  if (is.null(ylab)) {
    ylab <- rlang::as_name(rlang::ensym(y))
  }

  # --------------------------------- data -----------------------------------

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y),
      outlier.label = !!rlang::enquo(outlier.label)
    ) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
    tibble::as_tibble(x = .)

  # figuring out number of levels in the grouping factor
  x_n_levels <- length(levels(data$x))[[1]]

  # removing observations that don't have all repeated values
  data %<>%
    dplyr::filter(.data = ., !is.na(x)) %>%
    dplyr::group_by(.data = ., x) %>%
    dplyr::mutate(.data = ., id = dplyr::row_number()) %>%
    dplyr::ungroup(x = .) %>%
    dplyr::filter(.data = ., !is.na(y)) %>%
    dplyr::group_by(.data = ., id) %>%
    dplyr::mutate(.data = ., n = dplyr::n()) %>%
    dplyr::ungroup(x = .) %>%
    dplyr::filter(.data = ., n == x_n_levels) %>%
    dplyr::select(.data = ., -n)

  # if outlier.label column is not present, just use the values from `y` column
  if (!"outlier.label" %in% names(data)) {
    data %<>%
      dplyr::mutate(.data = ., outlier.label = y)
  }

  # add a logical column indicating whether a point is or is not an outlier
  data %<>%
    outlier_df(
      data = .,
      x = x,
      y = y,
      outlier.coef = outlier.coef,
      outlier.label = outlier.label
    )

  # figure out which test to run based on the number of levels of the
  # independent variables
  if (length(levels(as.factor(data$x))) < 3) {
    test <- "t-test"
  } else {
    test <- "anova"
  }

  # --------------------------------- sorting --------------------------------

  # if sorting is happening
  if (sort != "none") {
    data %<>%
      sort_xy(
        data = .,
        x = x,
        y = y,
        sort = sort,
        sort.fun = sort.fun
      )
  }

  # --------------------------------- basic plot ------------------------------

  # plot
  plot <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(x = x, y = y, group = id)
  ) +
    ggplot2::geom_point(
      alpha = 0.5,
      size = 3,
      na.rm = TRUE,
      ggplot2::aes(color = factor(x))
    ) +
    ggplot2::geom_boxplot(
      mapping = ggplot2::aes(x = x, y = y),
      inherit.aes = FALSE,
      fill = "white",
      width = 0.2,
      alpha = 0.5,
      notch = notch,
      notchwidth = notchwidth
    ) +
    ggplot2::geom_violin(
      mapping = ggplot2::aes(x = x, y = y),
      inherit.aes = FALSE,
      width = 0.5,
      alpha = 0.2,
      fill = "white",
      na.rm = TRUE
    )

  # add a connecting path only if there are only two groups
  if (test != "anova" && isTRUE(path.point)) {
    plot <- plot +
      ggplot2::geom_path(
        color = "grey50",
        size = 0.5,
        alpha = 0.5,
        linetype = "dashed"
      )
  }

  # --------------------- subtitle/caption preparation ------------------------

  if (isTRUE(results.subtitle)) {

    # figuring out which effect size to use
    effsize.type <- effsize_type_switch(effsize.type)

    # preparing the bayes factor message
    if (type %in% c("parametric", "p") && isTRUE(bf.message)) {
      # preparing the BF message for null
      if (test == "t-test") {
        caption <-
          bf_two_sample_ttest(
            data = data,
            x = x,
            y = y,
            bf.prior = bf.prior,
            caption = caption,
            paired = TRUE,
            output = "caption",
            k = k
          )
      } else if (test == "anova") {
        caption <-
          bf_oneway_anova(
            data = data,
            x = x,
            y = y,
            bf.prior = bf.prior,
            caption = caption,
            output = "caption",
            k = k
          )
      }
    }

    # extracting the subtitle using the switch function
    subtitle <-
      ggbetweenstats_switch(
        # switch based on
        type = type,
        test = test,
        # arguments relevant for subtitle helper functions
        data = data,
        x = x,
        y = y,
        paired = TRUE,
        effsize.type = effsize.type,
        partial = partial,
        effsize.noncentral = effsize.noncentral,
        var.equal = TRUE,
        sphericity.correction = sphericity.correction,
        bf.prior = bf.prior,
        tr = tr,
        nboot = nboot,
        conf.level = conf.level,
        k = k,
        messages = messages
      )
  } else {
    test <- "none"
  }

  # ---------------------------- outlier tagging -----------------------------

  # If `outlier.label` is not provided, outlier labels will just be values of
  # the `y` vector. If the outlier tag has been provided, just use the dataframe
  # already created.

  if (isTRUE(outlier.tagging)) {
    # finding and tagging the outliers
    data_outlier_label <- data %>%
      dplyr::filter(.data = ., isanoutlier) %>%
      dplyr::select(.data = ., -outlier)

    # applying the labels to tagged outliers with ggrepel
    plot <-
      plot +
      ggrepel::geom_label_repel(
        data = data_outlier_label,
        mapping = ggplot2::aes(x = x, y = y, label = outlier.label),
        fontface = "bold",
        color = outlier.label.color,
        max.iter = 3e2,
        box.padding = 0.35,
        point.padding = 0.5,
        segment.color = "black",
        force = 2,
        na.rm = TRUE,
        seed = 123
      )
  }

  # ---------------- mean value tagging -------------------------------------

  # computing mean and confidence interval for mean using helper function
  # creating label column based on whether just mean is to be displayed or
  # mean plus its CI
  mean_dat <-
    mean_labeller(
      data = data,
      x = x,
      y = y,
      mean.ci = mean.ci,
      k = k
    )

  # add labels for mean values
  if (isTRUE(mean.plotting)) {
    plot <- mean_ggrepel(
      plot = plot,
      mean.data = mean_dat,
      mean.size = mean.size,
      mean.color = mean.color,
      mean.label.size = mean.label.size,
      mean.label.fontface = mean.label.fontface,
      mean.label.color = mean.label.color,
      inherit.aes = FALSE
    )

    # if there should be lines connecting mean values across groups
    if (isTRUE(path.mean)) {
      plot <- plot +
        ggplot2::geom_path(
          data = mean_dat,
          mapping = ggplot2::aes(x = x, y = y, group = 1),
          color = "red",
          size = 2,
          alpha = 0.5,
          inherit.aes = FALSE
        )
    }
  }

  # ----------------- sample size labels --------------------------------------

  # adding sample size labels to the x axes
  if (isTRUE(sample.size.label)) {
    plot <- plot +
      ggplot2::scale_x_discrete(labels = c(unique(mean_dat$n_label)))
  }

  # ggsignif labels -----------------------------------------------------------

  if (isTRUE(pairwise.comparisons) && test == "anova") {
    # creating dataframe with pairwise comparison results
    df_pairwise <-
      pairwise_p(
        data = data,
        x = x,
        y = y,
        type = type,
        tr = tr,
        paired = TRUE,
        var.equal = TRUE,
        p.adjust.method = p.adjust.method,
        k = k,
        messages = FALSE
      )

    # display the results if needed
    if (isTRUE(messages)) {
      print(df_pairwise)
    }

    # adding the layer for pairwise comparisons
    plot <- ggsignif_adder(
      plot = plot,
      df_pairwise = df_pairwise,
      data = data,
      pairwise.annotation = pairwise.annotation,
      pairwise.display = pairwise.display
    )

    # preparing the caption for pairwise comparisons test
    caption <-
      pairwise_p_caption(
        type = type,
        var.equal = TRUE,
        paired = TRUE,
        p.adjust.method = p.adjust.method,
        caption = caption
      )
  }

  # ------------------------ annotations and themes -------------------------

  # specifying annotations and other aesthetic aspects for the plot
  plot <-
    aesthetic_addon(
      plot = plot,
      x = data$x,
      xlab = xlab,
      ylab = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      ggtheme = ggtheme,
      ggstatsplot.layer = ggstatsplot.layer,
      package = package,
      palette = palette,
      direction = direction,
      ggplot.component = ggplot.component
    )

  # don't do scale restriction in case of post hoc comparisons
  if (isTRUE(axes.range.restrict) && !isTRUE(pairwise.comparisons)) {
    plot <- plot +
      ggplot2::coord_cartesian(ylim = c(min(data$y), max(data$y))) +
      ggplot2::scale_y_continuous(limits = c(min(data$y), max(data$y)))
  }

  # --------------------- messages ------------------------------------------

  if (isTRUE(messages)) {

    # display normality test result as a message
    normality_message(
      x = data$y,
      lab = ylab,
      k = k,
      output = "message"
    )

    # display homogeneity of variance test as a message
    bartlett_message(
      data = data,
      x = x,
      y = y,
      lab = xlab,
      k = k,
      output = "message"
    )
  }

  # return the final plot
  return(plot)
}
