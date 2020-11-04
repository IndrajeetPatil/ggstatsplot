#' @title Box/Violin plots for group or condition comparisons in
#'   within-subjects (or repeated measures) designs.
#' @name ggwithinstats
#' @description A combination of box and violin plots along with raw
#'   (unjittered) data points for within-subjects designs with statistical
#'   details included in the plot as a subtitle.
#'
#' @note **Important**: Please note that the function expects that the data is
#'   already sorted by subject/repeated measures ID.
#'
#' @inheritParams ggbetweenstats
#' @param point.path,mean.path Logical that decides whether individual data
#'   points and means, respectively, should be connected using `geom_path`. Both
#'   default to `TRUE`. Note that `point.path` argument is relevant only when
#'   there are two groups (i.e., in case of a *t*-test). In case of large number
#'   of data points, it is advisable to set `point.path = FALSE` as these lines
#'   can overwhelm the plot.
#' @param mean.path.args,point.path.args A list of additional aesthetic
#'   arguments passed on to `geom_path` connecting raw data points and mean
#'   points.
#' @inheritParams statsExpressions::expr_anova_parametric
#'
#' @seealso \code{\link{grouped_ggbetweenstats}}, \code{\link{ggbetweenstats}},
#'  \code{\link{grouped_ggwithinstats}}
#'
#' @importFrom rlang exec !! enquo := !!! exec
#' @importFrom statsExpressions bf_ttest bf_oneway_anova
#' @importFrom pairwiseComparisons pairwise_comparisons pairwise_caption
#' @importFrom ipmisc outlier_df
#' @importFrom dplyr select mutate row_number group_by ungroup anti_join
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggwithinstats.html}
#'
#' @examples
#' \donttest{
#' # setup
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # two groups (*t*-test)
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
#'   data = WineTasting,
#'   x = Wine,
#'   y = Taste,
#'   type = "np",
#'   pairwise.comparisons = TRUE,
#'   outlier.tagging = TRUE,
#'   outlier.label = Taster
#' )
#' }
#' @export

# defining the function
ggwithinstats <- function(data,
                          x,
                          y,
                          type = "parametric",
                          pairwise.comparisons = TRUE,
                          pairwise.display = "significant",
                          p.adjust.method = "holm",
                          effsize.type = "unbiased",
                          bf.prior = 0.707,
                          bf.message = TRUE,
                          results.subtitle = TRUE,
                          xlab = NULL,
                          ylab = NULL,
                          caption = NULL,
                          title = NULL,
                          subtitle = NULL,
                          sample.size.label = TRUE,
                          k = 2L,
                          conf.level = 0.95,
                          nboot = 100L,
                          tr = 0.1,
                          mean.plotting = TRUE,
                          mean.ci = FALSE,
                          mean.point.args = list(size = 5, color = "darkred"),
                          mean.label.args = list(size = 3),
                          point.path = TRUE,
                          point.path.args = list(alpha = 0.5, linetype = "dashed"),
                          mean.path = TRUE,
                          mean.path.args = list(color = "red", size = 1, alpha = 0.5),
                          notch = FALSE,
                          notchwidth = 0.5,
                          outlier.tagging = FALSE,
                          outlier.label = NULL,
                          outlier.coef = 1.5,
                          outlier.label.args = list(),
                          outlier.point.args = list(),
                          violin.args = list(width = 0.5, alpha = 0.2),
                          ggsignif.args = list(textsize = 3, tip_length = 0.01),
                          ggtheme = ggplot2::theme_bw(),
                          ggstatsplot.layer = TRUE,
                          package = "RColorBrewer",
                          palette = "Dark2",
                          ggplot.component = NULL,
                          output = "plot",
                          ...) {

  # convert entered stats type to a standard notation
  type <- ipmisc::stats_type_switch(type)

  # ------------------------------ variable names ----------------------------

  # ensure the variables work quoted or unquoted
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  outlier.label <- if (!rlang::quo_is_null(rlang::enquo(outlier.label))) {
    rlang::ensym(outlier.label)
  }

  # if `xlab` and `ylab` is not provided, use the variable `x` and `y` name
  if (is.null(xlab)) xlab <- rlang::as_name(x)
  if (is.null(ylab)) ylab <- rlang::as_name(y)

  # --------------------------------- data -----------------------------------

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}, outlier.label = {{ outlier.label }}) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    as_tibble(.) %>%
    dplyr::group_by(.data = ., {{ x }}) %>%
    dplyr::mutate(.data = ., rowid = dplyr::row_number()) %>%
    dplyr::ungroup(.) %>%
    dplyr::anti_join(
      x = .,
      y = dplyr::filter(., is.na({{ y }})),
      by = "rowid"
    )

  # if `outlier.label` column is not present, just use the values from `y` column
  if (rlang::quo_is_null(rlang::enquo(outlier.label))) {
    data %<>% dplyr::mutate(.data = ., outlier.label = {{ y }})
  }

  # add a logical column indicating whether a point is or is not an outlier
  data %<>%
    ipmisc::outlier_df(
      data = .,
      x = {{ x }},
      y = {{ y }},
      outlier.coef = outlier.coef,
      outlier.label = outlier.label
    )

  # figure out which test to run based on the number of levels of the
  # independent variables
  test <- ifelse(nlevels(data %>% dplyr::pull({{ x }}))[[1]] < 3, "t", "anova")

  # --------------------- subtitle/caption preparation ------------------------

  if (isTRUE(results.subtitle)) {
    # preparing the bayes factor message
    if (type == "parametric" && isTRUE(bf.message)) {
      caption <-
        caption_function_switch(
          test = test,
          data = data,
          x = rlang::as_string(x),
          y = rlang::as_string(y),
          bf.prior = bf.prior,
          top.text = caption,
          paired = TRUE,
          output = "caption",
          k = k
        )
    }

    # extracting the subtitle using the switch function
    subtitle <-
      subtitle_function_switch(
        # switch based on
        type = type,
        test = test,
        # arguments relevant for subtitle helper functions
        data = data,
        x = {{ x }},
        y = {{ y }},
        paired = TRUE,
        effsize.type = effsize.type,
        var.equal = TRUE, ,
        bf.prior = bf.prior,
        tr = tr,
        nboot = nboot,
        conf.level = conf.level,
        k = k
      )
  }

  # quit early if only subtitle is needed
  if (output %in% c("subtitle", "caption")) {
    return(switch(
      EXPR = output,
      "subtitle" = subtitle,
      "caption" = caption
    ))
  }

  # --------------------------------- basic plot ------------------------------

  # plot
  plot <-
    ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}, group = rowid)
    ) +
    ggplot2::geom_point(
      alpha = 0.5,
      size = 3,
      na.rm = TRUE,
      ggplot2::aes(color = {{ x }})
    ) +
    ggplot2::geom_boxplot(
      mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}),
      inherit.aes = FALSE,
      fill = "white",
      width = 0.2,
      alpha = 0.5,
      notch = notch,
      notchwidth = notchwidth
    ) +
    rlang::exec(
      .fn = ggplot2::geom_violin,
      mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}),
      inherit.aes = FALSE,
      fill = "white",
      na.rm = TRUE,
      !!!violin.args
    )

  # add a connecting path only if there are only two groups
  if (test != "anova" && isTRUE(point.path)) {
    plot <- plot +
      rlang::exec(
        .fn = ggplot2::geom_path,
        na.rm = TRUE,
        !!!point.path.args
      )
  }

  # ---------------------------- outlier tagging -----------------------------

  # If `outlier.label` is not provided, outlier labels will just be values of
  # the `y` vector. If the outlier tag has been provided, just use the dataframe
  # already created.

  if (isTRUE(outlier.tagging)) {
    # applying the labels to tagged outliers with ggrepel
    plot <- plot +
      rlang::exec(
        .fn = ggrepel::geom_label_repel,
        data = dplyr::filter(.data = data, isanoutlier),
        mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}, label = outlier.label),
        show.legend = FALSE,
        min.segment.length = 0,
        inherit.aes = FALSE,
        na.rm = TRUE,
        !!!outlier.label.args
      )
  }

  # ---------------- mean value tagging -------------------------------------

  # add labels for mean values
  if (isTRUE(mean.plotting)) {
    plot <-
      mean_ggrepel(
        plot = plot,
        data = data,
        x = {{ x }},
        y = {{ y }},
        mean.ci = mean.ci,
        k = k,
        inherit.aes = FALSE,
        sample.size.label = sample.size.label,
        mean.path = mean.path,
        mean.path.args = mean.path.args,
        mean.point.args = mean.point.args,
        mean.label.args = mean.label.args
      )
  }

  # ggsignif labels -----------------------------------------------------------

  if (isTRUE(pairwise.comparisons) && test == "anova") {
    # creating dataframe with pairwise comparison results
    df_pairwise <-
      pairwiseComparisons::pairwise_comparisons(
        data = data,
        x = {{ x }},
        y = {{ y }},
        type = type,
        tr = tr,
        paired = TRUE,
        var.equal = TRUE,
        p.adjust.method = p.adjust.method,
        k = k,
        messages = FALSE
      )

    # adding the layer for pairwise comparisons
    plot <-
      ggsignif_adder(
        plot = plot,
        df_pairwise = df_pairwise,
        data = data,
        x = {{ x }},
        y = {{ y }},
        pairwise.display = pairwise.display,
        ggsignif.args = ggsignif.args
      )

    # preparing the caption for pairwise comparisons test
    if (type != "bayes") {
      caption <-
        pairwiseComparisons::pairwise_caption(
          caption,
          unique(df_pairwise$test.details),
          pairwise.display
        )
    }
  }

  # ------------------------ annotations and themes -------------------------

  # specifying annotations and other aesthetic aspects for the plot
  aesthetic_addon(
    plot = plot,
    x = data %>% dplyr::pull({{ x }}),
    xlab = xlab,
    ylab = ylab,
    title = title,
    subtitle = subtitle,
    caption = caption,
    ggtheme = ggtheme,
    ggstatsplot.layer = ggstatsplot.layer,
    package = package,
    palette = palette,
    ggplot.component = ggplot.component
  )
}
