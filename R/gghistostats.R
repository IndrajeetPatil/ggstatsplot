#' @title Histogram for distribution of a numeric variable
#' @name gghistostats
#' @aliases gghistostats
#' @description Histogram with statistical details from one-sample test included
#'   in the plot as a subtitle.
#' @author Indrajeet Patil
#'
#' @param bar.measure Character describing what value needs to be represented as
#'   height in the bar chart. This can either be `"count"`, which shows number
#'   of points in bin, or `"density"`, which density of points in bin, scaled to
#'   integrate to 1, or "`proportion`", which shows relative frequencies of
#'   observations in each bin, or "`mix`", which shows *both* count and
#'   proportion in the same plot.
#' @param fill.gradient Logical decides whether color fill gradient is to be
#'   displayed (Default: `FALSE`). If `FALSE`, the legend and the color gradient
#'   will also be removed. The default is set to `FALSE` because the gradient
#'   provides redundant information in light of y-axis labels.
#' @param low.color,high.color Colors for low and high ends of the gradient.
#'   Defaults are colorblind-friendly.
#' @param normal.curve Logical decides whether to super-impose a normal curve
#'   using `stats::dnorm(mean(x), sd(x))`. Default is `FALSE`.
#' @param normal.curve.color,normal.curve.linetype,normal.curve.size If
#'   `normal.curve = TRUE`, then these arguments can be used to modify color
#'   (Default: `"black"`), size (default: `1.0`), linetype (default: `"solid"`).
#' @param bar.fill If `fill.gradient = FALSE`, then `bar.fill` decides which
#'   color will uniformly fill all the bars in the histogram (Default:
#'   `"grey50"`).
#' @param binwidth The width of the histogram bins. Can be specified as a
#'   numeric value, or a function that calculates width from `x`. The default is
#'   to use the `max(x) - min(x) / sqrt(N)`. You should always check this value
#'   and explore multiple widths to find the best to illustrate the stories in
#'   your data.
#' @inheritParams theme_ggstatsplot
#' @inheritParams subtitle_t_onesample
#' @inheritParams histo_labeller
#' @inheritParams ggbetweenstats
#'
#' @seealso \code{\link{grouped_gghistostats}}, \code{\link{ggdotplotstats}},
#'  \code{\link{grouped_ggdotplotstats}}
#'
#' @import ggplot2
#'
#' @importFrom dplyr select bind_rows summarize mutate mutate_at mutate_if
#' @importFrom dplyr group_by n arrange
#' @importFrom rlang enquo as_name !!
#' @importFrom scales percent percent_format
#' @importFrom stats dnorm
#' @importFrom crayon green blue yellow red
#'
#' @examples
#'
#' # most basic function call with the defaults
#' # this is the **only** function where data argument can be `NULL`
#' ggstatsplot::gghistostats(
#'   x = ToothGrowth$len,
#'   xlab = "Tooth length",
#'   centrality.para = "median"
#' )
#'
#' # a detailed function call
#' ggstatsplot::gghistostats(
#'   data = iris,
#'   x = Sepal.Length,
#'   bar.measure = "mix",
#'   type = "p",
#'   caption = substitute(paste(italic("Note"), ": Iris dataset by Fisher.")),
#'   bf.prior = 0.8,
#'   test.value = 3,
#'   test.value.line = TRUE,
#'   binwidth = 0.10,
#'   bar.fill = "grey50"
#' )
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/gghistostats.html}
#'
#' @export

# function body
gghistostats <- function(data = NULL,
                         x,
                         binwidth = NULL,
                         bar.measure = "count",
                         xlab = NULL,
                         stat.title = NULL,
                         title = NULL,
                         subtitle = NULL,
                         caption = NULL,
                         type = "parametric",
                         test.value = 0,
                         bf.prior = 0.707,
                         bf.message = TRUE,
                         robust.estimator = "onestep",
                         effsize.type = "g",
                         effsize.noncentral = TRUE,
                         conf.level = 0.95,
                         nboot = 100,
                         k = 2,
                         ggtheme = ggplot2::theme_bw(),
                         ggstatsplot.layer = TRUE,
                         fill.gradient = FALSE,
                         low.color = "#0072B2",
                         high.color = "#D55E00",
                         bar.fill = "grey50",
                         results.subtitle = TRUE,
                         centrality.para = "mean",
                         centrality.color = "blue",
                         centrality.size = 1.0,
                         centrality.linetype = "dashed",
                         centrality.line.labeller = TRUE,
                         centrality.k = 2,
                         test.value.line = FALSE,
                         test.value.color = "black",
                         test.value.size = 1.0,
                         test.value.linetype = "dashed",
                         test.line.labeller = TRUE,
                         test.k = 0,
                         normal.curve = FALSE,
                         normal.curve.color = "black",
                         normal.curve.linetype = "solid",
                         normal.curve.size = 1.0,
                         ggplot.component = NULL,
                         return = "plot",
                         messages = TRUE) {

  # if data is not available then don't display any messages
  if (is.null(data)) {
    messages <- FALSE
  }

  # ================================= dataframe ==============================

  # preparing a dataframe out of provided inputs
  if (!is.null(data)) {
    # if xlab is not provided, use the variable x name
    if (is.null(xlab)) {
      xlab <- rlang::as_name(rlang::ensym(x))
    }

    # if dataframe is provided
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(x)
      )
  } else {
    # if vectors are provided
    data <-
      base::cbind.data.frame(x = x)
  }

  # convert to a tibble and remove NAs from `x`
  data %<>%
    tidyr::drop_na(data = .) %>%
    tibble::as_tibble(x = .)

  # Adding some binwidth sanity checking
  if (is.null(binwidth)) {
    binwidth <- (max(data$x) - min(data$x)) / sqrt(length(data$x))
  }

  # ================ stats labels ==========================================

  if (isTRUE(results.subtitle)) {

    # preparing the BF message for NULL
    if (isTRUE(bf.message)) {
      bf.caption.text <-
        bf_ttest(
          data = data,
          x = x,
          y = NULL,
          test.value = test.value,
          bf.prior = bf.prior,
          caption = caption,
          output = "caption",
          k = k
        )
    }

    # preparing the subtitle with statistical results
    subtitle <-
      subtitle_t_onesample(
        data = data,
        x = x,
        type = type,
        test.value = test.value,
        bf.prior = bf.prior,
        robust.estimator = robust.estimator,
        effsize.type = effsize.type,
        effsize.noncentral = effsize.noncentral,
        conf.type = "norm",
        conf.level = conf.level,
        nboot = nboot,
        k = k,
        stat.title = stat.title,
        messages = messages
      )
  }

  # ============================= plot ====================================

  # if no color fill is to be displayed, set low and high color to white
  if (!isTRUE(fill.gradient)) {
    low.color <- bar.fill
    high.color <- bar.fill
  }

  # preparing the basic layout of the plot based on whether counts or density
  # information is needed

  # only counts
  if (bar.measure %in% c("counts", "n", "count", "N")) {
    plot <- ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes(x = x)
    ) +
      ggplot2::stat_bin(
        col = "black",
        alpha = 0.7,
        binwidth = binwidth,
        na.rm = TRUE,
        mapping = ggplot2::aes(
          y = ..count..,
          fill = ..count..
        )
      ) +
      ggplot2::scale_fill_gradient(
        name = "count",
        low = low.color,
        high = high.color
      )
  }

  # only proportion
  if (bar.measure %in% c("percentage", "perc", "proportion", "prop", "%")) {
    plot <- ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes(x = x)
    ) +
      ggplot2::stat_bin(
        col = "black",
        alpha = 0.7,
        binwidth = binwidth,
        na.rm = TRUE,
        mapping = ggplot2::aes(
          y = ..count.. / sum(..count..),
          fill = ..count.. / sum(..count..)
        )
      ) +
      ggplot2::scale_fill_gradient(
        name = "proportion",
        low = low.color,
        high = high.color,
        labels = percent
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::ylab("proportion")
  }

  # only density
  if (bar.measure == "density") {
    plot <- ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes(x = x)
    ) +
      ggplot2::stat_bin(
        col = "black",
        alpha = 0.7,
        binwidth = binwidth,
        na.rm = TRUE,
        mapping = ggplot2::aes(
          y = ..density..,
          fill = ..density..
        )
      ) +
      ggplot2::scale_fill_gradient(
        name = "density",
        low = low.color,
        high = high.color
      )
  }

  # all things combined
  if (bar.measure %in% c("both", "mix", "all", "everything")) {
    plot <- ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes(x = x)
    ) +
      ggplot2::stat_bin(
        col = "black",
        alpha = 0.7,
        binwidth = binwidth,
        na.rm = TRUE,
        mapping = ggplot2::aes(
          y = ..count..,
          fill = ..count..
        )
      ) +
      ggplot2::scale_fill_gradient(
        name = "count",
        low = low.color,
        high = high.color
      ) +
      ggplot2::scale_y_continuous(
        sec.axis = ggplot2::sec_axis(
          trans = ~ . / nrow(data),
          labels = scales::percent_format(accuracy = 1),
          name = "proportion"
        )
      ) +
      ggplot2::ylab("count") +
      ggplot2::guides(fill = FALSE)
  }

  # ========================== normal curve ==================================

  # if normal curve overlay  needs to be displayed
  if (isTRUE(normal.curve)) {
    # adding normal curve density
    if (bar.measure == "density") {
      plot <- plot +
        ggplot2::stat_function(
          fun = dnorm,
          linetype = normal.curve.linetype,
          color = normal.curve.color,
          size = normal.curve.size,
          na.rm = TRUE,
          args = list(mean = mean(data$x), sd = sd(data$x))
        )
    }

    # adding normal curve count & mix
    if (bar.measure %in% c("both", "mix", "all", "everything", "counts", "n", "count", "N")) {
      plot <- plot +
        ggplot2::stat_function(
          fun = function(x, mean, sd, n, bw) {
            stats::dnorm(x = x, mean = mean, sd = sd) * n * bw
          },
          linetype = normal.curve.linetype,
          color = normal.curve.color,
          size = normal.curve.size,
          na.rm = TRUE,
          args = c(
            mean = mean(data$x),
            sd = sd(data$x),
            n = length(data$x),
            bw = binwidth
          )
        )
    }

    # adding normal curve proportion
    if (bar.measure %in% c("percentage", "perc", "proportion", "prop", "%")) {
      plot <- plot +
        ggplot2::stat_function(
          fun = function(x, mean, sd, n, bw) {
            stats::dnorm(x = x, mean = mean, sd = sd) * bw
          },
          linetype = normal.curve.linetype,
          color = normal.curve.color,
          size = normal.curve.size,
          na.rm = TRUE,
          args = c(
            mean = mean(data$x),
            sd = sd(data$x),
            n = length(data$x),
            bw = binwidth
          )
        )
    }
  }

  # if bayes factor message needs to be displayed
  if (isTRUE(results.subtitle) &&
    type %in% c("parametric", "p") && isTRUE(bf.message)) {
    caption <- bf.caption.text
  }

  # adding the theme and labels
  plot <- plot +
    ggstatsplot::theme_ggstatsplot(
      ggtheme = ggtheme,
      ggstatsplot.layer = ggstatsplot.layer
    ) +
    ggplot2::labs(
      x = xlab,
      title = title,
      subtitle = subtitle,
      caption = caption
    )

  # ====================== centrality line and label ========================

  # computing statistics needed for displaying labels
  y_label_pos <- median(
    x = ggplot2::layer_scales(plot)$y$range$range,
    na.rm = TRUE
  )

  # using custom function for adding labels
  plot <- histo_labeller(
    plot = plot,
    x = data$x,
    y.label.position = y_label_pos,
    centrality.para = centrality.para,
    centrality.color = centrality.color,
    centrality.size = centrality.size,
    centrality.linetype = centrality.linetype,
    centrality.line.labeller = centrality.line.labeller,
    centrality.k = centrality.k,
    test.value = test.value,
    test.value.line = test.value.line,
    test.value.color = test.value.color,
    test.value.size = test.value.size,
    test.value.linetype = test.value.linetype,
    test.line.labeller = test.line.labeller,
    test.k = test.k
  )

  # if no color fill gradient is used, then remove the legend
  if (!isTRUE(fill.gradient)) {
    plot <- plot +
      ggplot2::theme(legend.position = "none")
  }

  # ---------------- adding ggplot component ---------------------------------

  # if any additional modification needs to be made to the plot
  # this is primarily useful for grouped_ variant of this function
  plot <- plot + ggplot.component

  # ============================= messages =================================

  # display normality test result as a message
  if (isTRUE(messages)) {
    normality_message(
      x = data$x,
      lab = xlab,
      k = k,
      output = "message"
    )
  }

  # return the final plot
  return(switch(
    EXPR = return,
    "plot" = plot,
    "subtitle" = subtitle,
    "caption" = caption,
    plot
  ))
}
