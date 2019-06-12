#' @title Violin plots for group or condition comparisons in within-subjects
#'   designs repeated across all levels of a grouping variable.
#' @name grouped_ggwithinstats
#' @description A combined plot of comparison plot created for levels of a
#'   grouping variable.
#' @author Indrajeet Patil, Chuck Powell
#'
#' @inheritParams ggwithinstats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams combine_plots
#'
#' @import ggplot2
#'
#' @importFrom dplyr select bind_rows summarize mutate mutate_at mutate_if
#' @importFrom dplyr group_by n arrange
#' @importFrom rlang !! enquo quo_name ensym !!!
#' @importFrom glue glue
#' @importFrom purrr pmap
#'
#' @seealso \code{\link{ggwithinstats}}, \code{\link{ggbetweenstats}},
#' \code{\link{grouped_ggbetweenstats}}
#'
#' @inherit ggwithinstats return references
#' @inherit ggwithinstats return details
#'
#' @examples
#'
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # the most basic function call
#' ggstatsplot::grouped_ggwithinstats(
#'   data = VR_dilemma,
#'   x = modality,
#'   y = score,
#'   grouping.var = order,
#'   messages = TRUE
#' )
#' @export

# defining the function
grouped_ggwithinstats <- function(data,
                                  x,
                                  y,
                                  grouping.var,
                                  title.prefix = NULL,
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
                                  subtitle = NULL,
                                  caption = NULL,
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
                                  outlier.label = NULL,
                                  outlier.label.color = "black",
                                  outlier.color = "black",
                                  outlier.shape = 19,
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
                                  return = "plot",
                                  messages = TRUE,
                                  ...) {

  # =================== check user input and prep =========================

  # create a list of function call to check
  param_list <- as.list(match.call())

  # check that there is a grouping.var
  if (!"grouping.var" %in% names(param_list)) {
    stop("You must specify a grouping variable")
  }

  # check that conditioning and grouping.var are different
  if (as.character(param_list$x) == as.character(param_list$grouping.var)) {
    message(cat(
      crayon::red("\nError: "),
      crayon::blue(
        "Identical variable (",
        crayon::yellow(param_list$x),
        ") was used for both grouping and x axis, which is not allowed.\n"
      ),
      sep = ""
    ))
    return(invisible(param_list$x))
  }

  # ensure the grouping variable works quoted or unquoted
  grouping.var <- rlang::ensym(grouping.var)

  # if `title.prefix` is not provided, use the variable `grouping.var` name
  if (is.null(title.prefix)) {
    title.prefix <- rlang::as_name(grouping.var)
  }

  # ======================== preparing dataframe ==========================

  # creating a dataframe
  df <-
    dplyr::select(
      .data = data,
      !!rlang::enquo(x),
      !!rlang::enquo(y),
      !!rlang::enquo(grouping.var),
      !!rlang::enquo(outlier.label)
    ) %>%
    tidyr::drop_na(data = .)

  # creating a list for grouped analysis
  df %<>%
    grouped_list(data = ., grouping.var = !!rlang::enquo(grouping.var))

  # ============== build pmap list based on conditions =====================

  if (!"outlier.tagging" %in% names(param_list) || isFALSE(outlier.tagging)) {
    flexiblelist <- list(
      data = df,
      x = rlang::quo_text(rlang::ensym(x)),
      y = rlang::quo_text(rlang::ensym(y)),
      title = glue::glue("{title.prefix}: {names(df)}")
    )
  }

  if (isTRUE(outlier.tagging) && !"outlier.label" %in% names(param_list)) {
    flexiblelist <- list(
      data = df,
      x = rlang::quo_text(rlang::ensym(x)),
      y = rlang::quo_text(rlang::ensym(y)),
      outlier.tagging = TRUE,
      title = glue::glue("{title.prefix}: {names(df)}")
    )
  }

  if (isTRUE(outlier.tagging) && "outlier.label" %in% names(param_list)) {
    flexiblelist <- list(
      data = df,
      x = rlang::quo_text(rlang::ensym(x)),
      y = rlang::quo_text(rlang::ensym(y)),
      outlier.label = rlang::quo_text(rlang::ensym(outlier.label)),
      outlier.tagging = TRUE,
      title = glue::glue("{title.prefix}: {names(df)}")
    )
  }

  # ============== creating a list of plots using `pmap`=======================

  plotlist_purrr <-
    purrr::pmap(
      .l = flexiblelist,
      .f = ggstatsplot::ggwithinstats,
      # put common parameters here
      type = type,
      pairwise.comparisons = pairwise.comparisons,
      pairwise.annotation = pairwise.annotation,
      pairwise.display = pairwise.display,
      p.adjust.method = p.adjust.method,
      effsize.type = effsize.type,
      partial = partial,
      effsize.noncentral = effsize.noncentral,
      bf.prior = bf.prior,
      bf.message = bf.message,
      sphericity.correction = sphericity.correction,
      results.subtitle = results.subtitle,
      xlab = xlab,
      ylab = ylab,
      subtitle = subtitle,
      caption = caption,
      sample.size.label = sample.size.label,
      k = k,
      conf.level = conf.level,
      nboot = nboot,
      tr = tr,
      path.point = path.point,
      path.mean = path.mean,
      sort = sort,
      sort.fun = sort.fun,
      axes.range.restrict = axes.range.restrict,
      mean.label.size = mean.label.size,
      mean.label.fontface = mean.label.fontface,
      mean.label.color = mean.label.color,
      notch = notch,
      notchwidth = notchwidth,
      linetype = linetype,
      outlier.label.color = outlier.label.color,
      outlier.color = outlier.color,
      outlier.shape = outlier.shape,
      outlier.coef = outlier.coef,
      mean.plotting = mean.plotting,
      mean.ci = mean.ci,
      mean.size = mean.size,
      mean.color = mean.color,
      ggtheme = ggtheme,
      ggstatsplot.layer = ggstatsplot.layer,
      package = package,
      palette = palette,
      direction = direction,
      ggplot.component = ggplot.component,
      return = return,
      messages = messages
    )

  # combining the list of plots into a single plot
  if (return == "plot") {
    combined_object <-
      ggstatsplot::combine_plots(
        plotlist = plotlist_purrr,
        ...
      )

    # inform user this can't be modified further with ggplot commands
    if (isTRUE(messages)) {
      grouped_message()
    }
  } else {
    combined_object <- plotlist_purrr
  }

  # return the combined plot
  return(combined_object)
}
