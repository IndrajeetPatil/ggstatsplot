#' @title Violin plots for group or condition comparisons in between-subjects
#'   designs repeated across all levels of a grouping variable.
#' @name grouped_ggbetweenstats
#' @description A combined plot of comparison plot created for levels of a
#'   grouping variable.
#' @author Indrajeet Patil, Chuck Powell
#'
#' @param grouping.var A single grouping variable (can be entered either as a
#'   bare name `x` or as a string `"x"`).
#' @param title.prefix Character string specifying the prefix text for the fixed
#'   plot title (name of each factor level) (Default: `NULL`). If `NULL`, the
#'   variable name entered for `grouping.var` will be used.
#' @inheritParams ggbetweenstats
#' @inheritDotParams combine_plots
#'
#' @import ggplot2
#'
#' @importFrom dplyr select bind_rows summarize mutate mutate_at mutate_if
#' @importFrom dplyr group_by n arrange
#' @importFrom rlang !! enquo quo_name ensym
#' @importFrom glue glue
#' @importFrom purrr pmap
#'
#' @seealso \code{\link{ggbetweenstats}}, \code{\link{ggwithinstats}},
#'  \code{\link{grouped_ggwithinstats}}
#'
#' @inherit ggbetweenstats return references
#' @inherit ggbetweenstats return details
#'
#' @examples
#'
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#'
#' # the most basic function call
#' ggstatsplot::grouped_ggbetweenstats(
#'   data = dplyr::filter(ggplot2::mpg, drv != "4"),
#'   x = year,
#'   y = hwy,
#'   grouping.var = drv,
#'   conf.level = 0.99
#' )
#' \donttest{
#' # modifying individual plots using `ggplot.component` argument
#' ggstatsplot::grouped_ggbetweenstats(
#'   data = dplyr::filter(
#'     ggstatsplot::movies_long,
#'     genre %in% c("Action", "Comedy"),
#'     mpaa %in% c("R", "PG")
#'   ),
#'   x = genre,
#'   y = rating,
#'   grouping.var = mpaa,
#'   results.subtitle = FALSE,
#'   ggplot.component = ggplot2::scale_y_continuous(breaks = seq(1, 9, 1)),
#'   messages = FALSE
#' )
#' }
#'
#' @export

# defining the function
grouped_ggbetweenstats <- function(data,
                                   x,
                                   y,
                                   grouping.var,
                                   title.prefix = NULL,
                                   plot.type = "boxviolin",
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
                                   results.subtitle = TRUE,
                                   xlab = NULL,
                                   ylab = NULL,
                                   subtitle = NULL,
                                   stat.title = NULL,
                                   caption = NULL,
                                   sample.size.label = TRUE,
                                   k = 2,
                                   var.equal = FALSE,
                                   conf.level = 0.95,
                                   nboot = 100,
                                   tr = 0.1,
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
                                   point.jitter.width = NULL,
                                   point.jitter.height = 0,
                                   point.dodge.width = 0.60,
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
      .f = ggstatsplot::ggbetweenstats,
      # put common parameters here
      plot.type = plot.type,
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
      results.subtitle = results.subtitle,
      xlab = xlab,
      ylab = ylab,
      subtitle = subtitle,
      stat.title = stat.title,
      caption = caption,
      sample.size.label = sample.size.label,
      k = k,
      var.equal = var.equal,
      conf.level = conf.level,
      nboot = nboot,
      tr = tr,
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
      point.jitter.width = point.jitter.width,
      point.dodge.width = point.dodge.width,
      point.jitter.height = point.jitter.height,
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
