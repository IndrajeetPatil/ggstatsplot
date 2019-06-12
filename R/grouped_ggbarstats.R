#' @title Grouped bar (column) charts with statistical tests
#' @name grouped_ggbarstats
#' @description Helper function for `ggstatsplot::ggbarstats` to apply this
#'   function across multiple levels of a given factor and combining the
#'   resulting plots using `ggstatsplot::combine_plots`.
#' @author Indrajeet Patil, Chuck Powell
#'
#' @inheritDotParams combine_plots
#' @inheritParams ggbarstats
#' @inheritParams grouped_ggbetweenstats
#'
#' @import ggplot2
#'
#' @importFrom dplyr select bind_rows summarize mutate mutate_at mutate_if
#' @importFrom dplyr group_by n arrange
#' @importFrom rlang !! enquo quo_name ensym
#' @importFrom glue glue
#' @importFrom purrr map set_names
#'
#' @seealso \code{\link{ggbarstats}}, \code{\link{ggpiestats}},
#'  \code{\link{grouped_ggpiestats}}
#'
#' @inherit ggbarstats return references
#' @inherit ggbarstats return details
#' @inherit ggbarstats return return
#'
#' @examples
#'
#' \dontrun{
#' # with condition and with count data
#' library(jmv)
#'
#' ggstatsplot::grouped_ggbarstats(
#'   data = as.data.frame(HairEyeColor),
#'   main = Hair,
#'   condition = Eye,
#'   counts = Freq,
#'   grouping.var = Sex
#' )
#'
#' # the following will take slightly more amount of time
#' # for reproducibility
#' set.seed(123)
#'
#' # let's create a smaller dataframe
#' diamonds_short <- ggplot2::diamonds %>%
#'   dplyr::filter(.data = ., cut %in% c("Very Good", "Ideal")) %>%
#'   dplyr::filter(.data = ., clarity %in% c("SI1", "SI2", "VS1", "VS2")) %>%
#'   dplyr::sample_frac(tbl = ., size = 0.05)
#'
#' # plot
#' ggstatsplot::grouped_ggbarstats(
#'   data = diamonds_short,
#'   main = color,
#'   condition = clarity,
#'   grouping.var = cut,
#'   sampling.plan = "poisson",
#'   title.prefix = "Quality",
#'   bar.label = "both",
#'   messages = FALSE,
#'   perc.k = 1,
#'   nrow = 2
#' )
#' }
#' @export

# defining the function
grouped_ggbarstats <- function(data,
                               main,
                               condition,
                               counts = NULL,
                               grouping.var,
                               title.prefix = NULL,
                               ratio = NULL,
                               paired = FALSE,
                               results.subtitle = TRUE,
                               labels.legend = NULL,
                               stat.title = NULL,
                               sample.size.label = TRUE,
                               label.separator = " ",
                               label.text.size = 4,
                               label.fill.color = "white",
                               label.fill.alpha = 1,
                               bar.outline.color = "black",
                               bf.message = TRUE,
                               sampling.plan = "indepMulti",
                               fixed.margin = "rows",
                               prior.concentration = 1,
                               subtitle = NULL,
                               caption = NULL,
                               legend.position = "right",
                               x.axis.orientation = NULL,
                               conf.level = 0.95,
                               nboot = 100,
                               simulate.p.value = FALSE,
                               B = 2000,
                               legend.title = NULL,
                               xlab = NULL,
                               ylab = "Percent",
                               k = 2,
                               perc.k = 0,
                               bar.label = "percentage",
                               data.label = NULL,
                               bar.proptest = TRUE,
                               ggtheme = ggplot2::theme_bw(),
                               ggstatsplot.layer = TRUE,
                               package = "RColorBrewer",
                               palette = "Dark2",
                               direction = 1,
                               ggplot.component = NULL,
                               return = "plot",
                               messages = TRUE,
                               ...) {

  # ======================== check user input =============================

  # create a list of function call to check
  param_list <- as.list(match.call())

  # check that there is a grouping.var
  if (!"grouping.var" %in% names(param_list)) {
    stop("You must specify a grouping variable")
  }

  # check that conditioning and grouping.var are different
  if ("condition" %in% names(param_list)) {
    if (as.character(param_list$condition) == as.character(param_list$grouping.var)) {
      message(cat(
        crayon::red("\nError: "),
        crayon::blue(
          "Identical variable (",
          crayon::yellow(param_list$condition),
          ") was used for both grouping and conditioning, which is not allowed.\n"
        ),
        sep = ""
      ))
      return(invisible(param_list$condition))
    }
  }

  # ensure the grouping variable works quoted or unquoted
  grouping.var <- rlang::ensym(grouping.var)

  # if `title.prefix` is not provided, use the variable `grouping.var` name
  if (is.null(title.prefix)) {
    title.prefix <- rlang::as_name(grouping.var)
  }

  # ======================== preparing dataframe =============================

  # creating a dataframe
  df <-
    dplyr::select(
      .data = data,
      !!rlang::enquo(grouping.var),
      !!rlang::enquo(main),
      !!rlang::enquo(condition),
      !!rlang::enquo(counts)
    ) %>%
    tidyr::drop_na(data = .)

  # creating a list for grouped analysis
  df %<>%
    grouped_list(data = ., grouping.var = !!rlang::enquo(grouping.var))

  # ============== build pmap list based on conditions =====================

  if (missing(counts)) {
    flexiblelist <- list(
      data = df,
      main = rlang::quo_text(ensym(main)),
      condition = rlang::quo_text(ensym(condition)),
      title = glue::glue("{title.prefix}: {names(df)}")
    )
  }

  if (!missing(counts)) {
    flexiblelist <- list(
      data = df,
      main = rlang::quo_text(ensym(main)),
      condition = rlang::quo_text(ensym(condition)),
      counts = rlang::quo_text(ensym(counts)),
      title = glue::glue("{title.prefix}: {names(df)}")
    )
  }

  # ==================== creating a list of plots =======================

  # creating a list of plots using `pmap`
  plotlist_purrr <-
    purrr::pmap(
      .l = flexiblelist,
      .f = ggstatsplot::ggbarstats,
      # put common parameters here
      ratio = ratio,
      paired = paired,
      results.subtitle = results.subtitle,
      labels.legend = labels.legend,
      stat.title = stat.title,
      sample.size.label = sample.size.label,
      label.separator = label.separator,
      label.text.size = label.text.size,
      label.fill.color = label.fill.color,
      label.fill.alpha = label.fill.alpha,
      bar.outline.color = bar.outline.color,
      bf.message = bf.message,
      sampling.plan = sampling.plan,
      fixed.margin = fixed.margin,
      prior.concentration = prior.concentration,
      subtitle = subtitle,
      caption = caption,
      legend.position = legend.position,
      x.axis.orientation = x.axis.orientation,
      conf.level = conf.level,
      nboot = nboot,
      simulate.p.value = simulate.p.value,
      B = B,
      legend.title = legend.title,
      xlab = xlab,
      ylab = ylab,
      k = k,
      perc.k = perc.k,
      data.label = data.label,
      bar.label = bar.label,
      bar.proptest = bar.proptest,
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
