#' @title Bar(column) charts with statistical tests
#' @name ggbarstats
#' @aliases ggbarstats
#' @description Bar charts for categorical data with statistical details
#'   included in the plot as a subtitle.
#' @author Chuck Powell
#'
#' @param factor.levels A character vector with labels for factor levels of
#'   `main` variable.
#' @param title The text for the plot title.
#' @param caption The text for the plot caption.
#' @param legend.position The position of the legend
#'   "none", "left", "right", "bottom", "top" (Default: `"bottom"`).
#' @param x.axis.orientation The orientation of the x axis labels one of
#'   "slant" or "vertical" to change from the default horizontal
#'   orientation (Default: `NULL` which is horizontal).
#' @param sample.size.label Logical that decides whether sample size information
#'   should be displayed for each level of the grouping variable `condition`
#'   (Default: `TRUE`).
#' @param palette If a character string (e.g., `"Set1"`), will use that named
#'   palette. If a number, will index into the list of palettes of appropriate
#'   type. Default palette is `"Dark2"`.
#' @param facet.wrap.name The text for the facet_wrap variable label.
#' @param facet.proptest Decides whether proportion test for `main` variable is
#'   to be carried out for each level of `condition` (Default: `FALSE`).
#' @param perc.k Numeric that decides number of decimal places for percentage
#'   labels (Default: `0`).
#' @param label.text.size Numeric that decides size for bar labels
#'   (Default: `4`).
#' @param label.fill.color Character that specifies fill color for bar labels
#'   (Default: `white`).
#' @param label.fill.alpha Numeric that specifies fill color transparency or
#'   `"alpha"` for bar labels (Default: `1` range 0 to 1).
#' @param slice.label Character decides what information needs to be displayed
#'   on the label in each sub-divsion of the vertical bar. Possible options are `"percentage"`
#'   (default), `"counts"`, `"both"`.
#' @param bf.message Logical that decides whether to display a caption with
#'   results from bayes factor test in favor of the null hypothesis (default:
#'   `FALSE`).
#' @param ggplot.component A `ggplot` component to be added to the plot prepared
#'   by `ggstatsplot`. This argument is primarily helpful for `grouped_` variant
#'   of the current function. Default is `NULL`. The argument should be entered
#'   as a function. If the given function has an argument `axes.range.restrict`
#'   and if it has been set to `TRUE`, the added ggplot component *might* not
#'   work as expected.
#' @inheritParams bf_contingency_tab
#' @inheritParams subtitle_contingency_tab
#' @inheritParams subtitle_onesample_proptest
#' @inheritParams paletteer::scale_fill_paletteer_d
#' @inheritParams theme_ggstatsplot
#'
#' @import ggplot2
#'
#' @importFrom dplyr select group_by summarize n arrange if_else desc
#' @importFrom dplyr mutate mutate_at mutate_if
#' @importFrom rlang !! enquo quo_name
#' @importFrom crayon green blue yellow red
#' @importFrom paletteer scale_fill_paletteer_d
#' @importFrom groupedstats grouped_proptest
#' @importFrom tidyr uncount
#' @importFrom tibble as_tibble
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggbarstats.html}
#'
#' @return Unlike a number of statistical softwares, `ggstatsplot` doesn't
#'   provide the option for Yates' correction for the Pearson's chi-squared
#'   statistic. This is due to compelling amount of Monte-Carlo simulation
#'   research which suggests that the Yates' correction is overly conservative,
#'   even in small sample sizes. As such it is recommended that it should not
#'   ever be applied in practice (Camilli & Hopkins, 1978, 1979; Feinberg, 1980;
#'   Larntz, 1978; Thompson, 1988). For more, see-
#'   \url{http://www.how2stats.net/2011/09/yates-correction.html}
#'
#' @examples
#'
#' # for reproducibility
#' set.seed(123)
#'
#' # simple function call with the defaults (with condition)
#' ggstatsplot::ggbarstats(
#'   data = datasets::mtcars,
#'   main = vs,
#'   condition = cyl,
#'   bf.message = TRUE,
#'   nboot = 10,
#'   factor.levels = c("0 = V-shaped", "1 = straight"),
#'   legend.title = "Engine"
#' )
#'
#' # simple function call with the defaults (without condition; with count data)
#' library(jmv)
#'
#' ggstatsplot::ggbarstats(
#'   data = as.data.frame(HairEyeColor),
#'   main = Eye,
#'   condition = Hair,
#'   counts = Freq
#' )
#' @export

# defining the function
ggbarstats <- function(data,
                       main,
                       condition = NULL,
                       counts = NULL,
                       ratio = NULL, #does nothing
                       paired = FALSE,
                       factor.levels = NULL,
                       stat.title = NULL,
                       sample.size.label = TRUE,
                       label.text.size = 4,
                       label.fill.color = "white",
                       label.fill.alpha = 1,
                       bf.message = FALSE,
                       sampling.plan = "jointMulti",
                       fixed.margin = "rows",
                       prior.concentration = 1,
                       title = NULL,
                       caption = NULL,
                       legend.position = "bottom",
                       x.axis.orientation = NULL,
                       conf.level = 0.95,
                       nboot = 25,
                       simulate.p.value = FALSE,
                       B = 2000,
                       legend.title = NULL,
                       facet.wrap.name = NULL,
                       k = 2,
                       perc.k = 0,
                       slice.label = "percentage",
                       facet.proptest = FALSE,
                       ggtheme = ggplot2::theme_bw(),
                       ggstatsplot.layer = TRUE,
                       package = "RColorBrewer",
                       palette = "Dark2",
                       direction = 1,
                       ggplot.component = NULL,
                       messages = TRUE) {

  # ================= extracting column names as labels  =====================

  if (base::missing(condition)) {
    stop("You must specify a condition variable")
  }
  # if legend title is not provided, use the variable name for 'main'
  # argument
  if (is.null(legend.title)) {
    legend.title <- rlang::as_name(rlang::ensym(main))
  }
  # if facetting variable name is not specified, use the variable name for
  # 'condition' argument
  if (is.null(facet.wrap.name)) {
    facet.wrap.name <- rlang::as_name(rlang::ensym(condition))
  }


  # =============================== dataframe ================================

  # creating a dataframe based on whether counts
  if (base::missing(counts)) {
    data <-
      dplyr::select(
        .data = data,
        main = !!rlang::enquo(main),
        condition = !!rlang::quo_name(rlang::enquo(condition))
      )
  } else {
    data <-
      dplyr::select(
        .data = data,
        main = !!rlang::enquo(main),
        condition = !!rlang::quo_name(rlang::enquo(condition)),
        counts = !!rlang::quo_name(rlang::enquo(counts))
      )
    data %<>%
      tidyr::uncount(
        data = .,
        weights = counts,
        .remove = TRUE,
        .id = "id"
      )
  }

  # ============================ percentage dataframe ========================

  # main and condition need to be a factor for this analysis
  # also drop the unused levels of the factors

  data %<>%
    dplyr::mutate(.data = ., main = droplevels(as.factor(main))) %>%
    dplyr::filter(.data = ., !is.na(main)) %>%
    dplyr::mutate(.data = ., condition = droplevels(as.factor(condition))) %>%
    dplyr::filter(.data = ., !is.na(condition))

  # converting to tibble
  data %<>%
    tibble::as_tibble(x = .)

  df <-
    data %>%
    dplyr::group_by(.data = ., condition, main) %>%
    dplyr::summarize(.data = ., counts = n()) %>%
    dplyr::mutate(.data = ., perc = (counts / sum(counts)) * 100) %>%
    dplyr::ungroup(x = .) %>%
    dplyr::arrange(.data = ., dplyr::desc(x = main))


  # checking what needs to be displayed on pie slices as labels also decide on
  # the text size for the label; if both counts and percentages are going to
  # be displayed, then use a bit smaller text size
  if (slice.label == "percentage") {
    # only percentage
    df %<>%
      dplyr::mutate(
        .data = .,
        slice.label = paste0(round(x = perc, digits = perc.k), "%")
      )

  } else if (slice.label == "counts") {
    # only raw counts
    df %<>%
      dplyr::mutate(
        .data = .,
        slice.label = paste0("n = ", counts)
      )

  } else if (slice.label == "both") {
    # both raw counts and percentages
    df %<>%
      dplyr::mutate(
        .data = .,
        slice.label = paste0(
          round(x = perc, digits = perc.k),
          "% (",
          counts, ")"
        )
      )
  }

#  return(df)

  # ============================ sample size label ==========================

  # if labels are to be displayed on the bottom of the charts
  # for each facet
  if (isTRUE(sample.size.label)) {
    df_n_label <- data %>%
      group_by(condition) %>%
      summarize(N = n()) %>% mutate(N = paste0("n = ",N))
  }

#  return(df_n_label)

  # ================= preparing names for legend and facet_wrap ==============

  # reorder the category factor levels to order the legend
  df$main <- factor(
    x = df$main,
    levels = unique(df$main)
  )

  # getting labels for all levels of the 'main' variable factor
  if (is.null(factor.levels)) {
    legend.labels <- as.character(df$main)
  } else if (!missing(factor.levels)) {
    legend.labels <- factor.levels
  }

  # custom labeller function to use if the user wants a different name for
  # facet_wrap variable
  label_facet <- function(original_var, custom_name) {
    lev <- levels(x = as.factor(original_var))
    lab <- paste0(custom_name, ": ", lev)
    names(lab) <- lev
    return(lab)
  }

  # =================================== plot =================================
#  return(df)
  # if no. of factor levels is greater than the default palette color count
  palette_message(
    package = package,
    palette = palette,
    min_length = length(unique(levels(data$main)))[[1]]
  )

  p <- ggplot(data = df,
              aes(fill=main, y=perc, x=condition)) +
              geom_bar(stat="identity", position="fill", color = "gray") +
              ylab("Percent") +
              xlab(facet.wrap.name) +
              scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.10)) +
              geom_label(aes(label = slice.label, group = main),
                        show.legend = FALSE,
                        position = position_fill(vjust = 0.5),
                        size = label.text.size,
                        fill = label.fill.color,
                        alpha = label.fill.alpha
                        ) +
              ggstatsplot::theme_mprl(
                        ggtheme = ggtheme,
                        ggstatsplot.layer = ggstatsplot.layer
               ) +
              theme(panel.grid.major.x = element_blank(),
                    legend.position = legend.position) +
              ggplot2::guides(fill = ggplot2::guide_legend(title = legend.title))


  p <- p +
    paletteer::scale_fill_paletteer_d(
      package = !!package,
      palette = !!palette,
      direction = direction,
      name = "",
      labels = unique(legend.labels)
      )


#  return(p)
#  return(df)


  # =============== chi-square test (either Pearson or McNemar) =============

  # if facetting by condition is happening

    if (isTRUE(facet.proptest)) {
      # merging dataframe containing results from the proportion test with
      # counts and percentage dataframe
      df2 <-
        dplyr::full_join(
          x = df,
          # running grouped proportion test with helper functions
          y = groupedstats::grouped_proptest(
            data = data,
            grouping.vars = condition,
            measure = main
          ),
          by = "condition"
        ) %>%
        dplyr::mutate(
          .data = .,
          significance = dplyr::if_else(
            condition = duplicated(condition),
            true = NA_character_,
            false = significance
          )
        ) %>%
        dplyr::filter(.data = ., !is.na(significance))

      # display grouped proportion test results
      if (isTRUE(messages)) {
        # tell the user what these results are
        base::message(cat(
          crayon::green("Note: "),
          crayon::blue("Results from faceted one-sample proportion tests:\n"),
          sep = ""
        ))

        # print the tibble and leave out unnecessary columns
        print(tibble::as_tibble(df2) %>%
          dplyr::select(.data = ., -c(main:slice.label)))
      }
    }

#  return(p)

    # running approprate statistical test
    # unpaired: Pearson's Chi-square test of independence
    subtitle <-
      subtitle_contingency_tab(
        data = data,
        main = main,
        condition = condition,
        nboot = nboot,
        paired = paired,
        stat.title = stat.title,
        conf.level = conf.level,
        conf.type = "norm",
        simulate.p.value = simulate.p.value,
        B = B,
        messages = messages,
        k = k
      )

    # preparing the BF message for null hypothesis support
    if (isTRUE(bf.message)) {
      bf.caption.text <-
        bf_contingency_tab(
          data = data,
          main = main,
          condition = condition,
          sampling.plan = sampling.plan,
          fixed.margin = fixed.margin,
          prior.concentration = prior.concentration,
          caption = caption,
          output = "caption",
          k = k
        )
    }

    # if bayes factor message needs to be displayed
    if (isTRUE(bf.message)) {
      caption <- bf.caption.text
    }

    # ====================== facetted proportion test =======================

    # adding significance labels to pie charts for grouped proportion tests
    if (isTRUE(facet.proptest)) {
      p <-
        p +
        ggplot2::geom_text(data = df2,
                aes(x = condition, y = -0.10, label = significance, fill = NULL),
                size = 3,
                fontface = "bold"
                )
    }

    # adding sample size info
    if (isTRUE(sample.size.label)) {
      p <-
        p +
        ggplot2::geom_text(data = df_n_label,
                aes(x = condition, y = -0.05, label = N, fill = NULL),
                size = 3)
    }


  # =========================== putting all together ========================
  # if we need to modify x axis orientation
    if (!base::missing(x.axis.orientation)) {
      if (x.axis.orientation == "slant") {
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text( angle = 45, vjust = 1, hjust= 1, face = "bold"))
      } else if (x.axis.orientation == "vertical") {
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text( angle = 90, vjust = 0.5, hjust= 1, face = "bold"))
      }
    }

  # preparing the plot
  p <-
    p +
    ggplot2::labs(
      subtitle = subtitle,
      title = title,
      caption = caption
    )

    # ---------------- adding ggplot component ---------------------------------

    # if any additional modification needs to be made to the plot
    # this is primarily useful for grouped_ variant of this function
    p <- p + ggplot.component


  # return the final plot
  return(p)
}
