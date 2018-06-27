#'
#' @title Pie charts with statistical tests
#' @name ggpiestats
#' @aliases ggpiestats
#' @description Pie charts for categorical data with statistical details
#'   included in the plot as a subtitle.
#' @author Indrajeet Patil
#'
#' @param data The data as a data frame.
#' @param main A string naming the variable to use as the rows in the
#'   contingency table.
#' @param condition A string naming the variable to use as the columns in the
#'   contingency table.
#' @param factor.levels A character vector with labels for factor levels of
#'   `main` variable.
#' @param stat.title Title for the effect being investigated with the chi-square
#'   test. The default is `NULL`, i.e. no title will be added to describe the
#'   effect being shown. An exemple of a stat.title will be `"main x
#'   condition"`.
#' @param title The text for the plot title.
#' @param caption The text for the plot caption.
#' @param sample.size.label Logical that decides whether sample size information
#'   should be displayed for each level of the grouping variable `condition` (Default:
#'   `TRUE`).
#' @param nboot Number of bootstrap samples for computing effect size (Default:
#'   `25`).
#' @param palette If a character string (e.g., `"Set1"`), will use that named
#'   palette. If a number, will index into the list of palettes of appropriate
#'   type. Default palette is `"Dark2"`
#' @param k Number of decimal places expected for results.
#' @param legend.title Title of legend.
#' @param facet.wrap.name The text for the facet_wrap variable label.
#' @param facet.proptest Decides whether proportion test for `main` variable is
#'   to be carried out for each level of `condition` (Default: `TRUE`).
#' @param ggtheme A function, `ggplot2` theme name. Default value is
#'   `ggplot2::theme_bw()`. Allowed values are the official `ggplot2` themes,
#'   including `theme_grey()`, `theme_minimal()`, `theme_classic()`,
#'   `theme_void()`, etc.
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#'
#' @import ggplot2
#'
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr mutate_if
#' @importFrom dplyr desc
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom crayon green
#' @importFrom crayon blue
#' @importFrom crayon yellow
#' @importFrom crayon red
#' @importFrom jmv propTestN
#' @importFrom jmv contTables
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/ggpiestats.html}
#'
#' @examples
#'
#' set.seed(123)
#'
#' # simple function call with the defaults (with condition)
#' ggstatsplot::ggpiestats(data = datasets::mtcars,
#' main = am,
#' condition = cyl)
#'
#' # simple function call with the defaults (without condition)
#' ggstatsplot::ggpiestats(
#' data = datasets::iris,
#' main = Species
#' )
#'
#' @export
#'

# defining the function
ggpiestats <-
  function(data = NULL,
           main,
           condition = NULL,
           factor.levels = NULL,
           stat.title = NULL,
           sample.size.label = TRUE,
           title = NULL,
           caption = NULL,
           nboot = 25,
           palette = "Dark2",
           legend.title = NULL,
           facet.wrap.name = NULL,
           k = 3,
           facet.proptest = TRUE,
           ggtheme = ggplot2::theme_bw(),
           messages = TRUE) {
    # ========================================== messages ==================================================================

    new_palette <- palette
    # if data is not available then don't display any messages
    if (is.null(data)) {
      messages <- FALSE
    }
    # ================================= dataframe =======================================================
    # if dataframe is provided
    if (!is.null(data)) {
      # if condition variables is provided then include it in the dataframe
      if (base::missing(condition)) {
        if (is.null(legend.title)) {
          legend.title <-
            colnames(dplyr::select(.data = data,
                                   !!rlang::enquo(main)))[1]
        }
        # if condition argument is not provided then only include the 'main' argument in dataframe
        data <-
          dplyr::select(.data = data,
                        main = !!rlang::enquo(main))
      } else {
        # preparing labels from given dataframe
        lab.df <- colnames(dplyr::select(
          .data = data,
          !!rlang::enquo(main),
          !!rlang::enquo(condition)
        ))
        # if legend title is not provided, use the variable name for 'main' argument
        if (is.null(legend.title)) {
          legend.title <- lab.df[1]
        }
        # if facetting variable name is not specified, use the variable name for 'condition' argument
        if (is.null(facet.wrap.name)) {
          facet.wrap.name <- lab.df[2]
        }
        # if condition variable is provided, then include it in the dataframe
        data <-
          dplyr::select(
            .data = data,
            main = !!rlang::enquo(main),
            condition = !!rlang::quo_name(rlang::enquo(condition))
          )
      }
    } else {
      if (!is.null(condition)) {
        # if vectors are provided and condition vector is present
        data <-
          base::cbind.data.frame(main = main,
                                 condition = condition)
      } else {
        # if condition vector is absent
        data <-
          base::cbind.data.frame(main = main)
      }
      # if the user hasn't defined the legend.title name, default to the name 'main'
      if (is.null(legend.title)) {
        legend.title <- "main"
      }

      # if the user hasn't defined the facet_wrap name, default to the name 'condition'
      if (is.null(facet.wrap.name)) {
        facet.wrap.name <- "condition"
      }
    }

    # ======================================================== percentage dataframe ======================================================
    #
    # main needs to be a factor for this analysis it is possible that sometimes
    # the variable hasn't been converted to factor class and this will produce
    # an error
    if (is.factor(data$main)) {
      # drop the unused levels of factor
      data %<>%
        dplyr::mutate_at(
          .tbl = .,
          .vars = "main",
          .funs = ~ base::droplevels(x = .)
        )
    } else if (!is.factor(data$main)) {
      # convert to factor
      data$main <- base::as.factor(x = data$main)
      # drop the unused levels of factor
      data %<>%
        dplyr::mutate_at(
          .tbl = .,
          .vars = "main",
          .funs = ~ base::droplevels(x = .)
        )
      # display message
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::red("Warning: "),
          crayon::blue(
            "the argument for `main` was not a factor; converting it to factor"
          )
        ))
      }
    }

    if (!base::missing(condition)) {
      # condition needs to be a factor for this analysis it is possible that sometimes
      # the variable hasn't been converted to factor class and this will produce
      # an error
      if (is.factor(data$condition)) {
        # drop the unused levels of factor
        data %<>%
          dplyr::mutate_at(
            .tbl = .,
            .vars = "condition",
            .funs = ~ base::droplevels(x = .)
          )
      } else if (!is.factor(data$condition)) {
        # convert to factor
        data$condition <- base::as.factor(x = data$condition)
        # drop the unused levels of factor
        data %<>%
          dplyr::mutate_at(
            .tbl = .,
            .vars = "condition",
            .funs = ~ base::droplevels(x = .)
          )
        # display message
        if (isTRUE(messages)) {
          base::message(cat(
            crayon::red("Warning: "),
            crayon::blue(
              "the argument for `condition` was not a factor; converting it to factor"
            )
          ))
        }
      }
    }

    # convert the data into percentages; group by conditional variable if needed
    if (base::missing(condition)) {
      df <-
        data %>%
        dplyr::group_by(.data = ., main) %>%
        dplyr::summarize(.data = ., counts = n()) %>%
        dplyr::mutate(.data = ., perc = (counts / sum(counts)) * 100) %>%
        dplyr::ungroup(x = .) %>%
        dplyr::arrange(.data = ., dplyr::desc(x = main))
    } else {
      df <-
        data %>%
        dplyr::group_by(.data = ., condition, main) %>%
        dplyr::summarize(.data = ., counts = n()) %>%
        dplyr::mutate(.data = ., perc = (counts / sum(counts)) * 100) %>%
        dplyr::ungroup(x = .) %>%
        dplyr::arrange(.data = ., dplyr::desc(x = main))
    }

    # ======================================================== sample size label ======================================================

    # if sample size labels are to be displayed at the bottom of the pie charts
    # for each facet
    if (isTRUE(sample.size.label)) {
      if (!base::missing(condition)) {
        df_n_label <- dplyr::full_join(
          x = df,
          y = df %>%
            dplyr::group_by(.data = ., condition) %>%
            dplyr::summarize(.data = ., total_n = sum(counts)) %>%
            dplyr::ungroup(x = .) %>%
            dplyr::mutate(condition_n_label = paste("(n = ", total_n, ")", sep = "")) %>% # changing character variables into factors
            dplyr::mutate_if(
              .tbl = .,
              .predicate = purrr::is_bare_character,
              .funs = ~ base::as.factor(.)

            ),
          by  = "condition"
        ) %>%
          dplyr::mutate(
            .data = .,
            condition_n_label = dplyr::if_else(
              condition = duplicated(condition),
              true = NA_character_,
              false = as.character(condition_n_label)
            )
          ) %>%
          stats::na.omit(.)
      }
    }

    # ========================================= preparing names for legend and facet_wrap =============================

    # reorder the category factor levels to order the legend
    df$main <- factor(x = df$main,
                      levels = unique(df$main))

    # getting labels for all levels of the 'main' variable factor
    if (is.null(factor.levels)) {
      legend.labels <- as.character(df$main)
    } else if (!missing(factor.levels)) {
      legend.labels <- factor.levels
    }

    # custom labeller function to use if the user wants a different name for facet_wrap variable
    label_facet <- function(original_var, custom_name) {
      lev <- levels(as.factor(original_var))
      lab <- paste0(custom_name, ": ", lev)
      names(lab) <- lev
      return(lab)
    }

    # ======================================================= plot =====================================================

    # if facet_wrap is *not* happening
    if (base::missing(condition)) {
      p <- ggplot2::ggplot(data = df,
                           mapping = ggplot2::aes(x = "", y = counts)) +
        ggplot2::geom_col(
          position = "fill",
          color = "black",
          width = 1,
          ggplot2::aes(fill = factor(get("main")))
        ) +
        ggplot2::geom_label(
          ggplot2::aes(label = paste0(round(perc), "%"),
                       group = factor(get("main"))),
          position = position_fill(vjust = 0.5),
          color = "black",
          size = 5,
          show.legend = FALSE
        ) +
        ggplot2::coord_polar(theta = "y") # convert to polar coordinates
    } else {
      # if facet_wrap *is* happening
      p <- ggplot2::ggplot(data = df,
                           mapping = ggplot2::aes(x = "", y = counts)) +
        ggplot2::geom_col(
          position = "fill",
          color = "black",
          width = 1,
          ggplot2::aes(fill = factor(get("main")))
        ) +
        ggplot2::facet_wrap(
          facets = ~ condition,
          # creating facets and, if necessary, changing the facet_wrap name
          labeller = ggplot2::labeller(
            condition = label_facet(
              original_var = df$condition,
              custom_name = facet.wrap.name
            )
          )
        ) +
        ggplot2::geom_label(
          ggplot2::aes(label = paste0(round(perc), "%"), group = factor(get("main"))),
          position = position_fill(vjust = 0.5),
          color = "black",
          size = 5,
          show.legend = FALSE
        ) +
        ggplot2::coord_polar(theta = "y") # convert to polar coordinates
    }

    # formatting
    p <- p +
      ggplot2::scale_y_continuous(breaks = NULL) +
      ggplot2::scale_fill_brewer(name = "", labels = unique(legend.labels), palette = palette) +
      theme_pie(ggtheme = ggtheme) +
      ggplot2::guides(fill = guide_legend(override.aes = list(color = NA))) # remove black diagonal line from legend

    ############################################ chi-square test #####################################################

    # custom function to write results from chi-square test into subtitle for the plot
    # jmv_chi stands for the chi-square object from package jmv
    # effect is the text label that needs to be entered to denote which interaction effect
    # is being investigated in
    # the chi-square test presented...if not entered, the default will be "Chi-square test"

    chi_subtitle <- function(jmv_chi,
                             cramer_ci,
                             effect = NULL) {
      # if effect label hasn't been specified, use this default
      if (!is.null(effect)) {
        effect <- paste(effect, ": ", sep = "")
      }
      # preparing the subtitle
      base::substitute(
        expr =
          paste(
            y,
            italic(chi) ^ 2,
            "(",
            df,
            ") = ",
            estimate,
            ", ",
            italic("p"),
            " = ",
            pvalue,
            ", ",
            italic(V),
            " = ",
            cramer,
            ", 95% CI [",
            LL,
            ", ",
            UL,
            "]"
          ),
        env = base::list(
          y = effect,
          estimate = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_chi$chiSq)[[2]], k),
          df = as.data.frame(jmv_chi$chiSq)[[3]],
          # df always an integer
          pvalue = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_chi$chiSq)[[4]],
                                                  k,
                                                  p.value = TRUE),
          # select Cramer's V as effect size
          cramer = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_chi$nom)[[4]], k),
          LL = ggstatsplot::specify_decimal_p(x = cramer_ci$conf.low[[1]], k),
          UL = ggstatsplot::specify_decimal_p(x = cramer_ci$conf.high[[1]], k)
        )
      )
    }

    #################################### adding statistical test results ##################################################

    # if facetting by condition is happening
    if (!base::missing(condition)) {
      if (isTRUE(facet.proptest)) {
        # running grouped proportion test with helper functions
        group_prop <- grouped_proptest(data = data,
                                       grouping.vars = condition,
                                       measure = main)
        # merging dataframe containing results from the proportion test with counts and percentage dataframe
        df2 <-
          dplyr::full_join(x = df,
                           y = group_prop,
                           by = "condition") %>%
          dplyr::mutate(
            significance = dplyr::if_else(
              condition = duplicated(condition),
              true = NA_character_,
              false = significance
            )
          ) %>%
          stats::na.omit(.)
      }
      # running Pearson's Chi-square test of independence using jmv::contTables
      jmv_chi <- jmv::contTables(
        data = data,
        rows = "condition",
        cols = "main",
        phiCra = TRUE # provides Phi and Cramer's V, the latter will be displayed
      )
      # preparing Cramer's V object depending on whether V is NaN or not
      # it will be NaN in cases where there are no values of one categorial variable for level of another categorial variable
      if (is.nan(as.data.frame(jmv_chi$nom)[[4]])) {
        # NaN list in case Cramer's V is also NaN
        cramer_ci <- c(NaN, NaN, NaN)
      } else {
        # results for confidence interval of Cramer's V
        cramer_ci <- chisq_v_ci(
          data = data,
          rows = main,
          cols = condition,
          nboot = nboot,
          conf.level = 0.95
        )
      }
      # adding significance labels to pie charts for grouped proportion tests, if expected
      if (isTRUE(facet.proptest)) {
        p <-
          p +
          ggplot2::geom_text(
            data = df2,
            mapping = ggplot2::aes(label = significance, x = 1.65),
            position = ggplot2::position_fill(vjust = 1),
            size = 6,
            na.rm = TRUE
          )
      }

      # adding significance labels to pie charts for grouped proportion tests, if expected
      if (isTRUE(sample.size.label)) {
        p <-
          p +
          ggplot2::geom_text(
            data = df_n_label,
            mapping = ggplot2::aes(label = condition_n_label, x = 1.65),
            position = ggplot2::position_fill(vjust = 0.5),
            size = 5,
            na.rm = TRUE
          )
      }

      # adding chi-square results to the plot subtitle
      p <-
        p + ggplot2::labs(subtitle = chi_subtitle(
          # results from Pearson's chi-square test
          jmv_chi = jmv_chi,
          # effect size (Cramer's V and it's confidence interval)
          cramer_ci = cramer_ci,
          effect = stat.title
        ))
    } else {

      # conducting proportion test with jmv::propTestN()
      jmv_prop <- jmv::propTestN(data = data,
                                 var = "main")
      # if there is no value corresponding to one of the levels of the 'main'
      # variable, then no subtitle is needed
      if (is.nan(as.data.frame(jmv_prop$tests)$chi[[1]])) {
        proptest_subtitle <- NULL
        # display message
        base::message(cat(
          crayon::red("Warning: "),
          crayon::blue("Proportion test will not be run because it requires"),
          crayon::yellow(legend.title),
          crayon::blue("to have at least 2 levels with non-zero frequencies.")
        ))

      } else {

        # preparing proportion test subtitle for the plot
        proptest_subtitle <-
          base::substitute(
            expr =
              paste(
                #"Proportion test : ",
                italic(chi) ^ 2,
                "(",
                df,
                ") = ",
                estimate,
                ", ",
                italic("p"),
                " = ",
                pvalue,
                ", ",
                italic("n"),
                " = ",
                n
              ),
            env = base::list(
              estimate = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_prop$tests)[[1]], k),
              df = base::as.data.frame(jmv_prop$tests)[[2]],
              # df is always an integer
              pvalue = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_prop$tests)[[3]],
                                                      k,
                                                      p.value = TRUE),
              n = nrow(x = data)
            )
          )
      }
      # adding proportion test subtitle to the plot
      p <-
        p +
        ggplot2::labs(subtitle = proptest_subtitle)
    }

    #################################### putting all together ############################################

    # if legend title has not been provided, use the name of the variable corresponding to main
    if (is.null(legend.title)) {
      legend.title <- as.character(df$main)
    }
    # preparing the plot
    p <-
      p +
      ggplot2::labs(title = title,
                    caption = caption) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = legend.title))

    # return the final plot
    return(p)
  }
