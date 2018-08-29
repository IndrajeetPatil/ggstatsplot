#'
#' @title Pie charts with statistical tests
#' @name ggpiestats
#' @aliases ggpiestats
#' @description Pie charts for categorical data with statistical details
#'   included in the plot as a subtitle.
#' @author Indrajeet Patil
#'
#' @param data The data as a data frame (matrix or tables will not be accepted).
#' @param main The variable to use as the **rows** in the
#'   contingency table.
#' @param condition The variable to use as the **columns** in the contingency
#'   table. This argument is optional (Default: `NULL`). If this argument is
#'   provided, then Pearson's chi-square test of independence will be run. If
#'   not, a goodness of fit test will be run on the `main` variable.
#' @param counts A string naming a variable in data containing counts, or `NULL`
#'   if each row represents a single observation (Default).
#' @param ratio A vector of numbers: the expected proportions for the proportion
#'   test. Default is `NULL`, which means if there are two levels `ratio =
#'   c(1,1)`, etc.
#' @param paired A logical indicating whether to consider the values as paired.
#'   If `paired = FALSE` (default), details from Pearson's chi-square test of independence
#'   will be displayed. If `paired = TRUE`, details from McNemar's test will be
#'   displayed.
#' @param factor.levels A character vector with labels for factor levels of
#'   `main` variable.
#' @param stat.title Title for the effect being investigated with the chi-square
#'   test. The default is `NULL`, i.e. no title will be added to describe the
#'   effect being shown. An example of a `stat.title` argument will be something
#'   like `"main x condition"` or `"interaction"`.
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
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @inheritParams paletteer::scale_fill_paletteer_d
#' @inheritParams theme_ggstatsplot
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
#' @importFrom jmv contTablesPaired
#' @importFrom paletteer scale_fill_paletteer_d
#'
#' @references
#' \url{https://cran.r-project.org/web/packages/ggstatsplot/vignettes/ggpiestats.html}
#'
#' @examples
#'
#' # for reproducibility
#' set.seed(123)
#'
#' # simple function call with the defaults (with condition)
#' ggstatsplot::ggpiestats(
#'   data = datasets::mtcars,
#'   main = am,
#'   condition = cyl,
#'   nboot = 10
#' )
#'
#' # simple function call with the defaults (without condition)
#' ggstatsplot::ggpiestats(
#'   data = iris,
#'   main = Species
#' )
#' @export
#'

# defining the function
ggpiestats <-
  function(data,
             main,
             condition = NULL,
             counts = NULL,
             ratio = NULL,
             paired = FALSE,
             factor.levels = NULL,
             stat.title = NULL,
             sample.size.label = TRUE,
             title = NULL,
             caption = NULL,
             nboot = 25,
             legend.title = NULL,
             facet.wrap.name = NULL,
             k = 3,
             facet.proptest = TRUE,
             ggtheme = ggplot2::theme_bw(),
             ggstatsplot.layer = TRUE,
             package = "RColorBrewer",
             palette = "Dark2",
             direction = 1,
             messages = TRUE) {
    # ================================= extracting column names as labels  =======================================================

    if (base::missing(condition)) {
      # saving the column label for the 'main' variables
      if (is.null(legend.title)) {
        legend.title <-
          colnames(dplyr::select(
            .data = data,
            !!rlang::enquo(main)
          ))[1]
      }
    } else {
      # saving the column labels for the 'main' and the 'condition' variables
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
    }

    # ================================= dataframe ================================================================================

    # creating a dataframe based on which variables are provided
    if (base::missing(condition)) {
      if (base::missing(counts)) {
        data <-
          dplyr::select(
            .data = data,
            main = !!rlang::enquo(main)
          ) %>%
          tibble::as_data_frame(x = .)
      } else {
        data <-
          dplyr::select(
            .data = data,
            main = !!rlang::enquo(main),
            counts = !!rlang::enquo(counts)
          ) %>%
          tibble::as_data_frame(x = .)
      }
    } else {
      if (base::missing(counts)) {
        data <-
          dplyr::select(
            .data = data,
            main = !!rlang::enquo(main),
            condition = !!rlang::quo_name(rlang::enquo(condition))
          ) %>%
          tibble::as_data_frame(x = .)
      } else {
        data <-
          dplyr::select(
            .data = data,
            main = !!rlang::enquo(main),
            condition = !!rlang::quo_name(rlang::enquo(condition)),
            counts = !!rlang::quo_name(rlang::enquo(counts))
          ) %>%
          tibble::as_data_frame(x = .)
      }
    }

    # ======================================================== converting counts ========================================================

    # untable the dataframe based on the count for each obervation
    if (!base::missing(counts)) {
      data %<>% untable(data = ., counts = counts) %>%
        dplyr::select(.data = ., -counts)
    }

    # ======================================================== percentage dataframe ======================================================
    #
    # main and condition need to be a factor for this analysis
    # also drop the unused levels of the factors

    # main
    data %<>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "main",
        .funs = ~base::droplevels(x = base::as.factor(x = .))
      )

    # condition
    if (!base::missing(condition)) {
      data %<>%
        dplyr::mutate_at(
          .tbl = .,
          .vars = "condition",
          .funs = ~base::droplevels(x = base::as.factor(x = .))
        )
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
              .funs = ~base::as.factor(.)
            ),
          by = "condition"
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
      p <- ggplot2::ggplot(
        data = df,
        mapping = ggplot2::aes(x = "", y = counts)
      ) +
        ggplot2::geom_col(
          position = "fill",
          color = "black",
          width = 1,
          ggplot2::aes(fill = factor(get("main")))
        ) +
        ggplot2::geom_label(
          ggplot2::aes(
            label = paste0(round(perc), "%"),
            group = factor(get("main"))
          ),
          position = position_fill(vjust = 0.5),
          color = "black",
          size = 5,
          show.legend = FALSE
        ) +
        ggplot2::coord_polar(theta = "y") # convert to polar coordinates
    } else {
      # if facet_wrap *is* happening
      p <- ggplot2::ggplot(
        data = df,
        mapping = ggplot2::aes(x = "", y = counts)
      ) +
        ggplot2::geom_col(
          position = "fill",
          color = "black",
          width = 1,
          ggplot2::aes(fill = factor(get("main")))
        ) +
        ggplot2::facet_wrap(
          facets = ~condition,
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
      paletteer::scale_fill_paletteer_d(
        package = !!package,
        palette = !!palette,
        direction = direction,
        name = "",
        labels = unique(legend.labels)
      ) +
      theme_pie(ggtheme = ggtheme, ggstatsplot.layer = ggstatsplot.layer) +
      ggplot2::guides(fill = guide_legend(override.aes = list(color = NA))) # remove black diagonal line from legend

    # ===================================== chi-square test (either Pearson or McNemar) =====================================

    # if facetting by condition is happening
    if (!base::missing(condition)) {
      if (isTRUE(facet.proptest)) {
        # merging dataframe containing results from the proportion test with counts and percentage dataframe
        df2 <-
          dplyr::full_join(
            x = df,
            # running grouped proportion test with helper functions
            y = grouped_proptest(
              data = data,
              grouping.vars = condition,
              measure = main
            ),
            by = "condition"
          ) %>%
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
      if (!isTRUE(paired)) {
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
      } else if (isTRUE(paired)) {
        # carrying out McNemar's test
        jmv_chi <- jmv::contTablesPaired(
          data = data,
          rows = "condition",
          cols = "main",
          counts = NULL,
          chiSq = TRUE,
          chiSqCorr = FALSE,
          exact = FALSE,
          pcRow = FALSE,
          pcCol = FALSE
        )
      }

      # ========================================================== proportion test ============================================

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
      if (!isTRUE(paired)) {
        p <-
          p + ggplot2::labs(subtitle = chi_subtitle(
            # results from Pearson's chi-square test
            jmv_chi = jmv_chi,
            # effect size (Cramer's V and it's confidence interval)
            cramer_ci = cramer_ci,
            effect = stat.title,
            k = k
          ))
      } else if (isTRUE(paired)) {
        p <-
          p + ggplot2::labs(subtitle = mcnemar_subtitle(
            # results from McNemar test
            jmv_chi = jmv_chi,
            effect = stat.title,
            k = k
          ))
      }
    } else {
      # conducting proportion test with jmv::propTestN()
      jmv_prop <- jmv::propTestN(
        data = data,
        var = "main",
        ratio = ratio
      )
      # if there is no value corresponding to one of the levels of the 'main'
      # variable, then no subtitle is needed
      if (is.nan(as.data.frame(jmv_prop$tests)$chi[[1]])) {
        proptest_subtitle <-
          base::substitute(
            expr =
              paste(
                italic("n"),
                " = ",
                n
              ),
            env = base::list(n = nrow(x = data))
          )
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
                italic(chi)^2,
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
              pvalue = ggstatsplot::specify_decimal_p(
                x = as.data.frame(jmv_prop$tests)[[3]],
                k,
                p.value = TRUE
              ),
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
      ggplot2::labs(
        x = NULL,
        y = NULL,
        title = title,
        caption = caption
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = legend.title))

    # return the final plot
    return(p)
  }
