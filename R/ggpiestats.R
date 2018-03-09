#'
#' @title pie charts with statistical tests
#' @name ggpiestats
#' @aliases ggpiestats
#' @description Pie charts for categorical data with statistical details included in the plot as a subtitle.
#' @author Indrajeet Patil
#'
#' @param data The data as a data frame.
#' @param main A string naming the variable to use as the rows in the contingency table.
#' @param condition A string naming the variable to use as the columns in the contingency table.
#' @param factor.levels A character vector with labels for factor levels of `main` variable.
#' @param stat.title Title for the effect being investigated with the chi-square test.
#' @param title The text for the plot title.
#' @param caption The text for the plot caption.
#' @param k Number of decimal places expected for results.
#' @param legend.title Title of legend.
#' @param facet.wrap.name The text for the facet_wrap variable label.
#'
#' @import ggplot2
#' @import dplyr
#' @import rlang
#'
#' @importFrom crayon green
#' @importFrom crayon blue
#' @importFrom crayon yellow
#' @importFrom crayon red
#' @importFrom jmv propTestN
#' @importFrom jmv contTables
#' @importFrom DescTools CramerV
#'
#' @examples
#' library(datasets)
#' ggpiestats(data = mtcars, main = am, condition = cyl, factor.levels = c("zero", "one"),
#' stat.title = "interaction", title = "Dataset: mtcars", caption = "This is a demo",
#' legend.title = "transmission", facet.wrap.name = "No. of cylinders")
#'
#' @export
#'

# defining global variables and functions to quient the R CMD check notes
utils::globalVariables(
  c(
    "U",
    "V",
    "Z",
    "chi",
    "counts",
    "df",
    "df1",
    "df2",
    "effsize",
    "estimate",
    "eta",
    "omega",
    "perc",
    "phicoeff",
    "pvalue",
    "r",
    "rho",
    "xi",
    "y",
    "z_value",
    "italic",
    "rsubtitle",
    "stats_subtitle",
    "chi_subtitle",
    "proptest_subtitle"
  )
)

# defining the function
ggpiestats <-
  function(data = NULL,
           main,
           condition = NULL,
           factor.levels = NULL,
           stat.title = NULL,
           title = NULL,
           caption = NULL,
           legend.title = NULL,
           facet.wrap.name = NULL,
           k = 3) {
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

    # convert the data into percentages; group by conditional variable if needed
    if (base::missing(condition)) {
      df <-
        data %>%
        dplyr::group_by(.data = ., main) %>%
        dplyr::summarize(.data = ., counts = n()) %>%
        dplyr::mutate(.data = ., perc = (counts / sum(counts)) * 100) %>%
        dplyr::arrange(desc(perc))
    } else {
      df <-
        data %>%
        dplyr::group_by(.data = ., condition, main) %>%
        dplyr::summarize(.data = ., counts = n()) %>%
        dplyr::mutate(.data = ., perc = (counts / sum(counts)) * 100) %>%
        dplyr::arrange(.data = ., desc(perc))
    }

    # ========================================= preparing names for legend and facet_wrap =============================

    # reorder the category factor levels to order the legend
    df$main <- factor(x = df$main,
                      levels = unique(df$main))

    # getting labels for all levels of the 'main' variable factor
    if (is.null(factor.levels)) {
      labels <- as.character(df$main)
    } else {
      labels <- factor.levels
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
                           mapping = aes(x = "", y = counts)) +
        geom_col(
          position = "fill",
          color = "black",
          width = 1,
          aes(fill = factor(get("main")))
        ) +
        geom_label(
          aes(label = paste0(round(perc), "%"),
              group = factor(get("main"))),
          position = position_fill(vjust = 0.5),
          color = "black",
          size = 5,
          show.legend = FALSE
        ) +
        coord_polar(theta = "y") # convert to polar coordinates
    } else {
      # if facet_wrap *is* happening
      p <- ggplot2::ggplot(data = df,
                           mapping = aes(x = "", y = counts)) +
        geom_col(
          position = "fill",
          color = "black",
          width = 1,
          aes(fill = factor(get("main")))
        ) +
        facet_wrap(facets = ~ condition,
                   # creating facets and, if necessary, changing the facet_wrap name
                   labeller = labeller(
                     condition = label_facet(
                       original_var = df$condition,
                       custom_name = facet.wrap.name
                     )
                   )) +
        geom_label(
          aes(label = paste0(round(perc), "%"), group = factor(get("main"))),
          position = position_fill(vjust = 0.5),
          color = "black",
          size = 5,
          show.legend = FALSE
        ) +
        coord_polar(theta = "y") # convert to polar coordinates
    }

    # formatting
    p <- p +
      scale_y_continuous(breaks = NULL) +
      scale_fill_discrete(name = "", labels = unique(labels)) +
      theme_grey() +
      theme(
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.title.align = 0.5,
        legend.text.align = 0.5,
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, "lines"),
        legend.margin = margin(5, 5, 5, 5),
        legend.box.margin = margin(5, 5, 5, 5),
        panel.border = element_rect(
          colour = "black",
          fill = NA,
          size = 1
        ),
        plot.subtitle = element_text(
          color = "black",
          size = 14,
          hjust = 0.5
        ),
        plot.title = element_text(
          color = "black",
          size = 16,
          face = "bold",
          hjust = 0.5
        )
      ) +
      guides(fill = guide_legend(override.aes = base::list(colour = NA))) # remove black diagonal line from legend
    # +
    # scale_fill_brewer(palette = "Dark2") +
    # scale_colour_brewer(palette = "Dark2")

    ############################################ chi-square test #####################################################

    # custom function to write results from chi-square test into subtitle for the plot
    # jmv_chi stands for the chi-square object from package jmv
    # effect is the text label that needs to be entered to denote which interaction effect
    # is being investigated in
    # the chi-square test presented...if not entered, the default will be "Chi-square test"

    chi_subtitle <- function(jmv_chi, cramer_ci, effect = NULL) {
      # if effect label hasn't been specified, use this default
      if (is.null(effect)) {
        effect <- "Chi-square test"
      }

      base::substitute(
        expr =
          paste(
            y,
            " : ",
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
            phicoeff,
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
          pvalue = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_chi$chiSq)[[4]], k, p.value = TRUE),
          # select Cramer's V
          phicoeff = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_chi$nom)[[4]], k),
          LL = ggstatsplot::specify_decimal_p(x = cramer_ci[[2]], k),
          UL = ggstatsplot::specify_decimal_p(x = cramer_ci[[3]], k)
        )
      )
    }

    #################################### adding statistical test results ##################################################


    if (!base::missing(condition)) {
      # running Pearson's Chi-square test using jmv::contTables
      jmv_chi <- jmv::contTables(
        data = data,
        rows = "condition",
        cols = "main",
        phiCra = TRUE
      )
      # preparing Cramer's V object depending on whether V is NaN or not
      # it will be NaN in cases where there are no values of one categorial variable for level of another categorial variable
      if (is.nan(as.data.frame(jmv_chi$nom)[[4]])) {
        # NaN list in case Cramer's V is also NaN
        cramer_ci <- c(NaN, NaN, NaN)
      } else {
        # results for confidence interval of Cramer's V
        cramer_ci <- DescTools::CramerV(x = data$main,
                                        y = data$condition,
                                        conf.level = 0.95)
      }
      # adding chi-square results to the plot subtitle
      p <-
        p + labs(subtitle = chi_subtitle(
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

      # preparing proportion test subtitle for the plot
      proptest_subtitle <-
        base::substitute(
          expr =
            paste(
              "Proportion test : ",
              italic(chi) ^ 2,
              "(",
              df,
              ") = ",
              estimate,
              ", ",
              italic("p"),
              " = ",
              pvalue
            ),
          env = base::list(
            estimate = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_prop$tests)[[1]], k),
            df = as.data.frame(jmv_prop$tests)[[2]],
            # df is always an integer
            pvalue = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_prop$tests)[[3]], k, p.value = TRUE)
          )
        )
      # adding proportion test subtitle to the plot
      p <-
        p +
        labs(subtitle = proptest_subtitle)
    }

    #################################### putting all together ############################################

    # if legend title has not been provided, use the name of the variable corresponding to main
    if (is.null(legend.title)) {
      legend.title <- as.character(df$main)
    }
    # preparing the plot
    p <-
      p +
      labs(title = title,
           caption = caption) +
      guides(fill = guide_legend(title = legend.title))

    # return the final plot
    return(p)
  }
