### ggpie with percentages

# df$main should contain observations of interest
# df$condition can optionally be used to facet wrap
# labels should be a character vector of same length as group_by(df, main) or group_by(df, condition, main) if facet wrapping
# in case you don't want statistical results to be printed out along with the pie chart, set test = FALSE

#'
#' @title pie charts with statistical tests
#' @name ggpiestats
#' @author Indrajeet Patil
#'
#' @param data the data as a data frame
#' @param main a string naming the variable to use as the rows in the contingency table
#' @param condition a string naming the variable to use as the columns in the contingency table
#' @param labels a character vector of same length as (data, main) or (data, condition, main) if facet wrapping
#' @param stat_title title for the effect being investigated with the chi-square test
#' @param title title for the plot
#' @param caption caption for the plot
#' @param k number of decimal places expected for results
#'
#' @export
#'

ggpiestats <-
  function(data,
           main,
           condition = NULL,
           labels = NULL,
           stat_title = NULL,
           title = NULL,
           caption = NULL,
           legend_title = NULL,
           k = 3) {
    library(ggplot2)
    library(dplyr)
    # convert the data into percentages; group by conditional variable if needed
    df <- dplyr::group_by(data, .dots = c(condition, main)) %>%
      dplyr::summarize(counts = n()) %>%
      dplyr::mutate(perc = (counts / sum(counts)) * 100) %>%
      dplyr::arrange(desc(perc))

    # reorder the category factor levels to order the legend
    df[[main]] <- factor(df[[main]], levels = unique(df[[main]]))

    # if labels haven't been specified, use what's already there
    if (is.null(labels))
      labels <- as.character(df[[main]])

    ################################################## plot ##############################################

    if (!is.null(condition)) {
      p <- ggplot2::ggplot(df, aes('', counts)) +
        geom_col(
          position = 'fill',
          color = 'black',
          width = 1,
          aes(fill = factor(get(main)))
        ) +
        facet_wrap(condition, labeller = "label_both") +
        geom_label(
          aes(label = paste0(round(perc), "%"), group = factor(get(main))),
          position = position_fill(vjust = 0.5),
          color = 'black',
          size = 5,
          show.legend = FALSE
        ) +
        coord_polar(theta = "y") # convert to polar coordinates
    } else {
      p <- ggplot2::ggplot(df, aes('', counts)) +
        geom_col(
          position = 'fill',
          color = 'black',
          width = 1,
          aes(fill = factor(get(main)))
        ) +
        geom_label(
          aes(label = paste0(round(perc), "%"), group = factor(get(main))),
          position = position_fill(vjust = 0.5),
          color = 'black',
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
        panel.grid  = element_blank(),
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
        legend.direction = 'horizontal',
        legend.position = 'bottom',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'),
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
      guides(fill = guide_legend(override.aes = list(colour = NA))) + # remove black diagonal line from legend
      scale_fill_brewer(palette = "Dark2") +
      scale_colour_brewer(palette = "Dark2")

    ###################################### chi-square test ###############################################

    # custom function to write results from chi-square test into subtitle for the plot
    # x stands for the chi-square object
    # effect is the text label that needs to be entered to denote which interaction effect
    # is being investigated in
    # the chi-square test presented...if not entered, the default will be "Chi-square test"

    chi_subtitle <- function(x, effect = NULL) {
      # if effect label hasn't been specified, use this default
      if (is.null(effect))
        effect <- "Chi-square test"

      base::substitute(
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
          ", Cramer's ",
          italic(V),
          " = ",
          phicoeff
        ),
        list(
          y = effect,
          estimate = ggstatsplot::specify_decimal_p(x = as.data.frame(x$chiSq)[[2]], k),
          df = as.data.frame(x$chiSq)[[3]],
          # df always an integer
          pvalue = ggstatsplot::specify_decimal_p(x = as.data.frame(x$chiSq)[[4]], k, p.value = TRUE),
          phicoeff = ggstatsplot::specify_decimal_p(x = as.data.frame(x$nom)[[4]], k)
        )
      )

    }

    ###################################### proportion test ###############################################

    # custom function to write results from chi-square test into subtitle for the plot
    # x stands for the proportion test object from jmv::propTestN()

    proptest_subtitle <- function(x) {
      base::substitute(
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
        list(
          estimate = ggstatsplot::specify_decimal_p(x = as.data.frame(x$tests)[[1]], k),
          df = as.data.frame(x$tests)[[2]],
          # df is always an integer
          pvalue = ggstatsplot::specify_decimal_p(x = as.data.frame(x$tests)[[3]], k, p.value = TRUE)
        )
      )

    }

    #################################### statistical test results #######################################

    if (!is.null(condition)) {
      # create a dataframe on which chi-square tests will be carried out in case there is "condition" variable present
      # prepare the statistical test subtitle

      df2 <- data %>% dplyr::select(condition, main)
      colnames(df2) <- c("col1", "col2")
      p <-
        p + labs(subtitle = chi_subtitle(
          jmv::contTables(
            df2,
            rows = 'col1',
            cols = 'col2',
            phiCra = TRUE
          ),
          effect = stat_title
        ))

    } else {
      # create a dataframe on which proportion test will be carried out when there is no condition variable present
      df2 <- data %>% dplyr::select(main)
      colnames(df2) <- c("col1")
      # adding subtitle to the plot
      p <-
        p + labs(subtitle = proptest_subtitle(x = jmv::propTestN(data = df2, var = 'col1')))

    }

    ### adding the title for the entire plot and the legend title

    # if legend title has not been provided, use the name of the variable corresponding to main
    if (is.null(legend_title)) {
      legend_title <- as.character(main)
    }
    # preparing the plot
    p <-
      p + labs(title = title, caption = caption) + guides(fill = guide_legend(title = legend_title))

    return(p)

  }
