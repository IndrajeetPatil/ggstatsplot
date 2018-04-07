#' @title custom function to set upper and lower margins to legend title in
#'   ggplot2
#' @name legend_title_margin
#' @aliases legend_title_margin
#' @return A plot with desired margins between the legend title and the legend.
#'
#' @author Indrajeet Patil
#' @param plot Plot with the legend title whose margins need to be modified.
#' @param t.margin,b.margin Margins in grid units.
#'
#' @import grid
#' @import ggplot2
#' @import gtable
#'
#' @importFrom cowplot ggdraw
#'
#' @keywords internal
#'

legend_title_margin <- function(plot,
                                t.margin = unit(0, "mm"),
                                b.margin = unit(3, "mm")) {
  # get the plot grob
  g <- ggplot2::ggplotGrob(x = plot)

  # get the legend
  index <- base::which(x = g$layout$name == "guide-box")
  leg <- g$grobs[[index]][[1]][[1]]

  # get the legend title
  title <- leg$grobs[[4]]

  # set up the heights: for the two margins and the original title
  # unit.c produces a new unit object by combining the unit objects specified as arguments
  heights <-
    grid::unit.c(t.margin,
                 grid::unit(x = 1, units = "grobheight", data = title),
                 b.margin)

  # set up a column of three viewports
  vp <- grid::viewport(
    layout = grid::grid.layout(
      nrow = 3,
      ncol = 1,
      heights = heights
    ),
    name = "vp1"
  )

  # the middle row, where the title text will appear, is named as 'child_vp'.
  child_vp <-
    grid::viewport(layout.pos.row = 2,
                   clip = "off",
                   name = "child_vp")

  # put the title into a gTree containing one grob (the title) and the three viewports
  TitleText <- grid::gTree(
    children = grid::gList(title),
    vp = grid::vpTree(parent = vp, children = grid::vpList(child_vp))
  )

  # back to the legend: Set height for row 2 of legend to new height of TitleText
  leg$heights[2] <- sum(heights)

  # Add the new TitleText grob to row 2 of legend
  leg <- gtable::gtable_add_grob(
    x = leg,
    grobs = TitleText,
    t = 2,
    l = 2,
    r = 5,
    name = "TitleText"
  )

  # remove the original title
  leg$grobs <- leg$grobs[-4]
  leg$layout <- leg$layout[-4, ]

  # put the legend back into the plot
  g$grobs[[index]][[1]][[1]] <- leg

  class(g) <- c("legend_title_margin", class(g))

  # draw the plot
  g <- cowplot::ggdraw(g)

  # return the final plot
  return(g)
}


#'
#' @title Function to run proportion test on grouped data.
#' @name grouped_proptest
#' @aliases grouped_proptest
#' @author Indrajeet Patil
#' @return Dataframe with percentages and statistical details from a proportion
#'   test.
#'
#' @param data Dataframe from which variables are to be drawn.
#' @param grouping.vars List of grouping variables
#' @param measure A variable for which proportion test needs to be carried out
#'   for each combination of levels of factors entered in `grouping.vars`.
#'
#' @import dplyr
#' @import rlang
#'
#' @importFrom purrr map
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom tidyr spread
#'
#' @keywords internal
#'
#' @export

# defining global variables and functions to quient the R CMD check notes
utils::globalVariables(
  c(
    "Df",
    "F value",
    "F.value",
    "LL",
    "Pr(>F)",
    "UL",
    "complete",
    "data",
    "df1",
    "df2",
    "effect",
    "effsize",
    "formula",
    "hist",
    "median",
    "p0",
    "p100",
    "p50",
    "p25",
    "p75",
    "sd",
    "type",
    "Chi-squared",
    "df",
    "p-value",
    "chi_sq",
    "significance"
  )
)

grouped_proptest <- function(data, grouping.vars, measure) {
  # turn off warning messages because there are going to be many of them for tidyr::unnest
  options(warn = -1)
  # check how many variables were entered for this grouping variable
  grouping.vars <-
    as.list(rlang::quo_squash(rlang::enquo(grouping.vars)))
  grouping.vars <-
    if (length(grouping.vars) == 1) {
      # e.g., in mtcars dataset, grouping.vars = am
      grouping.vars
    } else {
      # e.g., in mtcars dataset, grouping.vars = c(am, cyl)
      grouping.vars[-1]
    }

  # getting the dataframe ready
  df <- dplyr::select(.data = data,
                      !!!grouping.vars,
                      measure = !!rlang::enquo(measure))

  # creating a nested dataframe
  df_nest <- df %>%
    dplyr::group_by(!!!grouping.vars) %>%
    tidyr::nest(data = .)

  # creating the final results with the
  df_results <- df_nest %>%
    dplyr::mutate(
      .data = .,
      percentage = data %>% purrr::map(
        .x = .,
        .f = ~  dplyr::group_by(.data = ., measure) %>%
          dplyr::summarize(.data = ., counts = length(measure)) %>%
          dplyr::mutate(
            .data = .,
            perc = paste0(ggstatsplot::specify_decimal_p(
              x = (counts / sum(counts)) * 100, k = 2
            ), "%", sep = "")
          ) %>%
          dplyr::select(.data = ., -counts) %>%
          tidyr::spread(
            data = .,
            key = measure,
            value = perc
          )
      )
    ) %>%
    dplyr::mutate(.data = .,
                  chi_sq = data %>% purrr::map(
                    .x = .,
                    .f = ~ stats::chisq.test(x = base::table(.$measure))
                  )) %>%
    dplyr::mutate(
      .data = .,
      results = chi_sq %>% purrr::map(
        .x = .,
        .f = ~
          base::cbind.data.frame(
            "Chi-squared" = as.numeric(as.character(
              ggstatsplot::specify_decimal_p(x = .$statistic, k = 3)
            )),
            "df" = as.numeric(as.character(
              ggstatsplot::specify_decimal_p(x = .$parameter, k = 0)
            )),
            "p-value" = as.numeric(as.character(
              ggstatsplot::specify_decimal_p(x = .$p.value,
                                             k = 3)
            ))
          )
      )
    ) %>%
    dplyr::select(.data = ., -data, -chi_sq) %>%
    tidyr::unnest(data = .) %>%
    signif_column(data = ., p = `p-value`)

  # for every level of grouping.vars, it is going to throw following errors
  # Warning in bind_rows_(x, .id) :
  # Unequal factor levels: coercing to character
  # Warning in bind_rows_(x, .id) :
  #   binding character and factor vector, coercing into character vector
  # Warning in bind_rows_(x, .id) :
  #   binding character and factor vector, coercing into character vector

  # this is due different columns having different types

  # clean up after yourself and change the options back to what are R base defaults
  options(warn = 1)

  # return the final results
  return(df_results)
}


#' @title Creating a new character type column with significance labels
#' @name signif_column
#' @aliases signif_column
#' @author Indrajeet Patil
#' @description This function will add a new column to a dataframe containing
#'   p-values
#' @return Returns the originally entered object (either a vector or a
#'   dataframe) in tibble format with an additional column corresponding to
#'   statistical significance.
#'
#' @param data Data frame from which variables specified are preferentially to
#'   be taken.
#' @param p The column containing p-values.
#'
#' @import dplyr
#'
#' @importFrom broom tidy
#' @importFrom crayon red
#' @importFrom crayon blue
#' @importFrom rlang enquo
#' @importFrom stats lm
#' @importFrom tibble as_data_frame
#'
#' @keywords internal
#'
#' @export
#'

signif_column <- function(data = NULL, p) {
  # storing variable name to be assigned later
  p_lab <- colnames(dplyr::select(.data = data,
                                  !!rlang::enquo(p)))
  # if dataframe is provided
  if (!is.null(data)) {
    df <-
      dplyr::select(.data = data,
                    # column corresponding to p-values
                    p = !!rlang::enquo(p),
                    dplyr::everything())
  } else {
    # if only vector is provided
    df <-
      base::cbind.data.frame(p = p) # column corresponding to p-values
  }

  #make sure the p-value column is numeric; if not, convert it to numeric and give a warning to the user
  if (!is.numeric(df$p)) {
    df$p <- as.numeric(as.character(df$p))
  }
  # add new significance column based on standard APA guidelines for describing different levels of significance
  df <- df %>%
    dplyr::mutate(
      .data = .,
      significance = dplyr::case_when(
        # first condition
        p >= 0.050 ~ "ns",
        # second condition
        p < 0.050 &
          p >= 0.010 ~ "*",
        # third condition
        p < 0.010 &
          p >= 0.001 ~ "**",
        # fourth condition
        p < 0.001 ~ "***"
      )
    ) %>%
    tibble::as_data_frame(x = .) # convert to tibble dataframe
  # change back from the generic p-value to the original name that was provided by the user for the p-value
  if (!is.null(data)) {
    # reordering the dataframe
    df <- df %>%
      dplyr::select(.data = ., -p, -significance, dplyr::everything())
    # renaming the p-value variable with the name provided by the user
    colnames(df)[which(names(df) == "p")] <- p_lab
  }
  # return the final tibble dataframe
  return(df)
}
