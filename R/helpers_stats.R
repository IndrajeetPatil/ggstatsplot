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
#' @note This is a helper function used internally in the package and not
#' exported. In case you want to use it, you can do so by
#' `ggstatsplot:::grouped_proptest`. Note that it is `:::` and not `::`.
#'

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

grouped_proptest <- function(data,
                             grouping.vars,
                             measure) {
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
  df <- dplyr::select(
    .data = data,
    !!!grouping.vars,
    measure = !!rlang::enquo(measure)
  )

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
        .f = ~dplyr::group_by(.data = ., measure) %>%
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
    dplyr::mutate(
      .data = .,
      chi_sq = data %>% purrr::map(
        .x = .,
        .f = ~stats::chisq.test(x = base::table(.$measure))
      )
    ) %>%
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
            ggstatsplot::specify_decimal_p(
              x = .$p.value,
              k = 3
            )
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
#' @note This is a helper function used internally in the package and not
#' exported. In case you want to use it, you can do so by
#' `ggstatsplot:::signif_column`. Note that it is `:::` and not `::`.
#'

signif_column <- function(data = NULL, p) {
  # storing variable name to be assigned later
  p_lab <- colnames(dplyr::select(
    .data = data,
    !!rlang::enquo(p)
  ))
  # if dataframe is provided
  if (!is.null(data)) {
    df <-
      dplyr::select(
        .data = data,
        # column corresponding to p-values
        p = !!rlang::enquo(p),
        dplyr::everything()
      )
  } else {
    # if only vector is provided
    df <-
      base::cbind.data.frame(p = p) # column corresponding to p-values
  }

  # make sure the p-value column is numeric; if not, convert it to numeric and give a warning to the user
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


## finding the outliers in the dataframe using Tukey's interquartile range rule

# defining function to detect outliers
check_outlier <- function(var, coef) {
  # compute the quantiles
  quantiles <- stats::quantile(
    x = var,
    probs = c(0.25, 0.75)
  )

  # compute the interquartile range
  IQR <- quantiles[2] - quantiles[1]

  # check for outlier and output a logical
  res <-
    ((var < (quantiles[1] - coef * IQR)) |
       (var > (quantiles[2] + coef * IQR)))

  # return the result
  return(res)
}
