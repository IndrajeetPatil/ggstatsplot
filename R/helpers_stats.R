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
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang quo_squash
#' @importFrom dplyr everything
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr mutate_if
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @importFrom purrr map
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom tidyr spread
#'
#' @family helper_stats
#'
#' @keywords internal
#'
#' @note This is a helper function used internally in the package and not
#' exported. In case you want to use it, you can do so by
#' `ggstatsplot:::grouped_proptest`. Note that it is `:::` and not `::`.
#'

# function body
grouped_proptest <- function(data,
                             grouping.vars,
                             measure) {
  # turn off warning messages because there are going to be many of them for
  # tidyr::unnest
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
      percentage = data %>%
        purrr::map(
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
      results = chi_sq %>%
        purrr::map(
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
    signif_column(data = ., p = `p-value`) %>%
    dplyr::mutate_if(
      .tbl = .,
      .predicate = purrr::is_bare_character,
      .funs = ~dplyr::if_else(condition = is.na(.), true = "0%", false = .)
    )

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
#'   *p*-values
#' @return Returns the originally entered object (either a vector or a
#'   dataframe) in tibble format with an additional column corresponding to
#'   statistical significance.
#'
#' @param data Data frame from which variables specified are preferentially to
#'   be taken.
#' @param p The column containing p-values.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr mutate_if
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @importFrom broom tidy
#' @importFrom crayon red
#' @importFrom crayon blue
#' @importFrom rlang enquo
#' @importFrom stats lm
#' @importFrom tibble as_data_frame
#'
#' @family helper_stats
#'
#' @examples
#' 
#' # vector as input
#' signif_column(p = c(0.05, 0.1, 1, 0.00001, 0.001, 0.01))
#' 
#' # dataframe as input
#' # preparing a newdataframe
#' df <- tibble(
#'   x = 1:5,
#'   y = 1,
#'   p.value = c(0.1, 0.5, 0.00001, 0.05, 0.01)
#' )
#' 
#' signif_column(data = df, p = p.value)
#' 
#' # numbers entered as characters are also tolerated
#' signif_column(p = c("1", "0.1", "0.0002", "0.03", "0.65"))
#' @export
#'

signif_column <- function(data = NULL, p) {

  # if dataframe is provided
  if (!is.null(data)) {

    # storing variable name to be assigned later
    p_lab <- colnames(dplyr::select(
      .data = data,
      !!rlang::enquo(p)
    ))

    # preparing dataframe
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
      base::cbind.data.frame(p = p)
  }

  # make sure the p-value column is numeric; if not, convert it to numeric
  if (!is.numeric(df$p)) {

    # display message about conversion
    base::message(cat(
      crayon::green("Note:"),
      crayon::blue(
        "The entered vector is of class",
        crayon::yellow(class(df$p)[[1]]),
        "; attempting to convert it to numeric."
      )
    ))

    # conversion
    df$p <- as.numeric(as.character(df$p))
  }

  # add new significance column based on standard APA guidelines for describing
  # different levels of significance
  df %<>%
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
    df %<>%
      dplyr::select(.data = ., -p, -significance, dplyr::everything())

    # renaming the p-value variable with the name provided by the user
    colnames(df)[which(names(df) == "p")] <- p_lab
  }

  # return the final tibble dataframe
  return(df)
}


#' @title Finding the outliers in the dataframe using Tukey's interquartile range rule
#' @name check_outlier
#' @author Indrajeet Patil
#' @description Returns a logical vector
#'
#' @param var A numeric vector.
#' @param coef Coefficient for outlier detection using Tukey's method.
#'   With Tukey's method, outliers are below (1st Quartile) or above (3rd
#'   Quartile) `coef` times the Inter-Quartile Range (IQR) (Default: `1.5`).
#'
#' @importFrom stats quantile
#'
#' @family helper_stats
#'
#' @keywords internal
#'

# defining function to detect outliers
check_outlier <- function(var, coef = 1.5) {
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

#' @title Untable a dataset
#' @name untable
#' @description Given a tabulated dataset, this will untabulate it by repeating
#'   each row by the number of times it was repeated.
#'
#' @param data A data.frame to untable.
#' @param counts A column containing counts.
#'
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate_at
#' @importFrom dplyr everything
#' @importFrom tibble rowid_to_column
#' @importFrom rlang enquo
#'
#' @family helper_stats
#'
#' @examples
#' 
#' # have a look at the Titanic_full dataset first
#' Titanic_full <- untable(data = as.data.frame(Titanic), counts = Freq)
#' dplyr::glimpse(Titanic_full)
#' @export
#'

untable <- function(data, counts) {
  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      counts = !!rlang::enquo(counts),
      dplyr::everything()
    )

  # a custom function to repeat dataframe `rep` number of times, which is going to
  # be count data for us
  rep_df <- function(df, rep) {
    df[base::rep(x = 1:nrow(df), times = rep), ]
  }

  # converting dataframe to full length based on count information
  data %<>%
    tibble::as_data_frame(.) %>%
    tibble::rowid_to_column(df = ., var = "id") %>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = dplyr::vars("id"),
      .funs = ~as.factor(.)
    ) %>%
    base::split(x = ., f = .$id) %>%
    purrr::map_dfr(.x = ., .f = ~rep_df(df = ., rep = .$counts))

  # returned the expanded dataset
  return(data)
}
