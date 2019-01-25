#' @title Summary dataframe for categorical variables.
#' @name cat_summary_label_maker
#' @description Creating a dataframe with an added column corresponding to
#'   summary for categorical variables.
#' @author Indrajeet Patil
#'
#' @param data A dataframe containing summaries for categorical variables.
#'   Should contain columns named either `"perc"` or `"counts"` or both.
#' @param label.col.name Character that decides the column name containing
#'   summary label. This can either be `"slice.label"` (default) or
#'   `"data.label"`.
#' @param label.content Character decides what information needs to be displayed
#'   on the label in each pie or bar slice. Possible options are `"percentage"`
#'   (default), `"counts"`, `"both"`.
#' @inheritParams ggpiestats
#'
#' @importFrom dplyr mutate
#' @importFrom rlang !! :=
#'
#' @examples
#' \dontrun{
#' # creating a dataframe with counts and percentage
#' df <-
#'   mtcars %>%
#'   dplyr::group_by(.data = ., am, cyl) %>%
#'   dplyr::summarize(.data = ., counts = n()) %>%
#'   dplyr::mutate(.data = ., perc = (counts / sum(counts)) * 100) %>%
#'   dplyr::ungroup(x = .) %>%
#'   dplyr::arrange(.data = ., dplyr::desc(x = cyl)) %>%
#'   dplyr::filter(.data = ., counts != 0L)
#'
#' # dataframe with label column
#' ggstatsplot:::cat_summary_label_maker(
#'   data = df,
#'   label.col.name = "slice.label",
#'   label.content = "both",
#'   perc.k = 1
#' )
#' }
#'
#' @keywords internal

# function body
cat_summary_label_maker <- function(data,
                                    label.col.name = "slice.label",
                                    label.content = "percentage",
                                    perc.k = 1) {
  # checking what needs to be displayed in a label
  if (label.content %in% c("percentage", "perc", "proportion", "prop")) {
    # only percentage
    data %<>%
      dplyr::mutate(
        .data = .,
        !!label.col.name := paste0(round(x = perc, digits = perc.k), "%")
      )
  } else if (label.content %in% c("counts", "n", "count")) {
    # only raw counts
    data %<>%
      dplyr::mutate(
        .data = .,
        !!label.col.name := paste0("n = ", counts)
      )
  } else if (label.content %in% c("both", "mix", "all", "everything")) {
    # both raw counts and percentages
    data %<>%
      dplyr::mutate(
        .data = .,
        !!label.col.name := paste0(
          "n = ",
          counts,
          "\n(",
          round(x = perc, digits = perc.k),
          "%)"
        )
      )
  }

  # return dataframe with label column
  return(data)
}
