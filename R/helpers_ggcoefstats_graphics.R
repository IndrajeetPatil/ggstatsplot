#' @title Create labels with statistical details for `ggcoefstats`
#' @name ggcoefstats_label_maker
#'
#' @param ... Currently ignored.
#' @param tidy_df A tidy dataframe.
#' @param ... Currently ignored.
#' @inheritParams ggcoefstats
#'
#' @importFrom purrr map
#' @importFrom dplyr group_nest mutate
#' @importFrom tidyr unnest
#'
#' @keywords internal

# function body
ggcoefstats_label_maker <- function(tidy_df,
                                    statistic = NULL,
                                    k = 2L,
                                    effsize = "eta",
                                    partial = TRUE,
                                    ...) {

  #----------------------- p-value cleanup ------------------------------------

  # formatting the p-values
  tidy_df %<>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "statistic",
      .funs = ~ specify_decimal_p(x = ., k = k)
    ) %>%
    signif_column(data = ., p = p.value) %>%
    p_value_formatter(df = ., k = k) %>%
    dplyr::mutate(.data = ., rowid = dplyr::row_number())

  #--------------------------- t-statistic ------------------------------------

  # if the statistic is t-value
  if (statistic == "t") {
    # check if df info is available somewhere
    if ("df.error" %in% names(tidy_df)) {
      # adding a new column with residual `df`
      tidy_df %<>%
        dplyr::group_nest(.tbl = ., rowid) %>%
        dplyr::mutate(
          .data = .,
          label = data %>%
            purrr::map(
              .x = .,
              .f = ~ paste(
                "list(~widehat(italic(beta))==",
                specify_decimal_p(x = .$estimate, k = k),
                ", ~italic(t)",
                "(",
                specify_decimal_p(x = .$df.error, k = 0L),
                ")==",
                .$statistic,
                ", ~italic(p)",
                .$p.value.formatted,
                ")",
                sep = ""
              )
            )
        )
    } else {
      # for objects like `rlm` there will be no parameter
      tidy_df %<>%
        dplyr::group_nest(.tbl = ., rowid) %>%
        dplyr::mutate(
          .data = .,
          label = data %>%
            purrr::map(
              .x = .,
              .f = ~ paste(
                "list(~widehat(italic(beta))==",
                specify_decimal_p(x = .$estimate, k = k),
                ", ~italic(t)",
                "==",
                .$statistic,
                ", ~italic(p)",
                .$p.value.formatted,
                ")",
                sep = ""
              )
            )
        )
    }
  }

  #--------------------------- z-statistic ---------------------------------

  # if the statistic is z-value
  if (statistic == "z") {
    tidy_df %<>%
      dplyr::group_nest(.tbl = ., rowid) %>%
      dplyr::mutate(
        .data = .,
        label = data %>%
          purrr::map(
            .x = .,
            .f = ~ paste(
              "list(~widehat(italic(beta))==",
              specify_decimal_p(x = .$estimate, k = k),
              ", ~italic(z)==",
              .$statistic,
              ", ~italic(p)",
              .$p.value.formatted,
              ")",
              sep = ""
            )
          )
      )
  }

  #--------------------------- f-statistic ---------------------------------

  if (statistic == "f") {
    # which effect size is needed?
    if (effsize == "eta") {
      if (isTRUE(partial)) {
        tidy_df$effsize.text <- list(quote(widehat(italic(eta)[p]^2)))
      } else {
        tidy_df$effsize.text <- list(quote(widehat(italic(eta)^2)))
      }
    }

    if (effsize == "omega") {
      if (isTRUE(partial)) {
        tidy_df$effsize.text <- list(quote(widehat(italic(omega)[p]^2)))
      } else {
        tidy_df$effsize.text <- list(quote(widehat(italic(omega)^2)))
      }
    }

    # which effect size is needed?
    tidy_df %<>%
      dplyr::group_nest(.tbl = ., rowid) %>%
      dplyr::mutate(
        .data = .,
        label = data %>%
          purrr::map(
            .x = .,
            .f = ~ paste(
              "list(~italic(F)",
              "(",
              .$df1,
              "*\",\"*",
              .$df2,
              ")==",
              .$statistic,
              ", ~italic(p)",
              .$p.value.formatted,
              ", ~",
              .$effsize.text,
              "==",
              specify_decimal_p(x = .$estimate, k = k),
              ")",
              sep = ""
            )
          )
      )
  }

  # unnest
  tidy_df %<>%
    tidyr::unnest(data = ., cols = c(label, data)) %>%
    dplyr::select(.data = ., -rowid)

  # return the final dataframe
  return(as_tibble(tidy_df))
}


#' @name extract_statistic
#'
#' @importFrom insight find_statistic
#' @importFrom purrr pmap_dfc
#' @importFrom dplyr select_if
#'
#' @noRd

extract_statistic <- function(x, ...) {
  # if not a dataframe, figure out what's the relevant statistic
  statistic <- insight::find_statistic(x)

  # standardize statistic type symbol for regression models
  # checking entered strings to extract the statistic
  grep_stat <- function(x, pattern) {
    if (isTRUE(grepl(pattern, x, ignore.case = TRUE))) {
      return(tolower(substring(x, 1, 1)))
    } else {
      return(NA_character_)
    }
  }

  # extracting statistic value
  purrr::pmap_dfc(
    .l =
      list(
        pattern = list("^t", "^f", "^z", "^chi"),
        x = list(statistic)
      ),
    .f = grep_stat
  ) %>%
    dplyr::select_if(.tbl = ., .predicate = ~ sum(!is.na(.)) > 0) %>%
    unlist(x = ., use.names = FALSE)
}
