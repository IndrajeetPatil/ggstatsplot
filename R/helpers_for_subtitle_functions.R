#' @noRd
#' @keywords internal

rcompanion_cleaner <- function(object, estimate.col) {
  object %>%
    tibble::as_tibble(x = .) %>%
    dplyr::rename(
      .data = .,
      estimate = estimate.col,
      conf.low = lower.ci,
      conf.high = upper.ci
    )
}
