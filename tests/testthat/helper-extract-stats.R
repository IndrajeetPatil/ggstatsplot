# shared fixtures for test-extract-stats.R

# number of extracted statistical details, subtitle and caption of a plot
extract_details <- function(plot) {
  list(
    length(extract_stats(plot)),
    extract_subtitle(plot),
    extract_caption(plot)
  )
}
