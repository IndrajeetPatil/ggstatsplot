extract_subtitle <- function(p) {
  stats_df <- extract_stats(p)
  stats_df$subtitle_data$expression[[1]]
}

extract_caption <- function(p) {
  stats_df <- extract_stats(p)
  stats_df$caption_data$expression[[1]]
}
