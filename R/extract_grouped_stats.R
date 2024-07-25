extract_grouped_stats <- function(p = NULL) {
  stats_0 <- NULL
  for (plot_i in 1:NROW(p)) {
    Group <- p[[plot_i]]$labels$title
    table_i <- extract_stats(p[[plot_i]])$subtitle_data
# Column 'expression' is a list, in order to make the output compatible
# with savinv functions as openxlsx::save_xlsx it must be converted to
# a vector of chars
    table_i$expression <- as.character(table_i$expression)
    stats_i <- data.frame(Group, table_i)
    stats_0 <- rbind(stats_0, stats_i)
  }
  stats_0
}


