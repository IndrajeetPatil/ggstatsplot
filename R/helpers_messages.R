
#' @title Message if palette doesn't have enough number of colors.
#' @name palette_message
#' @description A note to the user about not using the default color palette
#'   when the number of factor levels is greater than 8, the maximum number of
#'   colors allowed by `"Dark2"` palette from the `RColorBrewer` package.
#' @param package Name of package from which the palette is desired as string
#' or symbol.
#' @param palette Name of palette as string or symbol.
#' @param min_length Minimum number of colors needed.
#'
#' @importFrom dplyr filter select
#' @importFrom ipmisc green blue yellow red
#' @importFrom rlang !! enquo
#'
#' @noRd

# function body
palette_message <- function(package, palette, min_length) {
  # computing the number of colors in a given palette
  palette_df <-
    as_tibble(paletteer::palettes_d_names) %>%
    dplyr::filter(.data = ., package == !!package, palette == !!palette) %>%
    dplyr::select(.data = ., length)

  # if insufficient number of colors are available in a given palette
  if (palette_df$length[[1]] < min_length) {
    # message to display
    message(cat(
      ipmisc::red("Warning: "),
      ipmisc::blue("Number of labels is greater than default palette color count.\n"),
      ipmisc::blue("Try using another color `palette` (and/or `package`).\n")
    ),
    sep = ""
    )
  }
}


#' @title Message to display when adjusted p-values are displayed in correlation
#'   matrix.
#' @name ggcorrmat_matrix_message
#' @noRd

# function body
ggcorrmat_matrix_message <- function() {
  message(
    cat(
      ipmisc::green("Note: "),
      ipmisc::blue("In the correlation matrix,\n"),
      ipmisc::blue("the upper triangle: p-values adjusted for multiple comparisons\n"),
      ipmisc::blue("the lower triangle: unadjusted p-values.\n"),
      sep = ""
    )
  )
}
