# ggcorrmat_matrix_message is working ------------------------------------

testthat::test_that(
  desc = "ggcorrmat_matrix_message is working",
  code = {
    testthat::expect_output(
      ggstatsplot:::ggcorrmat_matrix_message(),
      "the upper triangle: p-values adjusted for multiple comparisons",
      fixed = TRUE
    )
  }
)

# palette_message is working ------------------------------------

testthat::test_that(
  desc = "palette_message is working",
  code = {
    testthat::expect_output(
      ggstatsplot:::palette_message(
        package = "RColorBrewer",
        palette = "Dark2",
        min_length = 20
      ),
      "Number of labels",
      fixed = TRUE
    )
  }
)
