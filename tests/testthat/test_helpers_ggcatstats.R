# context ------------------------------------------------------------
context(desc = "cat_summary_label_maker")

# cat_summary_label_maker works ----------------------------------------------

testthat::test_that(
  desc = "cat_summary_label_maker works",
  code = {
    testthat::skip_on_cran()

    set.seed(123)
    # creating a dataframe with counts and percentage
    summary_df <-
      ggplot2::mpg %>%
      dplyr::filter(.data = ., fl %in% c("c", "d")) %>%
      dplyr::group_by(.data = ., cyl, fl) %>%
      dplyr::summarize(.data = ., counts = n()) %>%
      dplyr::mutate(.data = ., perc = (counts / sum(counts)) * 100) %>%
      dplyr::ungroup(x = .) %>%
      dplyr::arrange(.data = ., dplyr::desc(x = fl)) %>%
      dplyr::filter(.data = ., counts != 0L)

    # mix of counts and percentage
    df1 <- ggstatsplot:::cat_summary_label_maker(
      data = summary_df,
      label.col.name = "slice.label",
      label.content = "both",
      perc.k = 1
    )

    # just counts
    df2 <- ggstatsplot:::cat_summary_label_maker(
      data = summary_df,
      label.col.name = "data.label",
      label.content = "n"
    )

    # just percentage
    df3 <- ggstatsplot:::cat_summary_label_maker(
      data = summary_df,
      label.col.name = "data.label",
      label.content = "perc",
      perc.k = 2
    )

    # checking data
    testthat::expect_equal(dim(df1), c(4L, 5L))
    testthat::expect_equal(dim(df2), c(4L, 5L))
    testthat::expect_equal(dim(df3), c(4L, 5L))
    testthat::expect_identical(
      colnames(df1),
      c("cyl", "fl", "counts", "perc", "slice.label")
    )
    testthat::expect_identical(
      colnames(df2),
      c("cyl", "fl", "counts", "perc", "data.label")
    )

    # checking the label column
    testthat::expect_identical(
      df1$slice.label,
      c("n = 3\n(75%)", "n = 1\n(100%)", "n = 1\n(100%)", "n = 1\n(25%)")
    )
    testthat::expect_identical(
      df2$data.label,
      c("n = 3", "n = 1", "n = 1", "n = 1")
    )
    testthat::expect_identical(
      df3$data.label,
      c("75%", "100%", "100%", "25%")
    )
  }
)
