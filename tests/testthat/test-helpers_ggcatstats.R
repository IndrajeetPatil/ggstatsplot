# context ------------------------------------------------------------
context(desc = "cat_label_df")

# cat_label_df works ----------------------------------------------

testthat::test_that(
  desc = "cat_label_df works",
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
    df1 <- ggstatsplot:::cat_label_df(
      data = summary_df,
      label.col.name = "slice.label",
      label.content = "both",
      label.separator = "\n",
      perc.k = 1
    )

    # just counts
    df2 <- ggstatsplot:::cat_label_df(
      data = summary_df,
      label.col.name = "data.label",
      label.content = "n"
    )

    # just percentage
    df3 <- ggstatsplot:::cat_label_df(
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

# cat_couter works ----------------------------------------------

testthat::test_that(
  desc = "cat_couter works",
  code = {
    testthat::skip_on_cran()

    set.seed(123)
    data_tooth <- dplyr::filter(ToothGrowth, supp != "VC")
    data_mtcars <- dplyr::filter(mtcars, cyl != "4")

    df1 <- ggstatsplot:::cat_counter(mtcars, am, "cyl")
    df2 <- ggstatsplot:::cat_counter(ggplot2::msleep, "vore")
    df3 <- ggstatsplot:::cat_counter(data_tooth, "supp", NULL)
    df4 <- ggstatsplot:::cat_counter(data_mtcars, "am", "cyl")
    df5 <- ggstatsplot:::cat_counter(ggplot2::mpg, "drv", "cyl", "fl")

    testthat::expect_equal(dim(df1), c(6L, 4L))
    testthat::expect_equal(dim(df2), c(5L, 3L))
    testthat::expect_equal(dim(df3), c(1L, 3L))
    testthat::expect_equal(dim(df4), c(4L, 4L))
    testthat::expect_equal(dim(df5), c(22L, 5L))

    testthat::expect_identical(colnames(df1), c("cyl", "am", "counts", "perc"))
    testthat::expect_identical(colnames(df2), c("vore", "counts", "perc"))
    testthat::expect_identical(colnames(df3), c("supp", "counts", "perc"))
    testthat::expect_identical(colnames(df4), colnames(df1))
    testthat::expect_identical(
      colnames(df5),
      c("cyl", "drv", "fl", "counts", "perc")
    )
  }
)
