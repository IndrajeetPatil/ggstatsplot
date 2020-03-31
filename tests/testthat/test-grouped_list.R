
# grouped_list works -----------------------------------------------------

testthat::test_that(
  desc = "grouped_list works",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # creating lists
    df1 <- ggstatsplot:::grouped_list(data = ggplot2::msleep, grouping.var = vore)
    df2 <- ggstatsplot:::grouped_list(
      data = dplyr::filter(.data = ggplot2::msleep, vore != "herbi"),
      grouping.var = "vore"
    )
    df5 <- ggstatsplot:::grouped_list(data = ggplot2::msleep, grouping.var = "vore")
    df6 <- ggstatsplot:::grouped_list(data = ggplot2::msleep, grouping.var = NULL)

    # testing lengths of lists
    testthat::expect_equal(length(df1), 4L)
    testthat::expect_equal(length(df2), 3L)
    testthat::expect_identical(class(df1), "list")
    testthat::expect_identical(df1$carni, df5$carni)
    testthat::expect_identical(ggplot2::msleep, df6)
  }
)
