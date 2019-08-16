# context ----------------------------------------------------------------
context(desc = "grouped_list works")

# grouped_list works -----------------------------------------------------

testthat::test_that(
  desc = "grouped_list works",
  code = {
    set.seed(123)

    # creating lists
    df1 <- ggstatsplot:::grouped_list(data = ggplot2::msleep, grouping.var = vore)
    df2 <- ggstatsplot:::grouped_list(
      data = dplyr::filter(.data = ggplot2::msleep, vore != "herbi"),
      grouping.var = "vore"
    )
    df5 <- ggstatsplot:::grouped_list(data = ggplot2::msleep, grouping.var = "vore")

    # checking if all character columns have been changed to factor
    df3 <- purrr::map_dfr(
      .x = df1,
      .f = ~ dim(purrr::keep(
        .x = ., .p = purrr::is_character
      ))
    )
    df4 <- purrr::map_dfr(
      .x = df2,
      .f = ~ dim(purrr::keep(
        .x = ., .p = purrr::is_character
      ))
    )

    # testing lengths of lists
    testthat::expect_equal(length(df1), 4L)
    testthat::expect_equal(length(df2), 3L)
    testthat::expect_identical(class(df1), "list")
    testthat::expect_identical(df1$carni, df5$carni)
    testthat::expect_equal(sum(df3[2, ]), 16L)
    testthat::expect_equal(sum(df4[2, ]), 12L)
  }
)
