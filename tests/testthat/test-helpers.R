# grouped_list works -----------------------------------------------------

test_that(
  desc = "grouped_list works",
  code = {
    skip_on_cran()
    set.seed(123)

    # creating lists
    df1 <- ggstatsplot:::grouped_list(data = ggplot2::msleep, grouping.var = vore)
    df2 <- ggstatsplot:::grouped_list(
      data = dplyr::filter(ggplot2::msleep, vore != "herbi"),
      grouping.var = "vore"
    )
    df5 <- ggstatsplot:::grouped_list(data = ggplot2::msleep, grouping.var = "vore")
    df6 <- ggstatsplot:::grouped_list(data = ggplot2::msleep, grouping.var = NULL)

    # testing lengths of lists
    expect_equal(length(df1), 4L)
    expect_equal(length(df2), 3L)
    expect_identical(class(df1), "list")
    expect_identical(df1$carni, df5$carni)
    expect_identical(ggplot2::msleep, df6)
  }
)


# palette_message is working ------------------------------------

test_that(
  desc = "palette_message is working",
  code = {
    expect_output(
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
