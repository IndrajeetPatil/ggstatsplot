# grouped_list works -----------------------------------------------------

test_that(
  desc = "grouped_list works",
  code = {
    skip_on_cran()
    set.seed(123)

    # creating lists
    df1 <- ggstatsplot:::grouped_list(ggplot2::msleep, grouping.var = vore)
    df2 <- ggstatsplot:::grouped_list(dplyr::filter(ggplot2::msleep, vore != "herbi"),
      grouping.var = "vore"
    )
    df5 <- ggstatsplot:::grouped_list(ggplot2::msleep, grouping.var = "vore")
    df6 <- ggstatsplot:::grouped_list(ggplot2::msleep, grouping.var = NULL)

    # testing lengths of lists
    expect_snapshot(list(length(df1), length(df2), length(df5), length(df6)))
    expect_snapshot(list(names(df1), names(df2), names(df5), names(df6)))
    expect_identical(df1$carni, df5$carni)
    expect_identical(ggplot2::msleep, df6)
  }
)


# palette_message is working ------------------------------------

test_that(
  desc = "palette_message is working",
  code = {
    expect_snapshot(
      ggstatsplot:::palette_message(
        package = "RColorBrewer",
        palette = "Dark2",
        min_length = 20
      )
    )
  }
)
