# output: plot ---------------------------------------------------------------

skip_if_not_installed("ggcorrplot")

test_that(
  desc = "grouped_ggcorrmat plots are as expected",
  code = {
    skip_if(getRversion() < "4.1")

    set.seed(123)
    expect_doppelganger(
      title = "without NAs",
      fig = grouped_ggcorrmat(
        iris,
        grouping.var = Species,
        cor.vars = Sepal.Length:Petal.Length
      )
    )

    set.seed(123)
    expect_doppelganger(
      title = "with NAs",
      fig = grouped_ggcorrmat(
        data = dplyr::select(ggplot2::msleep, dplyr::matches("sleep|awake|vore")),
        grouping.var = vore,
      )
    )
  }
)

# expected warnings -------------------------------------------

test_that(
  desc = "grouped_ggcorrmat produces error when grouping isn't specified",
  code = {
    expect_snapshot_error(grouped_ggcorrmat(iris))
  }
)

# output: data frame ---------------------------------------------------------------

test_that(
  desc = "grouped_ggcorrmat returns expected data frame",
  code = {
    options(tibble.width = Inf)

    # tidy data frame
    df <- grouped_ggcorrmat(
      data = dplyr::select(ggplot2::msleep, dplyr::matches("sleep|awake|vore")),
      grouping.var = vore,
      type = "r",
      output = "data"
    )

    set.seed(123)
    expect_snapshot(df)
  }
)
