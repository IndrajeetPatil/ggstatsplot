context("grouped_ggscatterstats")

testthat::test_that(
  desc = "grouped_ggscatterstats works",
  code = {

    # when the grouping and labelling variable are the same, the function
    # shouldn't work
    testthat::expect_error(
      object = grouped_ggscatterstats(
        data = iris,
        x = Sepal.Length,
        y = Petal.Width,
        grouping.var = Species,
        label.var = Species
      )
    )
  }
)
