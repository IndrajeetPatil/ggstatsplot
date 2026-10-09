test_that("checking if combining plots works", {
  set.seed(123)
  expect_doppelganger(
    title = "defaults work as expected",
    fig = combine_plots(
      plotlist = iris_plotlist,
      annotation.args = iris_annotation_args
    )
  )
})

test_that("annotation title stays bold when adding a custom annotation theme", {
  set.seed(123)
  expect_doppelganger(
    title = "annotation title remains bold with custom theme",
    fig = combine_plots(
      plotlist = iris_plotlist,
      annotation.args = c(
        iris_annotation_args,
        list(
          theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 18))
        )
      )
    )
  )
})
