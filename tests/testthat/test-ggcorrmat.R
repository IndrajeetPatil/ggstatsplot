test_that("checking ggcorrmat with entire dataset", {
  set.seed(123)
  expect_doppelganger(
    title = "parametric correlation - without NAs",
    fig = ggcorrmat(anscombe, type = "p")
  )

  set.seed(123)
  expect_doppelganger(
    title = "non-parametric correlation - with NAs",
    fig = ggcorrmat(ggplot2::msleep, type = "np")
  )
})

test_that("explicit `cor.vars = NULL` uses all numeric variables", {
  expect_identical(
    extract_stats(ggcorrmat(anscombe, cor.vars = NULL)),
    extract_stats(ggcorrmat(anscombe))
  )
})

test_that("single sample size is shown when all pairs share the same `n`", {
  legend_title <- function(data) {
    ggcorrmat(data)$scales$get_scales("fill")$name
  }
  expected <- legend_title(anscombe[-1L, ])

  # missing values in a non-numeric column are not used in any correlation
  expect_identical(
    legend_title(dplyr::mutate(anscombe[-1L, ], group = c(NA, letters[1:9]))),
    expected
  )

  # missing values in the same row of every numeric column
  anscombe_na <- anscombe
  anscombe_na[1L, ] <- NA
  expect_identical(legend_title(anscombe_na), expected)
})

# changing defaults -------------------------------------------

test_that("ggcorrmat works as expected with changed defaults", {
  set.seed(123)
  expect_doppelganger(
    title = "changing aesthetic defaults",
    fig = ggcorrmat(
      data = dplyr::select(iris, dplyr::contains("Petal")),
      type = "p",
      title = "Iris dataset",
      subtitle = "By Edgar Anderson",
      sig.level = 0.001,
      matrix.type = "full",
      colors = c("#0072B2", "white", "#D55E00"),
      p.adjust.method = "fdr",
      digits = 4L,
      ggcorrplot.args = list(
        lab_col = "white",
        pch.col = "white"
      )
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "applying ggplot2 function works",
    fig = ggcorrmat(
      data = dplyr::select(ggplot2::msleep, brainwt, sleep_rem, bodywt),
      type = "r",
      sig.level = 0.01,
      partial = TRUE,
      caption = "bla bla bla",
      p.adjust.method = "hommel",
      matrix.type = "upper"
    ) +
      labs(caption = NULL)
  )

  set.seed(123)
  expect_doppelganger(
    title = "unadjusted p-values label",
    fig = ggcorrmat(
      data = dplyr::select(iris, dplyr::contains("Sepal")),
      p.adjust.method = "none"
    )
  )
})

# grouped_ggcorrmat output: plot -------------------------------------

test_that("grouped_ggcorrmat plots are as expected", {
  set.seed(123)
  expect_doppelganger(
    title = "without NAs",
    fig = grouped_ggcorrmat(
      iris,
      grouping.var = Species,
      cor.vars = Sepal.Length:Petal.Length,
      colors = c("purple", "white", "orange")
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "with NAs",
    fig = grouped_ggcorrmat(
      data = dplyr::select(ggplot2::msleep, dplyr::matches("sleep|awake|vore")),
      grouping.var = vore
    )
  )
})

# expected errors -------------------------------------------

test_that("grouped_ggcorrmat produces error when grouping isn't specified", {
  expect_snapshot(grouped_ggcorrmat(iris), error = TRUE)
})
