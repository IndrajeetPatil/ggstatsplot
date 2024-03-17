test_that(
  "checking ggcorrmat with entier dataset",
  {
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
  }
)

# changing defaults -------------------------------------------

test_that(
  "ggcorrmat works as expected with changed defaults",
  {
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
        p.adjust.method = "fdr",
        colors = NULL,
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
  }
)


# grouped_ggcorrmat output: plot ---------------------------------------------------------------

test_that(
  "grouped_ggcorrmat plots are as expected",
  {
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
        grouping.var = vore
      )
    )
  }
)

# expected warnings -------------------------------------------

test_that(
  "grouped_ggcorrmat produces error when grouping isn't specified",
  {
    expect_snapshot(grouped_ggcorrmat(iris), error = TRUE)
  }
)
