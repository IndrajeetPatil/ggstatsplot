
skip_if_not_installed("ggcorrplot")

test_that(
  desc = "checking ggcorrmat with entier dataset",
  code = {
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

# expected warnings -------------------------------------------

test_that(
  desc = "ggcorrmat warnings are as expected",
  code = {
    expect_snapshot_warning(ggcorrmat(
      data = iris,
      cor.vars.names = "x"
    ))
  }
)

# changing defaults -------------------------------------------

test_that(
  desc = "ggcorrmat works as expected with changed defaults",
  code = {
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
        k = 4L,
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

skip_if_not_installed("ggcorrplot")

test_that(
  desc = "grouped_ggcorrmat plots are as expected",
  code = {
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
