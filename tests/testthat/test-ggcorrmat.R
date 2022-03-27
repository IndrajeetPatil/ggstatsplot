# entire dataset - without NAs ------------------------------------------------

test_that(
  desc = "checking ggcorrmat with entier dataset - without NAs",
  code = {
    skip_if_not_installed("vdiffr")
    skip_if(getRversion() < "4.1")
    skip_if(getRversion() >= "4.2")
    skip_if_not_installed("ggcorrplot")

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "parametric correlation - without NAs",
      fig = ggcorrmat(data = anscombe, type = "p")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "robust correlation - without NAs",
      fig = suppressWarnings(ggcorrmat(data = anscombe, type = "r"))
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "non-parametric correlation - without NAs",
      fig = ggcorrmat(data = anscombe, type = "np")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "bayesian correlation - without NAs",
      fig = suppressWarnings(ggcorrmat(data = anscombe, type = "bayes"))
    )
  }
)

# entire dataset - with NAs ------------------------------------------------

test_that(
  desc = "checking ggcorrmat with entier dataset - with NAs",
  code = {
    skip_if_not_installed("vdiffr")
    skip_if(getRversion() < "4.1")
    skip_if(getRversion() >= "4.2")
    skip_if_not_installed("ggcorrplot")

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "parametric correlation - with NAs",
      fig = ggcorrmat(data = ggplot2::msleep, type = "p")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "robust correlation - with NAs",
      fig = ggcorrmat(data = ggplot2::msleep, type = "r")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "non-parametric correlation - with NAs",
      fig = ggcorrmat(data = ggplot2::msleep, type = "np")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "bayesian correlation - with NAs",
      fig = suppressWarnings(ggcorrmat(data = ggplot2::msleep, type = "bayes"))
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
    skip_if_not_installed("vdiffr")
    skip_if(getRversion() < "4.1")
    skip_if(getRversion() >= "4.2")
    skip_if_not_installed("ggcorrplot")

    set.seed(123)
    vdiffr::expect_doppelganger(
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
        k = 4,
        ggcorrplot.args = list(
          lab_col = "white",
          pch.col = "white"
        )
      )
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
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

# checking all dataframe outputs -------------------------------------------

test_that(
  desc = "checking all dataframe outputs",
  code = {
    options(tibble.width = Inf)
    skip_if_not(.Platform$OS.type == "unix")

    set.seed(123)
    expect_snapshot(suppressWarnings(purrr::pmap(
      .l = list(
        data = list(dplyr::select(ggplot2::msleep, brainwt, sleep_rem, bodywt)),
        type = list("p", "p", "np", "np", "r", "r", "bf", "bayes"),
        output = list("dataframe"),
        partial = list(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
      ),
      .f = ggcorrmat
    )))
  }
)
