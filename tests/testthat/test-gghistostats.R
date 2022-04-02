# checking default outputs -----------------------------------------

test_that(
  desc = "checking default outputs",
  code = {
    skip_if_not_installed("vdiffr")
    skip_if(getRversion() < "4.1")
    skip_if(getRversion() >= "4.2")

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "parametric - without NA",
      fig = gghistostats(ggplot2::mpg, cty, test.value = 20, type = "p")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "non-parametric - without NA",
      fig = gghistostats(ggplot2::mpg, cty, test.value = 20, type = "np")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "robust - without NA",
      fig = gghistostats(ggplot2::mpg, cty, test.value = 20, type = "r")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "bayes - without NA",
      fig = gghistostats(ggplot2::mpg, cty, test.value = 20, type = "bayes")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "parametric - with NA",
      fig = gghistostats(dplyr::starwars, height, test.value = 150, type = "p")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "non-parametric - with NA",
      fig = gghistostats(dplyr::starwars, height, test.value = 150, type = "np")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "robust - with NA",
      fig = gghistostats(dplyr::starwars, height, test.value = 150, type = "r")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "bayes - with NA",
      fig = gghistostats(dplyr::starwars, height, test.value = 150, type = "bayes")
    )
  }
)

# aesthetic modifications work --------------------------------------

test_that(
  desc = "aesthetic modifications work",
  code = {
    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "aesthetic modifications work",
      fig = gghistostats(
        data = dplyr::starwars,
        x = height,
        xlab = "character height",
        title = "starwars: character heights",
        binwidth = 20,
        bin.args = list(
          col = "black",
          fill = "orange",
          alpha = 0.7
        ),
        results.subtitle = FALSE
      )
    )
  }
)

# normal curve works -------------------------------------

test_that(
  desc = "checking if normal curve work",
  code = {
    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "plot normal curve",
      fig = gghistostats(
        data = ggplot2::msleep,
        x = awake,
        binwidth = 1,
        results.subtitle = FALSE,
        normal.curve = TRUE,
        normal.curve.args = list(
          color = "red",
          size = 0.8
        )
      )
    )
  }
)

# subtitle output --------------------------------------------------

test_that(
  desc = "subtitle output",
  code = {
    # should output a list of length 3
    set.seed(123)
    p_sub <- gghistostats(
      data = ggplot2::msleep,
      x = brainwt,
      type = "np",
      output = "subtitle",
      test.value = 0.25
    )

    set.seed(123)
    sub <- statsExpressions::one_sample_test(
      data = ggplot2::msleep,
      x = brainwt,
      type = "np",
      test.value = 0.25
    )$expression[[1]]


    expect_equal(p_sub, sub, ignore_attr = TRUE)
  }
)

# utilities ---------------

test_that(
  desc = ".binwidth works as expected",
  code = {
    expect_equal(ggstatsplot:::.binwidth(mtcars$wt), 0.6913737, tolerance = 0.001)
  }
)
