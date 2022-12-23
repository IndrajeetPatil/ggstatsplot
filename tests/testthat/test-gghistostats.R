skip_if(getRversion() < "4.1")

# checking default outputs -----------------------------------------

test_that(
  desc = "checking default outputs",
  code = {
    set.seed(123)
    expect_doppelganger(
      title = "parametric - without NA",
      fig = gghistostats(ggplot2::mpg, cty, test.value = 20, type = "p")
    )

    set.seed(123)
    expect_doppelganger(
      title = "robust - with NA",
      fig = gghistostats(dplyr::starwars, height, test.value = 150, type = "r")
    )
  }
)

# aesthetic modifications work --------------------------------------

test_that(
  desc = "aesthetic modifications work",
  code = {
    set.seed(123)
    expect_doppelganger(
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
    expect_doppelganger(
      title = "plot normal curve",
      fig = gghistostats(
        data = ggplot2::msleep,
        x = awake,
        binwidth = 1,
        results.subtitle = FALSE,
        normal.curve = TRUE,
        normal.curve.args = list(color = "red", linewidth = 0.8)
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
      test.value = 0.25
    ) %>%
      extract_subtitle()

    set.seed(123)
    sub <- one_sample_test(
      data = ggplot2::msleep,
      x = brainwt,
      type = "np",
      test.value = 0.25
    )$expression[[1L]]

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

# grouped_gghistostats works ---------------------------------------------

test_that(
  desc = "grouped_gghistostats plotting works as expected",
  code = {
    set.seed(123)
    expect_doppelganger(
      title = "defaults as expected",
      fig = grouped_gghistostats(
        data = ggplot2::msleep,
        x = brainwt,
        grouping.var = vore,
        normal.curve = TRUE
      )
    )

    set.seed(123)
    expect_doppelganger(
      title = "modification with ggplot2 works",
      fig = suppressWarnings(grouped_gghistostats(
        data = ggplot2::msleep,
        x = brainwt,
        grouping.var = vore,
        results.subtitle = FALSE,
        ggplot.component = ggplot2::scale_x_continuous(
          sec.axis = ggplot2::dup_axis(name = ggplot2::element_blank())
        )
      ))
    )
  }
)
