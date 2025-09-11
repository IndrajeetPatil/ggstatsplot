# checking default outputs -----------------------------------------

test_that(
  "checking default outputs",
  {
    set.seed(123)
    expect_doppelganger(
      title = "parametric - without NA",
      fig = gghistostats(ggplot2::mpg, cty, test.value = 20, type = "p")
    )

    set.seed(123)
    expect_doppelganger(
      title = "robust - with NA",
      fig = gghistostats(ggplot2::msleep, sleep_total, test.value = 10, type = "r")
    )
  }
)

# aesthetic modifications work --------------------------------------

test_that(
  "aesthetic modifications work",
  {
    set.seed(123)
    expect_doppelganger(
      title = "aesthetic modifications work",
      fig = gghistostats(
        data = mtcars,
        x = wt,
        xlab = "weight",
        title = "mtcars: car wights",
        binwidth = 5,
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
  "checking if normal curve work",
  {
    set.seed(123)
    expect_doppelganger(
      title = "plot normal curve",
      fig = gghistostats(
        data = ggplot2::msleep,
        x = awake,
        binwidth = 1,
        results.subtitle = FALSE
      )
    )
  }
)

# subtitle output --------------------------------------------------

test_that(
  "subtitle output",
  {
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

# grouped_gghistostats works ---------------------------------------------

test_that(
  "grouped_gghistostats plotting works as expected",
  {
    set.seed(123)
    expect_doppelganger(
      title = "defaults as expected",
      fig = grouped_gghistostats(
        data = ggplot2::msleep,
        x = brainwt,
        grouping.var = vore
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
          sec.axis = ggplot2::dup_axis()
        )
      ))
    )
  }
)
