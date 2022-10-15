# grouped_gghistostats works ---------------------------------------------

test_that(
  desc = "grouped_gghistostats plotting works as expected",
  code = {
    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "defaults as expected",
      fig = grouped_gghistostats(
        data = ggplot2::msleep,
        x = brainwt,
        grouping.var = vore,
        normal.curve = TRUE
      )
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
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

# subtitle output --------------------------------------------------

test_that(
  desc = "subtitle output",
  code = {
    df <- dplyr::filter(ggplot2::msleep, vore == "omni")

    set.seed(123)
    ls_results <- grouped_gghistostats(
      data = df,
      x = brainwt,
      grouping.var = vore,
      test.value = 0.25,
      output = "subtitle"
    )

    set.seed(123)
    basic_results <- one_sample_test(
      data = df,
      x = brainwt,
      test.value = 0.25
    )$expression[[1]]

    expect_equal(ls_results$`omni`, basic_results)
  }
)
