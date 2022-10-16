# grouped_ggdotplotstats works -----------------------------------------------

test_that(
  desc = "grouped_ggdotplotstats works",
  code = {
    # removing factor level with very few no. of observations
    df <- dplyr::filter(ggplot2::mpg, cyl %in% c("4", "6", "8"))


    skip_if(getRversion() < "4.1")


    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "defaults work as expected",
      fig = grouped_ggdotplotstats(
        data = df,
        x = cty,
        y = manufacturer,
        xlab = "city miles per gallon",
        ylab = "car manufacturer",
        grouping.var = cyl,
        test.value = 15.5,
        point.args = list(color = "red", size = 5, shape = 13),
        results.subtitle = FALSE,
        ggtheme = ggplot2::theme_classic()
      )
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "further modification with ggplot works",
      fig = grouped_ggdotplotstats(
        data = df,
        x = cty,
        y = manufacturer,
        grouping.var = cyl,
        test.value = 15.5,
        results.subtitle = FALSE,
        effsize.type = "d",
        ggplot.component = ggplot2::scale_y_continuous(
          sec.axis = ggplot2::dup_axis(name = "percentile score"),
          breaks = seq(0, 12, 2)
        )
      )
    )
  }
)

# subtitle output --------------------------------------------------

test_that(
  desc = "subtitle output",
  code = {
    # removing factor level with very few no. of observations
    df <- dplyr::filter(ggplot2::mpg, cyl %in% c("4"))

    set.seed(123)
    ls_results <-
      grouped_ggdotplotstats(
        data = df,
        x = cty,
        y = manufacturer,
        grouping.var = cyl,
        test.value = 15.5,
        output = "subtitle"
      )

    set.seed(123)
    basic_results <- ggdotplotstats(
      data = df,
      x = cty,
      y = manufacturer,
      test.value = 15.5,
      output = "subtitle"
    )

    expect_equal(ls_results$`4`, basic_results)
  }
)
