test_that(
  desc = "grouped_ggscatterstats plotting works as expected",
  code = {
    skip_if_not_installed("vdiffr")
    skip_if(getRversion() < "4.1")

    skip_if_not_installed("ggside")

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "defaults work as expected",
      fig = grouped_ggscatterstats(
        data = iris,
        Sepal.Length,
        Petal.Width,
        grouping.var = Species
      )
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "aesthetic modifications work",
      fig = grouped_ggscatterstats(
        data = ggplot2::msleep,
        x = sleep_total,
        y = bodywt,
        results.subtitle = FALSE,
        grouping.var = vore,
        xlab = "total sleep",
        ylab = "body weight",
        ggplot.component = scale_y_continuous(breaks = seq(0, 6000, 1000))
      )
    )
  }
)

# subtitle output --------------------------------------------------

test_that(
  desc = "subtitle output",
  code = {

    # data
    df <- dplyr::filter(movies_long, genre %in% c("Action Drama"))

    set.seed(123)
    ls_results <- grouped_ggscatterstats(
      data = df,
      x = rating,
      y = length,
      grouping.var = genre,
      k = 3,
      conf.level = 0.99,
      output = "subtitle"
    )

    set.seed(123)
    basic_results <- statsExpressions::corr_test(
      data = df,
      x = rating,
      y = length,
      k = 3,
      conf.level = 0.99
    )$expression[[1]]


    expect_equal(length(ls_results), 1L)
    expect_equal(ls_results$`Action Drama`, basic_results, ignore_attr = TRUE)
  }
)

test_that(
  desc = "grouped_ggscatterstats errors when no grouping is present",
  code = {
    expect_snapshot_error(
      grouped_ggscatterstats(
        data = iris,
        x = Sepal.Length,
        y = Petal.Width,
      )
    )
  }
)
