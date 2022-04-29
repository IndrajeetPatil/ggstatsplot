# outlier labeling works --------------------------------------------------

test_that(
  desc = "grouped_ggbetweenstats defaults",
  code = {
    skip_if_not_installed("vdiffr")
    skip_if(getRversion() < "4.1")

    skip_if_not_installed("PMCMRplus")

    # expect error when no grouping.var is specified
    expect_snapshot_error(grouped_ggbetweenstats(dat, x = genre, y = rating))

    # creating a smaller dataframe
    set.seed(123)
    dat <- dplyr::sample_frac(movies_long, size = 0.25) %>%
      dplyr::filter(
        mpaa %in% c("R", "PG-13"),
        genre %in% c("Drama", "Comedy")
      )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "default plot as expected",
      fig = grouped_ggbetweenstats(
        data = dat,
        x = genre,
        y = rating,
        grouping.var = mpaa,
        outlier.tagging = TRUE,
        outlier.label = title,
        outlier.coef = 5,
        ggplot.component = ggplot2::scale_y_continuous(breaks = seq(1, 9, 1)),
      )
    )
  }
)

# expression output --------------------

test_that(
  desc = "expression output is as expected",
  code = {
    set.seed(123)
    grouped_expr <- grouped_ggbetweenstats(
      mtcars,
      grouping.var = am,
      x = vs,
      y = wt,
      output = "subtitle"
    )

    set.seed(123)
    base_expr <- ggbetweenstats(dplyr::filter(mtcars, am == "0"), vs, wt, output = "subtitle")

    expect_equal(grouped_expr$`0`, base_expr)
  }
)
