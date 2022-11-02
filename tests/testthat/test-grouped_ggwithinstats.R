# outlier labeling works --------------------------------------------------

test_that(
  desc = "default plotting as expected",
  code = {
    skip_if(getRversion() < "4.1")

    skip_if_not_installed("PMCMRplus")
    skip_if_not_installed("afex")
    skip_if_not_installed("WRS2")

    # expect error when no grouping.var is specified
    expect_snapshot_error(grouped_ggbetweenstats(bugs_long, x = condition, y = desire))

    # outlier tagging is not required
    set.seed(123)
    expect_doppelganger(
      title = "default plots",
      fig = grouped_ggwithinstats(
        data = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
        x = condition,
        y = desire,
        grouping.var = gender,
        results.subtitle = FALSE
      )
    )

    set.seed(123)
    expect_doppelganger(
      title = "outlier tagging and themes work",
      fig = grouped_ggwithinstats(
        data = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
        x = condition,
        y = desire,
        grouping.var = gender,
        ggtheme = ggplot2::theme_linedraw(),
        results.subtitle = FALSE,
        outlier.tagging = TRUE
      )
    )
  }
)

# subtitle output with NA --------------------------------------------------

test_that(
  desc = "subtitle output",
  code = {
    skip_if_not_installed("afex")

    # data
    df <- dplyr::filter(bugs_long, region %in% c("North America"))

    # should output a list of length 2
    set.seed(123)
    ls_results <- grouped_ggwithinstats(
      data = df,
      x = condition,
      y = desire,
      grouping.var = region,
      output = "subtitle",
      bf.message = FALSE
    )

    set.seed(123)
    basic_results <- ggwithinstats(
      data = df,
      x = condition,
      y = desire,
      output = "subtitle",
      bf.message = FALSE
    )

    expect_equal(ls_results$`North America`, basic_results)
  }
)
