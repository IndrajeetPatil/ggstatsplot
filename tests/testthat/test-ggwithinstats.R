# graphical pairwise comparisons are tested in `test-pairwise_ggsignif.R`
skip_if_not_installed("PMCMRplus")
skip_if_not_installed("afex")
skip_if_not_installed("WRS2")

data_bugs_2 <- dplyr::filter(bugs_long, subject <= 30L, condition %in% c("HDLF", "HDHF"))

# defaults plots ---------------------------------

test_that(
  desc = "defaults plots",
  code = {
    expect_snapshot_error(grouped_ggbetweenstats(bugs_long, x = condition, y = desire))

    set.seed(123)
    expect_doppelganger(
      title = "defaults plots - two groups",
      fig = ggwithinstats(
        data = data_bugs_2,
        x = condition,
        y = desire,
        pairwise.comparisons = FALSE,
        ggsignif.args = list(textsize = 6, tip_length = 0.01),
        point.path.args = list(color = "red"),
        centrality.path.args = list(color = "blue", size = 2, alpha = 0.8),
        centrality.point.args = list(size = 3, color = "darkgreen", alpha = 0.5),
        title = "bugs dataset"
      )
    )

    set.seed(123)
    expect_doppelganger(
      title = "defaults plots - more than two groups",
      fig = ggwithinstats(
        data = WRS2::WineTasting,
        x = Wine,
        y = Taste,
        pairwise.comparisons = FALSE,
        title = "wine tasting data"
      )
    )
  }
)

# aesthetic modifications work ------------------------------------------

test_that(
  desc = "aesthetic modifications work",
  code = {
    set.seed(123)
    expect_doppelganger(
      title = "ggplot2 commands work",
      fig = ggwithinstats(
        data = WRS2::WineTasting,
        x = Wine,
        y = Taste,
        results.subtitle = FALSE,
        pairwise.comparisons = FALSE,
        ggplot.component = ggplot2::labs(y = "Taste rating")
      )
    )

    set.seed(123)
    expect_doppelganger(
      title = "centrality path can be turned off",
      fig = ggwithinstats(
        iris_long,
        condition,
        value,
        centrality.point.args = list(size = 5, alpha = 0.5, color = "darkred"),
        centrality.path = FALSE,
        results.subtitle = FALSE,
        pairwise.comparisons = FALSE
      )
    )
  }
)
