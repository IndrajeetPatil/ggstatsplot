# pairwise comparisons testing is done `test-pairwise_ggsignif.R`

skip_if(getRversion() < "4.1")
skip_if_not_installed("PMCMRplus")
skip_if_not_installed("afex")
skip_if_not_installed("WRS2")

# data
data_bugs_2 <- dplyr::filter(bugs_long, subject <= 30, condition %in% c("HDLF", "HDHF"))

# defaults plots ---------------------------------

test_that(
  desc = "defaults plots",
  code = {
    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "defaults plots - two groups",
      fig = ggwithinstats(
        data = data_bugs_2,
        x = condition,
        y = desire,
        outlier.tagging = TRUE,
        outlier.label = "region",
        outlier.coef = 1.5,
        pairwise.comparisons = FALSE,
        ggsignif.args = list(textsize = 6, tip_length = 0.01),
        point.path.args = list(color = "red"),
        centrality.path.args = list(color = "blue", size = 2, alpha = 0.8),
        centrality.point.args = list(size = 3, color = "darkgreen", alpha = 0.5),
        title = "bugs dataset"
      )
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "defaults plots - more than two groups",
      fig = ggwithinstats(
        data = WRS2::WineTasting,
        x = Wine,
        y = Taste,
        outlier.tagging = TRUE,
        outlier.coef = 2.5,
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
    vdiffr::expect_doppelganger(
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
    vdiffr::expect_doppelganger(
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

# outlier labeling works --------------------------------------------------

test_that(
  desc = "default plotting as expected",
  code = {
    # expect error when no grouping.var is specified
    expect_snapshot_error(grouped_ggbetweenstats(bugs_long, x = condition, y = desire))

    # outlier tagging is not required
    set.seed(123)
    vdiffr::expect_doppelganger(
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
    vdiffr::expect_doppelganger(
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
