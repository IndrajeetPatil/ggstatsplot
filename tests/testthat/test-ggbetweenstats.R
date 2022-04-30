# pairwise comparisons testing is done `test-pairwise_ggsignif.R`

# checking labels and data from plot -------------------------------------

test_that(
  desc = "plotting features work as expected",
  code = {
    skip_if_not_installed("vdiffr")
    skip_if(getRversion() < "4.1")


    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "outlier tagging works",
      fig = ggbetweenstats(
        data = ggplot2::msleep,
        x = vore,
        y = brainwt,
        title = "mammalian sleep",
        caption = "From ggplot2 package",
        xlab = "vore",
        ylab = "brain weight",
        pairwise.comparisons = FALSE,
        results.subtitle = FALSE,
        outlier.tagging = TRUE,
        outlier.label = name,
        outlier.label.args = list(color = "darkgreen")
      )
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "modification with ggplot2 works as expected",
      fig = ggbetweenstats(
        data = tibble::as_tibble(mtcars, rownames = "name") %>%
          dplyr::rename(n = wt),
        x = cyl,
        y = n,
        pairwise.comparisons = FALSE,
        results.subtitle = FALSE
      ) +
        ggplot2::coord_cartesian(ylim = c(1, 6)) +
        ggplot2::scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, 1))
    )

    # edge case
    df_small <- data.frame(
      centrality.a = c(1.1, 0.9, 0.94, 1.58, 1.2, 1.4),
      group = c("a", "a", "a", "b", "b", "b")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "mean shown with scarce data",
      fig = suppressWarnings(ggbetweenstats(
        data = df_small,
        x = group,
        y = centrality.a,
        pairwise.comparisons = FALSE,
        results.subtitle = FALSE
      ))
    )
  }
)


# can produce different plots --------------------------------------

test_that(
  desc = "checking if `plot.type` argument works",
  code = {
    skip_if_not_installed("vdiffr")
    skip_if(getRversion() < "4.1")


    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "box plot",
      fig = ggbetweenstats(
        data = ToothGrowth,
        x = supp,
        y = len,
        plot.type = "box",
        outlier.tagging = TRUE,
        results.subtitle = FALSE,
        centrality.point.args = list(size = 5, color = "darkgreen"),
        centrality.label.args = list(color = "blue", nudge_x = 0.4, segment.linetype = 4)
      )
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "violin plot",
      fig = ggbetweenstats(
        data = ToothGrowth,
        x = supp,
        y = len,
        plot.type = "violin",
        outlier.tagging = TRUE,
        results.subtitle = FALSE
      )
    )
  }
)

# subtitle output works ------------------------------------------------

test_that(
  desc = "subtitle output works",
  code = {
    skip_on_cran()

    df <- mtcars
    df$wt[3] <- NA

    # plot
    set.seed(123)
    subtitle_exp <- ggbetweenstats(
      data = df,
      x = am,
      y = wt,
      output = "subtitle"
    )

    set.seed(123)
    sub <- statsExpressions::two_sample_test(
      data = df,
      x = am,
      y = wt
    )$expression[[1]]

    expect_equal(as.character(subtitle_exp), as.character(sub))
  }
)
