# significant display works -------------------------------------------------

test_that(
  desc = "check comparison significant displays - FDR-corrected",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggbetweenstats(
        data = ggplot2::msleep,
        x = vore,
        y = brainwt,
        results.subtitle = FALSE,
        bf.message = FALSE,
        pairwise.display = "s",
        caption = "mammalian sleep",
        k = 3
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data[[6]])
    expect_snapshot(pb$plot$labels)
  }
)


# non-significant display works ---------------------------------------------

test_that(
  desc = "check non-significant comparison displays - no adjustment",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggbetweenstats(
        data = movies_wide,
        x = mpaa,
        y = votes,
        results.subtitle = FALSE,
        bf.message = FALSE,
        p.adjust.method = "none",
        pairwise.display = "ns",
        pairwise.annotation = "p.value",
        k = 3
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data[[6]])
    expect_snapshot(pb$plot$labels)
  }
)

# mixed display works -------------------------------------------------

test_that(
  desc = "check mixed comparison displays - FDR-corrected",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggbetweenstats(
        data = dplyr::filter(
          movies_long,
          genre %in% c("Action", "Comedy", "RomCom")
        ),
        x = genre,
        y = rating,
        results.subtitle = FALSE,
        type = "np",
        bf.message = FALSE,
        p.adjust.method = "fdr",
        pairwise.display = "all",
        k = 3,
        palette = "Set3"
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(list(pb$data[[6]], pb$data[[7]]))
    expect_snapshot(pb$plot$labels)
  }
)

# robust test works -------------------------------------------------

test_that(
  desc = "check robust test display - FDR-corrected",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggbetweenstats(
        data = ggplot2::mpg,
        x = drv,
        y = cty,
        results.subtitle = FALSE,
        bf.message = FALSE,
        k = 3,
        type = "r",
        nboot = 20,
        pairwise.display = "s",
        pairwise.annotation = "p.value"
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(list(pb$data[[6]], pb$data[[7]]))
    expect_snapshot(pb$plot$labels)
  }
)

# student's t test works -------------------------------------------------

test_that(
  desc = "check student's t test display - FDR-corrected",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggbetweenstats(
        data = mtcars,
        x = cyl,
        y = wt,
        results.subtitle = FALSE,
        bf.message = FALSE,
        k = 3,
        type = "p",
        p.adjust.method = "bonferroni",
        var.equal = TRUE,
        pairwise.display = "everything",
        pairwise.annotation = "p"
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(list(pb$data[[6]], pb$data[[7]]))
    expect_snapshot(pb$plot$labels)
  }
)
