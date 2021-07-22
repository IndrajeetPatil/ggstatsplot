# significant display works -------------------------------------------------

test_that(
  desc = "check mcp displays - parametric - significant",
  code = {
    # creating the plot
    set.seed(123)
    p <- ggbetweenstats(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      results.subtitle = FALSE,
      bf.message = FALSE,
      p.adjust.method = "none",
      pairwise.display = "ns",
      caption = "mammalian sleep",
      k = 3
    )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(list(pb$data[[6]], pb$data[[7]]))
    expect_snapshot(pb$plot$labels)
  }
)


# non-significant display works ---------------------------------------------

test_that(
  desc = "check mcp displays - non-significant",
  code = {
    # creating the plot
    set.seed(123)
    p1 <- ggbetweenstats(
      data = movies_long,
      x = mpaa,
      y = votes,
      results.subtitle = FALSE,
      bf.message = FALSE,
      p.adjust.method = "none",
      pairwise.display = "ns",
      k = 3
    )

    set.seed(123)
    p2 <- ggbetweenstats(
      data = movies_long,
      x = mpaa,
      y = votes,
      results.subtitle = FALSE,
      bf.message = FALSE,
      var.equal = TRUE,
      p.adjust.method = "none",
      pairwise.display = "s",
      k = 4
    )

    # build the plot
    pb1 <- ggplot2::ggplot_build(p1)
    pb2 <- ggplot2::ggplot_build(p2)

    # check data
    set.seed(123)
    expect_snapshot(list(
      pb1$data[[6]], pb1$data[[7]],
      pb2$data[[6]], pb2$data[[7]]
    ))
    expect_snapshot(list(pb1$plot$labels, pb2$plot$labels))
  }
)

# mixed display works -------------------------------------------------

test_that(
  desc = "check mixed comparison displays - nonparametric",
  code = {
    # creating the plot
    set.seed(123)
    p <- ggbetweenstats(
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
      k = 3
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
    # creating the plot
    set.seed(123)
    p <- ggbetweenstats(
      data = ggplot2::mpg,
      x = drv,
      y = cty,
      results.subtitle = FALSE,
      bf.message = FALSE,
      k = 3,
      type = "r",
      nboot = 20,
      pairwise.display = "s"
    )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(list(pb$data[[6]], pb$data[[7]]))
    expect_snapshot(pb$plot$labels)
  }
)

# check bayesian test display works ----------------------------------------

test_that(
  desc = "check bayesian test display",
  code = {
    # creating the plot
    set.seed(123)
    p <- ggbetweenstats(
      data = iris,
      x = Species,
      y = Sepal.Length,
      results.subtitle = FALSE,
      bf.message = FALSE,
      k = 3,
      type = "bf",
      pairwise.display = "everything"
    )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(list(pb$data[[6]], pb$data[[7]]))
    expect_snapshot(pb$plot$labels)
  }
)

test_that(
  desc = "additional test",
  code = {
    set.seed(123)
    p1 <- ggbetweenstats(dplyr::filter(movies_long, genre == "Comedy"), mpaa, length,
      results.subtitle = FALSE,
      bf.message = FALSE
    )

    set.seed(123)
    p2 <- ggbetweenstats(dplyr::filter(movies_long, genre == "Action"), mpaa, length,
      results.subtitle = FALSE,
      bf.message = FALSE
    )

    # build the plot
    pb1 <- ggplot2::ggplot_build(p1)
    pb2 <- ggplot2::ggplot_build(p2)

    # check data
    set.seed(123)
    expect_snapshot(length(pb1$data))
    expect_snapshot(list(pb2$data[[6]], pb2$data[[7]]))
    expect_snapshot(pb1$plot$labels)
    expect_snapshot(pb2$plot$labels)
  }
)
