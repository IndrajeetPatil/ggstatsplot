test_that(
  "checking if extract_stats works",
  {
    skip_if_not_installed("rstantools")
    options(tibble.width = Inf)

    set.seed(123)
    expect_snapshot({
      p1 <- ggbetweenstats(mtcars, am, mpg)
      list(
        length(extract_stats(p1)),
        extract_subtitle(p1),
        extract_caption(p1)
      )
    })

    set.seed(123)
    expect_snapshot({
      p2 <- ggscatterstats(mtcars, wt, mpg, marginal = FALSE, type = "r")
      list(
        length(extract_stats(p2)),
        extract_subtitle(p2),
        extract_caption(p2)
      )
    })

    set.seed(123)
    expect_snapshot({
      p3 <- ggcorrmat(iris)
      list(
        length(extract_stats(p3)),
        extract_subtitle(p3),
        extract_caption(p3)
      )
    })

    set.seed(123)
    expect_snapshot({
      p4 <- ggbetweenstats(mtcars, cyl, mpg)
      list(
        length(extract_stats(p4)),
        extract_subtitle(p4),
        extract_caption(p4)
      )
    })

    set.seed(123)
    expect_snapshot({
      p5 <- ggpiestats(mtcars, cyl)
      list(
        length(extract_stats(p5)),
        extract_subtitle(p5),
        extract_caption(p5)
      )
    })

    set.seed(123)
    expect_snapshot({
      p6 <- ggbarstats(mtcars, cyl, am)
      list(
        length(extract_stats(p6)),
        extract_subtitle(p6),
        extract_caption(p6)
      )
    })

    set.seed(123)
    expect_snapshot({
      p7 <- ggcoefstats(lm(wt ~ mpg, mtcars))
      list(
        length(extract_stats(p7)),
        extract_subtitle(p7),
        extract_caption(p7)
      )
    })
  }
)
