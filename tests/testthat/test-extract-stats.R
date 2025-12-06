test_that(
  "checking if extract_stats works",
  {
    skip_if_not_installed("rstantools")
    skip_if_not_installed("withr")
    withr::local_options(tibble.width = Inf)

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

    # https://github.com/kassambara/ggcorrplot/issues/57
    withr::with_options(
      list(warn = -1L),
      code = {
        set.seed(123)
        expect_snapshot({
          p3 <- ggcorrmat(iris)
          list(
            length(extract_stats(p3)),
            extract_subtitle(p3),
            extract_caption(p3)
          )
        })
      }
    )

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


test_that(
  "checking if extract_stats works for grouped plots",
  {
    expect_snapshot({
      p8 <- grouped_ggpiestats(mtcars, x = cyl, grouping.var = am)
      extracted_data <- extract_stats(p8)
      summary(extracted_data)
      extract_subtitle(p8)
      extract_caption(p8)
    })
  }
)

test_that(
  "checking if extract_stats produces NULL on supported objects",
  {
    expect_length(purrr::compact(extract_stats(iris)), 0L)
  }
)
