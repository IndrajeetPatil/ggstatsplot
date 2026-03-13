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

    # Explicitly test that extract functions return list for grouped plots
    set.seed(123)
    p9 <- grouped_gghistostats(iris, Sepal.Length, grouping.var = Species)
    subtitle_result <- extract_subtitle(p9)
    caption_result <- extract_caption(p9)

    expect_type(subtitle_result, "list")
    expect_type(caption_result, "list")
    expect_length(subtitle_result, 3L) # 3 species groups
    expect_length(caption_result, 3L)
  }
)

test_that(
  "checking if extract_stats produces NULL on supported objects",
  {
    expect_length(purrr::compact(extract_stats(iris)), 0L)
  }
)

test_that(
  "checking .extract_stats_data function factory",
  {
    # Test the factory function directly to ensure coverage of factory internals

    # Create custom extractors using the factory
    subtitle_extractor <- ggstatsplot:::.extract_stats_data("subtitle_data") # nolint: undesirable_operator_linter.
    caption_extractor <- ggstatsplot:::.extract_stats_data("caption_data") # nolint: undesirable_operator_linter.

    # Test subtitle extractor on non-grouped plot (cover if branch on line 78)
    set.seed(123)
    p_single <- ggbetweenstats(mtcars, am, mpg)
    result_single_sub <- subtitle_extractor(p_single)
    expect_type(result_single_sub, "language")

    # Test caption extractor on non-grouped plot
    result_single_cap <- caption_extractor(p_single)
    expect_type(result_single_cap, "language")

    # Test subtitle extractor on grouped plot (cover else branch on line 78)
    set.seed(123)
    p_grouped <- grouped_ggpiestats(mtcars, x = cyl, grouping.var = am)
    result_grouped_sub <- subtitle_extractor(p_grouped)
    expect_type(result_grouped_sub, "list")
    expect_length(result_grouped_sub, 2L)

    # Test caption extractor on grouped plot
    result_grouped_cap <- caption_extractor(p_grouped)
    expect_type(result_grouped_cap, "list")
    expect_length(result_grouped_cap, 2L)
  }
)
