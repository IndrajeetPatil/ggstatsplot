test_that(
  desc = "checking if extract_stats works",
  code = {
    skip_if_not_installed("PMCMRplus")
    options(tibble.width = Inf)

    set.seed(123)
    p1 <- ggbetweenstats(mtcars, am, mpg)
    expect_snapshot(length(extract_stats(p1)))

    set.seed(123)
    p2 <- ggscatterstats(mtcars, wt, mpg, marginal = FALSE, type = "r")
    expect_snapshot(length(extract_stats(p2)))

    set.seed(123)
    p3 <- ggcorrmat(iris)
    expect_snapshot(length(extract_stats(p3)))

    set.seed(123)
    p4 <- ggbetweenstats(mtcars, cyl, mpg)
    expect_snapshot(length(extract_stats(p4)$pairwise_comparisons_data))

    set.seed(123)
    p5 <- ggpiestats(mtcars, cyl)
    expect_snapshot(length(extract_stats(p5)))

    set.seed(123)
    p6 <- ggbarstats(mtcars, cyl, am)
    expect_snapshot(length(extract_stats(p6)))
  }
)
