context("grouped_ggcorrmat")

testthat::test_that(
  desc = "grouped_ggcorrmat works",
  code = {

    # creating a smaller dataframe
    movies_filtered <- ggstatsplot::movies_long %>%
      dplyr::filter(.data = ., mpaa != "NC-17") %>%
      dplyr::sample_frac(tbl = ., size = 0.25)

    #--------------------- output: plot -------------------------------

    #--------------------- with cor.vars specified ---------------------

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(object = inherits(
      x = suppressWarnings(expr = ggstatsplot::grouped_ggcorrmat(
        data = movies_filtered,
        grouping.var = mpaa,
        cor.vars = length:votes,
        corr.method = "p",
        messages = FALSE
      )),
      what = "gg"
    ))

    # when arguments are entered as character
    set.seed(123)
    testthat::expect_true(object = inherits(
      x = suppressWarnings(expr = ggstatsplot::grouped_ggcorrmat(
        data = movies_filtered,
        grouping.var = "mpaa",
        cor.vars = c("length":"votes"),
        corr.method = "np",
        messages = FALSE
      )),
      what = "gg"
    ))

    #--------------------- without cor.vars specified ---------------------

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(object = inherits(
      x = suppressWarnings(expr = ggstatsplot::grouped_ggcorrmat(
        data = movies_filtered,
        grouping.var = mpaa,
        corr.method = "p",
        messages = FALSE
      )),
      what = "gg"
    ))

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(object = inherits(
      x = suppressWarnings(expr = ggstatsplot::grouped_ggcorrmat(
        data = movies_filtered,
        grouping.var = "mpaa",
        corr.method = "r",
        messages = FALSE
      )),
      what = "gg"
    ))
  }
)
