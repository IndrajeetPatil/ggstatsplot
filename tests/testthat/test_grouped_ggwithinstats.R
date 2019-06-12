# context -----------------------------------------------------------------
context(desc = "grouped_ggwithinstats")

# outlier labeling works --------------------------------------------------

testthat::test_that(
  desc = "grouping.var works across vector types",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # expect error when no grouping.var is specified
    testthat::expect_error(
      ggstatsplot::grouped_ggwithinstats(
        VR_dilemma,
        x = modality,
        y = score
      )
    )

    # expect error when x and grouping.var are same
    testthat::expect_output(
      ggstatsplot::grouped_ggwithinstats(
        VR_dilemma,
        x = modality,
        y = score,
        grouping.var = modality
      )
    )

    # outlier tagging is not required
    testthat::expect_is(
      ggstatsplot::grouped_ggwithinstats(
        VR_dilemma,
        x = modality,
        y = score,
        grouping.var = order,
        outlier.tagging = FALSE,
        messages = TRUE
      ),
      "ggplot"
    )

    # `outlier.label` is not specified
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggwithinstats(
        VR_dilemma,
        x = modality,
        y = score,
        grouping.var = order,
        type = "p",
        bf.message = TRUE,
        outlier.tagging = TRUE,
        pairwise.comparisons = TRUE,
        pairwise.annotation = "p.score",
        messages = TRUE
      ),
      what = "gg"
    ))

    # `outlier.label` is character
    # also x, y, and outlier.label arguments as characters
    set.seed(123)
    dat <- iris_long
    dat$id <- as.character(dat$id)

    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggwithinstats(
        data = dat,
        x = "attribute",
        y = "value",
        grouping.var = Species,
        messages = FALSE,
        type = "r",
        pairwise.comparisons = TRUE,
        outlier.tagging = TRUE,
        outlier.label = "id",
        outlier.coef = 2
      ),
      what = "gg"
    ))
  }
)

# outlier labeling works --------------------------------------------------

testthat::test_that(
  desc = "grouping.var works across vector types",
  code = {
    testthat::skip_on_cran()

    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggwithinstats(
        data = VR_dilemma,
        x = modality,
        y = score,
        grouping.var = order,
        results.subtitle = FALSE,
        ggplot.component = ggplot2::scale_y_continuous(
          limits = c(0, 1),
          breaks = seq(0, 1, 0.1)
        ),
        messages = FALSE
      ),
      what = "gg"
    ))
  }
)

# subtitle return --------------------------------------------------

testthat::test_that(
  desc = "subtitle return",
  code = {
    testthat::skip_on_cran()

    # should return a list of length 3
    set.seed(123)
    ls_results <- ggstatsplot::grouped_ggwithinstats(
      data = iris_long,
      x = condition,
      y = value,
      grouping.var = Species,
      return = "subtitle",
      messages = FALSE
    )

    # tests
    testthat::expect_equal(length(ls_results), 3L)
    testthat::expect_identical(
      ls_results[[1]],
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "1.88",
          ",",
          "91.96",
          ") = ",
          "4276.86",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          omega^
            2,
          " = ",
          "0.98",
          ", CI"["95%"],
          " [",
          "0.98",
          ", ",
          "0.99",
          "]",
          ", ",
          italic("n"),
          " = ",
          50L
        )
      )
    )
    testthat::expect_identical(
      ls_results[[2]],
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "2.16",
          ",",
          "106.01",
          ") = ",
          "2821.59",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          omega^2,
          " = ",
          "0.95",
          ", CI"["95%"],
          " [",
          "0.97",
          ", ",
          "0.98",
          "]",
          ", ",
          italic("n"),
          " = ",
          50L
        )
      )
    )
    testthat::expect_identical(
      ls_results[[3]],
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "1.67",
          ",",
          "81.87",
          ") = ",
          "1910.86",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          omega^2,
          " = ",
          "0.94",
          ", CI"["95%"],
          " [",
          "0.96",
          ", ",
          "0.97",
          "]",
          ", ",
          italic("n"),
          " = ",
          50L
        )
      )
    )
  }
)
