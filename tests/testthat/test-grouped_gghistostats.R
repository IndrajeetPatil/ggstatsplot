# grouped_gghistostats works ---------------------------------------------

testthat::test_that(
  desc = "grouped_gghistostats works",
  code = {
    testthat::skip_on_cran()

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_gghistostats(
        data = ggplot2::msleep,
        x = brainwt,
        grouping.var = vore,
        type = "p",
        results.subtitle = FALSE,
        normal.curve = TRUE,
        bar.measure = "mix",
        bf.message = TRUE,
        messages = FALSE
      ),
      what = "gg"
    ))

    # when arguments are entered as character
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_gghistostats(
        data = ggplot2::msleep,
        x = "brainwt",
        grouping.var = "vore",
        type = "r",
        results.subtitle = FALSE,
        normal.curve = TRUE,
        effsize.type = "d",
        effsize.noncentral = FALSE,
        bar.measure = "proportion",
        ggplot.component = ggplot2::scale_x_continuous(
          sec.axis = ggplot2::dup_axis(name = ggplot2::element_blank())
        ),
        messages = FALSE
      ),
      what = "gg"
    ))
  }
)

# subtitle output --------------------------------------------------

testthat::test_that(
  desc = "subtitle output",
  code = {
    testthat::skip_on_cran()

    df <- dplyr::filter(ggplot2::msleep, vore == "omni")

    set.seed(123)
    ls_results <-
      ggstatsplot::grouped_gghistostats(
        data = df,
        x = brainwt,
        grouping.var = vore,
        test.value = 0.25,
        output = "subtitle",
        messages = FALSE
      )

    set.seed(123)
    basic_results <-
      statsExpressions::expr_t_onesample(
        data = df,
        x = brainwt,
        test.value = 0.25,
        output = "subtitle",
        messages = FALSE
      )

    # tests
    testthat::expect_equal(ls_results$`omni`, basic_results)
  }
)
