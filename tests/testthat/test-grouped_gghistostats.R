context("grouped_gghistostats")

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

    # when the data argument is missing, expect error
    # this is supposed to be the case only for `gghistostats` and not its
    # `grouped_` variant
    set.seed(123)
    testthat::expect_error(
      ggstatsplot::grouped_gghistostats(
        x = ggplot2::msleep$brainwt,
        grouping.var = "vore",
        type = "np",
        results.subtitle = FALSE,
        normal.curve = TRUE,
        bar.measure = "density",
        messages = FALSE
      )
    )
  }
)

# subtitle output --------------------------------------------------

testthat::test_that(
  desc = "subtitle output",
  code = {
    testthat::skip_on_cran()

    # should output a list of length 3
    set.seed(123)
    ls_results <-
      ggstatsplot::grouped_gghistostats(
        data = ggplot2::msleep,
        x = brainwt,
        grouping.var = vore,
        output = "subtitle",
        messages = FALSE
      )

    # tests
    testthat::expect_equal(
      ls_results,
      list(
        carni = ggplot2::expr(paste(
          NULL,
          italic("t"),
          "(",
          "8",
          ") = ",
          "2.31",
          ", ",
          italic("p"),
          " = ",
          "0.049",
          ", ",
          widehat(italic("g")),
          " = ",
          "0.69",
          ", CI"["95%"],
          " [",
          "0.00",
          ", ",
          "1.60",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          9L
        )),
        herbi = ggplot2::expr(paste(
          NULL,
          italic("t"),
          "(",
          "19",
          ") = ",
          "1.77",
          ", ",
          italic("p"),
          " = ",
          "0.093",
          ", ",
          widehat(italic("g")),
          " = ",
          "0.38",
          ", CI"["95%"],
          " [",
          "-0.07",
          ", ",
          "0.87",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          20L
        )),
        insecti = ggplot2::expr(paste(
          NULL,
          italic("t"),
          "(",
          "4",
          ") = ",
          "1.38",
          ", ",
          italic("p"),
          " = ",
          "0.239",
          ", ",
          widehat(italic("g")),
          " = ",
          "0.45",
          ", CI"["95%"],
          " [",
          "-0.43",
          ", ",
          "1.74",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          5L
        )),
        omni = ggplot2::expr(paste(
          NULL,
          italic("t"),
          "(",
          "16",
          ") = ",
          "1.85",
          ", ",
          italic("p"),
          " = ",
          "0.083",
          ", ",
          widehat(italic("g")),
          " = ",
          "0.43",
          ", CI"["95%"],
          " [",
          "-0.06",
          ", ",
          "0.97",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          17L
        ))
      )
    )
  }
)
