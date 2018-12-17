context("subtitle_templates")

# single parameter -----------------------------------------------------------

testthat::test_that(
  desc = "checking if subtitle template works with a single parameter",
  code = {

    # subtitle
    set.seed(123)
    p <- ggplot2::expr(paste(
      NULL, italic(chi)^2, "(", "2", ") = ", "8.74",
      ", ", italic("p"), " = ", "0.013",
      ", ", italic(V), " = ", "0.52", ", CI"["99%"],
      " [", "0.13", ", ", "0.93", "]",
      ", ", italic("n"), " = ", 32
    ))

    # created using a template maker
    template_1 <- ggstatsplot::subtitle_template_1(
      statistic.text = quote(italic(chi)^2),
      statistic = 8.74,
      parameter = 2,
      p.value = 0.013,
      effsize.estimate = 0.52,
      effsize.LL = 0.13,
      effsize.UL = 0.93,
      conf.level = 0.99,
      k = 2L,
      k.parameter = 0L,
      n = 32,
      effsize.text = quote(italic(V))
    )

    # check if they are equivalent
    testthat::expect_identical(p, template_1)
  }
)
