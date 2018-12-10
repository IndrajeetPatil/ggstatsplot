context("helpers_bf_tests")

# bayes factor plus posterior checks (correlation) --------------------------

testthat::test_that(
  desc = "bayes factor plus posterior checks (correlation)",
  code = {
    # creating a dataframe
    set.seed(123)
    df <- suppressMessages(ggstatsplot::bf_extractor(
      BayesFactor::correlationBF(
        x = ggplot2::msleep$brainwt,
        y = ggplot2::msleep$sleep_total
      ),
      posterior = TRUE,
      iterations = 500,
      cred.int = 0.90
    ))

    # check bayes factor values
    testthat::expect_equal(df$bf10, 8.990505, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, 2.196169, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, 0.9537841, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # check posterior values
    testthat::expect_equal(df$posterior.median, -0.34, tolerance = 0.01)
    testthat::expect_equal(df$posterior.sd, 0.13, tolerance = 0.01)
    testthat::expect_equal(df$posterior.std.error, 0.00411, tolerance = 0.001)
    testthat::expect_equal(df$HDI.low, -0.5403865, tolerance = 0.001)
    testthat::expect_equal(df$HDI.high, -0.1230767, tolerance = 0.001)
    testthat::expect_equal(df$cred.int, 0.90, tolerance = 0.01)
  }
)


# bayes factor plus posterior checks (paired t-test) ----------------------

testthat::test_that(
  desc = "bayes factor plus posterior checks (paired t-test)",
  code = {
    # creating a dataframe
    set.seed(123)
    data("bugs", package = "jmv")
    dat <- dplyr::filter(bugs, !is.na(HDLF), !is.na(HDHF))

    df <- suppressMessages(ggstatsplot::bf_extractor(
      BayesFactor::ttestBF(
        x = dat$HDLF,
        y = dat$HDHF,
        rscale = 0.8,
        paired = TRUE
      ),
      posterior = TRUE,
      iterations = 500,
      cred.int = 0.95
    ))

    # creating a tidy dataframe
    dat_tidy <- dat %>%
      tidyr::gather(data = ., key, value, c(HDLF, HDHF))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <- ggstatsplot::bf_two_sample_ttest(
      data = dat_tidy,
      x = key,
      y = value,
      paired = TRUE,
      bf.prior = 0.8,
      output = "results"
    )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 40.36079, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, 3.697859, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, 1.60596, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)

    # check posterior values
    testthat::expect_equal(df$posterior.median, -0.016, tolerance = 0.01)
    testthat::expect_equal(df$posterior.sd, 47.6, tolerance = 0.01)
    testthat::expect_equal(df$posterior.std.error, 1.064368, tolerance = 0.001)
    testthat::expect_equal(df$HDI.low, -1.778363, tolerance = 0.001)
    testthat::expect_equal(df$HDI.high, 10.33179, tolerance = 0.001)
    testthat::expect_equal(df$cred.int, 0.95, tolerance = 0.01)
  }
)

# bayes factor caption maker check --------------------------

testthat::test_that(
  desc = "bayes factor caption maker check",
  code = {

    # bayes factor results
    bf_results <- tibble::tribble( ~ log_e_bf01, ~ bf.prior,
                                   1.1, 0.88)

    # expected
    using1 <- ggstatsplot::bf_caption_maker(
      bf.df = bf_results,
      k = 3,
      caption = substitute(paste(italic("Note", ": made up data")))
    )

    testthat::expect_identical(using1,
                               ggplot2::expr(atop(
                                 displaystyle(paste(italic(
                                   "Note", ": made up data"
                                 ))),
                                 expr = paste(
                                   "In favor of null: ",
                                   "log"["e"],
                                   "(BF"["01"],
                                   ") = ",
                                   "1.100",
                                   ", Prior width = ",
                                   "0.880"
                                 )
                               )))

  })
