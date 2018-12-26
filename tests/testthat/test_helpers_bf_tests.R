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



# bayes factor plus posterior checks (paired t-test) ----------------------

testthat::test_that(
  desc = "bayes factor plus posterior checks (paired t-test)",
  code = {
    # creating a dataframe
    set.seed(123)
    df <- suppressMessages(ggstatsplot::bf_extractor(
      BayesFactor::ttestBF(
        x = iris$Petal.Length,
        mu = 5.5,
        rscale = 0.99
      ),
      posterior = TRUE,
      iterations = 500,
      cred.int = 0.95
    ))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <- ggstatsplot::bf_one_sample_ttest(
      x = iris$Petal.Length,
      test.value = 5.5,
      bf.prior = 0.99,
      output = "results"
    )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 5.958171e+20, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, 47.83647, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, 20.77511, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)

    # check posterior values
    testthat::expect_equal(df$posterior.median, 3.23, tolerance = 0.01)
    testthat::expect_equal(df$posterior.sd, 267.03, tolerance = 0.01)
    testthat::expect_equal(df$posterior.std.error, 5.970972, tolerance = 0.001)
    testthat::expect_equal(df$HDI.low, 0.4319726, tolerance = 0.001)
    testthat::expect_equal(df$HDI.high, 13.50321, tolerance = 0.001)
    testthat::expect_equal(df$cred.int, 0.95, tolerance = 0.01)
  }
)



# bayes factor plus posterior checks (contingency tab) ----------------------

testthat::test_that(
  desc = "bayes factor plus posterior checks (contingency tab)",
  code = {

    # extracting results from where this function is implemented
    set.seed(123)
    df <- suppressMessages(ggstatsplot::bf_extractor(
      BayesFactor::contingencyTableBF(
        x = table(mtcars$am, mtcars$cyl),
        sampleType = "jointMulti",
        fixedMargin = "rows"
      ),
      posterior = TRUE,
      iterations = 500,
      cred.int = 0.95
    ))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <- ggstatsplot::bf_contingency_tab(
      data = mtcars,
      main = am,
      condition = cyl,
      sampling.plan = "jointMulti",
      fixed.margin = "rows",
      output = "results"
    )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 28.07349, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, 3.334826, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, 1.448296, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)

    # check posterior values
    testthat::expect_equal(df$posterior.median, 0.13, tolerance = 0.01)
    testthat::expect_equal(df$posterior.sd, 0.11, tolerance = 0.01)
    testthat::expect_equal(df$posterior.std.error, 0.002008316, tolerance = 0.001)
    testthat::expect_equal(df$HDI.low, 0.01865475, tolerance = 0.001)
    testthat::expect_equal(df$HDI.high, 0.3931479, tolerance = 0.001)
    testthat::expect_identical(class(df)[[1]], "tbl_df")
  }
)


# bayes factor caption maker check --------------------------

testthat::test_that(
  desc = "bayes factor caption maker check",
  code = {

    # bayes factor results
    bf_results <- tibble::tribble(
      ~log_e_bf01, ~bf.prior,
      1.1, 0.88
    )

    # expected
    using1 <- ggstatsplot::bf_caption_maker(
      bf.df = bf_results,
      k = 3,
      caption = substitute(paste(italic("Note", ": made up data")))
    )

    testthat::expect_identical(
      using1,
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
      ))
    )
  }
)
