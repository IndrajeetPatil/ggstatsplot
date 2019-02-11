context("helpers_bf_tests")

# bayes factor plus posterior checks (correlation) --------------------------

testthat::test_that(
  desc = "bayes factor plus posterior checks (correlation)",
  code = {
    testthat::skip_on_cran()

    # creating a dataframe
    set.seed(123)
    df <- suppressMessages(ggstatsplot::bf_extractor(
      BayesFactor::correlationBF(
        x = ggplot2::msleep$brainwt,
        y = ggplot2::msleep$sleep_total
      )
    ))

    # check bayes factor values
    testthat::expect_equal(df$bf10, 8.990505, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, 2.196169, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, 0.9537841, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)
  }
)

# bayes factor plus posterior checks (paired t-test) ----------------------

testthat::test_that(
  desc = "bayes factor plus posterior checks (paired t-test)",
  code = {
    testthat::skip_on_cran()

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
      )
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
  }
)



# bayes factor plus posterior checks (paired t-test) ----------------------

testthat::test_that(
  desc = "bayes factor plus posterior checks (paired t-test)",
  code = {
    testthat::skip_on_cran()

    # creating a dataframe
    set.seed(123)
    df <- suppressMessages(ggstatsplot::bf_extractor(
      BayesFactor::ttestBF(
        x = iris$Petal.Length,
        mu = 5.5,
        rscale = 0.99
      )
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
  }
)



# bayes factor plus posterior checks (contingency tab) ----------------------

testthat::test_that(
  desc = "bayes factor plus posterior checks (contingency tab)",
  code = {
    testthat::skip_on_cran()

    # extracting results from where this function is implemented
    set.seed(123)
    df <- suppressMessages(ggstatsplot::bf_extractor(
      BayesFactor::contingencyTableBF(
        x = table(mtcars$am, mtcars$cyl),
        sampleType = "jointMulti",
        fixedMargin = "rows"
      )
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
  }
)


# bayes factor caption maker check --------------------------

testthat::test_that(
  desc = "bayes factor caption maker check",
  code = {
    testthat::skip_on_cran()

    # bayes factor results
    set.seed(123)
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
          ", ",
          italic("r")["Cauchy"],
          " = ",
          "0.880"
        )
      ))
    )
  }
)
