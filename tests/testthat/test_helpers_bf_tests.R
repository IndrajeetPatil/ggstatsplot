context("helpers_bf_tests")

# bayes factor (correlation) --------------------------

testthat::test_that(
  desc = "bayes factor (correlation)",
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

# bayes factor (independent samples t-test) ----------------------

testthat::test_that(
  desc = "bayes factor (independent samples t-test)",
  code = {
    testthat::skip_on_cran()

    # from Bayes Factor
    df <- suppressMessages(ggstatsplot::bf_extractor(
      BayesFactor::ttestBF(
        formula = len ~ supp,
        data = as.data.frame(ToothGrowth),
        rscale = 0.99,
        paired = FALSE
      )
    ))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <- ggstatsplot::bf_ttest(
      data = ToothGrowth,
      x = supp,
      y = len,
      paired = FALSE,
      bf.prior = 0.99,
      output = "results"
    )

    # check bayes factor values
    testthat::expect_equal(df$log_e_bf10, -0.001119132, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -0.0004860328, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)
  }
)

# Bayes factor (paired t-test) ---------------------------------------------

testthat::test_that(
  desc = "bayes factor (paired t-test)",
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
    df_results <- ggstatsplot::bf_ttest(
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

# bayes factor (one sample t-test) ----------------------

testthat::test_that(
  desc = "bayes factor (one sample t-test)",
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
    df_results <- ggstatsplot::bf_ttest(
      data = iris,
      x = Petal.Length,
      y = NULL,
      test.value = 5.5,
      bf.prior = 0.99,
      output = "results"
    )

    # check Bayes factor values
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

# bayes factor (between-subjects - anova) ---------------------------------

testthat::test_that(
  desc = "bayes factor (between-subjects - anova)",
  code = {
    testthat::skip_on_cran()

    # dataframe
    set.seed(123)
    dat <- dplyr::filter(ggplot2::msleep, !is.na(brainwt), !is.na(vore)) %>%
      dplyr::mutate(.data = ., vore = as.factor(vore))

    # creating a dataframe
    set.seed(123)
    df <- suppressMessages(ggstatsplot::bf_extractor(
      BayesFactor::anovaBF(
        formula = brainwt ~ vore,
        data = as.data.frame(dat),
        progress = FALSE,
        rscaleFixed = 0.99
      )
    ))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <- ggstatsplot::bf_oneway_anova(
      data = dat,
      x = vore,
      y = brainwt,
      bf.prior = 0.99,
      output = "results"
    )

    # extracting caption - null
    set.seed(123)
    results1 <- ggstatsplot::bf_oneway_anova(
      data = dat,
      x = vore,
      y = brainwt,
      bf.prior = 0.88,
      output = "null"
    )

    # extracting caption - alternative
    set.seed(123)
    results2 <- ggstatsplot::bf_oneway_anova(
      data = dat,
      x = vore,
      y = brainwt,
      bf.prior = 0.88,
      output = "alternative"
    )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 0.1177186, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -2.139458, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -0.9291548, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)

    # call for null and alternative
    testthat::expect_identical(
      results1,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "1.92",
          ", ",
          italic("r")["Cauchy"],
          " = ",
          "0.88"
        )
      ))
    )

    testthat::expect_identical(
      results2,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "-1.92",
          ", ",
          italic("r")["Cauchy"],
          " = ",
          "0.88"
        )
      ))
    )
  }
)

# bayes factor (between-subjects - anova) ---------------------------------

testthat::test_that(
  desc = "bayes factor (within-subjects - anova)",
  code = {
    testthat::skip_on_cran()

    # dataframe
    dat <- WRS2::WineTasting

    # creating a dataframe
    set.seed(123)
    df <- suppressMessages(ggstatsplot::bf_extractor(
      BayesFactor::anovaBF(
        formula = Taste ~ Wine + Taster,
        data = as.data.frame(dat),
        progress = FALSE,
        whichRandom = "Taster",
        rscaleFixed = 0.99,
        rscaleRandom = 1
      )
    ))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <- ggstatsplot::bf_oneway_anova(
      data = dat,
      x = Wine,
      y = Taste,
      paired = TRUE,
      bf.prior = 0.99,
      output = "results"
    )

    # extracting caption - null
    set.seed(123)
    results1 <- ggstatsplot::bf_oneway_anova(
      data = dat,
      x = Wine,
      y = Taste,
      k = 4,
      paired = TRUE,
      bf.prior = 0.88,
      output = "null"
    )

    # extracting caption - alternative
    set.seed(123)
    results2 <- ggstatsplot::bf_oneway_anova(
      data = dat,
      x = Wine,
      y = Taste,
      k = 4,
      paired = TRUE,
      bf.prior = 0.88,
      output = "alternative"
    )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 6.364917, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, 1.850801, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, 0.8037927, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)

    # call for null and alternative
    testthat::expect_identical(
      results1,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-1.9580",
          ", ",
          italic("r")["Cauchy"],
          " = ",
          "0.8800"
        )
      ))
    )

    testthat::expect_identical(
      results2,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "1.9580",
          ", ",
          italic("r")["Cauchy"],
          " = ",
          "0.8800"
        )
      ))
    )
  }
)


# bayes factor (proportion test) --------------------------------------

testthat::test_that(
  desc = "bayes factor (proportion test)",
  code = {
    testthat::skip_on_cran()

    # extracting results from where this function is implemented
    set.seed(123)
    df <- ggstatsplot::bf_contingency_tab(
      data = mtcars,
      main = am,
      output = "results"
    )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 0.2465787, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -1.400074, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -0.6080444, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # caption
    set.seed(123)
    caption_text <- ggstatsplot::bf_contingency_tab(
      data = mtcars,
      main = cyl,
      output = "alternative",
      prior.concentration = 10
    )

    testthat::expect_identical(
      caption_text,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "-0.55",
          ", ",
          italic("a"),
          " = ",
          "10.00"
        )
      ))
    )
  }
)

# bayes factor (contingency tab) --------------------------------------

testthat::test_that(
  desc = "bayes factor (contingency tab)",
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

    # caption
    caption_text <- ggstatsplot::bf_contingency_tab(
      data = mtcars,
      main = am,
      condition = cyl,
      sampling.plan = "jointMulti",
      fixed.margin = "rows",
      output = "alternative"
    )

    # with counts
    caption_text2 <- ggstatsplot::bf_contingency_tab(
      data = as.data.frame(Titanic),
      main = Survived,
      condition = Sex,
      counts = "Freq",
      sampling.plan = "jointMulti",
      fixed.margin = "rows",
      output = "alternative"
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

    # caption text
    testthat::expect_identical(caption_text, ggplot2::expr(atop(
      displaystyle(NULL),
      expr = paste(
        "In favor of alternative: ",
        "log"["e"],
        "(BF"["10"],
        ") = ",
        "3.33",
        ", sampling = ",
        "joint multinomial",
        ", ",
        italic("a"),
        " = ",
        "1.00"
      )
    )))

    testthat::expect_identical(
      caption_text2,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "214.25",
          ", sampling = ",
          "joint multinomial",
          ", ",
          italic("a"),
          " = ",
          "1.00"
        )
      ))
    )
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
    using2 <- ggstatsplot::bf_caption_maker(
      bf.df = bf_results,
      output = "H1",
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

    testthat::expect_identical(
      using2,
      ggplot2::expr(atop(
        displaystyle(paste(italic(
          "Note", ": made up data"
        ))),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "-1.10",
          ", ",
          italic("r")["Cauchy"],
          " = ",
          "0.88"
        )
      ))
    )
  }
)
