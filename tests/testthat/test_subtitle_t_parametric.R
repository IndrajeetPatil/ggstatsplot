context("subtitle_t_parametric")

testthat::test_that(
  desc = "parametric t-test works (between-subjects without NAs)",
  code = {

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      suppressWarnings(
        ggstatsplot::subtitle_t_parametric(
          data = dplyr::filter(
            ggstatsplot::movies_long,
            genre == "Action" | genre == "Drama"
          ),
          x = genre,
          y = rating,
          effsize.type = "d",
          effsize.noncentral = TRUE,
          var.equal = TRUE,
          conf.level = .99,
          k = 5,
          messages = FALSE
        )
      )

    # expected output
    # this test will have to be changed with the next release of `effsize`
    # d here should be negative but is displayed as positive
    # this is a bug in effsize and has been fixed in the development version
    # (https://github.com/mtorchiano/effsize/commit/3561d93f9e9f5a61b3460ba120b316f7e4c3352f)
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          italic("t"),
          "(",
          c(df = "1317.00000"),
          ") = ",
          c(t = "-9.46816"),
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("d"),
          " = ",
          c(Action = "0.51775"),
          ", CI"["99%"],
          " [",
          "0.36213",
          ", ",
          "0.67319",
          "]",
          ", ",
          italic("n"),
          " = ",
          1319L
        )
      )

    # testing overall idenitcal
    testthat::expect_equal(
      object = using_function1,
      expected = results1
    )

    # testing t value
    testthat::expect_equal(
      object = using_function1[6],
      expected = results1[6]
    )

    # testing p value
    testthat::expect_identical(
      object = as.character(using_function1)[10],
      expected = as.character(results1)[10]
    )

    # testing sample size
    testthat::expect_identical(
      object = using_function1[23],
      expected = results1[23]
    )
  }
)

testthat::test_that(
  desc = "parametric t-test works (between-subjects with NAs)",
  code = {

    # for reproducibility
    set.seed(123)

    # loading the dataset
    data("bugs", package = "jmv")

    # expected output from jamovi
    jmv_df <-
      jmv::ttestPS(
        data = bugs,
        pairs = list(
          list(i2 = "HDLF", i1 = "HDHF")
        ),
        bf = TRUE,
        miss = "listwise"
      )

    # t-value extraction
    t_statistic <- as.data.frame(jmv_df$ttest)$`stat[stud]`

    # preparing long format dataframe
    bugs_long <-
      tibble::as.tibble(x = bugs) %>%
      dplyr::select(.data = ., HDLF, HDHF) %>%
      tidyr::gather(data = ., "key", "value", convert = TRUE)

    # output from ggstatsplot helper subtitle
    subtitle <-
      subtitle_t_bayes(
        data = bugs_long,
        x = key,
        y = value,
        paired = TRUE
      )

    # extracting only the numbers and creating a tibble
    subtitle_vec <- num_parser(ggstats.obj = subtitle)

    # testing values

    # t-value from student's t-test
    testthat::expect_equal(t_statistic, subtitle_vec[[2]], tolerance = 1e-3)
  }
)
