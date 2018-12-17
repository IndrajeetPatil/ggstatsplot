context("pairwise_p")

# between-subjects design --------------------------------------------------

testthat::test_that(
  desc = "`pairwise_p()` works for between-subjects design",
  code = {
    set.seed(123)

    # student's t
    df1 <- ggstatsplot::pairwise_p(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      messages = FALSE,
      type = "p",
      var.equal = TRUE,
      paired = FALSE,
      p.adjust.method = "bonferroni"
    )

    testthat::expect_equal(
      df1$mean.difference,
      c(
        0.54234194,
        -0.05770556,
        0.06647562,
        -0.60004750,
        -0.47586632,
        0.12418118
      ),
      tolerance = 0.001
    )

    # games-howell
    df2 <- ggstatsplot::pairwise_p(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      messages = FALSE,
      type = "p",
      var.equal = FALSE,
      paired = FALSE,
      p.adjust.method = "bonferroni"
    )

    testthat::expect_equal(df2$mean.difference,
      c(0.476, -0.066, -0.124, -0.542, -0.600, -0.058),
      tolerance = 0.001
    )

    # Dwass-Steel-Crichtlow-Fligner test
    df3 <- ggstatsplot::pairwise_p(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      messages = FALSE,
      type = "np",
      paired = FALSE,
      p.adjust.method = "none"
    )

    testthat::expect_equal(
      df3$W,
      c(
        -0.8000000,
        -2.3570226,
        -1.7152791,
        -2.4019223,
        -0.9483623,
        1.6070259
      ),
      tolerance = 0.001
    )

    # robust t test
    df4 <- ggstatsplot::pairwise_p(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      messages = FALSE,
      type = "r",
      paired = FALSE,
      p.adjust.method = "fdr"
    )

    testthat::expect_equal(
      df4$psihat,
      c(
        -0.055602667,
        -0.052966319,
        0.002102889,
        0.055069208,
        0.057705556,
        0.110671875
      ),
      tolerance = 0.001
    )

    # checking the edge case where factor level names contain `-`
    set.seed(123)
    df5 <- ggstatsplot::pairwise_p(
      data = movies_wide,
      x = mpaa,
      y = rating,
      var.equal = TRUE,
      messages = FALSE
    )

    testthat::expect_equal(df5$group1, c("PG-13", "R", "R"))
    testthat::expect_equal(df5$group2, c("PG", "PG", "PG-13"))
    testthat::expect_equal(df5$mean.difference,
      c(0.1042746, 0.3234094, 0.2191348),
      tolerance = 0.001
    )
    testthat::expect_equal(df5$conf.low,
      c(-0.13957173, 0.09444830, 0.05700094),
      tolerance = 0.001
    )
    testthat::expect_equal(df5$conf.high,
      c(0.3481209, 0.5523704, 0.3812686),
      tolerance = 0.001
    )
    testthat::expect_equal(df5$p.value,
      c(0.315931518, 0.002825407, 0.003100279),
      tolerance = 0.001
    )
  }
)


# within-subjects design --------------------------------------------------

testthat::test_that(
  desc = "`pairwise_p()` works for within-subjects design",
  code = {
    set.seed(123)
    library(jmv)
    data("bugs", package = "jmv")

    # converting to long format
    bugs_long <- bugs %>%
      tibble::as_tibble(.) %>%
      tidyr::gather(., key, value, LDLF:HDHF)

    # student's t test
    df1 <- ggstatsplot::pairwise_p(
      data = bugs_long,
      x = key,
      y = value,
      type = "p",
      k = 3,
      paired = TRUE,
      messages = FALSE,
      p.adjust.method = "bonferroni"
    )

    testthat::expect_equal(
      df1$mean.difference,
      c(
        -1.1115026,
        -0.4741400,
        -2.1382071,
        0.6373626,
        -1.0267045,
        -1.6640671
      ),
      tolerance = 0.001
    )

    testthat::expect_identical(
      df1$p.value.label,
      c(
        "p = 0.003",
        "p = 0.424",
        "p <= 0.001",
        "p = 0.274",
        "p = 0.006",
        "p <= 0.001"
      )
    )

    testthat::expect_identical(
      df1$significance,
      c("**", "ns", "***", "ns", "**", "***")
    )

    # Durbin-Conover test
    df2 <- ggstatsplot::pairwise_p(
      data = bugs_long,
      x = key,
      y = value,
      type = "np",
      k = 3,
      paired = TRUE,
      messages = FALSE,
      p.adjust.method = "BY"
    )

    testthat::expect_equal(
      df2$statistic,
      c(4.780042, 2.443931, 8.014657, 2.336111, 3.234615, 5.570726),
      tolerance = 0.001
    )

    testthat::expect_identical(
      df2$p.value.label,
      c(
        "p <= 0.001",
        "p = 0.045",
        "p <= 0.001",
        "p = 0.050",
        "p = 0.005",
        "p <= 0.001"
      )
    )

    testthat::expect_identical(
      df2$significance,
      c("***", "*", "***", "*", "**", "***")
    )

    # robust t test
    df3 <- ggstatsplot::pairwise_p(
      data = bugs_long,
      x = key,
      y = value,
      type = "r",
      k = 3,
      paired = TRUE,
      messages = FALSE,
      p.adjust.method = "hommel"
    )

    testthat::expect_equal(
      df3$psihat,
      c(
        -0.7013889,
        0.5000000,
        0.9375000,
        1.1597222,
        1.5416667,
        2.0972222
      ),
      tolerance = 0.001
    )

    testthat::expect_identical(
      df3$p.value.label,
      c(
        "p = 0.062",
        "p = 0.062",
        "p = 0.014",
        "p = 0.001",
        "p <= 0.001",
        "p <= 0.001"
      )
    )

    testthat::expect_identical(
      df3$significance,
      c("ns", "ns", "*", "**", "***", "***")
    )
  }
)

# messages - between subjects --------------------------------------------------

testthat::test_that(
  desc = "`pairwise_p()` messages are correct for between-subjects",
  code = {
    set.seed(123)

    # student's t
    messages1 <- capture.output(ggstatsplot::pairwise_p(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      messages = TRUE,
      type = "p",
      var.equal = TRUE,
      paired = FALSE,
      p.adjust.method = "BH"
    ))

    testthat::expect_match(messages1[2], "Student's t-test", fixed = TRUE)
    testthat::expect_match(messages1[3], "BH", fixed = TRUE)

    # games-howell
    messages2 <- capture.output(ggstatsplot::pairwise_p(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      messages = TRUE,
      type = "p",
      var.equal = FALSE,
      paired = FALSE,
      p.adjust.method = "bonferroni"
    ))

    testthat::expect_match(messages2[2], "Games-Howell test", fixed = TRUE)
    testthat::expect_match(messages2[3], "bonferroni", fixed = TRUE)

    # Dwass-Steel-Crichtlow-Fligner test
    messages3 <- capture.output(ggstatsplot::pairwise_p(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      messages = TRUE,
      type = "np",
      paired = FALSE,
      p.adjust.method = "none"
    ))

    testthat::expect_match(messages3[2], "Dwass-Steel-Crichtlow-Fligner test", fixed = TRUE)
    testthat::expect_match(messages3[3], "none", fixed = TRUE)

    # robust t test
    messages4 <- capture.output(ggstatsplot::pairwise_p(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      messages = TRUE,
      type = "r",
      paired = FALSE,
      p.adjust.method = "hochberg"
    ))

    testthat::expect_match(messages4[2], "Yuen's trimmed means", fixed = TRUE)
    testthat::expect_match(messages4[3], "hochberg", fixed = TRUE)

    # bayes factor
    testthat::expect_error(
      ggstatsplot::pairwise_p(
        data = ggplot2::msleep,
        x = vore,
        y = brainwt,
        messages = TRUE,
        type = "bf",
        p.adjust.method = "hochberg"
      )
    )
  }
)


# messages - within subjects --------------------------------------------------

testthat::test_that(
  desc = "`pairwise_p()` messages are correct for within-subjects",
  code = {
    set.seed(123)
    library(jmv)
    data("bugs", package = "jmv")

    # converting to long format
    bugs_long <- bugs %>%
      tibble::as_tibble(.) %>%
      tidyr::gather(., key, value, LDLF:HDHF)

    # student's t test
    messages1 <- capture.output(ggstatsplot::pairwise_p(
      data = bugs_long,
      x = key,
      y = value,
      type = "p",
      paired = TRUE,
      messages = TRUE,
      p.adjust.method = "fdr"
    ))

    testthat::expect_match(messages1[2], "Student's t-test", fixed = TRUE)
    testthat::expect_match(messages1[3], "fdr", fixed = TRUE)

    # Durbin-Conover test
    messages2 <- capture.output(ggstatsplot::pairwise_p(
      data = bugs_long,
      x = key,
      y = value,
      type = "np",
      paired = TRUE,
      messages = TRUE,
      p.adjust.method = "BY"
    ))

    testthat::expect_match(messages2[2], "Durbin-Conover test", fixed = TRUE)
    testthat::expect_match(messages2[3], "BY", fixed = TRUE)

    # robust t test
    messages3 <- capture.output(ggstatsplot::pairwise_p(
      data = bugs_long,
      x = key,
      y = value,
      type = "r",
      paired = TRUE,
      messages = TRUE,
      p.adjust.method = "hommel"
    ))

    testthat::expect_match(messages3[2], "Yuen's trimmed means", fixed = TRUE)
    testthat::expect_match(messages3[3], "hommel", fixed = TRUE)

    # bayes factor
    testthat::expect_error(
      ggstatsplot::pairwise_p(
        data = bugs_long,
        x = key,
        y = value,
        paired = TRUE,
        messages = TRUE,
        type = "bf",
        p.adjust.method = "hochberg"
      )
    )
  }
)
