context("bf_meta_message")

testthat::test_that(
  desc = "bf_meta_message works",
  code = {
    testthat::skip_on_cran()

    # setup
    set.seed(123)
    library(metaBMA)

    # creating a dataframe
    df1 <-
      structure(
        .Data = list(
          term = c("1", "2", "3", "4", "5"),
          estimate = c(
            0.382047603321706,
            0.780783111514665,
            0.425607573765058,
            0.558365541235078,
            0.956473848429961
          ),
          std.error = c(
            0.0465576338644502,
            0.0330218199731529,
            0.0362834986178494,
            0.0480571500648261,
            0.062215818388157
          )
        ),
        row.names = c(NA, -5L),
        class = c("tbl_df", "tbl", "data.frame")
      )

    df2 <- dplyr::select(df1, -term)
    df3 <- dplyr::select(df2, -estimate)

    # getting bayes factor in favor of null hypothesis
    set.seed(123)
    subtitle1 <- ggstatsplot::bf_meta_message(
      data = df1,
      k = 3,
      messages = TRUE,
      sample = 50
    )
    set.seed(123)
    subtitle2 <- ggstatsplot::bf_meta_message(
      data = df2,
      k = 3,
      messages = FALSE,
      sample = 50
    )

    testthat::expect_identical(subtitle1, subtitle2)
    testthat::expect_identical(subtitle1, ggplot2::expr(atop(
      displaystyle(NULL),
      expr = paste(
        "In favor of null: ",
        "log"["e"],
        "(BF"["01"],
        ") = ",
        "-2.680",
        ", ",
        italic("d")["mean"]^"posterior",
        " = ",
        "0.491",
        ", CI"["95%"],
        " [",
        "0.147",
        ", ",
        "0.775",
        "]"
      )
    )))

    # expecting error
    testthat::expect_error(ggstatsplot::bf_meta_message(df3))
  }
)
