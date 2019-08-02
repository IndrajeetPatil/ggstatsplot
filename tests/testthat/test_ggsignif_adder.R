context(desc = "ggsignif_adder")

# ggsignif_adder works ----------------------------------------------------

testthat::test_that(
  desc = "ggsignif_adder works",
  code = {
    testthat::skip_on_cran()
    set.seed(123)
    library(ggplot2)

    # data
    df <- data.frame(x = iris$Species, y = iris$Sepal.Length)

    # plot
    p <- ggplot(df, aes(x, y)) + geom_boxplot()

    # dataframe with pairwise comparison test results
    df_pair <- ggstatsplot::pairwise_p(df, x, y, messages = FALSE)

    # adding plot with
    p_new <- ggstatsplot:::ggsignif_adder(
      plot = p,
      x = x,
      y = y,
      df_pairwise = df_pair,
      data = df
    )

    # build the plot
    pb <- ggplot2::ggplot_build(p_new)

    # tests
    testthat::expect_equal(length(pb$data), 2L)
    testthat::expect_identical(
      as.character(unique(pb$data[[2]]$group)),
      c(
        "setosa-versicolor-1",
        "setosa-virginica-2",
        "versicolor-virginica-3"
      )
    )
  }
)
