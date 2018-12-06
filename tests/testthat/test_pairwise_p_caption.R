context("pairwise_p_caption")

# pairwise_p_caption works --------------------------------------------------

testthat::test_that(
  desc = "`pairwise_p_caption()` works",
  code = {
    set.seed(123)

    p_message1 <- ggstatsplot:::pairwise_p_caption(
      type = "p",
      paired = TRUE,
      var.equal = FALSE,
      p.adjust.method = "fdr"
    )

    p_message2 <- ggstatsplot:::pairwise_p_caption(
      type = "np",
      paired = TRUE,
      p.adjust.method = "holm",
      caption = "this is caption"
    )

    p_message3 <- ggstatsplot:::pairwise_p_caption(
      type = "p",
      paired = FALSE,
      var.equal = FALSE,
      caption = substitute(paste(italic("n"), "= 678"))
    )

    # testing outputs
    testthat::expect_identical(
      p_message1,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "Pairwise comparisons: ",
          bold("Student's t-test"),
          "; Adjustment (p-value): ",
          bold("Benjamini & Hochberg")
        )
      ))
    )

    testthat::expect_identical(
      p_message2,
      ggplot2::expr(atop(
        displaystyle("this is caption"),
        expr = paste(
          "Pairwise comparisons: ",
          bold("Durbin-Conover test"),
          "; Adjustment (p-value): ",
          bold("Holm")
        )
      ))
    )

    testthat::expect_identical(
      p_message3,
      ggplot2::expr(atop(
        displaystyle(paste(italic("n"), "= 678")),
        expr = paste(
          "Pairwise comparisons: ",
          bold("Games-Howell test"),
          "; Adjustment (p-value): ",
          bold("Holm")
        )
      ))
    )
  }
)
