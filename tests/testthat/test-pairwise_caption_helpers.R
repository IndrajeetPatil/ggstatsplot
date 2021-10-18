# pairwise_caption works ------------------------------------------------

test_that(
  desc = "pairwise_caption works",
  code = {
    expr1 <- pairwise_caption("my caption", "Student's t-test", "s")
    expr2 <- pairwise_caption(NULL, "Yuen's t-test", "ns")
    expr3 <- pairwise_caption(NULL, "Yuen's t-test", "all")
    expr4 <- pairwise_caption("my caption", "Student's t-test", "sig")

    expect_identical(
      as.character(expr1),
      c(
        "atop",
        "displaystyle(\"my caption\")",
        "paste(\"Pairwise test: \", bold(\"Student's t-test\"), \"; Comparisons shown: \", bold(\"only significant\"))"
      )
    )

    expect_identical(
      as.character(expr2),
      c(
        "atop",
        "displaystyle(NULL)",
        "paste(\"Pairwise test: \", bold(\"Yuen's t-test\"), \"; Comparisons shown: \", bold(\"only non-significant\"))"
      )
    )

    expect_identical(
      as.character(expr3),
      c(
        "atop",
        "displaystyle(NULL)",
        "paste(\"Pairwise test: \", bold(\"Yuen's t-test\"), \"; Comparisons shown: \", bold(\"all\"))"
      )
    )

    expect_identical(as.character(expr1), as.character(expr4))
  }
)

# switch for p adjustment ------------------------------------------------

test_that(
  desc = "switch for p adjustment works",
  code = {
    expect_identical(p_adjust_text("none"), "None")
    expect_identical(
      p_adjust_text("fdr"),
      p_adjust_text("BH")
    )
    expect_identical(p_adjust_text("hochberg"), "Hochberg")
    expect_identical(p_adjust_text("bonferroni"), "Bonferroni")
    expect_identical(p_adjust_text("holm"), "Holm")
    expect_identical(p_adjust_text("hommel"), "Hommel")
    expect_identical(p_adjust_text("BY"), "BY")
    expect_identical(p_adjust_text("xyz"), "Holm")
  }
)
