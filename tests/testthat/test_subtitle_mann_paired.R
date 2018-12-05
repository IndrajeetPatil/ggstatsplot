context("subtitle_mann_paired")

testthat::test_that(
  desc = "subtitle_mann_paired works",
  code = {

    # made up data
    mydata <-
      structure(
        list(
          id = structure(
            c(
              1L,
              12L,
              14L,
              15L,
              16L,
              17L,
              18L,
              19L,
              20L,
              2L,
              3L,
              4L,
              5L,
              6L,
              7L,
              8L,
              9L,
              10L,
              11L,
              13L,
              1L,
              12L,
              14L,
              15L,
              16L,
              17L,
              18L,
              19L,
              20L,
              2L,
              3L,
              4L,
              5L,
              6L,
              7L,
              8L,
              9L,
              10L,
              11L,
              13L
            ),
            .Label = c(
              "student1",
              "student10",
              "student11",
              "student12",
              "student13",
              "student14",
              "student15",
              "student16",
              "student17",
              "student18",
              "student19",
              "student2",
              "student20",
              "student3",
              "student4",
              "student5",
              "student6",
              "student7",
              "student8",
              "student9"
            ),
            class = "factor"
          ),
          time = structure(
            c(
              1L,
              1L,
              1L,
              1L,
              1L,
              1L,
              1L,
              1L,
              1L,
              1L,
              1L,
              1L,
              1L,
              1L,
              1L,
              1L,
              1L,
              1L,
              1L,
              1L,
              2L,
              2L,
              2L,
              2L,
              2L,
              2L,
              2L,
              2L,
              2L,
              2L,
              2L,
              2L,
              2L,
              2L,
              2L,
              2L,
              2L,
              2L,
              2L,
              2L
            ),
            .Label = c("test1", "test2"),
            class = "factor"
          ),
          grade = c(
            42.9,
            51.8,
            71.7,
            51.6,
            63.5,
            58,
            59.8,
            50.8,
            62.5,
            61.9,
            50.4,
            52.6,
            63,
            58.3,
            53.3,
            58.7,
            50.1,
            64.2,
            57.4,
            57.1,
            44.6,
            54,
            72.3,
            53.4,
            63.8,
            59.3,
            60.8,
            51.6,
            64.3,
            63.2,
            51.8,
            52.2,
            63,
            60.5,
            57.1,
            60.1,
            51.7,
            65.6,
            58.3,
            60.1
          )
        ),
        row.names = c(NA, -40L),
        reshapeLong = list(
          varying = list(c("grade_test1", "grade_test2")),
          v.names = "grade",
          idvar = "id",
          timevar = "withintmp"
        ),
        class = "data.frame"
      )

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      ggstatsplot::subtitle_mann_nonparametric(
        data = mydata,
        x = time,
        y = grade,
        k = 5,
        paired = TRUE,
        messages = FALSE
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          italic(U),
          " = ",
          "2.00000",
          ", ",
          italic(Z),
          " = ",
          "-0.98761",
          ", ",
          italic(" p"),
          " = ",
          "2e-04",
          ", ",
          italic("r"),
          " = ",
          "-0.15615",
          ", ",
          italic("n"),
          " = ",
          40L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)
