context("t_test_subtitles")

test_that("t-test subtitles work", {
  set.seed(123)

  data("bugs", package = "jmv")

  # expected output from jamovi
  jmv_df <- jmv::ttestPS(
    data = bugs,
    pairs = list(
      list(i1 = "HDLF", i2 = "HDHF")
    ),
    bf = TRUE,
    miss = "listwise"
  )

  # preparing long format dataframe
  bugs_long <- tibble::as.tibble(x = bugs) %>%
    dplyr::select(.data = ., HDLF, HDHF) %>%
    tidyr::gather(data = ., "key", "value", convert = TRUE)

  # output from ggstatsplot helper subtitle
  subtitle <-
    subtitle_ggbetween_t_bayes(
      data = bugs_long,
      x = key,
      y = value,
      paired = TRUE
    )

  # extracting only the numbers and creating a tibble
  subtitle_vec <-
    stringr::str_extract(string = as.character(subtitle), pattern = "\\-*\\d+\\.*\\d*") %>%
    tibble::as.tibble() %>%
    stats::na.omit(.)

  # converting to numeric
  subtitle_vec$value <- as.numeric(as.character(subtitle_vec$value))

  # testing values

  # t-value from student's t-test
  testthat::expect_equal(
    expected = as.data.frame(jmv_df$ttest)$`stat[stud]`,
    object = subtitle_vec$value[[2]],
    tolerance = 1e-3
  )
})
