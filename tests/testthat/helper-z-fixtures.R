survey_data <- dplyr::tibble(
  `1st survey` = c("Approve", "Approve", "Disapprove", "Disapprove"),
  `2nd survey` = c("Approve", "Disapprove", "Approve", "Disapprove"),
  Counts = c(794L, 150L, 86L, 570L)
)

survey_data_NA <- dplyr::tibble(
  `1st survey` = c("Approve", "Approve", "Disapprove", "Disapprove"),
  `2nd survey` = c("Approve", "Disapprove", "Approve", "Disapprove"),
  Counts = c(794L, 150L, NA_integer_, 570L)
)

df_meta <- tibble::tibble(
  estimate = c(0.111, 0.245, 0.8, 1.1, 0.03),
  std.error = c(0.05, 0.111, 0.001, 0.2, 0.01)
)

morley_new <- dplyr::mutate(
  datasets::morley,
  Expt = dplyr::case_when(
    Expt == 1L ~ "1st",
    Expt == 2L ~ "2nd",
    Expt == 3L ~ "3rd",
    Expt == 4L ~ "4th",
    Expt == 5L ~ "5th"
  )
) |>
  tibble::as_tibble()

morley_new[3L, 3L] <- NA_integer_
morley_new[23L, 3L] <- NA_integer_
morley_new[87L, 3L] <- NA_integer_

fixture_data <- new.env(parent = emptyenv())
utils::data("bugs_long", package = "statsExpressions", envir = fixture_data)

data_bugs_2 <- dplyr::filter(
  fixture_data$bugs_long,
  subject <= 30L,
  condition %in% c("HDLF", "HDHF")
)

rm(fixture_data)
