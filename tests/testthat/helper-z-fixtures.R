survey_data <- dplyr::tibble(
  `1st survey` = c("Approve", "Approve", "Disapprove", "Disapprove"),
  `2nd survey` = c("Approve", "Disapprove", "Approve", "Disapprove"),
  Counts = c(794L, 150L, 86L, 570L)
)

survey_data_NA <- survey_data
survey_data_NA$Counts[3L] <- NA_integer_

# Keep standard errors away from zero: near-zero values make the Stan sampler
# behind `metaBMA::meta_random()` (Bayesian meta-analysis) extremely slow.
df_meta <- tibble::tibble(
  estimate = c(0.111, 0.245, 0.8, 1.1, 0.03),
  std.error = c(0.05, 0.111, 0.1, 0.2, 0.01)
)

morley_new <- dplyr::mutate(
  datasets::morley,
  Expt = c("1st", "2nd", "3rd", "4th", "5th")[Expt]
) |>
  tibble::as_tibble()

morley_new[c(3L, 23L, 87L), 3L] <- NA_integer_

data_bugs_2 <- dplyr::filter(
  statsExpressions::bugs_long,
  subject <= 30L,
  condition %in% c("HDLF", "HDHF")
)
