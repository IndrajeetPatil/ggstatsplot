# converting data frame to full length based on count information
Titanic_full <-
  datasets::Titanic |>
  tibble::as_tibble() |>
  tidyr::uncount(n) |>
  dplyr::mutate(dplyr::across(
    where(is.character),
    ~ droplevels(as.factor(.x))
  )) |>
  tibble::rownames_to_column(var = "id") |>
  dplyr::mutate(dplyr::across("id", ~ as.numeric(as.character(.x))))

# reordering the Class variables
Titanic_full$Class <- factor(
  Titanic_full$Class,
  levels = c("1st", "2nd", "3rd", "Crew"),
  ordered = TRUE
)

dplyr::glimpse(Titanic_full)

readr::write_csv(Titanic_full, file = "data-raw/Titanic_full.csv")
save(Titanic_full, file = "data/Titanic_full.rdata")
