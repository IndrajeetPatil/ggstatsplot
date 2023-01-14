# converting the iris dataset to long format
iris_long <- datasets::iris |>
  dplyr::mutate(id = dplyr::row_number(Species)) |>
  tidyr::gather(
    key = "condition",
    value = "value",
    Sepal.Length:Petal.Width,
    convert = TRUE,
    factor_key = TRUE
  ) |>
  tidyr::separate(
    col = "condition",
    into = c("attribute", "measure"),
    remove = FALSE,
    sep = "\\.",
    convert = TRUE
  ) |>
  dplyr::mutate(across(where(purrr::is_bare_character, ~ droplevels(as.factor(.x))))) |>
  dplyr::select(id, dplyr::everything()) |>
  tibble::as_tibble()

dplyr::glimpse(iris_long)

readr::write_csv(iris_long, file = "data-raw/iris_long.csv")
save(iris_long, file = "data/iris_long.rdata")
