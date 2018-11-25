# needed libraries
library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# for reproducibility
set.seed(123)

# having a look at iris before converting to long format
dplyr::glimpse(iris)

# converting the iris dataset to long format
iris_long <- datasets::iris %>%
  dplyr::mutate(.data = ., id = dplyr::row_number(x = Species)) %>%
  tidyr::gather(
    data = .,
    key = "condition",
    value = "value",
    Sepal.Length:Petal.Width,
    convert = TRUE,
    factor_key = TRUE
  ) %>%
  tidyr::separate(
    col = "condition",
    into = c("attribute", "measure"),
    sep = "\\.",
    convert = TRUE
  ) %>% # converting column types
  dplyr::mutate_if(
    .tbl = .,
    .predicate = purrr::is_bare_character,
    .funs = ~ base::as.factor(.)
  ) %>%
  dplyr::mutate_if(
    .tbl = .,
    .predicate = base::is.factor,
    .funs = ~ base::droplevels(.)
  ) %>%
  dplyr::select(.data = ., id, dplyr::everything()) %>%
  tibble::as_tibble(x = .)

# looking at the long format data
dplyr::glimpse(x = iris_long)

# saving the files
readr::write_csv(x = iris_long, path = "data-raw/iris_long.csv")
base::save(iris_long, file = "data/iris_long.rdata", version = 2)
