# loading the needed libraries
library(datasets)
library(tibble)
library(dplyr)
library(purrr)

# looking at the table
dplyr::glimpse(x = Titanic)

# converting to tibble
tibble::as_data_frame(x = Titanic)

# a custom function to repeat dataframe `rep` number of times, which is going to
# be count data for us
rep_df <- function(df, rep) {
  df[rep(1:nrow(df), rep), ]
}

# converting dataframe to full length based on count information
Titanic_full <- tibble::as_data_frame(datasets::Titanic) %>%
  tibble::rowid_to_column(df = ., var = "id") %>%
  dplyr::mutate_at(
    .tbl = .,
    .vars = dplyr::vars("id"),
    .funs = ~ as.factor(.)
  ) %>%
  base::split(x = ., f = .$id) %>%
  purrr::map_dfr(.x = ., .f = ~ rep_df(df = ., rep = .$n)) %>%
  dplyr::mutate_at(
    .tbl = .,
    .vars = dplyr::vars("id"),
    .funs = ~ as.numeric(as.character(.))
  ) %>%
  dplyr::mutate_if(
    .tbl = .,
    .predicate = is.character,
    .funs = ~ base::as.factor(.)
  ) %>%
  dplyr::mutate_if(
    .tbl = .,
    .predicate = is.factor,
    .funs = ~ base::droplevels(.)
  ) %>%
  dplyr::select(.data = ., -n, -id) %>%
  tibble::rownames_to_column(df = ., var = "id") %>%
  dplyr::mutate_at(
    .tbl = .,
    .vars = "id",
    .funs = ~ as.numeric(as.character(.))
  )

# reordering the Class variables
Titanic_full$Class <-
  base::factor(
    x = Titanic_full$Class,
    levels = c("1st", "2nd", "3rd", "Crew", ordered = TRUE)
  )

# looking at the final dataset
dplyr::glimpse(Titanic_full)

# saving the files
readr::write_csv(x = Titanic_full, path = "data-raw/Titanic_full.csv")
base::save(Titanic_full, file = "data/Titanic_full.rdata")
