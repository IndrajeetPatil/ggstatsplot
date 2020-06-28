# loading the needed libraries
library(ggplot2movies)
library(dplyr)

# looking at the table
dplyr::glimpse(x = ggplot2movies::movies)

# clean data leave it in wide format
movies_wide <-
  ggplot2movies::movies %>%
  dplyr::select(.data = ., c(title:votes, mpaa:Short)) %>%
  dplyr::filter(.data = ., mpaa != "", mpaa != "NC-17") %>%
  dplyr::filter(.data = ., Short != 1, Documentary != 1) %>%
  dplyr::select(.data = ., -c(Short, Documentary)) %>%
  stats::na.omit(.) %>%
  dplyr::mutate(.data = ., budget = budget / 1000000) %>%
  dplyr::mutate(.data = ., mpaa = factor(mpaa)) %>%
  dplyr::mutate(.data = ., NumGenre = as.integer(rowSums(select(., Action:Romance)))) %>%
  dplyr::filter(.data = ., NumGenre > 0)

# see the selected data (we have data from 1,579 movies)
dplyr::glimpse(x = movies_wide)

# converting to long format

movies_long <-
  movies_wide %>%
  dplyr::mutate(
    genre = dplyr::case_when(
      Action == 1 & Comedy == 1 & Animation == 0 ~ "Action Comedy",
      Action == 1 & Drama == 1 & Animation == 0 ~ "Action Drama",
      Comedy == 1 & Romance == 1 & Animation == 0 ~ "RomCom",
      Comedy == 1 & Drama == 1 & Animation == 0 ~ "Comedy Drama",
      Romance == 1 & Drama == 1 & Animation == 0 ~ "Romance Drama",
      Action == 1 & Animation == 0 ~ "Action",
      Animation == 1 ~ "Animated",
      Comedy == 1 ~ "Comedy",
      Drama == 1 ~ "Drama",
      Romance == 1 ~ "Romance Drama" # judgment call there are only 3 of them
    )
  ) %>%
  stats::na.omit(.) %>%
  # removing NAs
  dplyr::mutate(genre = factor(genre)) %>%
  dplyr::select(.data = ., -c(Action:NumGenre)) %>%
  dplyr::arrange(.data = ., desc(rating), title, year)

# see the selected data (we have data from the exact same 1,579 movies)
dplyr::glimpse(x = movies_long)

# saving the files
readr::write_csv(x = movies_wide, path = "data-raw/movies_wide.csv")
base::save(movies_wide, file = "data/movies_wide.rdata")
readr::write_csv(x = movies_long, path = "data-raw/movies_long.csv")
base::save(movies_long, file = "data/movies_long.rdata")
