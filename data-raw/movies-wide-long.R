library(magrittr)

# clean data leave it in wide format
movies_wide <-
  ggplot2movies::movies %>%
  dplyr::select(c(title:votes, mpaa:Short)) %>%
  dplyr::filter(mpaa != "", mpaa != "NC-17", Short != 1L, Documentary != 1L) %>%
  dplyr::select(-c(Short, Documentary)) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(
    budget = budget / 1000000,
    mpaa = factor(mpaa),
    NumGenre = as.integer(rowSums(dplyr::select(., Action:Romance)))
  ) %>%
  dplyr::filter(NumGenre > 0L)

# see the selected data (we have data from 1,579 movies)
dplyr::glimpse(movies_wide)

# converting to long format
movies_long <- movies_wide %>%
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
      Romance == 1 ~ "Romance Drama" # there are only 3 of them
    )
  ) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(genre = factor(genre)) %>%
  dplyr::select(-(Action:NumGenre)) %>%
  dplyr::arrange(desc(rating), title, year)

# see the selected data (we have data from the exact same 1,579 movies)
dplyr::glimpse(movies_long)

readr::write_csv(movies_wide, file = "data-raw/movies_wide.csv")
base::save(movies_wide, file = "data/movies_wide.rdata")

readr::write_csv(movies_long, file = "data-raw/movies_long.csv")
base::save(movies_long, file = "data/movies_long.rdata")
