# loading the needed libraries
library(ggplot2movies)
library(tibble)
library(dplyr)
library(purrr)

# looking at the table
dplyr::glimpse(x = ggplot2movies::movies)

# converting to wide format
movies_wide <- ggplot2movies::movies %>%
  dplyr::select(.data = ., c(title:votes, mpaa:Short)) %>% # `.` is just a placeholder for the data
  dplyr::filter(.data = ., mpaa != "") %>% # removing movies without mpaa ratings
  stats::na.omit(.) %>% # removing NAs
  dplyr::mutate(.data = ., budget = budget / 1000000) %>% # convert the budge to millions of dollars
  dplyr::mutate_if(
    .tbl = .,
    # convert mpaa ratings to a factor
    .predicate = purrr::is_bare_character,
    .funs = ~as.factor(.)
  )


# see the selected data (we have data from 1813 movies)
dplyr::glimpse(x = movies_wide)

# converting to long format
movies_long <- movies_wide %>%
  tidyr::gather(
    data = .,
    key = "genre",
    value = "value",
    Action:Short,
    convert = TRUE,
    factor_key = TRUE
  ) %>%
  dplyr::filter(
    .data = .,
    value == 1
  ) %>%
  dplyr::select(.data = ., -value) %>%
  dplyr::arrange(.data = ., desc(rating), title, year)

# removing levels of factors without enough number of observations
movies_long %<>%
  dplyr::full_join(
    x = (.),
    y = (.) %>% dplyr::group_by(.data = ., genre, mpaa) %>% dplyr::tally(x = .),
    by = c("genre", "mpaa")
  ) %>%
  dplyr::filter(.data = ., n > 1) %>%
  dplyr::filter(.data = ., genre != "Short", mpaa != "NC-17") %>%
  dplyr::mutate_if(
    .tbl = .,
    .predicate = purrr::is_bare_character,
    .funs = ~base::as.factor(.)
  ) %>%
  dplyr::mutate_if(
    .tbl = .,
    .predicate = base::is.factor,
    .funs = ~base::droplevels(.)
  ) %>%
  dplyr::select(.data = ., -n)

# see the selected data
dplyr::glimpse(x = movies_long)

# saving the files
readr::write_csv(x = movies_wide, path = "data-raw/movies_wide.csv")
base::save(movies_wide, file = "data/movies_wide.rdata")
readr::write_csv(x = movies_long, path = "data-raw/movies_long.csv")
base::save(movies_long, file = "data/movies_long.rdata")
