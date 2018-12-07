# loading the needed libraries
library(ggplot2movies)
library(dplyr)

# looking at the table
dplyr::glimpse(x = ggplot2movies::movies)

# clean data leave it in wide format
movies_wide <-
  ggplot2movies::movies %>% # `.` is just a placeholder for the data throughout
  dplyr::select(.data = ., c(title:votes, mpaa:Short)) %>% # eliminate the decile columns r1 through r10
  dplyr::filter(.data = ., mpaa != "", mpaa != "NC-17") %>% # removing movies without mpaa ratings & NC-17
  dplyr::filter(.data = ., Short != 1, Documentary != 1) %>% # removing Shorts and Documentaries
  dplyr::select(.data = ., -c(Short, Documentary)) %>% # remove the now useless columns
  stats::na.omit(.) %>%                                 # removing NAs
  dplyr::mutate(.data = ., budget = budget / 1000000) %>% # convert the budget to millions of dollars
  dplyr::mutate(.data = ., mpaa = factor(mpaa)) %>% # convert mpaa rating to a factor
  dplyr::mutate(.data = ., NumGenre = as.integer(rowSums(select(., Action:Romance)))) %>% # calculate number of genres a film appears in
  dplyr::filter(.data = ., NumGenre > 0) # remove films that are not listed in any genre

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
  stats::na.omit(.) %>% # removing NAs
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


##### Testing
ggstatsplot::ggbetweenstats(movies_long,
                            mpaa,
                            rating,
                            type = "parametric",
                            effsize.type = "biased",
                            partial = FALSE,
                            var.equal = TRUE,
                            mean.ci = TRUE,
                            pairwise.comparisons = TRUE,
                            p.adjust.method = "holm",
                            outlier.tagging = TRUE,
                            outlier.label = title
                            )


ggstatsplot::grouped_ggbetweenstats(
  data = dplyr::filter(.data = movies_long,
                       genre %in% c("Animated", "Action Comedy", "Action", "Action Drama")),
  x = mpaa,
  y = length,
  grouping.var = genre,             # grouping variable
  pairwise.comparisons = TRUE,      # display significant pairwise comparisons
  pairwise.annotation = "p.value",  # how do you want to annotate the pairwise comparisons
  p.adjust.method = "bonferroni",   # method for adjusting p-values for multiple comparisons
  bf.message = TRUE,                # display Bayes Factor in favor of the null hypothesis
  conf.level = 0.99,                # changing confidence level to 99%
  k = 3,
  title.prefix = "Movie genre",
  caption = substitute(paste(italic("Source"),
                             ":IMDb (Internet Movie Database)")),
  palette = "default_jama",
  package = "ggsci",
  messages = FALSE,
  nrow = 2,
  ncol = 2,
  title.text = "Differences in movie length by mpaa ratings for different genres"
)
