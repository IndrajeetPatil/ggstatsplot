library(tidyverse)

# for reproducibility
set.seed(123)

# first run the example from the base code
# creating dataframe
mtcars_new <- mtcars %>%
  tibble::rownames_to_column(., var = "car") %>%
  tibble::as_data_frame(x = .)

# simple function call with the defaults
ggstatsplot::ggscatterstats(
  data = mtcars_new,
  x = wt,
  y = mpg,
  type = "np",
  # have to quote car variable now
  label.var = "car",
  label.expression = "wt < 4 & mpg < 20",
  axes.range.restrict = TRUE,
  centrality.para = "median"
)
# works as advertised

# too many rows for debugging makes it slow.  Let's sample
movies_short <- ggstatsplot::movies_wide %>% group_by(mpaa) %>% sample_n(7) %>% ungroup
str(movies_short)

# let's split the dataframe and create a list by mpaa rating
mpaa_list <- movies_short %>%
  base::split(x = ., f = .$mpaa, drop = TRUE)

# this created a list with 4 elements, one for each mpaa rating
# you can check the structure of the file for yourself
str(mpaa_list)

# checking the length and names of each element
length(mpaa_list)
#> [1] 4
names(mpaa_list)
#> [1] "NC-17" "PG"    "PG-13" "R"

# running function on every element of this list note that if you want the same
# value for a given argument across all elements of the list, you need to
# specify it just once
plot_list1 <- purrr::pmap(
  .l = list(
    data = mpaa_list,
    x = "budget",
    y = "rating",
    xlab = "Budget (in millions of US dollars)",
    ylab = "Rating on IMDB",
    title = list(
      "MPAA Rating: NC-17",
      "MPAA Rating: PG",
      "MPAA Rating: PG-13",
      "MPAA Rating: R"
    ),
#    label.var = list("title", "year", "votes", "genre"),
# genre doesn't exist!
    label.var = list("title", "year", "votes", "length"),
    label.expression = list(
      ("rating > 6 & budget < 50"),
      ("rating > 6 & budget < 100"),
      ("rating > 6 & budget < 50"),
      ("rating > 6 & budget < 10")
    ),
# Tried lots of permutations but failed
# It works hard coded as you can see in the code
#    label.expression = list("rating > 6","rating > 6","rating > 6","rating > 6"),
    type = list("r", "np", "p", "np"),
    method = list(MASS::rlm, "lm", "lm", "lm"),
    marginal.type = list("histogram", "boxplot", "density", "violin"),
    centrality.para = "mean",
    xfill = list("#56B4E9", "#009E73", "#999999", "#0072B2"),
    yfill = list("#D55E00", "#CC79A7", "#F0E442", "#D55E00"),
    ggtheme = list(
      ggplot2::theme_grey(),
      ggplot2::theme_classic(),
      ggplot2::theme_light(),
      ggplot2::theme_minimal()
    ),
    messages = FALSE
  ),
  .f = ggstatsplot::ggscatterstats
)
plot_list1
