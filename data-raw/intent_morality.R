
# loading the needed libraries
library(dplyr)
library(magrittr)
library(readr)

# reading the datafile
intent_morality <- readr::read_csv(file = "data-raw/Patil_et_al_2017_2.csv")

# modifying the dataframe
intent_morality %<>%
  dplyr::mutate(
    .data = .,
    harm = dplyr::case_when(
      item == "Grace" ~ "Poisoning",
      item == "Frank" ~ "Nerve Pain",
      item == "Irene" ~ "Sting",
      item == "Anna" ~ "Burn"
    )
  ) %>%
  dplyr::select(.data = .,
                id,
                gender,
                item,
                harm,
                belief,
                outcome,
                condition,
                question,
                rating) %>%
  dplyr::mutate_if(
    .tbl = .,
    .predicate = purrr::is_bare_character,
    .funs = ~ as.factor(.)
  )

# saving the data
base::save(intent_morality, file = "data/intent_morality.rdata")
