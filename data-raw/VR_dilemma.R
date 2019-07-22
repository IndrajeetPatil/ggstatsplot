# loading the needed libraries
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# reading the datafile
VR_dilemma <- readr::read_csv(file = "data-raw/VR_dilemma.csv")

# selecting only the decision columns
VR_dilemma %<>%
  dplyr::select(.data = ., ID:vrD) %>%
  tidyr::gather(
    data = .,
    key = "modality",
    value = "score",
    textD:vrD,
    na.rm = TRUE
  ) %>%
  dplyr::mutate(
    .data = .,
    modality = stringr::str_remove(modality, "D$")
  ) %>%
  dplyr::mutate(
    .data = .,
    order = tolower(sjlabelled::as_label(order))
  ) %>%
  dplyr::rename(.data = ., id = ID)

# saving the data
save(VR_dilemma, file = "data/VR_dilemma.rdata")
