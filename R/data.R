#' @title Movie information and user ratings from IMDB.com (wide format).
#' @name movies_wide
#' @details Modified dataset from `ggplot2movies` package.
#'
#' The internet movie database, \url{https://imdb.com/}, is a website devoted
#' to collecting movie data supplied by studios and fans. It claims to be the
#' biggest movie database on the web and is run by amazon.
#'
#' Movies were selected for inclusion if they had a known length and had been
#' rated by at least one imdb user. Small categories such as documentaries
#' and NC-17 movies were removed.
#'
#' @format A data frame with 1,579 rows and 13 variables
#' \itemize{
#'   \item title. Title of the movie.
#'   \item year. Year of release.
#'   \item budget. Total budget in millions of US dollars
#'   \item length. Length in minutes.
#'   \item rating. Average IMDB user rating.
#'   \item votes. Number of IMDB users who rated this movie.
#'   \item mpaa. MPAA rating.
#'   \item action, animation, comedy, drama, documentary, romance, short. Binary
#'   variables representing if movie was classified as belonging to that genre.
#'   \item NumGenre. The number of different genres a film was classified in an
#'   integer between one and four
#' }
#'
#' @source \url{https://CRAN.R-project.org/package=ggplot2movies}
#'
#' @examples
#' dim(movies_wide)
#' head(movies_wide)
#' dplyr::glimpse(movies_wide)
"movies_wide"

#' @title Movie information and user ratings from IMDB.com (long format).
#' @name movies_long
#' @details Modified dataset from `ggplot2movies` package.
#'
#' The internet movie database, \url{https://imdb.com/}, is a website devoted
#' to collecting movie data supplied by studios and fans. It claims to be the
#' biggest movie database on the web and is run by amazon.
#'
#' Movies were are identical to those selected for inclusion in movies_wide but this
#' dataset has been constructed such that every movie appears in one and only one
#' genre category.
#'
#' @format A data frame with 1,579 rows and 8 variables
#' \itemize{
#'   \item title. Title of the movie.
#'   \item year. Year of release.
#'   \item budget. Total budget (if known) in US dollars
#'   \item length. Length in minutes.
#'   \item rating. Average IMDB user rating.
#'   \item votes. Number of IMDB users who rated this movie.
#'   \item mpaa. MPAA rating.
#'   \item genre. Different genres of movies (action, animation, comedy, drama,
#'   documentary, romance, short).
#' }
#'
#' @source \url{https://CRAN.R-project.org/package=ggplot2movies}
#'
#' @examples
#' dim(movies_long)
#' head(movies_long)
#' dplyr::glimpse(movies_long)
"movies_long"

#' @title Titanic dataset.
#' @name Titanic_full
#' @details This data set provides information on the fate of passengers on the
#'   fatal maiden voyage of the ocean liner 'Titanic', summarized according to
#'   economic status (class), sex, age and survival.
#'
#' This is a modified dataset from `datasets` package.
#'
#' @format A data frame with 2201 rows and 5 variables
#' \itemize{
#'   \item id. Dummy identity number for each person.
#'   \item Class. 1st, 2nd, 3rd, Crew.
#'   \item Sex. Male, Female.
#'   \item Age. Child, Adult.
#'   \item Survived. No, Yes.
#' }
#'
#' @examples
#' dim(Titanic_full)
#' head(Titanic_full)
#' dplyr::glimpse(Titanic_full)
"Titanic_full"

#' @title Edgar Anderson's Iris Data in long format.
#' @name iris_long
#' @details This famous (Fisher's or Anderson's) iris data set gives the
#'   measurements in centimeters of the variables sepal length and width and
#'   petal length and width, respectively, for 50 flowers from each of 3 species
#'   of iris. The species are Iris setosa, versicolor, and virginica.
#'
#' This is a modified dataset from `datasets` package.
#'
#' @format A data frame with 600 rows and 5 variables
#' \itemize{
#'   \item id. Dummy identity number for each flower (150 flowers in total).
#'   \item Species. The species are *Iris setosa*, *versicolor*, and
#'   *virginica*.
#'   \item condition. Factor giving a detailed description of the attribute
#'   (Four levels: `"Petal.Length"`, `"Petal.Width"`,  `"Sepal.Length"`,
#'   `"Sepal.Width"`).
#'   \item attribute. What attribute is being measured (`"Sepal"` or `"Pepal"`).
#'   \item measure. What aspect of the attribute is being measured (`"Length"`
#'   or `"Width"`).
#'   \item value. Value of the measurement.
#' }
#'
#' @examples
#' dim(iris_long)
#' head(iris_long)
#' dplyr::glimpse(iris_long)
"iris_long"


#' @title Virtual reality moral dilemmas.
#' @name VR_dilemma
#' @details Dataset from a study where participants completed identical moral
#'   dilemmas in two different sessions held on separate days: in one session,
#'   they read text description of the scenario, while in another session they
#'   completed the same scenarios in Virtual Reality (videos:
#'   \url{https://www.youtube.com/watch?v=ebdU3HhhYs8}). The study investigated
#'   if there was a discrepancy between how people judged the same scenarios
#'   while reading them in text versus experiencing them in virtual reality.
#'
#' @format A data frame with 68 rows and 4 variables
#' \itemize{
#'   \item id. Dummy identity number for each participant.
#'   \item order. The order in which the participants completed the two
#'   sessions: `"text_first"` (`0`) or `"text_second"` (`1`).
#'   \item modality. Describes how the moral dilemmas were presented to the
#'   participants: either in text format (`"text"`) or in Virtual Reality
#'   (`"vr"`).
#'   \item score. Proportion of "utilitarian" decisions. In other words, of the
#'   4 decisions, how many affirmative were responses. Range: 0 (all
#'   utilitarian) - 1 (none utilitarian).
#' }
#'
#' @source
#' \url{https://psyarxiv.com/ry3ap/}
#'
#' @examples
#' dim(VR_dilemma)
#' head(VR_dilemma)
#' dplyr::glimpse(VR_dilemma)
"VR_dilemma"


#' @title Tidy version of the "Bugs" dataset.
#' @name bugs_long
#' @details This data set, "Bugs", provides the extent to which men and women
#'   want to kill arthropods that vary in freighteningness (low, high) and
#'   disgustingness (low, high). Each participant rates their attitudes towards
#'   all anthropods. Subset of the data reported by Ryan et al. (2013).
#'
#' @format A data frame with 372 rows and 6 variables
#' \itemize{
#'   \item subject. Dummy identity number for each participant.
#'   \item gender. Participant's gender (Female, Male).
#'   \item region. Region of the world the participant was from.
#'   \item education. Level of education.
#'   \item condition. Condition of the experiment the participant gave rating
#'   for (**LDLF**: low freighteningness and low disgustingness; **LFHD**: low
#'   freighteningness and high disgustingness; **HFHD**: high freighteningness
#'   and low disgustingness; **HFHD**: high freighteningness and high
#'   disgustingness).
#'   \item desire. The desire to kill an arthropod was indicated on a scale from
#'   0 to 10.
#' }
#'
#' @source
#' \url{https://www.sciencedirect.com/science/article/pii/S0747563213000277}
#'
#' @examples
#' dim(bugs_long)
#' head(bugs_long)
#' dplyr::glimpse(bugs_long)
"bugs_long"


#' @title Wide-format version of the "Bugs" dataset.
#' @name bugs_wide
#' @details This data set, "Bugs", provides the extent to which men and women
#'   want to kill arthropods that vary in freighteningness (low, high) and
#'   disgustingness (low, high). Each participant rates their attitudes towards
#'   all anthropods. Subset of the data reported by Ryan et al. (2013).
#'
#' @format A data frame with 93 rows and 6 variables
#' \itemize{
#'   \item subject. Dummy identity number for each participant.
#'   \item gender. Participant's gender (Female, Male).
#'   \item region. Region of the world the participant was from.
#'   \item education. Level of education.
#'   \item ldlf,ldhf,hdlf,hdhf.The desire to kill an arthropod was indicated on
#'   a scale from 0 to 10 in each condition of the experiment (**LDLF**: low
#'   freighteningness and low disgustingness; **LFHD**: low freighteningness and
#'   high disgustingness; **HFHD**: high freighteningness and low
#'   disgustingness; **HFHD**: high freighteningness and high disgustingness).
#' }
#'
#' @source
#' \url{https://www.sciencedirect.com/science/article/pii/S0747563213000277}
#'
#' @examples
#' dim(bugs_wide)
#' head(bugs_wide)
#' dplyr::glimpse(bugs_wide)
"bugs_wide"
