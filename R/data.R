#' @title Movie information and user ratings from IMDB.com (wide format).
#' @name movies_wide
#' @details Modified dataset from `ggplot2movies` package.
#'
#' The internet movie database, \url{http://imdb.com/}, is a website devoted
#' to collecting movie data supplied by studios and fans.  It claims to be the
#' biggest movie database on the web and is run by amazon.  More about
#' information imdb.com can be found online,
#' \url{http://imdb.com/help/show_leaf?about}, including information about
#' the data collection process,
#' \url{http://imdb.com/help/show_leaf?infosource}.
#'
#' Movies were selected for inclusion if they had a known length and had been
#' rated by at least one imdb user.
#'
#' @format A data frame with 1813 rows and 14 variables
#' \itemize{
#'   \item title.  Title of the movie.
#'   \item year.  Year of release.
#'   \item budget.  Total budget (if known) in US dollars
#'   \item length.  Length in minutes.
#'   \item rating.  Average IMDB user rating.
#'   \item votes.  Number of IMDB users who rated this movie.
#'   \item mpaa.  MPAA rating.
#'   \item action, animation, comedy, drama, documentary, romance, short.
#'     Binary variables representing if movie was classified as belonging to that genre.
#' }
#'
#' @source \url{https://CRAN.R-project.org/package=ggplot2movies}
#'
#' @examples
#' dim(movies_wide)
#' head(movies_wide)
"movies_wide"

#' @title Movie information and user ratings from IMDB.com (long format).
#' @name movies_long
#' @details Modified dataset from `ggplot2movies` package.
#'
#' The internet movie database, \url{http://imdb.com/}, is a website devoted
#' to collecting movie data supplied by studios and fans.  It claims to be the
#' biggest movie database on the web and is run by amazon.  More about
#' information imdb.com can be found online,
#' \url{http://imdb.com/help/show_leaf?about}, including information about
#' the data collection process,
#' \url{http://imdb.com/help/show_leaf?infosource}.
#'
#' Movies were selected for inclusion if they had a known length and had been
#' rated by at least one imdb user.
#'
#' @format A data frame with 2433 rows and 8 variables
#' \itemize{
#'   \item title.  Title of the movie.
#'   \item year.  Year of release.
#'   \item budget.  Total budget (if known) in US dollars
#'   \item length.  Length in minutes.
#'   \item rating.  Average IMDB user rating.
#'   \item votes.  Number of IMDB users who rated this movie.
#'   \item mpaa.  MPAA rating.
#'   \item genre. Different genres of movies (action, animation, comedy, drama, documentary, romance, short).
#' }
#'
#' @source \url{https://CRAN.R-project.org/package=ggplot2movies}
#'
#' @examples
#' dim(movies_long)
#' head(movies_long)
"movies_long"

#' @title Titanic dataset.
#' @name Titanic_full
#' @details This data set provides information on the fate of passengers on the fatal
#' maiden voyage of the ocean liner 'Titanic', summarized according to economic
#' status (class), sex, age and survival.
#'
#' This is a modified dataset from `datasets` package.
#'
#' @format A data frame with 2201 rows and 5 variables
#' \itemize{
#'   \item id. Dummy identity number for each person.
#'   \item Class.	1st, 2nd, 3rd, Crew.
#'   \item Sex.	Male, Female.
#'   \item Age.	Child, Adult.
#'   \item Survived.	No, Yes.
#' }
#'
#' @source \url{https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/Titanic.html}
#'
#' @examples
#' dim(Titanic_full)
#' head(Titanic_full)
"Titanic_full"

#' @title Moral judgments about third-party moral behavior.
#' @name intent_morality
#' @details This dataset contains data from a recent study about how people
#'   judge behavior of others when they unintentionally or intentionally cause
#'   harm to others.
#'
#' Participants responded to four different vignettes that contains four different types of conditions-
#'  \itemize{
#'   \item accidental harm. neutral belief, harmful/negative outcome
#'   \item intentional harm. harmful/negative belief, harmful/negative outcome
#'   \item attempted harm. harmful/negative belief, neutral outcome
#'   \item neutral harm. neutral belief, neutral outcome
#' }
#'
#' Additionally, participants saw one of the four variants for each of the four
#' items. Each of the item had a different type of harm.
#'
#' @format A data frame with 4016 rows and 8 variables
#' \itemize{
#'   \item id.  Participant id.
#'   \item gender.  Participant's gender.
#'   \item item.  Which story/vignette participants read for a given `condition`.
#'   \item harm.  What kind of harm was involved in the `item`.
#'   \item belief.  What kind of belief the actor had (neutral or negative/harmful).
#'   \item outcome.  What kind of outcome the actor caused (neutral or negative/harmful).
#'   \item condition.  Type of harm, composed of `belif` and `outcome`.
#'   \item question. Type of moral judgment asked (`wrongess` or `punishment`).
#'   \item rating. Moral judgment rating on a scale of 1 to 7.
#' }
#'
#' @source \url{https://www.nature.com/articles/s41598-017-05299-9}
#'
#' @examples
#' dim(intent_morality)
#' head(intent_morality)
"intent_morality"

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
#'   \item Species.	The species are *Iris setosa*, *versicolor*, and *virginica*.
#'   \item attribute.	What attribute is being measured (`"Sepal"` or `"Pepal"`).
#'   \item measure.	What aspect of the attribute is being measured (`"Length"`
#'   or `"Width"`).
#'   \item value.	Value of the measurement.
#' }
#'
#' @source \url{https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/iris.html}
#'
#' @examples
#' dim(iris_long)
#' head(iris_long)
"iris_long"
