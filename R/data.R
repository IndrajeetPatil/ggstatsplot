#'
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
#'
"movies_wide"


#'
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
#'
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
#'   \item id. Dummy identiy number for each person.
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
#'
"Titanic_full"
