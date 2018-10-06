#' @title Setting Working Directory in RStudio to where the R Script is.
#' @name set_cwd
#' @description This function will change the current working directory to
#'   whichever directory the R script you are currently working on is located.
#'   This preempts the trouble of setting the working directory manually.
#' @return Path to changed working directory.
#'
#' @note This function will work **only with RStudio IDE**. Reference:
#'   https://eranraviv.com/r-tips-and-tricks-working-directory/
#'
#' @importFrom rstudioapi getActiveDocumentContext
#'
#' @export

# function body
set_cwd <- function() {
  # get path to the folder whereever the R script is located
  current_path <-
    rstudioapi::getActiveDocumentContext()$path

  # set working directory to that path
  base::setwd(dir = dirname(path = current_path))

  # print the current directory to confirm you are in the right directory
  print(x = paste("setting current working directory to: ", base::getwd(),
                  sep = ""))
}
