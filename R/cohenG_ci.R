#' @title Confidence intervals for Cohen's *g* effect size.
#' @name cohenG_ci
#' @author Indrajeet Patil
#'
#' @inheritParams rcompanion::cohenG
#' @inheritParams t1way_ci
#'
#' @importFrom rcompanion cohenG
#' @importFrom boot boot boot.ci
#'
#' @examples
#' \dontrun{
#' library(rcompanion)
#' ggstatsplot:::cohenG_ci(AndersonRainGarden)
#' }
#'
#' @keywords internal

cohenG_ci <- function(x,
                      nboot = 100,
                      conf.level = 0.95,
                      conf.type = "norm",
                      ...) {
  ellipsis::check_dots_used()

  g <- rcompanion::cohenG(x, digits = 7)$Global.statistics$g[[1]]

  X <- as.table(x)

  Counts <- as.data.frame(X)

  Long <- Counts[rep(row.names(Counts), Counts$Freq), c(1, 2)]

  rownames(Long) <- seq(1:nrow(Long))

  Data <- Long

  Function <- function(input, index) {
    Input <- input[index, ]
    Stat <- rcompanion::cohenG(table(Input))$Global.statistics$g
    return(Stat)
  }

  ### Test Function

  n <- length(Data[, 1])

  Function(Data, 1:n)

  ### Produce confidence interval by bootstrap

  Boot <- boot::boot(Long, Function, R = nboot)
  BCI <- boot::boot.ci(Boot, conf = conf.level, type = conf.type)

  # type of confidence interval
  if (conf.type == "norm") {
    CI1 <- BCI$normal[2]
    CI2 <- BCI$normal[3]
  }
  if (conf.type == "basic") {
    CI1 <- BCI$basic[4]
    CI2 <- BCI$basic[5]
  }
  if (conf.type == "perc") {
    CI1 <- BCI$percent[4]
    CI2 <- BCI$percent[5]
  }
  if (conf.type == "bca") {
    CI1 <- BCI$bca[4]
    CI2 <- BCI$bca[5]
  }

  # return the dataframe with effect size and confidence intervals
  return(data.frame(r = g, lower.ci = CI1, upper.ci = CI2))
}
