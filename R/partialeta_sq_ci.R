#'
#' @title Confidence intervals for Partial Eta Squared
#' @name partialeta_sq_ci
#' @author Indrajeet Patil
#'
#' @param lm_object stats::lm linear model object
#' @param conf.level Level of confidence for the confidence interval
#' @export

partialeta_sq_ci <- function(lm_object, conf.level = 0.95) {
  # get the linear model object and turn it into a matrix and turn row names into a variable called "effect"
  # compute partial eta-squared for each effect
  # add additional columns containing data and formula that was used to create these effects
  x <-
    dplyr::left_join(
      data.table::setDT(as.data.frame(as.matrix(
        stats::anova(lm_object)
      )), keep.rownames = "effect"),
      data.table::setDT(as.data.frame(
        cbind(
          "effsize" = sjstats::eta_sq(stats::anova(lm_object),
                                      partial = TRUE),
          "data" = as.character(lm_object$call[3]),
          "formula" = as.character(lm_object$call[2])
        )
      ),
      keep.rownames = "effect"),
      by = "effect"
    )
  # create a new column for residual degrees of freedom
  x$df2 <- x$Df[x$effect == "Residuals"]
  # remove sum of squares columns since they will not be useful
  x <- x %>% dplyr::select(-c(grep("Sq", names(x))))
  # remove NAs, which would remove the row containing Residuals (redundant at this point)
  x <- na.omit(x)
  # rename to something more meaningful and tidy
  x <- plyr::rename(x, c("Df" = "df1", "F value" = "F.value"))
  # rearrange the columns
  x <-
    x[, c("F.value",
          "df1",
          "df2",
          "effect",
          "effsize",
          "Pr(>F)",
          "data",
          "formula")]
  # convert the effect into a factor
  x$effect <- as.factor(x$effect)
  # for each type of effect, compute partial eta-squared confidence intervals, which would return a list
  library(plyr); library(dplyr)
  ci_df <-
    plyr::dlply(
      .data = x,
      .variables = .(effect),
      .fun = function(data)
        apaTables::get.ci.partial.eta.squared(
          F.value = data$F.value,
          df1 = data$df1,
          df2 = data$df2,
          conf.level = conf.level
        )
    )
  # get elements from the effect size confidence intervals list into a neat dataframe
  ci_df <-
    plyr::ldply(ci_df, function(x)
      cbind("LL" = x[[1]], "UL" = x[[2]]))
  # merge the dataframe containing effect sizes with the dataframe containing rest of the information
  effsize_ci <- merge(x, ci_df, by = "effect")
  return(effsize_ci)

}
