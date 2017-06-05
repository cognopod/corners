#' prepCorners
#' 
#' prepCorners makes a dataframe containing the five numbers needed for each path in order to draw a deadNettle plot
#' @param data is the raw data
#' @param solution is the QCApro output list from eQMC() function
#' @param outcomeCol is the original outcome column
#' @param ruleOut is the rule for recoding the target outcome in car::recode format (so if you looked at avoiding an outcome you need to use "0=1;1=0")
#' @return A list of componments
#'    \item{pimChart}{The prime implicants table}
#'    \item{pimCorners}{The five numbers needed to draw the fourCorners plot for each of the paths. Counts are the number of cases with each outcome.}
#' \describe{
#'  \item{PATHOUT}{The number of cases on the path and with the outcome. When this is proportionally large, most of the cases can be explained by this configuration.}
#'  \item{OTHOUT}{The number of cases not on the path, but with the outcome. Cases not covered by this configuration.}
#'  \item{PATHNOT}{The number of cases on the path, but without the outcome. Counterfactuals.}
#'  \item{OTHNOT}{The number of cases not on the path, but without the outcome. Irrelevant.} 
#'  \item{UNIQOUT}{The number of cases with the outcome that are uniquely covered by this path.}
#' }
#' @export
prepCorners <- function (data, solution, outcomeCol, ruleOut = "0=0;1=1") {
  # need to use the car libary
  # eventually need :: notation
  # library(car)
  # retrieve the prime implicants table
  pims <- solution$pims
  # create a blank data frame to hold the count data
  pimNettle <- data.frame(matrix(NA, nrow = ncol(pims), ncol = 5))
  # name the columns
  colnames(pimNettle) <- c("PATHOUT", "OTHOUT", "PATHNOT", "OTHNOT", "UNIQOUT")
  # name the rows by the prime implicants (paths)
  rownames(pimNettle) <- colnames(pims)
  # copy over the outcome column
  outcome <- data[which(colnames(data) == outcomeCol)]
  colnames(outcome) <- "OUT"
  # recode if necessary
  pims$OUT <- car::recode(outcome$OUT, ruleOut)
  pims$NOT <- car::recode(pims$OUT, "0=1;1=0")
  # add a column for how many times that path is connect to OUT
  if (ncol(solution$pims) > 1) {
    pims$ALLPATHS <- rowSums(pims[,c(1:ncol(solution$pims))], dims=1)
  } else {
    pims$ALLPATHS <- pims[,1]
  }
  # add a column for something. Not sure what we use this for
  if (ncol(solution$pims) > 1) {
    pims$PATH <- car::recode(rowSums(pims[,c(1:ncol(solution$pims))], dims=1), "0=0;1:hi=1")
  } else {
    pims$PATH <- car::recode(pims[,1], "0=0;1:hi=1")
  }
  # output the numbers based on OUT, PATHOUT, and ALLPATHS
  for (i in 1:nrow(pimNettle)) {
    pimNettle$PATHOUT[i] <- sum(pims$OUT[which(pims[,i] == 1)])
    pimNettle$OTHOUT[i] <- sum(pims$OUT[which(pims[,i] == 0)])
    pimNettle$PATHNOT[i] <- colSums(pims[i]) - pimNettle$PATHOUT[i]
    pimNettle$OTHNOT[i] <- length(which(pims[,i] == 0)) - sum(pims$OUT[which(pims[,i] == 0)])
    pimNettle$UNIQOUT[i] <- sum(pims[which(pims$ALLPATHS == 1),i])
  }
  # construct the list
  nettlePatch <- list("pimChart" = pims, "pimCorners" = pimNettle)
  # return the list
  return(nettlePatch)
}