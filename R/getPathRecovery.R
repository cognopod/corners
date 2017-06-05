#' getPathRecovery
#'
#' What percentage of paths were recovered in the sample?
#' 
#' @param QCAoutput the output from a QCApro analysis
#' @param simOutput the output from simCases
#' @export

getPathRecovery <- function (QCAoutput, simOutput) {
  k = 0
  nQCASolutions <- length(QCAoutput$solution[[1]])
  nMCSolutions <- length(simOutput$paths[,1])
  for (i in 1:nMCSolutions) {
    for (j in 1:nQCASolutions)  {
      if (identical(as.character(simOutput$paths[i,1]),as.character(QCAoutput$solution[[1]][j])) == TRUE) {
        k <- k + 1
        break
      }
    }
  }
  return(k/length(simOutput$paths[,1]))
}