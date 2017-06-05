#' simFuzz
#' 
#' simFuzz is a function to generate simulated continous data for cluster analysis
#' 
#' @param numVars creates n vars that are all in fuzzy subset relationships. Must be greater than 2. Default is 2.
#' @param numCases is the number of cases to include in the generated fake data set. Must be greater than 1. Default is 50.
#' @return A data frame with one column per variable.
#' @export
#
simFuzz <- function(numVars = 2, numCases = 50) {
  # create a data frame of random numbers
  preScatter <- data.frame(replicate(numVars,sample(0:1000,numCases,rep=TRUE)))
  # convert to decimals
  preScatter <- preScatter/1000
  # name the columns
  colnames(preScatter) <- LETTERS[1:numVars]
  # generate a blank data frame to hold the actual generated data
  genScatter <- data.frame(matrix(NA, nrow = numCases, ncol = numVars))
  # name the columns
  colnames(genScatter) <- LETTERS[1:numVars]
  # move the first column to the generated data frame
  genScatter$A <- preScatter$A
  # for the other columns, place above the diagonal with the transform below
  for (i in 2:ncol(preScatter)) {
    for(j in 1:nrow(preScatter)) {
      genScatter[j,i] <- (1- preScatter[j,1]) * preScatter[j,i] + preScatter[j,1]
    }
  }
  # send back the generated data frame
  return(genScatter)
}