#' allCorners
#' 
#' allCorners allows you to plot all of the plots in a prepCorners() list
#' @param cornerSet is an dataframe produced by prepNettle()
#' @param outcomeLabel is a passthrough variable for the name of the outcome
#' @export
allCorners <- function(cornerSet, outcomeLabel = "OUTCOME") {
  PATHNOT <- sum(cornerSet$pimChart$NOT[which(cornerSet$pimChart$PATH == 1)])
  OTHNOT <- sum(cornerSet$pimChart$NOT[which(cornerSet$pimChart$PATH == 0)])
  PATHOUT <- sum(cornerSet$pimChart$OUT[which(cornerSet$pimChart$PATH == 1)])
  OTHOUT <- sum(cornerSet$pimChart$OUT[which(cornerSet$pimChart$PATH == 0)])
  plotCorners(PATHOUT, OTHOUT, PATHNOT, OTHNOT, 0, "ALL PATHS", outcomeLabel)
}