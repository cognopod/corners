#' fourCorners
#' 
#' fourCorners allows you to plot all of the plots in a prepCorners() list
#' @param cornerSet is an dataframe produced by prepNettle()
#' @param outcomeLabel is a passthrough variable for the name of the outcome
#' @export
fourCorners <- function(cornerSet, outcomeLabel = "OUTCOME") {
  # draw a deadNettle plot for each path 
  for (k in 1:nrow(cornerSet$pimCorners)) {
    plotCorners(cornerSet$pimCorners$PATHOUT[k], 
                    cornerSet$pimCorners$OTHOUT[k], 
                    cornerSet$pimCorners$PATHNOT[k], 
                    cornerSet$pimCorners$OTHNOT[k], 
                    cornerSet$pimCorners$UNIQOUT[k], 
                    row.names(cornerSet$pimCorners)[k], 
                    outcomeLabel)
  }
}