#' fourCorners
#'
#' fourCorners allows you to plot all of the plots in a prepCorners() list
#' @param cornerSet is an dataframe produced by prepNettle()
#' @param outcomeLabel is a passthrough variable for the name of the outcome
#' @param pathColor is the color of the on-path cases with the outcome
#' @param othersColor is the color of the cases with the outcome not on the path
#' @param pathNotColor is the color of the on-path cases without the outcome
#' @param othersNotColor is the color of the irrelevant cases (without the outcome and not on the path)
#' @param shape is the shape to be used in plotting
#' @export
fourCorners <- function(cornerSet, outcomeLabel = "OUTCOME", pathColor = "#5e3c99", othersColor = "#b2abd2", pathNotColor = "#e66101", othersNotColor = "#fdb863", shape = "square") {
  # draw a deadNettle plot for each path
  for (k in 1:nrow(cornerSet$pimCorners)) {
    plotCorners(cornerSet$pimCorners$PATHOUT[k],
                    cornerSet$pimCorners$OTHOUT[k],
                    cornerSet$pimCorners$PATHNOT[k],
                    cornerSet$pimCorners$OTHNOT[k],
                    cornerSet$pimCorners$UNIQOUT[k],
                    row.names(cornerSet$pimCorners)[k],
                    outcomeLabel,
                pathColor = pathColor,
                othersColor = othersColor,
                pathNotColor = pathNotColor,
                othersNotColor = othersNotColor,
                shape = shape)
  }
}
