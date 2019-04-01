#' allCorners
#'
#' allCorners allows you to plot all of the plots in a prepCorners() list
#' @param cornerSet is an dataframe produced by prepNettle()
#' @param outcomeLabel is a passthrough variable for the name of the outcome
#' @param pathColor is the color of the on-path cases with the outcome
#' @param othersColor is the color of the cases with the outcome not on the path
#' @param pathNotColor is the color of the on-path cases without the outcome
#' @param othersNotColor is the color of the irrelevant cases (without the outcome and not on the path)
#' @param shape is the shape to be used in plotting
#' @export
allCorners <- function(cornerSet, outcomeLabel = "OUTCOME", pathColor = "#5e3c99", othersColor = "#b2abd2", pathNotColor = "#e66101", othersNotColor = "#fdb863", shape = "square") {
  PATHNOT <- sum(cornerSet$pimChart$NOT[which(cornerSet$pimChart$PATH == 1)])
  OTHNOT <- sum(cornerSet$pimChart$NOT[which(cornerSet$pimChart$PATH == 0)])
  PATHOUT <- sum(cornerSet$pimChart$OUT[which(cornerSet$pimChart$PATH == 1)])
  OTHOUT <- sum(cornerSet$pimChart$OUT[which(cornerSet$pimChart$PATH == 0)])
  plotCorners(PATHOUT, OTHOUT, PATHNOT, OTHNOT, 0, "ALL PATHS", outcomeLabel,
              pathColor = pathColor,
              othersColor = othersColor,
              pathNotColor = pathNotColor,
              othersNotColor = othersNotColor,
              shape = shape)
}
