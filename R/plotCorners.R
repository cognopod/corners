#' plotCorners
#'
#' plotCorners is a function that draws the full four leaf fourCorners plot
#'
#' @param pathOutcome is the number of cases that have the path leading to the outcome
#' @param otherOutcome is the number of cases without the path but with the outcome -- used to calculate coverage
#' @param pathNotOutcome is the number of cases that have the path, but not the outcome -- used to calculate consistency
#' @param othersNotOutcome is the number of cases that are irrelevant (no path or outcome)
#' @param uniquePathOutcome is the number of cases that only have that path to the outcome (and not some other)
#' @param path is the name of the path
#' @param outcome is the name of the outcome
#' @param pathColor is the color of the on-path cases with the outcome
#' @param othersColor is the color of the cases with the outcome not on the path
#' @param pathNotColor is the color of the on-path cases without the outcome
#' @param othersNotColor is the color of the irrelevant cases (without the outcome and not on the path)
#' @param shape is the shape to be used in plotting
#' @keywords internal
#'
plotCorners <- function(pathOutcome, othersOutcome, pathNotOutcome, othersNotOutcome, uniquePathOutcome, path = "PATH", outcome = "OUTCOME", textPosition = .25, pathColor = "#5e3c99", othersColor = "#b2abd2", pathNotColor = "#e66101", othersNotColor = "#fdb863", shape = "square") {
  offset = 0.028
  rElement = 0.02
  # draw deadnettle plot
  # lower left is (0,0)
  # upper right is (1,1)
  # library(grid)
  # use the Cairo library to output to a png device
  # must draw lines around objects for anti-aliasing
  #library(Cairo)
  #Cairo(file="Cairo_PNG_72_dpi_white.png",
  #      bg="white",
  #      type="png",
  #      units="in",
  #      width=5,
  #      height=5,
  #      pointsize=12,
  #      dpi=72)
  #
  #
  # use svg() to output vector graphics
  #svg(filename="Std_SVG.svg",
  #    width=5,
  #    height=5,
  #    pointsize=12)
  #tryCatch(dev.off(), error = function() {as.null(NULL)})
  # create a new page for each plot
  grid::grid.newpage()
  # create main plot viewport
  dp <- grid::viewport(x=0.5, y=0.5, width=0.9, height=0.9)
  # make dp the focus
  grid::pushViewport(dp)
  # `rectangle in the center of the plot
  # helps to separate the leaves and define areas
  grid::grid.lines(c(.5, .5), c(.45, .55), gp = grid::gpar(lty = "dotted"))
  grid::grid.lines(c(.45, .55), c(.5, .5), gp = grid::gpar(lty = "dotted"))
  grid::grid.rect(x = .5, y = .5, width = .02, height = .02, gp = grid::gpar(lty = "solid"))
  # create grid of four viewports that define the locations of the leaves
  dp.LL <- grid::viewport(x=.25, y=.25, width = .48, height = .48)
  dp.UL <- grid::viewport(x=.25, y=.75, width = .48, height = .48)
  dp.LR <- grid::viewport(x=.75, y=.25, width = .48, height = .48)
  dp.UR <- grid::viewport(x=.75, y=.75, width = .48, height = .48)
  #
  # draw the four leaves
  # the drawing of dots needs to be opposite of the sector location
  reloffset = .20/sqrt(max(pathOutcome, othersOutcome, pathNotOutcome, othersNotOutcome))
  relrElement = reloffset*5/7
  drawCorner(dp.UL, pathOutcome, 0, 1, pathColor, offset = reloffset, rElement = relrElement, shape = shape)
  drawCorner(dp.LL, othersOutcome, 1, 1, othersColor, offset = reloffset, rElement = relrElement, shape = shape)
  drawCorner(dp.UR, pathNotOutcome, 0, 0, pathNotColor, offset = reloffset, rElement = relrElement, shape = shape)
  drawCorner(dp.LR, othersNotOutcome, 1, 0, othersNotColor, offset = reloffset, rElement = relrElement, shape = shape)
  # draw the unique path-outcome leaf
  drawCorner(dp.UL, uniquePathOutcome, 0, 1, pathColor, unique = TRUE, offset = reloffset, rElement = relrElement, shape = shape)
  # draw the text on the graph for the path
  # grid.text(path, x = .5, y = .58, rot = 90, just = c("left", "center"), gp = gpar(fontsize = 10, col = "#888888"))
  # grid.text("ALL OTHER PATHS", x = .5, y = .42, rot = 90, just = c("right", "center"), gp = gpar(fontsize = 10, col = "#888888"))
  #grid.lines(c(.5, .5), c(.58, .65), gp = gpar(lty = "dotted"))
  #grid.lines(c(.5, .5), c(.42, .35), gp = gpar(lty = "dotted"))
  # grid.text(outcome, x = .42, y = .5, rot = 0, just = c("right", "center"), gp = gpar(fontsize = 10, col = "#888888"))
  # grid.text("ALL OTHER OUTCOMES", x = .58, y = .5, rot = 0, just = c("left", "center"), gp = gpar(fontsize = 10, col = "#888888"))
  #grid.lines(c(.58, .65), c(.5, .5), gp = gpar(lty = "dotted"))# text for the outcome
  #grid.lines(c(.42, .35), c(.5, .5), gp = gpar(lty = "dotted"))# text for the outcome
  # text
  grid::grid.text(path, x = 0.5, y = textPosition, rot = 0)
  # calculate some consistency and coverage scores
  consistency <- format(round(pathOutcome/(pathOutcome+pathNotOutcome),3), nsmall = 3)
  coverage <- format(round(pathOutcome/(pathOutcome+othersOutcome),3), nsmall = 3)
  grid::grid.text("consistency", x = 0.5, y = 0.78, rot = 0, gp = grid::gpar(col="grey", fontsize=10))
  grid::grid.text(consistency, x = 0.5, y = 0.75, rot = 0, gp = grid::gpar(col="grey", fontsize=10))
  grid::grid.text("coverage", x = 0.22, y = 0.5, rot = 90,  gp = grid::gpar(col="grey", fontsize=10))
  grid::grid.text(coverage, x = 0.25, y = 0.5, rot = 90,  gp = grid::gpar(col="grey", fontsize=10))
  # use these variables is you need to calculate an odds ratio for some reason
  # but for consistencies of 1, the odds ratio will be infinite
  # pPathOutcome <- pathOutcome/(pathOutcome + pathNotOutcome)
  # pPathNotOutcome <- pathNotOutcome/(pathOutcome + pathNotOutcome)
  # pOthersOutcome <- othersOutcome/(othersOutcome + othersNotOutcome)
  # pOthersNotOutcome <- othersNotOutcome/(othersOutcome + othersNotOutcome)
  # all done drawing, back to the base viewport
  grid::popViewport(0)
}
