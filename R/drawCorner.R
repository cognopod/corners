#' drawCorner
#'
#' drawCorner is a function that is used by fourCorners to draw one of the leaves.
#'
#' @param sector is a predefined viewport
#' @param nElements is the number of elements to plot
#' @param drawTop is half of the definition of the corner of the viewport to draw the elements in (1 = top)
#' @param drawRight is the other half of the definition of the corner (1 = right)
#' @param leafColor is the color for that particular leaf
#' @param offSet is the distance between elements
#' @param rElement is the size of the element
#' @param unique draws with white fill so that a fifth call can draw unique elements
#' @param shape draws shape (either "circle" or "square")
#' @keywords internal
#'
drawCorner <- function(sector, nElements, drawTop, drawRight, leafColor, offset = 0.028, rElement = 0.02, unique = FALSE, shape = "square") {
  # set fill color on unique
  if (unique == FALSE) {
    fillColor = leafColor
  } else {
    fillColor = "white"
  }
  # check shape
  if (shape != "square" & shape != "circle") {
    shape = "square"
  }
  # load the grid library (this should change to :: notation at some point)
  # library(grid)
  # change the drawing focus to sector
  grid::pushViewport(sector)
  # figure out the next square less than the number of elements
  thisSquare <- as.integer(sqrt(nElements))
  # draw the square of elements if there are elements
  if (nElements > 0) {
    for (i in 1:thisSquare) {
      for (j in 1:thisSquare) {
        if (shape == "circle") {
          grid::grid.circle(x = abs(drawRight - 2*offset*j), y = abs(drawTop - 2*offset*i), r = rElement, gp = grid::gpar(fill=fillColor, col = leafColor))
        } else {
          grid::grid.rect(x = abs(drawRight - 2*offset*j), y = abs(drawTop - 2*offset*i), width = 2*offset, height = 2*offset, gp = grid::gpar(fill=fillColor, col = leafColor))
        }
      }
    }
  }
  # draw the extra elements that were beyond the square
  # one on each side alternating
  if (nElements != thisSquare^2 && nElements > 0) {
    for (i in 1:(nElements - thisSquare^2)) {
      if (i %% 2) {
        if (shape == "circle") {
          grid:: grid.circle(x = abs(drawRight - 2*offset*as.integer(i/2+0.5)), y = abs(drawTop - 2*offset*(thisSquare+1)), r = rElement, gp = grid::gpar(fill=fillColor, col = leafColor))
        } else {
          grid:: grid.rect(x = abs(drawRight - 2*offset*as.integer(i/2+0.5)), y = abs(drawTop - 2*offset*(thisSquare+1)), width = 2*offset, height = 2*offset, gp = grid::gpar(fill=fillColor, col = leafColor))
        }
      } else {
        if (shape == "circle") {
          grid::grid.circle(x = abs(drawRight - 2*offset*(thisSquare+1)), y = abs(drawTop - 2*offset*as.integer(i/2+0.5)), r = rElement, gp = grid::gpar(fill=fillColor, col = leafColor))
        } else {
          grid::grid.rect(x = abs(drawRight - 2*offset*(thisSquare+1)), y = abs(drawTop - 2*offset*as.integer(i/2+0.5)),width = 2*offset, height = 2*offset, gp = grid::gpar(fill=fillColor, col = leafColor))
        }
      }
    }
  }
  # move back to the base viewport
  grid::upViewport()
}
