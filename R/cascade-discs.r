#' @title cascade_disks
#' @description Creates SpatialPolygonDataFrame for a suite of imbriqued disks
#' @aliases cascade_disks
#' 
#' @usage cascade_disks(center, spacings, quadsegs)
#' @param center SpatialPoints or SpatialPointsDataFrame object as defined in package sp
#' @param spacings vector giving the width of each ring
#' @param quadsegs Number of line segments to use to approximate a quarter circle.
#' @return a SpatialPointsDataFrame with a unqiue attribute \code{id}
#' 
#' @import sp 
#' @importFrom rgeos gBuffer gDifference
#' 
#' @export
#' 
#' @author Pierre Roudier
#' 
#' @examples
#' library(sp)
#' pt <- SpatialPoints(data.frame(x = 1000000 * runif(1), y = 1000000 * runif(1)))
#' res <- cascade_disks(center = pt, spacings = 10)
#' plot(res, col = "lightgrey") 
#' plot(pt, add = TRUE, col = 2)
#' 
cascade_disks <- function(center, spacings, quadsegs = 20) {

  res <- list()

  res[[1]] <- gBuffer(center, width = spacings[1], quadsegs = quadsegs)

  if (length(spacings) > 1) {
    for (i in 2:length(spacings)) {

      # Create current disk
      d <- gBuffer(center, width = sum(spacings[1:i]), quadsegs = quadsegs)
      # Create previous disk
      dm1 <- gBuffer(center, width = sum(spacings[1:(i - 1)]), quadsegs = quadsegs)
      # Compute difference and give unique ID
      res[[i]] <- spChFIDs(gDifference(d, dm1), as.character(i))
    }
  }

  SpatialPolygonsDataFrame(do.call(rbind, res), data = data.frame(id = 1:length(spacings)), match.ID = FALSE)
}
