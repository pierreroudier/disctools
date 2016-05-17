#' @title split_disc
#' @description Splits a disc into wedges
#' @aliases split_disc
#' 
#' @usage split_disc(center, width, splits)
#' @param center a object of class \code{SpatialPoints} or \code{SpatialPointsDataFrame}
#' @param width radius of the disc to split into wedges
#' @param splits number of splits/wedges
#' @return An object of class \code{SpatialPolygonsDataFrame}
#' 
#' @import sp
#' @importFrom rgeos gBuffer gIntersection
#' 
#' @export
#' 
#' @author Pierre Roudier
#' 
#' @examples 
#' library(sp)
#' pt <- SpatialPoints(data.frame(x = 1000000 * runif(1), y = 1000000 * runif(1)))
#' splits <- split_disc(center = pt, width = 100, splits = 50)
#' 
#' plot(splits, col = "lightgrey")
#' plot(pt, add = TRUE, pch = "+", col = 2)
#' 
split_disc <- function(center, width, splits) {
  
  # This code does not work for n = 2
  if (splits < 3) stop("Splits should be more than 2.")
  
  disc <- gBuffer(center, width = width, quadsegs = 20)
  
  # Create the vector of angles (in radians) starting at pi / 2 (vertical, up)
  alpha <- seq(pi / 2, 2 * pi + pi / 2, length.out = splits + 1)
  
  # First we create a collection of points that will generate the
  # lines that go from the center of the disc
  #
  # Little trigonometry reminder:
  #   sin(alpha) = y / radius
  #   cos(alpha) = x /radius
  
  # We compute a fudge factor so that the triangle coorresponding to each wedge
  # is outside of the disk (as opposed to on the circle)
  x_mid_angle <- cos(alpha[2] / 2) * width
  fudge <- 1.5 * tan(alpha[2] / 2) * x_mid_angle
  
  # Compute the X and Y and create the split points data.frame
  x <- coordinates(center)[, 1] + cos(alpha) * (width + fudge) 
  y <- coordinates(center)[, 2] + sin(alpha) * (width + fudge)
  
  split_pts <- SpatialPoints(data.frame(x = x, y = y))
  
  # Now create polygons from the center of the disk and
  # the split points
  coords_split <- coordinates(split_pts)
  
  pols <- lapply(2:nrow(coords_split), function(i) {
    pt1 <- coords_split[i - 1,]
    pt2 <- coordinates(center)
    pt3 <- coords_split[i,]
    pt4 <- coords_split[i - 1,]
    
    Polygons( list( Polygon(rbind(pt1, pt2, pt3, pt4)) ), ID = as.character(i - 1) ) 
  })
  
  sp_pols <- SpatialPolygons(pols)
  
  # Crop with original circle
  res <- gIntersection(sp_pols, disc, byid = TRUE)
  
  # Adds IDs
  SpatialPolygonsDataFrame(res, data = data.frame(id = 1:length(res)), match.ID = FALSE)
}

#' @import sp
 NULL
