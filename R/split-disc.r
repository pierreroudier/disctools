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
#' @importFrom rgeos gBuffer gIntersection
#' @importFrom sp coordinates SpatialPoints Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
#' 
#' @export
#' 
#' @author Pierre Roudier
#' 
#' @examples 
#' pt <- sp::SpatialPoints(data.frame(x = 1000000 * runif(1), y = 1000000 * runif(1)))
#' splits <- split_disc(center = pt, radius = 100, splits = 50)
#' 
#' plot(splits, col = "lightgrey")
#' plot(pt, add = TRUE, pch = "+", col = 2)
#' 

split_disc <- function(center, width, splits) {
  
  # This code does not work for n = 2
  if (splits < 3) stop("Splits should be more than 2.")
  
  disc <- gBuffer(center, width = width, quadsegs = 20)
  
  # Create the vector of angles (in radians)
  alpha <- seq(0, 2 * pi, length.out = splits + 1)
  
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
  split_pts <- SpatialPoints(data.frame(x, y))
  
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
  gIntersection(sp_pols, disc, byid = TRUE)
}