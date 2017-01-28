#'	Nearest neighbor spatial join
#'
#' Spatial join between two point layers based on nearest neighbors.
#'
#'	@param	x	geometry (locations) of the queries; A matrix of n-D points with row denoting points, first column x/longitude, second column y/latitude, or a \code{Spatial} object that has a coordinates method
#'	@param	y	layer from which the geometries or attributes are queried; A matrix of n-D points with row denoting points, first column x/longitude, second column y/latitude, or a \code{Spatial} object that has a coordinates method
#'	@return	If \code{y} is only geometry (\code{SpatialPoints}), a vector with the index of \code{y} for each geometry matching x. If \code{y} has attribute data (\code{SpatialPointsDataFrame}), attribute data are returned.
#'	@examples
#' library(rgeos)
#' data(states)
#' data(cities)
#' ctr = gCentroid(states, byid = TRUE)
#' # Plot
#' plot(cities)
#' plot(states, add = TRUE, border = "grey")
#' plot(ctr, add = TRUE, col = "red")
#' # Nearest neighbor spatial join
#' nn_state_ctr = overNN(x = cities, y = ctr)
#' # Draw lines between each city and nearest state centroid
#' for(i in 1:nrow(cities)) {
#' plot(
#'   sp::SpatialLines(
#'   list(sp::Lines(
#'   list(sp::Line(rbind(
#'   sp::coordinates(cities[i,]),
#'   sp::coordinates(ctr[nn_state_ctr[i],])))),
#'   ID = "a"
#'   )),
#'   proj4string = sp::CRS(sp::proj4string(ctr))
#'   ), add = TRUE
#' )
#' }
#' @export

# Simple 'spatial only' join between two point layers according to 'nearest neighbor' criterion
overNN = function(x, y) {

  # Pairwise distance matrix
  dist_matrix = sp::spDists(x, y)

  # ID of nearest point in 'y' for each point in 'x'
  nn_ids = apply(dist_matrix, 1, which.min)

  # Take attributes from 'y' for the specified IDs
  if(class(y) == "SpatialPointsDataFrame") y@data[nn_ids, ] else nn_ids

}


