#'	Nearest neighbor spatial join
#'
#' Spatial join between two point layers based on nearest neighbors.
#'
#'	@param	x	geometry (locations) of the queries
#'	@param	y	layer from which the geometries or attributes are queried
#'	@return	If \code{y} is only geometry (\code{SpatialPoints}), a vector with the index of \code{y} for each geometry matching x. If \code{y} has attribute data (\code{SpatialPointsDataFrame}), attribute data are returned.
#'	@examples
#' library(rgeos)
#' data(states)
#' data(cities)
#' usAtlas = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
#' states = spTransform(states, usAtlas)
#' cities = spTransform(cities, usAtlas)
#' ctr = gCentroid(states, byid = TRUE)
#' # Plot
#' plot(cities)
#' plot(states, add = TRUE, border = "grey")
#' plot(ctr, add = TRUE, col = "red")
#' # Nearest neighbor spatial join
#' nn_state_ctr = overNNf(x = cities, y = ctr)
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
#'
#' @export

# Simple 'spatial only' join between two point layers according to 'nearest neighbor' criterion
overNNf = function(x, y) {

  if(
    class(x) %in% c("SpatialPoints", "SpatialPointsDataFrame") &
    class(y) %in% c("SpatialPoints", "SpatialPointsDataFrame")
    ) {
    stopifnot(identicalCRS(x, y))
    stopifnot(is.projected(x) & is.projected(y))
    x_coord = sp::coordinates(x)
    y_coord = sp::coordinates(y)
  } else {
    if(class(x) %in% c("SpatialPoints", "SpatialPointsDataFrame"))
      x_coord = sp::coordinates(x) else x_coord = as.matrix(x)
    if(class(y) %in% c("SpatialPoints", "SpatialPointsDataFrame"))
      y_coord = sp::coordinates(y) else y_coord = as.matrix(y)
  }

  # ID of nearest point in 'y' for each point in 'x'
  nn = RANN::nn2(data = y_coord, query = x_coord, k = 1)
  nn_ids = nn$nn.idx

  # Take attributes from 'y' for the specified IDs
  if(class(y) == "SpatialPointsDataFrame")
    y@data[nn_ids, , drop = FALSE] else
      nn_ids

}


