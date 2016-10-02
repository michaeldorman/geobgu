#'	Nearest neighbor spatial join
#'
#'  Spatial join between two point layers based on nearest neighbors.
#'
#'	@param	x	geometry (locations) of the queries
#'	@param	y	layer from which the geometries or attributes are queried
#'	@return	If \code{y} is only geometry (\code{SpatialPoints}), a vector with the index of \code{y} for each geometry matching x. If \code{y} has attribute data (\code{SpatialPointsDataFrame}), attribute data are returned.
#'	@examples
#'  library(maps)
#'  library(maptools)
#'  library(rgeos)
#'  # Get data
#'  cities = us.cities[us.cities$capital == 2, ]
#'  coordinates(cities) = ~ long + lat
#'  proj4string(cities) = "+proj=longlat +datum=WGS84"
#'  usa = map("state", fill = TRUE, plot = FALSE)
#'  IDs = sapply(strsplit(usa$names, ":"), function(x) x[1])
#'  usa = map2SpatialPolygons(usa, IDs=IDs, proj4string = CRS("+proj=longlat +datum=WGS84"))
#'  ctr = gCentroid(usa, byid = TRUE)

#'  # Plot
#'  plot(cities)
#'  plot(usa, add = TRUE, border = "grey")
#'  plot(ctr, add = TRUE, col = "red")

#'  # Nearest neighbor spatial join
#'  nn_state_ctr = overNN(cities, ctr)

#'  # Draw lines between each city and nearest state centroid
#'  for(i in 1:nrow(cities)) {
#'    plot(
#'      SpatialLines(
#'        list(Lines(
#'          list(Line(rbind(coordinates(cities[i,]), coordinates(ctr[nn_state_ctr[i],])))),
#'          ID = "a"
#'        )),
#'        proj4string = CRS(proj4string(ctr))
#'      ), add = TRUE
#'    )
#'  }

# Simple 'spatial only' join between two point layers according to 'nearest neighbor' criterion
overNN = function(x, y) {

  # Both objects need to be SpatialPoints or SpatialPointsDataFrame
  stopifnot(class(x) %in% c("SpatialPoints", "SpatialPointsDataFrame"))
  stopifnot(class(y) %in% c("SpatialPoints", "SpatialPointsDataFrame"))

  # Pairwise distance matrix
  dist_matrix = sp::spDists(x, y)

  # ID of nearest point in 'y' for each point in 'x'
  nn_ids = apply(dist_matrix, 1, which.min)

  # Take attributes from 'y' for the specified IDs
  if(class(y) == "SpatialPointsDataFrame") y@data[nn_ids, ] else nn_ids

}


