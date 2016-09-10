
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

# Example:

# library(maps)
# library(maptools)
# library(rgeos)
# 
# cities = us.cities[us.cities$capital == 2, ]
# coordinates(cities) = ~ long + lat
# proj4string(cities) = "+proj=longlat +datum=WGS84"
# 
# usa = map("state", fill = TRUE, plot = FALSE)
# IDs = sapply(strsplit(usa$names, ":"), function(x) x[1])
# usa = map2SpatialPolygons(usa, IDs=IDs, proj4string = CRS("+proj=longlat +datum=WGS84"))
# ctr = gCentroid(usa, byid = TRUE)
# 
# plot(ctr)
# plot(cities, add = TRUE, col = "red")
# 
# nn_state_ctr = over_nn(cities, ctr)
# 
# text(cities, row.names(ctr)[nn_state_ctr])
# 
# lines(ctr[1,], cities[1,])

# library(aodlur)
# library(sp)
#
# data(cities)
# data(county)
#
# over_nn(x = cities, y = county)
#
# cities@data = cbind(cities@data, over_nn(cities, county))
# cities@data

