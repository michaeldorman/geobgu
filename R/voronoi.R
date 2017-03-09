#'	Voronoi polygons within given extent
#'
#' Create voronoi polygons, given points and extent
#'
#'	@param pnt points (locations) of the queries
#'	@param pol layer from which the geometries or attributes are queried
#'	@return	Voronoi polygons (\code{SpatialPolygons}).
#'	@examples
#' library(rgdal)
#' data(states)
#' data(cities)
#' usAtlas = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
#' states = spTransform(states, usAtlas)
#' cities = spTransform(cities, usAtlas)
#' cities = cities[states, ]
#' plot(states)
#' plot(cities, add = TRUE)
#' v = voronoi(cities, states)
#' plot(v, add = TRUE, border = "red")
#' @export
#' @references
#' http://stackoverflow.com/questions/12156475/combine-voronoi-polygons-and-maps

voronoi = function(pnt, pol) {
  if (.hasSlot(x, 'coords')) {
    crds = pnt@coords
  } else crds = pnt
  bb = bbox(pol)
  rw = as.numeric(t(bb))
  z = deldir::deldir(crds[,1], crds[,2],rw=rw)
  w = deldir::tile.list(z)
  polys = vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP = SpatialPolygons(polys)

  voronoi = SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1],
                                                          y=crds[,2], row.names=sapply(slot(SP, 'polygons'),
                                                                                       function(x) slot(x, 'ID'))))

  return(voronoi)

}
