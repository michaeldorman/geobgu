# Source
# http://rpubs.com/edzer/6767

setClass("igraph")
setClass("SpatialLines")
setClass(
  "SpatialLinesNetwork",
  representation(
    sl = "SpatialLines",
    g = "igraph",
    nb = "list"
  ),
  validity = function(object) {
    stopifnot(length(object@sl) == length(E(object@g)))
    stopifnot(length(object@nb) == length(V(object@g)))
  }
)

#' Title
#'
#' @param sl
#'
#' @return sln
#' @export
#'
#' @examples
#'
SpatialLinesNetwork = function(sl) {
  stopifnot(is(sl, "SpatialLines"))
  if (!is(sl, "SpatialLinesDataFrame"))
    sl = new(
      "SpatialLinesDataFrame",
      sl,
      data = data.frame(id = 1:length(sl))
    )
  if (!all(sapply(sl@lines, length) == 1))
    stop("SpatialLines is not simple: each Lines element should have only a single Line")
  startEndPoints = function(x) {
    firstLast = function(L) {
      cc = coordinates(L)[[1]]
      rbind(cc[1, ], cc[nrow(cc), ])
    }
    do.call(rbind, lapply(x@lines, firstLast))
  }
  s = startEndPoints(sl)
  zd = zerodist(SpatialPoints(s))
  pts = 1:nrow(s)

  # the following can't be done vector-wise, there is a progressive effect:
  if (nrow(zd) > 0) {
    for (i in 1:nrow(zd)) pts[zd[i, 2]] = pts[zd[i, 1]]
  }
  stopifnot(identical(s, s[pts, ]))

  # map to 1:length(unique(pts))
  pts0 = match(pts, unique(pts))
  node = rep(1:length(sl), each = 2)
  nb = lapply(1:length(unique(pts)), function(x) node[which(pts0 == x)])
  g = graph(pts0, directed = FALSE)  # edges
  nodes = s[unique(pts), ]
  g$x = nodes[, 1]  # x-coordinate vertex
  g$y = nodes[, 2]  # y-coordinate vertex
  g$n = as.vector(table(pts0))  # nr of edges
  # line lengths:
  sl$length = sapply(sl@lines, function(x) LineLength(x@Lines[[1]]))
  E(g)$weight = sl$length
  # create list with vertices, starting/stopping for each edge?  add for
  # each SpatialLines, the start and stop vertex
  pts2 = matrix(pts0, ncol = 2, byrow = TRUE)
  sl$start = pts2[, 1]
  sl$end = pts2[, 2]
  new("SpatialLinesNetwork", sl = sl, g = g, nb = nb)
}

#' Title
#'
#' @param sl
#'
#' @return sln
#' @export
#'
#' @examples
#'
sp.get.shortest.path <- function(sln, from, to, fromColumn = "start", toColumn = "end") {

  # if columns with start and end points are given, find matching between name
  # of node and ID of node
  if (fromColumn != "start" || toColumn != "end") {
    data <- sln@sl@data
    fromID <- c(data$start[data[fromColumn] == from], data$end[data[toColumn] ==
                                                                 from])[1]
    toID <- c(data$start[data[fromColumn] == to], data$end[data[toColumn] ==
                                                             to])[1]
  } else {
    fromID <- from
    toID <- to
  }

  # calculate shortest path, return both sequences of nodes and edges, and add
  # names of start and and point to the result
  sp = get.shortest.paths(sln@g, fromID, toID, output = "both")
  sp$from <- from
  sp$to <- to
  return(sp)
}

#' Title
#'
#' @param sl
#'
#' @return sln
#' @export
#'
#' @examples
#'
sp2sl <- function(sln, sp) {
  line.sequence <- unlist(sp$epath)
  return(sln@sl[line.sequence, ])
}

# # accepts: sln = SpatialLinesNetwork, sp = result of sp.get.shortest.path,
# # zoom = plotting can be zoomed to bounding box of shortest path
# plot.sp <- function(sln, sp, zoom = FALSE) {
#
#   # calculate shortest path and return it as a SpatialLines object
#   sp.path <- sp2sl(sln, sp)
#
#   # extend of plotting area
#   if (zoom) {
#     bb <- bbox(sp.path)
#   } else {
#     bb <- bbox(sln@sl)
#   }
#
#   # plotting
#   plot(sln@sl, xlim = bb[1, ], ylim = bb[2, ], col = "grey", axes = TRUE)
#   plot(sp.path, col = "orange", lwd = 2, add = T)
#   points(sln@g$x, sln@g$y, col = "grey", pch = 16, cex = 1.2)
#   points(sln@g$x[unlist(sp$vpath)], sln@g$y[unlist(sp$vpath)], col = "red",
#          cex = 2)
#
#   # additional text
#   start <- unlist(sp$vpath)[1]
#   end <- rev(unlist(sp$vpath))[1]
#   text(sln@g$x[c(start, end)], sln@g$y[c(start, end)], c("s", "e"))
#   title(paste("Shortest path from", sp$from, "to", sp$to))
#
# }
