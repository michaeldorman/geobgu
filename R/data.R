#' US states
#'
#' Polygonal layer with US states
#'
#' @format An \code{sf} layer of type \code{POLYGON}
#' @examples
#' data(states)
#'
#' @importFrom stats complete.cases
#' @importFrom stats predict
#' @import sf stars

"states"

#' US cities
#'
#' Point layer with US cities
#'
#' @format An \code{sf} layer of type \code{POINT}
#' @examples
#' data(cities)

"cities"

#' 7-Parameter ITM projection
#'
#' Proj string of the 7-Parameter ITM projection
#'
#' @format \code{character}
#' @references \url{https://www.systematics.co.il/wp-content/uploads/ITM2WGS84_Transformation1.pdf}
#' @examples
#' data(itm7)

"itm7"

#' Small Digital Elevation Model
#'
#' A \code{stars} object representing a 13*11 Digital Elevation Model (DEM)
#'
#' @format A \code{stars} object with 1 attribute:
#' \describe{
#'   \item{elevation}{Elevation above sea level, in meters}
#' }

"dem"

