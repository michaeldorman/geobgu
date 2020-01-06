#' Determine UTM zone
#'
#' The function accepts a vector layer (\code{sf}, \code{sgc} or \code{sfg}) and returns the EPSG code of the UTM zone according to the centroid of the bounding box.
#'
#' @param x An \code{sf}, \code{sgc} or \code{sfg} object
#'
#' @return EPSG code (\code{numeric}) which can be passed to other functions such as \code{st_transform}
#'
#' @export
#'
#' @examples
#' zone(cities[cities$name == "Boston MA", ])

zone = function(x) {

  # Verify that CRS not 'NA'
  stopifnot(!is.na(sf::st_crs(x)))

  # Get lon-lat of centroid
  x = sf::st_bbox(x)
  x = sf::st_as_sfc(x)
  oldw = getOption("warn")
  options(warn = -1)
  x = sf::st_centroid(x)
  options(warn = oldw)
  x = sf::st_transform(x, 4326)
  longitude = sf::st_coordinates(x)[, 1]
  latitude = sf::st_coordinates(x)[, 1]

  # Determine UTM zone
  zone = floor((longitude + 180) / 6) + 1
  if(nchar(zone) == 1) zone = paste0("o", zone)
  if(latitude > 0) epsg = paste0("326", zone) else epsg = paste0("327", zone)
  epsg = as.numeric(epsg)

  # Return
  return(epsg)

}

