#' Function to make a circular weights matrix of given radius and resolution
#'
#' @param radius 
#' @param res 
#'
#' @return
#' 
#' @note 
#' NB radius must me an even multiple of res!
#' 
#' @references 
#' https://scrogster.wordpress.com/2012/10/05/applying-a-circular-moving-window-filter-to-raster-data-in-r/
#' 
#' @export
#'
#' @examples
#' m = make_circ_filter(400, 30) # filer for 400-m buffer on a 30-m resolution raster
#' image(m, asp = 1)

makeCircFilter = function(radius, res){
  circ_filter = matrix(NA, nrow = 1 + (2 * radius / res), ncol = 1 + (2 * radius / res))
  dimnames(circ_filter)[[1]] = seq(-radius, radius, by = res)
  dimnames(circ_filter)[[2]] = seq(-radius, radius, by = res)
  sweeper = function(mat) {
    for(row in 1:nrow(mat)) {
      for(col in 1:ncol(mat)) {
        dist = sqrt((as.numeric(dimnames(mat)[[1]])[row])^2 +
                     (as.numeric(dimnames(mat)[[1]])[col])^2)
        if(dist<=radius) {mat[row, col] = 1}
      }
    }
    return(mat)
  }
  out = sweeper(circ_filter)
  return(out)
}