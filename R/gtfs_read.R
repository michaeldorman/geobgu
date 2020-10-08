#' Read GTFS files
#'
#' @param dir Path to directory with \code{*.txt} GTFS files
#'
#' @return \code{list} where each element is a \code{data.frame} created from a GTFS \code{*.txt} file
#'
#' @export
#'
#' @examples
#' dir = "~/Downloads/gtfs"
#' dat = gtfs_read(dir)
#' names(dat)
#' lapply(dat, head)

gtfs_read = function(dir) {

  files = list.files(path = dir, full.names = TRUE)
  result = list()

  for(i in files) {
    print(i)
    dat = utils::read.csv(i, stringsAsFactors = FALSE, quote = "")
    n = i
    n = gsub("\\..*", "", n)
    n = gsub(".*/", "", n)
    result[[n]] = dat
  }

  return(result)

}

