#' Downloads and unzips GTFS files
#'
#' @param url URL of the GTFS zip file
#' @param dir Path to destination directory
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#' \dontrun{
#' url = "ftp://199.203.58.18/israel-public-transportation.zip"
#' dir = "/home/michael/Downloads/gtfs_test"
#' gtfs_download(url, dir)
#' list.files(dir)
#' }

gtfs_download = function(url, dir) {
  
  # STOP if 'dir' doesn't exist!
  stopifnot(dir.exists(dir))

  # STOP if dir contains anything other than '*.txt' and 'zzz_source'
  stopifnot(all(grepl("*.txt$|zzz_source", list.files(path = dir))))

  # Set filename
  filename = gsub(".*/", "", url)
  
  # Download
  utils::download.file(url, file.path(dir, filename))
  
  # Unzip
  utils::unzip(file.path(dir, filename), exdir = dir)
  
  # Delete ZIP file
  file.remove(file.path(dir, filename))

  # Write metadata
  metadata = c(url, as.character(Sys.time(), usetz = TRUE))
  writeLines(metadata, file.path(dir, "zzz_source"))

}

