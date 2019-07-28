#' Split PDF to separate files
#'
#' Split a PDF file to separate files, one file per page. Requires \code{pdftk} command-line utility.
#'
#' The function create new files with pattern \code{filename_page_%s.pdf} in the same location where \code{filename.pdf} is.
#'
#' @param filename A PDF file name or file path
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pdf_split(filename = "~/test.pdf")
#' }

pdf_split = function(filename) {

  # Normalize path
  input = normalizePath(filename)

  # Get number of pages in PDF
  pages = system(paste0("pdftk ", input, " dump_data | grep NumberOfPages"), intern = TRUE)
  pages = gsub("NumberOfPages: ", "", pages)
  pages = as.numeric(pages)

  # Extract pages to files
  for(i in 1:pages) {

    n = formatC(i, width = 3, flag = "0")
    output = gsub("\\.pdf$", paste0("\\_page_", i, ".pdf"), input)
    system(paste0("pdftk ", input, " cat ", i, " output ", output))

  }

}

