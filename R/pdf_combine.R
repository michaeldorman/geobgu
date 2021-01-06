#' Combines separate PDF files into one page
#'
#' Combines PDF files to into one file/page. Requires \code{pdfjam} command-line utility.
#'
#' @param input A vector of PDF file names or paths
#' @param nrow Number of rows in output
#' @param ncol Number of columns in output
#' @param height Output file height, in inches (default is \code{11.7})
#' @param width Output file width, in inches (default is \code{8.3})
#' @param output The name of the combined PDF file name
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#' \dontrun{
#' }

pdf_combine = function(input, nrow, ncol, height = 11.7, width = 8.3, output = "combined.pdf") {

  # Collapse input file names
  input = paste0(input, collapse = "' '")

  # Combine
  x = sprintf("pdfjam '%s' --papersize '{%sin,%sin}' --nup %sx%s --outfile %s", input, width, height, ncol, nrow, output)
  system(x)

}

