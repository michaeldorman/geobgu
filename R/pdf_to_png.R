#' Convert PDF to PNG
#'
#' Convert a PDF file to a PNG file. Requires \code{inkscape} command-line utility.
#'
#' The function creates an PNG file at the same location where the PDF is.
#'
#' @param filename A PDF file name or file path
#' @param dpi Required DPI
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pdf_to_png(filename = "~/test.pdf")
#' }

pdf_to_png = function(filename, dpi = 300) {
  input = normalizePath(filename)
  output = gsub("\\.pdf$", "\\.png", input)
  expr = paste0("inkscape ", input," --export-dpi=", dpi, " --export-area-drawing --export-type=png -o ", output)
  system(expr)
}

