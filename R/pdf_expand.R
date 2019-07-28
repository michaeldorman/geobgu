#' Expand PDF page size
#'
#' Expand the page size of a PDF file to include all of its contents. Requires \code{inkscape} command-line utility.
#'
#' The function modifies an existing PDF file!
#'
#' @param filename A PDF file name or file path
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pdf_expand(filename = "~/test.pdf")
#' }

pdf_expand = function(filename) {
  input = normalizePath(filename)
  tmp_svg = gsub("\\.pdf$", "\\.svg", input)
  system(paste0('inkscape -z -f ', filename, ' -l ', tmp_svg))
  system(paste0("inkscape --verb=FitCanvasToDrawing --verb=FileSave --verb=FileQuit ", tmp_svg))
  system(paste0("inkscape ", tmp_svg, " --export-pdf=", input))
  system(paste0("rm ", tmp_svg))
}

