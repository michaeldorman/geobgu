#' Find the corresponding column number for an excel column letter
#'
#' @param x A vector of Excel column letters
#'
#' @return A vector of column numbers
#'
#' @references
#' \url{https://stackoverflow.com/questions/34537243/convert-excel-column-names-to-numbers}
#'
#' @export
#'
#' @examples
#' column_letter_to_num("A")
#' column_letter_to_num("Z")
#' column_letter_to_num(c("Z", "AA"))
#' column_letter_to_num("BA")
#' column_letter_to_num("AAA")
#' column_letter_to_num(LETTERS)

column_letter_to_num = function(x) {
  s_upper = toupper(x)
  s_split = unlist(strsplit(s_upper, split = ""))
  s_number = sapply(s_split, function(x) {which(LETTERS == x)})
  numbers = 26^((length(s_number)-1):0)
  column_number = sum(s_number * numbers)
  column_number
}
column_letter_to_num = Vectorize(column_letter_to_num)

