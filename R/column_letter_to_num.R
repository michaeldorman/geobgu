#' Find the corresponding column number for an Excel column letter
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
#' column_letter_to_num(NA)

column_letter_to_num = function(x) {
  result = rep(NA, length(x))
  for(i in 1:length(x)) {
    s = x[i]
    if(!is.na(s)) {
      s_upper = toupper(s)
      s_split = unlist(strsplit(s_upper, split = ""))
      s_number = sapply(s_split, function(x) {which(LETTERS == x)})
      numbers = 26^((length(s_number)-1):0)
      column_number = sum(s_number * numbers)
      result[i] = column_number
    }
  }
  result
}

