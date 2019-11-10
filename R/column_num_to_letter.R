#' Find the corresponding column letter for a column number in Excel
#'
#' @param x A vector of Excel column numbers
#'
#' @return A vector of column letters
#'
#' @references
#' \url{https://stackoverflow.com/questions/34537243/convert-excel-column-names-to-numbers}
#'
#' @export
#'
#' @examples
#' column_num_to_letter(1)
#' column_num_to_letter(26)
#' column_num_to_letter(c(26, 27))
#' column_num_to_letter(53)
#' column_num_to_letter(703)
#' column_num_to_letter(1:26)

column_num_to_letter = function(x) {
  result = rep(NA, length(x))
  for(i in 1:length(x)) {
    n = x[i]
    if(!is.na(n)) {
      letters = ""
      while (n > 0) {
        r = (n - 1) %% 26  # remainder
        letters = paste0(intToUtf8(r + utf8ToInt("A")), letters)  # ascii
        n = (n - 1) %/% 26  # quotient
      }
      result[i] = letters
    }
  }
  result
}

