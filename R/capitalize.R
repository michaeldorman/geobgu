#' Simple word capitalization
#'
#' @param x A character vector
#'
#' @return A capitalized character vector
#'
#' @references
#' \url{https://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string}
#'
#' @export
#'
#' @examples
#' name = c("zip code", "state", "final count")
#' capitalize(name)

capitalize = function(x) {
  sapply(x, function(x) {
    s = strsplit(x, " ")[[1]]
    paste(
      toupper(substring(s, 1, 1)),
      tolower(substring(s, 2)),
      sep = "",
      collapse = " "
      )
  })
}

