#' Return sentences from the lipsum corpus
#'
#' A single paragraph with the sentences will be displayed
#' @param id the number of the sentence
#' @param n the number of random sentences. Only used when `id` is not given.
#' @param words an optional prefered number of words. Only used when `ìd` is not given.
#' @param characters an optional prefered number of characters. Only used when `ìd` and `words` are not given.
#' @return a string of sentences
#' @export
#' @examples
#' sentence()
#' sentence(1)
#' sentence(2:3)
#' sentence(words = 5)
sentence <- function(id, n = 1, words, characters) {
  if (missing(id)) {
    if (missing(words)) {
      id <- seq_along(lipsum$sentence)
    } else if (missing(characters)) {
      stopifnot(
        is.numeric(words),
        length(words) == 1
      )
      delta <- abs(lipsum$words - words)
      extra <- 0
      while (length(which(delta <= min(delta) + extra)) < n) {
        extra <- extra + 1
      }
      id <- which(delta == min(delta))
    } else {
      stopifnot(
        is.numeric(characters),
        length(words) == 1
      )
      delta <- abs(lipsum$characters - characters)
      extra <- 0
      while (length(which(delta <= min(delta) + extra)) < n) {
        extra <- extra + 1
      }
      id <- which(delta == min(delta))
    }
    if (length(id) > 1) {
      id <- sample(id, size = n)
    }
    return(sentence(id = id))
  }
  stopifnot(
    is.numeric(id),
    length(id) >= 1,
    all(id >= 1),
    all(id <= nrow(lipsum))
  )
  return(paste(lipsum$sentence[id], collapes = " "))
}