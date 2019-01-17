#' Return sentences from the lipsum corpus
#'
#' A single paragraph with the sentences will be displayed
#' @param id the number of the sentence
#' @param n the number of random sentences. Only used when `id` is not given.
#' @param words an optional prefered number of words. Only used when `ìd` is not given.
#' @param characters an optional prefered number of characters. Only used when `ìd` and `words` are not given.
#' @param verbose print the sentences
#' @return an invisible vector of sentences
#' @export
#' @examples
#' sentence()
#' sentence(1)
#' z <- sentence(1, verbose = FALSE)
#' cat(z)
#' sentence(2:3)
#' sentence(words = 5)
sentence <- function(id, n = 1, words, characters, verbose = TRUE) {
  if (missing(id)) {
    if (missing(words)) {
      id <- sample(nrow(lipsum), size = n, replace = FALSE)
    } else if (missing(characters)) {
      delta <- abs(lipsum$words - words)
      id <- sample(
        which(delta == min(delta)),
        size = n,
        replace = FALSE
      )
    } else {
      delta <- abs(lipsum$characters - characters)
      id <- sample(
        which(delta == min(delta)),
        size = n,
        replace = FALSE
      )
    }
    return(sentence(id = id, verbose = verbose))
  }
  stopifnot(
    is.numeric(id),
    length(id) >= 1,
    all(id >= 1),
    all(id <= nrow(lipsum))
  )
  selection <- lipsum$sentence[id]
  if (isTRUE(verbose)) {
    cat(selection, sep = " ")
  }
  return(invisible(selection))
}