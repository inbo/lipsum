#' return lipsum paragraphs
#' @param id the id of the paragraph
#' @param n the number of random paragraphs. Only used when `id` is not given.
#' @inheritParams sentence
#' @return an string of paragraphs. The paragraphs are separated by a double newline character.
#' @export
#' @importFrom stats aggregate
#' @examples
#' paragraph()
#' paragraph(n = 2)
#' paragraph(10)
#' paragraph(c(7, 1))
#' paragraph(words = 12, n = 2)
#' paragraph(characters = 90)
paragraph <- function(id, words, characters, n = 1) {
  if (missing(id)) {
    if (missing(words)) {
      id <- unique(lipsum$paragraph)
    } else if (missing(characters)) {
      stopifnot(
        is.numeric(words),
        length(words) == 1
      )
      available <- aggregate(words ~ paragraph, lipsum, sum)
      delta <- abs(available$words - words)
      extra <- 0
      while (length(which(delta <= min(delta) + extra)) < n) {
        extra <- extra + 1
      }
      id <- available$paragraph[delta <= min(delta) + extra]
    } else {
      stopifnot(
        is.numeric(characters),
        length(words) == 1
      )
      available <- aggregate(characters ~ paragraph, lipsum, sum)
      delta <- abs(available$characters - characters)
      extra <- 0
      while (length(which(delta <= min(delta) + extra)) < n) {
        extra <- extra + 1
      }
      id <- available$paragraph[delta <= min(delta) + extra]
    }
    if (length(id) > 1) {
      id <- sample(id, size = n)
    }
    return(paragraph(id = id))
  }
  stopifnot(
    is.numeric(id),
    length(id) >= 1,
    all(id >= 1),
    all(id <= max(lipsum$paragraph))
  )
  selection <- lipsum[lipsum$paragraph %in% id, ]
  selection <- aggregate(
    sentence ~ paragraph, data = selection, FUN = paste, collapse = " "
  )
  rownames(selection) <- selection$paragraph
  return(paste(selection[as.character(id), "sentence"], collapse = "\n\n"))
}
