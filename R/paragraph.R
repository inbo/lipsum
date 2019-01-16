#' return lipsum paragraphs
#' @param id the id of the paragraph
#' @param n the number of random paragraphs. Only used when `id` is not given.
#' @param words an optional prefered number of words. Only used when `ìd` is not given.
#' @param verbose print the paragraphs
#' @return an invisible vector of paragraphs
#' @export
#' @importFrom stats aggregate
#' @examples
#' paragraph()
#' paragraph(n = 2)
#' paragraph(10)
#' paragraph(c(7, 1))
#' z <- paragraph(1, verbose = FALSE)
#' cat(z)
#' paragraph(words = 10, n = 2)
paragraph <- function(id, words, n = 1, verbose = TRUE) {
  if (missing(id)) {
    if (missing(words)) {
      id <- sample(max(lipsum$paragraph), size = n)
    } else {
      stopifnot(
        is.numeric(words),
        length(words) == 1
      )
      available <- aggregate(words ~ paragraph, lipsum, sum)
      id <- sample(
        available$paragraph,
        size = n,
        replace = FALSE,
        prob = 1 / (1 + abs(available$words - words) ^ 2)
      )
    }
    return(paragraph(id = id, verbose = verbose))
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
  selection <- selection[as.character(id), ]
  if (isTRUE(verbose)) {
    cat(selection$sentence, sep = "\n\n")
  }
  return(invisible(selection$sentence))
}
