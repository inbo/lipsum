#' return lipsum paragraphs
#' @param id the id of the paragraph
#' @param n the number of random paragraphs. Only used when `id` is not given.
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
paragraph <- function(id, n = 1, verbose = TRUE) {
  if (missing(id)) {
    return(
      paragraph(
        id = sample(max(lipsum$paragraph), size = n),
        verbose = verbose
      )
    )
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
