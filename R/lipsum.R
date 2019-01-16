#' The lipsum corpus
#'
#' A data frame with sentences from the lipsum corpuse
#'
#' @format A data frame with 3189 rows and 4 variables:
#' \describe{
#'   \item{paragraph}{an integer id of the paragraph}
#'   \item{characters}{the number of characters in the sentence}
#'   \item{words}{the number of words in the sentence}
#'   \item{sentence}{the actual sentence}
#' }
#' @name lipsum
#' @keywords data
#' @docType data
globalVariables("lipsum", "lipsum")
