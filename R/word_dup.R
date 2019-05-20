#' Remove duplicated words from a vector of strings
#' 
#' Given a (nested) list of descriptions, remove the duplicated words.
#'
#' @param desc nested list of descriptions
#' @return desc but duplicated words removed
worddup <- function(desc) {

  out <- map(desc, function(w) 
             map(w, function(x) 
                 map(x, function(y) {
                       w <- !duplicated(y)
                       y <- y[w]
                       y})))

  out
}

