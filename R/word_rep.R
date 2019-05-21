#' Synonmize terms
#'
#' Replace words in lithological description with one word
#' @param desc description
#' @param wordd vector of synonymous words
#' @return description vector with synonyms replaced (no changes if no synonyms.)
#' @export
wordrep <- function(desc, wordd) {
  out <- map(desc, function(y) 
             map_chr(y, function(x) {
                   w <- x %in% wordd
                   x[w] <- wordd[1]
                   x}))

  out
}

