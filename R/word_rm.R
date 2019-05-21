#' Remove words from lithological description
#' 
#' Given a string, remove occurrences of certain words.
#'
#' @param desc string description
#' @param words vector of strings to be removed from desc
#' @return desc but with words removed
#' @export
wordrm <- function(desc, words) {
  temp <- 
    map(desc, function(a) 
        map(a, function(x) 
            map(x, function(n) {
                  w <- !(n %in% words)
                  n <- n[w]
                  n})))
  temp

}
