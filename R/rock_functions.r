# make lithology occurrence matrix
lith.matrix <- function(lit) {
  rock.desc <- unique(Reduce(c, llply(lit, function(x) x[, 1])))
  # make composition matrix
  # columns are rock type, rows are strat unit
  rock.matrix <- matrix(0, nrow = length(lit), ncol = length(rock.desc))
  colnames(rock.matrix) <- rock.desc
  rownames(rock.matrix) <- names(lit)
  for(ii in seq(length(lit))) {
    mm <- which(rock.desc %in% lit[[ii]][, 1])
    rock.matrix[ii, mm] <- lit[[ii]][, 2]
    rock.matrix[ii, mm]
  }
  rock.freq <- table(Reduce(c, llply(lit, function(x) x[, 1])))
  bcomp <- match(names(which.max(rock.freq)), colnames(rock.matrix))
  rock.matrix[, c(1, bcomp)] <- rock.matrix[, c(bcomp, 1)]
  rock.matrix
}

# replace words in lith desc with one word
wordrep <- function(dec, wordd) {
  out <- llply(dec, function(y) 
               llply(y, function(x) {
                       w <- x %in% wordd
                       x[w] <- wordd[1]
                       x}))
  #out <- llply(out, function(y) laply(y, function(x) paste0(x, collapse = ' ')))
  out
}

# remove individual words
wordrm <- function(dec, words) {
  aa <- dec
  bb <- llply(dec, function(y) llply(y, function(x) x %in% words))
  aa <- Map(function(a, b) Map(function(x, y) x[!y], a, b), 
                   aa , bb)
  aa
}


# remove duplicated words
worddup <- function(dec) {
  ndup <- llply(dec, function(y) llply(y, function(x) !duplicated(x)))
  out <- Map(function(a, b) Map(function(x, y) x[y], a, b), dec, ndup)
  out <- llply(out, function(x) 
                     laply(x, function(y) paste0(y, collapse = ' ')))
  out 
}
