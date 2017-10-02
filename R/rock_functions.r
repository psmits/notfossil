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
  bcomp <- match(names(which.max(rock.freq)), colnames(rock.matrix))
  rock.matrix[, c(1, bcomp)] <- rock.matrix[, c(bcomp, 1)]
  rock.matrix
}

