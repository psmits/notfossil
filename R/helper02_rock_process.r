strict.lithology <- function(strat) {
  # make into a statement and a percentage
  lit <- laply(strat$lith, function(x) str_split(x, '\\|'))
  lit <- llply(lit, str_trim)
  lit <- llply(lit, function(x) str_split(x, '  ~ ', simplify = TRUE))
  names(lit) <- strat$unit_id
  
  # get rid of bad information
  bad <- c('igneous', 'volcanic', 'metamorphic', 'chemical',
           'anhydrite', 'evaporite', 'halite')
  dec <- llply(lit, function(x) str_split(x[, 1], ' '))
  torm <- llply(dec, function(y) laply(y, function(x) any(bad %in% x)))
  lit <- Map(function(x, y) x[!y, , drop = FALSE], x = lit, y = torm)
  lit <- lit[laply(lit, nrow) > 0]
  
  #synonymie certain words
  dec <- llply(lit, function(x) str_split(x[, 1], ' '))
  dup.words <- list(c('green', 'greenish'), 
                    c('limestone', 'lime'),
                    c('red', 'reddish'), 
                    c('blue', 'bluish'),
                    c('brown', 'brownish'),
                    c('yellow', 'yellowish'),
                    c('thin', 'thinly'),
                    c('thick', 'thickly'),
                    c('bedded', 'laminated'),
                    c('dolomite', 'dolomitic'),
                    c('chert', 'cherty'),
                    c('sandstone', 'sand', 'sandy'),
                    c('mudstone', 'mud'),
                    c('laminated', 'laminations'),
                    c('shale', 'shaly', 'shaley'),
                    c('siltstone', 'silty', 'siliceous'),
                    c('arkose', 'arkosic'),
                    c('argillite', 'argillaceous'))
  for(ii in seq(length(dup.words))) {
    dec <- wordrep(dec, dup.words[[ii]])
  }
  # cut out specific words
  dec <- wordrm(dec, c('sedimentary', 'light', 'dark', 'white', 'grey', 
                       'phosphatic', 'tan', 'yellow', 'medium', 'red',
                       'black'))
  dec <- llply(dec, function(y) laply(y, function(x) paste0(x, collapse = ' ')))
  lit <- Map(function(x, y) {x[, 1] <- y; x}, lit, dec)

  # get rid of duplicated words because that's stupid
  lit.words <- llply(lit, function(x) str_split(x[, 1], ' '))
  dups <- worddup(lit.words)
  lit <- Map(function(x, y) {x[, 1] <- y; x}, lit, dups)

  # coarse vs fine, carbonate vs clastics
  #   fine: siltstone, claystone, mudstone, shale, argillite
  dec <- purrr::map(lit, ~ str_split(.x[, 1], ' '))
  # fine + siliciclastic
  # coarse + siliciclastic
  # carbonate
  fine <- c('siltstone', 'claystone', 'mudstone', 'shale', 'argillite')
  dec <- purrr::map(dec, function(y) 
                    purrr::map(y, ~ ifelse(any(.x == 'siliciclastic') &
                                           any(.x %in% fine),
                                         'fine siliciclastic', 
                                         ifelse(any(.x == 'siliciclastic') &
                                                !(any(.x %in% fine)),
                                              'coarse siliciclastic', 
                                              ifelse(any(.x == 'dolomite'), 
                                                     'dolomite', 'other')))))
  # replace description with single word
  lit <- Map(function(x, y) {x[, 1] <- y; x}, lit, dec) # 
  lit <- purrr::map(lit, ~ matrix(unlist(.x), ncol = 2))
  # aggregate identicals
  lit <- llply(lit, function(x) 
               aggregate(as.numeric(x[, 2]) ~ x[, 1], FUN = sum))

  lit <- purrr::map(lit, function(x) {x[, 2] <- x[, 2] / sum(x[, 2]); x})
  lit <- lith.matrix(lit)
  lit
}

full.lithology <- function(strat) {
  # make into a statement and a percentage
  lit <- laply(strat$lith, function(x) str_split(x, '\\|'))
  lit <- llply(lit, str_trim)
  lit <- llply(lit, function(x) str_split(x, '  ~ ', simplify = TRUE))
  names(lit) <- strat$unit_id

  # get rid of bad information
  bad <- c('igneous', 'volcanic', 'metamorphic', 'chemical',
           'anhydrite', 'evaporite', 'halite')
  dec <- llply(lit, function(x) str_split(x[, 1], ' '))
  torm <- llply(dec, function(y) laply(y, function(x) any(bad %in% x)))
  lit <- Map(function(x, y) x[!y, , drop = FALSE], x = lit, y = torm)
  lit <- lit[laply(lit, nrow) > 0]

  # synonymize/replace certain words
  dec <- llply(lit, function(x) str_split(x[, 1], ' '))
  dup.words <- list(c('green', 'greenish'), 
                    c('limestone', 'lime'),
                    c('red', 'reddish'), 
                    c('blue', 'bluish'),
                    c('brown', 'brownish'),
                    c('yellow', 'yellowish'),
                    c('thin', 'thinly'),
                    c('thick', 'thickly'),
                    c('bedded', 'laminated'),
                    c('dolomite', 'dolomitic'),
                    c('chert', 'cherty'),
                    c('sandstone', 'sand', 'sandy'),
                    c('mudstone', 'mud'),
                    c('laminated', 'laminations'),
                    c('shale', 'shaly', 'shaley'),
                    c('siltstone', 'silty', 'siliceous'),
                    c('arkose', 'arkosic'),
                    c('argillite', 'argillaceous'))
  for(ii in seq(length(dup.words))) {
    dec <- wordrep(dec, dup.words[[ii]])
  }
  # cut out specific words
  dec <- wordrm(dec, c('sedimentary', 'light', 'dark', 'white', 'grey', 
                       'phosphatic', 'tan', 'yellow', 'medium', 'red',
                       'black'))
  dec <- llply(dec, function(y) laply(y, function(x) paste0(x, collapse = ' ')))
  lit <- Map(function(x, y) {x[, 1] <- y; x}, lit, dec)

  # get rid of duplicated words because that's stupid
  lit.words <- llply(lit, function(x) str_split(x[, 1], ' '))
  dups <- worddup(lit.words)
  lit <- Map(function(x, y) {x[, 1] <- y; x}, lit, dups)

  # short alphabetically
  ss <- llply(lit, function(x) str_split(x[, 1], ' '))
  ss <- llply(ss, function(x) llply(x, sort))
  ss <- llply(ss, function(x) laply(x, function(y) paste0(y, collapse = ' ')))
  lit <- Map(function(x, y) {x[, 1] <- y; x}, lit, ss)

  # clear blanks
  ss <- llply(lit, function(x) str_split(x[, 1], ' '))
  ss <- wordrm(ss, c(' '))
  ss <- llply(ss, function(y) laply(y, function(x) paste0(x, collapse = ' ')))
  lit <- Map(function(x, y) {x[, 1] <- y; x}, lit, ss)

  # some descriptions have been lost all together
  extrabad <- llply(lit, function(x) x[, 1] != '')
  lit <- Map(function(x, y) x[y, , drop = FALSE], lit, extrabad)

  lit <- llply(lit, function(x) aggregate(as.numeric(x[, 2]) ~ x[, 1], FUN = sum))
  lit <- lit[laply(lit, nrow) > 0]

  rock.matrix <- lith.matrix(lit)
  rock.matrix <- t(apply(rock.matrix, 1, function(x) x / sum(x)))

  # is there a way to programmatically simplify?
  rock.wordfreq <- table(Reduce(c, str_split(colnames(rock.matrix), ' ')))
  too.short <- names(which(rock.wordfreq == 1))
  rock.desc <- Reduce(c, llply(lit, function(x) x[, 1]))
  by.word <- str_split(rock.desc, ' ')
  sho.words <- llply(lit, function(x) str_split(x[, 1], ' '))
  sho <- wordrm(sho.words, too.short)
  sho <- llply(sho, function(y) laply(y, function(x) paste0(x, collapse = ' ')))
  sho <- Map(function(x, y) {x[, 1] <- y; x}, lit, sho)
  sho <- llply(sho , function(x) aggregate(as.numeric(x[, 2]) ~ x[, 1], FUN = sum))
  short.matrix <- lith.matrix(sho)  

  # i still think there are too many lithology types
  # can i do better than this?
  #   colors?
  #   similar words (e.g. shale, shaly)
  still.words <- table(Reduce(c, str_split(colnames(short.matrix), ' ')))
  apcount <- apply(short.matrix, 2, function(x) sum(x > 0))
  solos <- names(which(apcount >= 10))
  #sm <- apply(short.matrix[, solos], 1, function(x) x > 0)
  sm <- short.matrix[, (colnames(short.matrix) %in% solos)]
  smc <- apply(sm, 2, function(x) sum(x > 0))
  sm <- sm[, order(smc, decreasing = TRUE)]
  short.matrix <- sm
  short.matrix <- short.matrix[rowSums(short.matrix) != 0, ]

  # covariates
  short.matrix <- t(apply(short.matrix, 1, function(x) x / sum(x)))
  short.matrix
}

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
