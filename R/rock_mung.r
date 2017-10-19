source('rock_functions.r')

process.strat <- function(strat.ord) {
  # fossils in those rocks
  rock.fossil <- data.frame(nocc = strat.ord$pbdb_occurrences, 
                            ncol = strat.ord$pbdb_collections)
  rock.fossil$yesno <- (rock.fossil$ncol > 0) * 1
  rownames(rock.fossil) <- strat.ord$unit_id

  # given data call, want to extract rock covariates
  # get lithology words and composition
  lit <- laply(strat.ord$lith, function(x) str_split(x, '\\|'))
  lit <- llply(lit, str_trim)
  lit <- llply(lit, function(x) str_split(x, '  ~ ', simplify = TRUE))
  names(lit) <- strat.ord$unit_id


  # cull unhelpful descriptions
  bad <- c('igneous', 'volcanic', 'metamorphic', 'chemical',
           'anhydrite', 'evaporite', 'halite')
  dec <- llply(lit, function(x) str_split(x[, 1], ' '))
  torm <- llply(dec, function(y) laply(y, function(x) any(bad %in% x)))
  lit <- Map(function(x, y) x[!y, , drop = FALSE], x = lit, y = torm)
  lit <- lit[laply(lit, nrow) > 0]


  # synonymize some words
  dec <- llply(lit, function(x) str_split(x[, 1], ' '))
  # replace certain words
  dup.words <- list(c('green', 'greenish'), 
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

  #table(Reduce(c, Reduce(c, dec)))
  # cut out specific words
  dec <- wordrm(dec, c('sedimentary', 'light', 'dark', 'white', 
                       'phosphatic', 'tan', 'yellow', 'medium'))
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
  comp.tr <- ilr(rock.matrix)  # isometric log ratio transform
  # to read coefs from a reg, do ilrInv to the coefs given comp.tr
  # i.e. ilrInv(coefs, x = comp.tr)
  # turns out this matrix is super big and relatively not helpful
  #   too many descriptions

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

  # covariates
  short.matrix <- t(apply(short.matrix, 1, function(x) x / sum(x)))
  shcm.tr <- ilr(short.matrix)  # isometric log ratio transform


  # i still think there are too many lithology types
  # can i do better than this?
  #   colors?
  #   similar words (e.g. shale, shaly)
  still.words <- table(Reduce(c, str_split(colnames(short.matrix), ' ')))
  apcount <- apply(short.matrix, 2, function(x) sum(x > 0))
  solos <- names(which(apcount == 1))
  sw <- apply(short.matrix[, solos], 1, function(x) x > 0)

  # consolidate
  rock.fossil <- rock.fossil[rownames(rock.fossil) %in% rownames(short.matrix), ]


  ####
  # non lithological covariates
  #   thickness in km
  #   column area
  #   contact with other columns (binary)
  #     above
  #     below     
  #   geographic change in km based on paleo coord
  #   duration of column from continuous time model in My
  # all either binary or arm::rescale-d 
  so <- strat.ord[strat.ord$unit_id %in% rownames(rock.fossil), ]


  # thickness
  # some have reversed(?) thick ests
  tt <- c('max_thick', 'min_thick')
  weird <- so$max_thick < so$min_thick
  so[weird, tt] <- so[weird, rev(tt)]
  # covariate form
  thik.h <- arm::rescale(log1p(so$max_thick))
  thik.l <- arm::rescale(log1p(so$min_thick))
  thik.h.imp <- so$max_thick == 0  # will need imputation?
  thik.l.imp <- so$min_thick == 0  # will need imputation?


  # area of the column
  # area estimate sq km; not very accurate
  cola <- arm::rescale(log(so$col_area))  # rescaled


  # contact units
  cont.a <- (so$units_above != '0') * 1  # any units above (binary)
  cont.b <- (so$units_below != '0') * 1  # any units below (binary)


  # amount of geographic change from bottom to top of column; paleo coord
  tcoo <- cbind(so$t_plng, so$t_plat)  # top coord
  bcoo <- cbind(so$b_plng, so$b_plat)  # bot coord
  loc.change <- distGeo(tcoo, bcoo)  # distance is cal in meters
  loc.change <- loc.change / 1000  # to kilometers
  loc.ch <- arm::rescale(log1p(loc.change))  # rescaled


  # tropical vs temperate @ top and bottom
  top.temp <- (so$t_plat > 20 | so$t_plat < -20) * 1
  bot.temp <- (so$b_plat > 20 | so$b_plat < -20) * 1
  # crosses equator
  cros.eq <- (sign(so$b_plat) != sign(so$t_plat)) * 1
  # becomes topical
  swit <- (bot.temp != top.temp) * 1


  # top, bottom age est; continuous time model
  # difference between b_age and t_age
  #   the plng and plat variables follow this time model
  dr.col <- (so$b_age - so$t_age)
  dr.col <- arm::rescale(log(dr.col))



  clean.data <- list(unit.id = rownames(rock.fossil),
                     fossils = rock.fossil, 
                     lithology = list(raw = short.matrix, ilr.trans = shcm.tr),
                     thickness = list(high = thik.h, low = thik.l, 
                                      impute.high = thik.h.imp, impute.low = thik.l.imp),
                     column.area = cola,
                     contact = list(above = cont.a, below = cont.b),
                     change = loc.ch,
                     location = list(top.temp = top.temp, bot.temp = bot.temp, 
                                     cross.eq = cros.eq, switches = swit),
                     duration = dr.col)

  clean.data
}
