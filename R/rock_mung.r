library(plyr)
library(stringr)
library(compositions)
library(glmnet)

source('download_scrap.r')
source('rock_functions.r')

# fossils in those rocks
rock.fossil <- data.frame(nocc = strat.ord$pbdb_occurrences, 
                          ncol = strat.ord$pbdb_collections)
rock.fossil$yesno <- (rock.fossil$ncol > 0) * 1

# given data call, want to extract rock covariates
# thickness
impute.me <- strat.ord$max_thick == 0  # will need imputation
# some have reversed(?) thick ests
tt <- c('max_thick', 'min_thick')
weird <- strat.ord$max_thick < strat.ord$min_thick
strat.ord[weird, tt] <- strat.ord[weird, rev(tt)]

# get lithology words and composition
lit <- laply(strat.ord$lith, function(x) str_split(x, '\\|'))
lit <- llply(lit, str_trim)
lit <- llply(lit, function(x) str_split(x, '  ~ ', simplify = TRUE))
# get rid of duplicated words because that's stupid
lit.words <- llply(lit, function(x) str_split(x[, 1], ' '))
ndup <- llply(lit.words, function(x) llply(x, function(y) !duplicated(y)))
lit.words <- Map(function(a, b) Map(function(x, y) x[y], a, b), lit.words, ndup)
lit.words <- llply(lit.words, function(x) 
                   laply(x, function(y) paste0(y, collapse = ' ')))
lit <- Map(function(x, y) {x[, 1] <- y; x}, lit, lit.words)
names(lit) <- strat.ord$unit_id
# some lithologies appear/counted twice which makes no sense

# break up the words for fun and profit
rock.freq <- table(Reduce(c, llply(lit, function(x) x[, 1])))
rock.desc <- unique(Reduce(c, llply(lit, function(x) x[, 1])))
rock.words <- Reduce(c, str_split(rock.desc, ' '))
rock.word <- unique(rock.words)
rock.wordfreq <- table(rock.words)

lit <- llply(lit, function(x) aggregate(as.numeric(x[, 2]) ~ x[, 1], FUN = sum))
rock.matrix <- lith.matrix(lit)
comp.tr <- ilr(rock.matrix)
# to read coefs from a reg, do ilrInv to the coefs given comp.tr
# i.e. ilrInv(coefs, x = comp.tr)
# turns out this matrix is super big and relatively not helpful
#   too many descriptions

# is there a way to programmatically simplify?
too.short <- names(which(rock.wordfreq == 1))
by.word <- str_split(rock.desc, ' ')
sho.words <- llply(lit, function(x) str_split(x[, 1], ' '))
sho.bad <- llply(sho.words, function(y) llply(y, function(x) x %in% too.short))
sho.words <- Map(function(a, b) Map(function(x, y) x[!y], a, b), 
                 sho.words, sho.bad)
sho.words <- llply(sho.words, function(x) 
                   laply(x, function(y) paste0(y, collapse = ' ')))
sho <- Map(function(x, y) {x[, 1] <- y; x}, lit, sho.words)
sho <- llply(sho, function(x) aggregate(as.numeric(x[, 2]) ~ x[, 1], FUN = sum))
short.matrix <- lith.matrix(sho)
#table(Reduce(c, str_split(unique(Reduce(c, sho.words)), ' ')))

# i still think there are too many lithology types
# can i do better than this?
#   colors?
#   similar words (e.g. shale, shaly)



## top, bottom age est; continuous time model
#strat.ord$t_age
#strat.ord$b_age
#
## top, bottom age
#strat.ord$t_int_age
#strat.ord$b_int_age
#
## top coord; paleo
#strat.ord$t_plat
#strat.ord$t_plng
## bot coord; paleo
#strat.ord$b_plat
#strat.ord$b_plng
#
## area estimate sq km; not very accurate
#strat.ord$col_area
#
## contact units
#strat.ord$units_above
#strat.ord$units_below
