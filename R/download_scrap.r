library(paleobioDB)
library(plyr)
library(stringr)
library(compositions)
#library(glmnet)

# macrostrat strat data from the orodovician
strat.ord <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Ordovician&response=long&format=csv', stringsAsFactors = FALSE)
#strat.sil <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Silurian&response=long&format=csv', stringsAsFactors = FALSE)

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
names(lit) <- strat.ord$unit_id
# some lithologies appear/counted twice which makes no sense
lit <- llply(lit, function(x) aggregate(as.numeric(x[, 2]) ~ x[, 1], FUN = sum))

# break up the words for fun and profit
rock.freq <- table(Reduce(c, llply(lit, function(x) x[, 1])))
rock.desc <- unique(Reduce(c, llply(lit, function(x) x[, 1])))
rock.words <- Reduce(c, str_split(rock.desc, ' '))
rock.word <- unique(rock.words)
rock.wordfreq <- table(rock.words)

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


comp.tr <- ilr(rock.matrix)
# to read coefs from a reg, do ilrInv to the coefs given comp.tr
# i.e. ilrInv(coefs, x = comp.tr)
# turns out this matrix is super big and relatively not helpful
#   too many descriptions


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





## macrostrat fossil data from the orodovician
#fossil.ord <- read.csv('https://macrostrat.org/api/v2/fossils?interval_name=Ordovician&response=long&format=csv', stringsAsFactors = FALSE)
## match(fossil.ord$unit_id, strat.ord$unit_id)  # match fossil occurrence to strat unit
#
## get the unique ids and run them by the pbdb
#genus.ids <- laply(fossil.ord$genus_no, function(x) str_split(x, '\\|'))
#genus.ids <- genus.ids[laply(genus.ids, function(x) all(x != ''))]
#genus.ids <- sort(unique(unlist(genus.ids)))
#occ.info <- pbdb_occurrences(id = genus.ids, limit = 'all', vocab = 'pbdb', 
#                             show = 'phylo', 'coords')
