library(paleobioDB)
library(plyr)
library(stringr)

# macrostrat strat data from the orodovician
strat.ord <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Ordovician&response=long&format=csv', stringsAsFactors = FALSE)
#strat.sil <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Silurian&response=long&format=csv', stringsAsFactors = FALSE)

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


# macrostrat fossil data from the orodovician
fossil.ord <- read.csv('https://macrostrat.org/api/v2/fossils?interval_name=Ordovician&response=long&format=csv', stringsAsFactors = FALSE)
# match(fossil.ord$unit_id, strat.ord$unit_id)  # match fossil occurrence to strat unit

# get the unique ids and run them by the pbdb
genus.ids <- laply(fossil.ord$genus_no, function(x) str_split(x, '\\|'))
genus.ids <- genus.ids[laply(genus.ids, function(x) all(x != ''))]
genus.ids <- sort(unique(unlist(genus.ids)))
occ.info <- pbdb_occurrences(id = genus.ids, limit = 'all', vocab = 'pbdb', 
                             show = 'phylo', 'coords')
