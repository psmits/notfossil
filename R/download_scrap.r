library(paleobioDB)
library(plyr)
library(stringr)

strat.ord <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Ordovician&response=long&format=csv', stringsAsFactors = FALSE)
#strat.sil <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Silurian&response=long&format=csv', stringsAsFactors = FALSE)
#strat.ord$max_thick == 0  # will need imputation
tt <- c('max_thick', 'min_thick')
weird <- strat.ord$max_thick < strat.ord$min_thick
strat.ord[weird, tt] <- strat.ord[weird, rev(tt)]


lit <- laply(strat.ord$lith, function(x) str_split(x, '\\|'))
lit <- llply(lit, str_trim)
lit <- llply(lit, function(x) str_split(x, '  ~ ', simplify = TRUE))
names(lit) <- strat.ord$unit_id


#strat.ord$pbdb_collections




fossil.ord <- read.csv('https://macrostrat.org/api/v2/fossils?interval_name=Ordovician&response=long&format=csv', stringsAsFactors = FALSE)
# match(fossil.ord$unit_id, strat.ord$unit_id)  # match fossil occurrence to strat unit

genus.ids <- laply(fossil.ord$genus_no, function(x) str_split(x, '\\|'))
genus.ids <- genus.ids[laply(genus.ids, function(x) all(x != ''))]
genus.ids <- sort(unique(unlist(genus.ids)))
occ.info <- pbdb_occurrences(id = genus.ids, limit = 'all', vocab = 'pbdb', 
                             show = 'phylo')
