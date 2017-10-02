library(paleobioDB)

# macrostrat strat data from the orodovician
strat.ord <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Ordovician&response=long&format=csv', stringsAsFactors = FALSE)
#strat.sil <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Silurian&response=long&format=csv', stringsAsFactors = FALSE)


# macrostrat fossil data from the orodovician
fossil.ord <- read.csv('https://macrostrat.org/api/v2/fossils?interval_name=Ordovician&response=long&format=csv', stringsAsFactors = FALSE)
# match(fossil.ord$unit_id, strat.ord$unit_id)  # match fossil occurrence to strat unit

# get the unique ids and run them by the pbdb
genus.ids <- laply(fossil.ord$genus_no, function(x) str_split(x, '\\|'))
genus.ids <- genus.ids[laply(genus.ids, function(x) all(x != ''))]
genus.ids <- sort(unique(unlist(genus.ids)))
occ.info <- pbdb_occurrences(id = genus.ids, limit = 'all', vocab = 'pbdb', 
                             show = c('phylo', 'coords'))
