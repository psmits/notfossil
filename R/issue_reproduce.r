# first things first (i eat your brains)
#   download ordovician geologic units
#   download ordovician fossil information
# then (i start rocking gold teeth and fangs)
#   get pbdb genus ids
#     how many taxon ids from macrostrat return pbdb listings?
#     should be 100% because they are pbdb identifies, but actually closer to 50%
#   get pbdb collection ids
#     how many collection ids from macrostrat return pbdb listings?
#     should be 100% because they are pbdb identifies, but actually closer to 50%

library(plyr)
library(stringr)


# macrostrat strat data from the orodovician
strat.ord <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Ordovician&response=long&format=csv', 
                      stringsAsFactors = FALSE)

# macrostrat fossil data from the orodovician
fossil.url <- paste0('https://macrostrat.org/api/v2/fossils?unit_id=', 
                     paste0(strat.ord$unit_id, collapse = ','), 
                     '&response=long&format=csv') 
ord <- read.csv(fossil.url, stringsAsFactors = FALSE)

# by genus id
genus.ids <- laply(ord$genus_no, function(x) str_split(x, '\\|'))
unique.taxa <- unique(unlist(genus.ids))

# grab genus information from pbdb
fossil.url <- paste0('https://paleobiodb.org/data1.2/occs/list.txt?taxon_id=',
                     paste0(unique.taxa, collapse = ','), 
                     '&min_ma=443.8&max_ma=460.4&show=full')
taxon <- read.csv(fossil.url, stringsAsFactors = FALSE)
# how many taxon ids returned entries
taxon.percent <- sum(unique.taxa %in% taxon$accepted_no) / length(unique.taxa)
print(paste('% pbdb taxon ids in macrostrat that are reflected in the pbdb: ', 
            round(taxon.percent, digits = 2)))


# grab collection information from pbdb
fossil.url <- paste0('https://paleobiodb.org/data1.2/colls/list.txt?coll_id=',
                     paste0(ord$cltn_id, collapse = ','), 
                     '&show=full')
cltn <- read.csv(fossil.url, stringsAsFactors = FALSE)

cltn.percent <- sum(unique(ord$cltn_id) %in% unique(cltn$collection_no)) /
  length(unique(ord$cltn_id))
print(paste('% pbdb collection ids in macrostrat that are reflected in the pbdb:', 
            round(cltn.percent, digits = 2)))



# do collections at least make sense temporally?
# lets look just in macrostrat
times <- strat.ord[, c('unit_id', 't_age', 'b_age')]
ff <- ord[, c('cltn_id', 'unit_id', 't_age', 'b_age')]
ffunit <- times[times$unit_id %in% ff$unit_id, ]

exu <- ffunit[ffunit$unit_id == ff$unit_id[1], ]
ex <- ff[ff$unit_id == ff$unit_id[1], ]

# macrostrat collection age is identical to macrostrat unit age
#   no superposition information recorded here
macro.compare <- all(apply(ex[, c('t_age', 'b_age')], 1, function(x) 
                           all(x == exu[, c('t_age', 'b_age')])))

# compare macrostrat collection info to pbdb collection information
cc <- cltn[, c('collection_no', 'min_ma', 'max_ma')]
ffcc <- ff[ff$cltn_id %in% cc$collection_no, ]

exc <- ffcc[ffcc$cltn_id == cc$collection_no[1], ]  # based on macrostat
ex <- cc[cc$collection_no == cc$collection_no[1], ]  # based on pbdb

print('collection information from macrostrat:')
print(exc)
print('collection information from pbdb:')
print(ex)
