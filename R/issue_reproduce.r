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

# grab genus information from pbdb based on what occurres in collections
fossil.url <- paste0('https://paleobiodb.org/data1.2/occs/list.txt?coll_id=',
                     paste0(ord$cltn_id, collapse = ','), 
                     '&min_ma=443.8&max_ma=460.4&show=full')
taxon <- read.csv(fossil.url, stringsAsFactors = FALSE)
