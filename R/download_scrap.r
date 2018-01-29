# macrostrat strat data from the orodovician
strat <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Ordovician&response=long&format=csv', 
                      stringsAsFactors = FALSE)
#strat.sil <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Silurian&response=long&format=csv', 
#                      stringsAsFactors = FALSE)

# macrostrat fossil data from the orodovician
fossil.url <- paste0('https://macrostrat.org/api/v2/fossils?unit_id=', 
                     paste0(strat$unit_id, collapse = ','), 
                     '&response=long&format=csv') 
fossil <- read.csv(fossil.url, stringsAsFactors = FALSE)
#fossil.url <- paste0('https://macrostrat.org/api/v2/fossils?unit_id=', 
#                     paste0(strat.sil$unit_id, collapse = ','), 
#                     '&response=long&format=csv') 
#fossil.sil <- read.csv(fossil.url, stringsAsFactors = FALSE)

# grab genus information from pbdb based on what occurres in collections
fossil.url <- paste0('https://paleobiodb.org/data1.2/occs/list.txt?coll_id=',
                     paste0(fossil$cltn_id, collapse = ','), 
                     '&min_ma=443.8&max_ma=460.4&show=full')
taxon <- read.csv(fossil.url, stringsAsFactors = FALSE)
taxon <- taxon[taxon$genus != '', ]
