# macrostrat strat data from the orodovician
strat1 <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Ordovician&response=long&format=csv', 
                      stringsAsFactors = FALSE)
strat2 <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Silurian&response=long&format=csv', 
                      stringsAsFactors = FALSE)
strat <- dplyr::union(strat1, strat2)
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
lc <- length(fossil$cltn_id)
f1 <- fossil$cltn_id[1:(lc / 2)]
f2 <- fossil$cltn_id[((lc / 2) + 1):lc]
fossil.url <- paste0('https://paleobiodb.org/data1.2/occs/list.txt?coll_id=',
                     paste0(f1, collapse = ','), '&show=full')
taxon1 <- read.csv(fossil.url, stringsAsFactors = FALSE)
fossil.url <- paste0('https://paleobiodb.org/data1.2/occs/list.txt?coll_id=',
                     paste0(f2, collapse = ','), '&show=full')
taxon2 <- read.csv(fossil.url, stringsAsFactors = FALSE)
taxon <- dplyr::union(dplyr::select(taxon1, 
                                    -plant_organ, -geoplate, 
                                    -associated_parts, -bioerosion), 
                      dplyr::select(taxon2, 
                                    -plant_organ, -geoplate, 
                                    -associated_parts, -bioerosion))
taxon <- taxon[taxon$genus != '', ]
