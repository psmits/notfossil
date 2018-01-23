# macrostrat strat data from the orodovician
strat.ord <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Ordovician&response=long&format=csv', 
                      stringsAsFactors = FALSE)
strat.sil <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Silurian&response=long&format=csv', 
                      stringsAsFactors = FALSE)

# macrostrat fossil data from the orodovician
fossil.url <- paste0('https://macrostrat.org/api/v2/fossils?unit_id=', 
                     paste0(strat.ord$unit_id, collapse = ','), 
                     '&response=long&format=csv') 
fossil.ord <- read.csv(fossil.url, stringsAsFactors = FALSE)
fossil.url <- paste0('https://macrostrat.org/api/v2/fossils?unit_id=', 
                     paste0(strat.sil$unit_id, collapse = ','), 
                     '&response=long&format=csv') 
fossil.sil <- read.csv(fossil.url, stringsAsFactors = FALSE)

# collection information
fossil.url <- paste0('https://paleobiodb.org/data1.2/colls/list.txt?coll_id=',
                     paste0(fossil.ord$cltn_id, collapse = ','), 
                     '&show=full')
collec.ord <- read.csv(fossil.url, stringsAsFactors = FALSE)
fossil.url <- paste0('https://paleobiodb.org/data1.2/colls/list.txt?coll_id=',
                     paste0(fossil.sil$cltn_id, collapse = ','), 
                     '&show=full')
collec.sil <- read.csv(fossil.url, stringsAsFactors = FALSE)
