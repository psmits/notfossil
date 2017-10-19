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
