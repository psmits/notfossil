# macrostrat strat data from the orodovician
strat.ord <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Ordovician&response=long&format=csv', stringsAsFactors = FALSE)
#strat.sil <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Silurian&response=long&format=csv', stringsAsFactors = FALSE)

# macrostrat fossil data from the orodovician
fossil.ord <- read.csv('https://macrostrat.org/api/v2/fossils?interval_name=Ordovician&response=long&format=csv', stringsAsFactors = FALSE)
#fossil.sil <- read.csv('https://macrostrat.org/api/v2/fossils?interval_name=Silurian&response=long&format=csv', stringsAsFactors = FALSE)



# match(fossil.ord$unit_id, strat.ord$unit_id)  # match fossil occurrence to strat unit
