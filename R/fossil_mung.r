library(rstanarm)
library(arm)
library(plyr)
library(stringr)
library(compositions)
library(geosphere)
#library(glmnet)
#library(rpart)

source('download_scrap.r')


# genus id numbers
genus.ids <- laply(fossil.ord$genus_no, function(x) str_split(x, '\\|'))
genus.unit <- genus.ids
names(genus.unit) <- fossil.ord$unit_id  # match genus_no w/ unit_id
genus.ids <- genus.ids[laply(genus.ids, function(x) all(x != ''))]
genus.ids <- sort(unique(unlist(genus.ids)))

# collection id numbers
colec.ids <- laply(fossil.ord$cltn_id, function(x) str_split(x, '\\|'))
colec.unit <- colec.ids
names(colec.unit) <- fossil.ord$unit_id
colec.ids <- colec.ids[laply(colec.ids, function(x) all(x != ''))]
colec.ids <- sort(unique(unlist(colec.ids)))

# read in all specimens based on their collection numbers
make.url <- paste0('https://paleobiodb.org/data1.2/occs/list.txt?coll_id=', 
                   paste0(colec.ids,collapse=','), '&show=full')
fossils <- read.csv(make.url, stringsAsFactors = FALSE)

## lithology stuff
#fossils$lithology1
#fossils$lithology2
#fossils$fossilsfrom1
#fossils$fossilsfrom2

## collection type
#fossils$collection_type
#fossils$collection_coverage


# curiosity: how many non-zero ids
taxonomy <- c('phylum', 'class', 'order', 'family', 'genus')
how.many <- apply(fossils[, taxonomy], 2, function(x) sum(x != ''))

# deal with subgenus nonsense
fossils$genus <- Reduce(c, str_match_all(fossils$genus, '^\\S*'))
fossils <- fossils[fossils$order != '', ]

# give new column that is unit_id
fossils$unit_id <- NA
nn <- as.numeric(names(colec.unit))
for(ii in seq(length(colec.unit))) {
  fossils$unit_id[fossils$collection_no %in% colec.unit[[ii]]] <- nn[ii]
}


not.enough <- fossils[fossils$phylum == '' | fossils$class == '', taxonomy]
# some of the orders missing phylum/class are easy to fix
# trilobite group, radiolarians, brach relatives
