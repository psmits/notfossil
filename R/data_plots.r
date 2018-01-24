library(reshape2)
library(plyr)
library(stringr)

library(ggplot2)
library(scales)
library(grid)
library(ggridges)
theme_set(theme_bw())  # graph setting

source('../R/rock_functions.r')
source('../R/rock_mung.r')
source('../R/fossil_functions.r')
source('../R/fossil_mung.r')

source('../R/download_scrap.r')  # just macrostrat

# constants
shelly <- c('Brachiopoda', 'Trilobita', 'Bivalvia', 'Gastropoda')
shelly <- shelly[1]
ord <- c(460.4, 443.8)
mid <- 445.6


# by genus id
genus.ids <- laply(fossil.ord$genus_no, function(x) str_split(x, '\\|'))
names(genus.ids) <- fossil.ord$unit_id
unique.taxa <- unique(unlist(genus.ids))
genusy <- melt(genus.ids)  # genus id, unit id
names(genusy) <- c('taxon_id', 'unit_id')

# grab genus information from pbdb
fossil.url <- paste0('https://paleobiodb.org/data1.2/occs/list.txt?taxon_id=',
                     paste0(unique.taxa, collapse = ','), 
                     '&min_ma=443.8&max_ma=460.4&show=full')
taxon <- read.csv(fossil.url, stringsAsFactors = FALSE)
# how many taxon ids returned entries
taxon.percent <- sum(unique.taxa %in% taxon$accepted_no) / length(unique.taxa)



# do collections make sense temporally?
# lets look just in macrostrat
times <- strat.ord[, c('unit_id', 't_age', 'b_age')]
ff <- fossil.ord[, c('cltn_id', 'unit_id', 't_age', 'b_age')]
ffunit <- times[times$unit_id %in% ff$unit_id, ]

exu <- ffunit[ffunit$unit_id == ff$unit_id[1], ]
ex <- ff[ff$unit_id == ff$unit_id[1], ]

# macrostrat collection age is identical to macrostrat unit age
#   no superposition information recorded here
macro.compare <- all(apply(ex[, c('t_age', 'b_age')], 1, function(x) 
                           all(x == exu[, c('t_age', 'b_age')])))


# now check if pbdb collection age is at least similar to macrostrat age
# grab collection information from pbdb
fossil.url <- paste0('https://paleobiodb.org/data1.2/colls/list.txt?coll_id=',
                     paste0(fossil.ord$cltn_id, collapse = ','), 
                     '&show=full')
cltn <- read.csv(fossil.url, stringsAsFactors = FALSE)
cltn.percent <- sum(fossil.ord$cltn_id %in% cltn$collection_no) /
  length(fossil.ord$cltn_id)

cc <- cltn[, c('collection_no', 'min_ma', 'max_ma')]
ffcc <- ff[ff$cltn_id %in% cc$collection_no, ]

exc <- ffcc[ffcc$cltn_id == cc$collection_no[1], ]  # based on macrostat
ex <- cc[cc$collection_no == cc$collection_no[1], ]  # based on pbdb

  
  
# what specific taxonomic group(s)
taxon <- taxon[taxon$phylum %in% shelly | taxon$class %in% shelly, ]  # just good ones

# chosen fossils in units
melty <- melty[melty$taxon_id %in% taxon$taxon_no, ]


# stratigraphic units
unit.dur <- data.frame(id = strat.ord$unit_id,
                       start = strat.ord$b_age,
                       stop = strat.ord$t_age)
# have fossil of interest
unit.dur$yesno <- factor((unit.dur$id %in% melty$unit_id) * 1)  
unit.dur <- unit.dur[order(unit.dur$start), ] 
unit.dur$id <- factor(unit.dur$id, levels = unique(unit.dur$id))


# get age of fossil occurrences
# use the age data from macrostrat
uni.collec <- unique(fossil.ord$cltn_id)
fossil.url <- paste0('https://paleobiodb.org/data1.2/colls/list.txt?coll_id=',
                     paste0(uni.collec, collapse = ','), 
                     '&show=full')
collec <- read.csv(fossil.url, stringsAsFactors = FALSE)
# collections have ages
collec <- collec[, c('collection_no', 'max_ma', 'min_ma')]
# calculate collection age
collec$mid_ma <- apply(collec[, c('max_ma', 'min_ma')], 1, mean)


# stick that back into fossil.ord
fossil.ord$mid_ma <- NA
for(ii in seq(nrow(fossil.ord))) {
  mm <- collec[collec$collection_no %in% fossil.ord$cltn_id[ii], ]
  if(nrow(mm) > 0) fossil.ord$mid_ma[ii] <- mm$mid_ma
}
fossil.ord$unit_id <- factor(fossil.ord$unit_id, levels = levels(unit.dur$id))






# PBDB dates don't line up with macrostrat dates






# now i have two data frames
#   unit.dur: geologic units, start stop times, if they bear fossils
#   fosocc: fossil occurrence times by taxonomy
#     note, currently only works for single taxonomic group (e.g. brachiopods)

# skeleton graph
unitgg <- ggplot(unit.dur, aes(x = start, y = id, 
                               colour = yesno))
unitgg <- unitgg + geom_segment(aes(xend = stop, yend = id))
unitgg <- unitgg + geom_point(data = fossil.ord, 
                              mapping = aes(x = mid_ma, 
                                            y = unit_id, 
                                            colour = NULL),
                              alpha = 0.1)
unitgg <- unitgg + geom_vline(xintercept = mid)
unitgg <- unitgg + coord_cartesian(xlim = ord)
unitgg <- unitgg + scale_colour_manual(values = c('skyblue', 'goldenrod'),
                                       name = 'Fossil bearing?')
unitgg <- unitgg + labs(x = 'Time (Mya)', y = 'Macrostrat unit')
unitgg <- unitgg + theme(axis.text.y = element_blank(),
                         axis.ticks.y = element_blank(),
                         legend.position = 'top')
