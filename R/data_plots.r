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
#shelly <- c('Brachiopoda', 'Trilobita', 'Bivalvia', 'Gastropoda')
shelly <- 'Trilobita'
ord <- c(460.4, 443.8)
mid <- 445.6


# have yes/no label based on taxonomy
unit.dur <- data.frame(id = strat.ord$unit_id,
                       start = strat.ord$b_age,
                       stop = strat.ord$t_age)
for(ii in seq(length(shelly))) {
  foss.info <- process.fossil(fossil.ord, shelly = shelly[ii])
  yesno <- (as.character(strat.ord$unit_id) %in% foss.info$unit) * 1
  unit.dur[, ncol(unit.dur) + 1] <- yesno
}

names(unit.dur)[-(1:3)] <- shelly
unit.dur <- unit.dur[order(unit.dur$start), ] 
unit.dur$id <- factor(unit.dur$id, levels = unique(unit.dur$id))


oo <- list()
for(ii in seq(length(shelly))) {
  tt <- which(names(unit.dur) == shelly[ii])
  oo[[ii]] <- unit.dur[, c(1:3, tt)]
  names(oo[[ii]]) <- c(names(unit.dur)[1:3], 'fossils')
  oo[[ii]]$taxon <- shelly[ii]
}
unit.dur <- Reduce(rbind, oo)
unit.dur$fossils <- mapvalues(unit.dur$fossils,
                              from = 0:1,
                              to = c('no', 'yes'))


# fossils occurr at a specific time
# genus id numbers
genus.ids <- laply(fossil.ord$genus_no, function(x) str_split(x, '\\|'))
names(genus.ids) <- fossil.ord$unit_id
genus.ids <- genus.ids[laply(genus.ids, function(x) all(x != ''))]
genus.uni <- sort(unique(unlist(genus.ids)))

# grab genus information from pbdb
fossil.url <- paste0('https://paleobiodb.org/data1.2/taxa/list.txt?taxon_id=',
                     paste0(genus.uni, collapse = ','), 
                     '&show=full')
taxon <- read.csv(fossil.url, stringsAsFactors = FALSE)
taxon <- taxon[taxon$class %in% shelly, ]  # just good ones
# goal is extract the genera that i actually see
good.unit <- which(laply(genus.ids, function(x) any(x %in% taxon$taxon_no)))




# get age of fossil occurrences
# use the age data from macrostrat
fosocc <- fossil.ord[, c('cltn_id', 'unit_id', 't_age', 'b_age')]
fosocc$ma <- apply(fosocc[, c('t_age', 'b_age')], 1, mean)
names(fosocc)[1:2] <- c('collec', 'id')
fosocc <- fosocc[fosocc$id %in% good.unit, ]  # specifically have trilobites
fosocc$id <- factor(fosocc$id, levels = levels(unit.dur$id))
fosocc$taxon <- shelly











# now i have two data frames
#   unit.dur: geologic units, start stop times, if they bear fossils
#   fosocc: fossil occurrence times by taxonomy
#     note, currently only works for single taxonomic group (e.g. brachiopods)

# skeleton graph
unitgg <- ggplot(unit.dur, aes(x = start, y = id, 
                               colour = fossils))
unitgg <- unitgg + geom_segment(aes(xend = stop, yend = id))
unitgg <- unitgg + geom_point(data = fosocc, 
                              mapping = aes(x = ma, y = id, colour = NULL),
                              alpha = 0.1)
unitgg <- unitgg + geom_vline(xintercept = mid)
unitgg <- unitgg + facet_wrap( ~ taxon)
unitgg <- unitgg + coord_cartesian(xlim = ord)
unitgg <- unitgg + scale_colour_manual(values = c('skyblue', 'goldenrod'),
                                       name = 'Fossil bearing?')
unitgg <- unitgg + labs(x = 'Time (Mya)', y = 'Macrostrat unit')
unitgg <- unitgg + theme(axis.text.y = element_blank(),
                         axis.ticks.y = element_blank(),
                         legend.position = 'top')
