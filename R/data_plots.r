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
ord <- c(460.4, 443.8)
mid <- 445.6
bracket <- c(ord[1], mid, ord[2])


# have yes/no label based on taxonomy
unit.dur <- data.frame(id = strat.ord$unit_id,
                       start = strat.ord$b_age,
                       stop = strat.ord$t_age)
for(ii in seq(length(shelly))) {
  foss.info <- process.fossil(fossil.ord, shelly = shelly[ii])
  yesno <- (as.character(strat.ord$unit_id) %in% foss.info$unit) * 1
  unit.dur[, ncol(unit.dur) + 1] <- yesno
}
names(unit.dur) <- shelly
unit.dur <- unit.dur[order(unit.dur$start), ] 
unit.dur$id <- factor(unit.dur$id, levels = unique(unit.dur$id))

# collection dates
collecs <- data.frame(id = collec.ord$collection_no,
                      age = apply(collec.ord[, c('max_ma', 'min_ma')], 1, mean))

# next goal is put collection X-s in
unit.dur$id %in% factor(unique(foss.info$unit))




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





# skeleton graph
unitgg <- ggplot(unit.dur, aes(x = start, xend = stop, 
                                  y = id, yend = id, 
                                  colour = fossils))
unitgg <- unitgg + geom_segment()
unitgg <- unitgg + geom_vline(xintercept = 445.6)
unitgg <- unitgg + facet_wrap( ~ taxon)
unitgg <- unitgg + coord_cartesian(xlim = c(460.4, 443.8))
unitgg <- unitgg + scale_colour_manual(values = c('skyblue', 'goldenrod'),
                                       name = 'Fossil bearing?')
unitgg <- unitgg + labs(x = 'Time (Mya)', y = 'Macrostrat unit')
unitgg <- unitgg + theme(axis.text.y = element_blank(),
                         axis.ticks.y = element_blank(),
                         legend.position = 'top')



# restricted data set
#   start/stop in late ordovician
#   start/stop before hirnantian
#   start/stop during hirnantian
unit.info <- process.strat(strat.ord, bracket = bracket)  
yesno <- (as.character(unit.info$unit.id) %in% foss.info$unit) * 1

unit.dur <- data.frame(id = unit.info$unit.id,
                          fossils = mapvalues(yesno,
                                              from = 0:1,
                                              to = c('no', 'yes')),
                          start = unit.info$age$bottom.age,
                          stop = unit.info$age$top.age)
unit.dur <- unit.dur[order(unit.dur$start), ] 
unit.dur$id <- factor(unit.dur$id, levels = unique(unit.dur$id))

restgg <- ggplot(unit.dur, aes(x = start, xend = stop, 
                                  y = id, yend = id, 
                                  colour = fossils))
restgg <- restgg + geom_segment()
restgg <- restgg + geom_vline(xintercept = 445.6)
restgg <- restgg + coord_cartesian(xlim = c(460.4, 443.8))
restgg <- restgg + scale_colour_manual(values = c('skyblue', 'goldenrod'),
                                       name = 'Fossil bearing?')
restgg <- restgg + labs(x = 'Time (Mya)', y = 'Macrostrat unit')
restgg <- restgg + theme(axis.text.y = element_blank(),
                         axis.ticks.y = element_blank(),
                         legend.position = 'top')
