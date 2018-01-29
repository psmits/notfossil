library(reshape2)
library(plyr)
library(stringr)

library(ggplot2)
library(scales)
library(grid)
library(ggridges)
theme_set(theme_bw())  # graph setting
blind <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
           "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

source('../R/rock_functions.r')
source('../R/rock_mung.r')
source('../R/fossil_functions.r')
source('../R/fossil_mung.r')

source('../R/download_scrap.r')  # all api calls, macrostrat + pbdb


# constants
shelly <- c('Brachiopoda', 'Trilobita', 'Bivalvia', 'Gastropoda')
ord <- c(460.4, 443.8)
hirnantian <- 445.6

# mid age of unit
strat$m_age <- apply(strat[, c('t_age', 'b_age')], 1, mean)
# cypher for which collections in what units
cltn2unit <- fossil[, c('cltn_id', 'unit_id')]


# associate each fossil occurrence with a macrostrat unit
taxon$unit <- cltn2unit[match(taxon$collection_no, cltn2unit[, 1]), 2]

# filter to just the taxa i want
taxon <- taxon[taxon$phylum %in% shelly | taxon$class %in% shelly, ]

# put occurrences at middle of unit
ss <- fossil[, c('unit_id', 'b_age', 't_age')]
ss$mid <- apply(ss[, 2:3], 1, mean)
taxon$mid_ma <- ss$mid[match(taxon$unit, ss$unit_id)]

# make nice
strat <- strat[order(strat$b_age), ]
strat$unit_id <- factor(strat$unit_id, levels = unique(strat$unit_id))
taxon$unit <- factor(taxon$unit, levels = levels(strat$unit_id))

tt <- split(taxon, as.character(taxon$unit))
tt <- llply(tt, function(x) x[!duplicated(x$genus), ])
taxon <- Reduce(rbind, tt)


strat$fossils <- factor((strat$unit_id %in% taxon$unit))
taxon$group <- NA
for(ii in seq(length(shelly))) {
  taxon$group[taxon$phylum %in% shelly[ii]] <- shelly[ii]
  taxon$group[taxon$class %in% shelly[ii]] <- shelly[ii]
}



# the plot
unitgg <- ggplot(strat, aes(x = b_age, y = unit_id, colour = fossils)) 
unitgg <- unitgg + geom_segment(mapping = aes(xend = t_age, yend = unit_id),
                                alpha = 0.5)
unitgg <- unitgg + geom_point(mapping = aes(x = m_age))
unitgg <- unitgg + geom_point(data = taxon,
                              mapping = aes(x = mid_ma, 
                                            y = unit, 
                                            colour = NULL), 
                              shape = 4, alpha = 0.1, size = 1.1)
unitgg <- unitgg + facet_wrap( ~ group)
unitgg <- unitgg + labs(x = 'Mya', y = 'macrostrat unit')
unitgg <- unitgg + geom_vline(xintercept = hirnantian, 
                              linetype = 'dashed',
                              colour = 'blue')  # start of hirnantian
unitgg <- unitgg + coord_cartesian(xlim = ord)  # zoom in on ordovician
unitgg <- unitgg + theme(axis.text.y = element_blank(),
                         axis.ticks.y = element_blank())
unitgg <- unitgg + scale_colour_manual(values = blind)

# now figure out logical breaks
timerange <- abs(diff(ord))
brks <- timerange / 10
brks <- data.frame(x = seq(from = ord[2], to = ord[1], by = brks))
unitgg <- unitgg + geom_vline(data = brks, 
                              mapping = aes(xintercept = x), 
                              linetype = 'dotted', 
                              colour = 'darkred')
# mark beginning, end ordovician
odf <- data.frame(x = ord)
unitgg <- unitgg + geom_vline(data = odf, 
                              mapping = aes(xintercept = x),
                              colour = 'black', size = 1.5)
ggsave(plot = unitgg, filename = '../doc/figure/data_plot_brach.png',
       height = 5.5, width = 8)
