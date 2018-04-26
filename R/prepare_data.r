# taxon variable is shelly
library(arm)
library(plyr)
library(tidyverse)
library(compositions)
library(geosphere)
library(reshape2)
library(rstan)
library(caret)
library(dplyr)
source('../R/rock_mung.r')
source('../R/prepare_foo.r')

# bring in data
#   fossil.ord # fossil occurrences in the ordovician
#   strat.ord # fossil occurrences in the ordovician
source('../R/download_scrap.r')  # just macrostrat

# constants
constant <- 20
shelly <- c('Brachiopoda', 'Anthozoa', 'Trilobita', 
            'Bivalvia', 'Gastropoda')#, 'Cephalopoda')
ord <- c(460.4, 427.4)
hirnantian <- 445.6


# mid age of unit
strat$m_age <- apply(strat[, c('t_age', 'b_age')], 1, mean)

# associate each fossil occurrence with a macrostrat unit
cltn2unit <- fossil[, c('cltn_id', 'unit_id')]
taxon$unit <- cltn2unit[match(taxon$collection_no, cltn2unit[, 1]), 2]

# filter to just the taxa i want
taxon <- taxon[taxon$phylum %in% shelly | taxon$class %in% shelly, ]

# put occurrences at middle of unit
ss <- fossil[, c('unit_id', 'b_age', 't_age')]
ss$mid <- apply(ss[, 2:3], 1, mean)
taxon$mid_ma <- ss$mid[match(taxon$unit, ss$unit_id)]

# don't double count
tt <- split(taxon, as.character(taxon$unit))
tt <- llply(tt, function(x) x[!duplicated(x$genus), ])
taxon <- Reduce(rbind, tt)

taxon$group <- NA
for(ii in seq(length(shelly))) {
  taxon$group[taxon$phylum %in% shelly[ii]] <- shelly[ii]
  taxon$group[taxon$class %in% shelly[ii]] <- shelly[ii]
}

# only fossil bearing units
strat <- strat[strat$unit_id %in% taxon$unit, ]  # does unit have fossils

# keep uits that have their mid point in time range
strat <- strat[strat$m_age < ord[1] & strat$m_age > ord[2], ]

# bin data
# figure out logical breaks
ra <- range(strat$m_age)
timerange <- abs(diff(ra))
#timerange <- abs(diff(ord))
rr <- timerange / constant
brks1 <- seq(from = hirnantian, to = ra[2] + 1, by = rr) # 
brks2 <- seq(from = hirnantian, to = ra[1] - 1, by = -rr) # 
brks <- c(rev(brks2[-1]), brks1)
brks <- cbind(brks[-1], brks[-length(brks)])
brks <- brks[rev(seq(nrow(brks))), ]
write_rds(brks, path = '../data/breaks.rds')

# one pesky observation that doesn't play nice with bins
strat <- strat[strat$m_age > min(brks), ]


# get the count information from the data
out <- purrr::map(shelly, ~ get_values(taxon, strat, .x))
names(out) <- shelly


# make a mollusc category after the fact
tt <- full_join(out$Bivalvia, out$Gastropoda)
tt <- tt %>%
  group_by(unit_id, bin) %>%
  dplyr::mutate(diversity = sum(diversity),
                collections = sum(collections)) %>%
  ungroup() %>%
  distinct(unit_id, .keep_all = TRUE)
out$Mollusca <- tt
shelly <- names(out)

out <- purrr::map(out, function(x) {
                    m <- x$unit_id != 19112
                    x <- x[m, ]
                    x})

# do all the exporting
# partials make this easier, imo
esd <- partial(export_standata, type = 'diversity')
purrr::walk2(out, shelly, esd)
eso <- partial(export_standata, type = 'occurrence')
purrr::walk2(out, shelly, eso)
