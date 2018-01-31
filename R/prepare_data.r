# this script is called entirely for the side effects 
# taxon variable is shelly
library(arm)
library(plyr)
library(stringr)
library(compositions)
library(geosphere)
library(reshape2)
library(rstan)
library(caret)
#library(glmnet)
#library(rpart)

# bring in data
#   fossil.ord # fossil occurrences in the ordovician
#   strat.ord # fossil occurrences in the ordovician
source('../R/download_scrap.r')  # just macrostrat

# constants
constant <- 10
shelly <- c('Brachiopoda', 'Trilobita', 'Bivalvia', 'Gastropoda')
ord <- c(460.4, 443.8)
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
timerange <- abs(diff(ord))
brks <- timerange / constant
brks <- seq(from = ord[2], to = ord[1], by = brks)
brks <- cbind(brks[-1], brks[-length(brks)])
brks <- brks[rev(seq(nrow(brks))), ]

out <- list()
for(jj in seq(length(shelly))) {
  # which occurrences correspond to focal group
  focus <- taxon[taxon$group == shelly[jj], ]
  # these occurrences are in collections
  # collections are from units
  # geologic units with fossils from focal
  unitfocus <- cltn2unit[cltn2unit[, 1] %in% focus$collection_no, 2]  
  stratfocus <- strat[strat$unit_id %in% unitfocus, ]
  dim(stratfocus)
  length(unique(unitfocus))

  # assign a bin
  stratfocus$bin <- NA
  for(ii in seq(nrow(brks))) {
    mm <- stratfocus$m_age <= brks[ii, 1] & stratfocus$m_age > brks[ii, 2]
    stratfocus$bin[mm] <- ii
  }

  unitdiv <- llply(split(focus, focus$unit), nrow)
  mm <- match(stratfocus$unit_id, names(unitdiv))
  stratfocus$diversity <- unlist(unitdiv)[mm]

  out[[jj]] <- stratfocus[, c('unit_id', 'col_area', 't_age', 'b_age', 
                              'max_thick', 'min_thick', 'lith', 'environ', 
                              'units_above', 'units_below', 'clat', 'clng', 
                              'bin', 'diversity')]
  out[[jj]]$taxon <- shelly[jj]
}
names(out) <- shelly

# get the data in stan format
# ignore covariates for now
# each taxon group individually
for(ii in seq(length(out))) {
  bpod <- out[[ii]][, c('unit_id', 'bin', 'diversity')]

  standata <- list()
  standata$y <- bpod$diversity
  standata$t <- bpod$bin
  standata$N <- length(bpod$diversity)
  standata$T <- max(bpod$bin)
  temp.name <- paste0('../data/data_dump/diversity_data_', shelly[ii], '.data.R')
  with(standata, {stan_rdump(list = alply(names(standata), 1),
                             file = temp.name)})
  temp.name <- paste0('../data/data_dump/diversity_image_', shelly[ii], '.rdata')
  save(standata, file = temp.name)
}

# combined hierarchical dataset
combo <- Reduce(rbind, out)

standata <- list()
standata$d <- as.numeric(factor(combo$taxon))
standata$D <- max(standata$d)
standata$u <- as.numeric(factor(combo$unit_id))
standata$U <- max(standata$u)
standata$y <- combo$diversity
standata$N <- nrow(combo)
standata$t <- combo$bin
standata$T <- max(combo$bin)
temp.name <- paste0('../data/data_dump/diversity_data_full.data.R')
with(standata, {stan_rdump(list = alply(names(standata), 1),
                           file = temp.name)})
temp.name <- paste0('../data/data_dump/diversity_image_full.rdata')
save(standata, file = temp.name)
