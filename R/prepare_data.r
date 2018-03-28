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

# bring in data
#   fossil.ord # fossil occurrences in the ordovician
#   strat.ord # fossil occurrences in the ordovician
source('../R/download_scrap.r')  # just macrostrat
unique(taxon[taxon$class == 'Cephalopoda', 'order'])
sort(unique(taxon$phylum))
sort(unique(taxon$class))
sort(unique(taxon$order))

# constants
constant <- 20
shelly <- c('Brachiopoda', 'Anthozoa', 'Trilobita', 
            'Bivalvia', 'Gastropoda', 'Cephalopoda')
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


out <- list()
for(jj in seq(length(shelly))) {
  # which occurrences correspond to focal group
  focus <- taxon[taxon$group == shelly[jj], ]
  # these occurrences are in collections
  # collections are from units
  # geologic units with fossils from focal
  unitfocus <- cltn2unit[cltn2unit[, 1] %in% focus$collection_no, 2]  
  stratfocus <- strat[strat$unit_id %in% unitfocus, ]

  # assign a bin
  stratfocus$bin <- NA
  for(ii in seq(nrow(brks))) {
    mm <- stratfocus$m_age <= brks[ii, 1] & stratfocus$m_age > brks[ii, 2]
    stratfocus$bin[mm] <- ii
  }

  unitdiv <- llply(split(focus, focus$unit), nrow)
  mm <- match(stratfocus$unit_id, names(unitdiv))
  stratfocus$diversity <- unlist(unitdiv)[mm]

  out[[jj]] <- stratfocus
  out[[jj]]$taxon <- shelly[jj]
}
names(out) <- shelly

# make a mollusc category after the fact
tt <- full_join(out$Bivalvia, out$Gastropoda)
tt <- tt %>%
  group_by(unit_id, bin) %>%
  dplyr::mutate(diversity = sum(diversity)) %>%
  ungroup() %>%
  distinct(unit_id, .keep_all = TRUE)
out$Mollusca <- tt
shelly <- names(out)

#suspicious <- unique(c(arrange(out$Brachiopoda, desc(diversity))[1:10, ]$unit_id,
#                       arrange(out$Trilobita, desc(diversity))[1:10, ]$unit_id,
#                       arrange(out$Bivalvia, desc(diversity))[1:10, ]$unit_id,
#                       arrange(out$Gastropoda, desc(diversity))[1:10, ]$unit_id))
#checktime <- strat[strat$unit_id %in% suspicious, ]
#checktime <- dplyr::select(checktime, 
#                           unit_id, col_area, unit_name, t_age, b_age, 
#                           max_thick, pbdb_collections, pbdb_occurrences)

out <- purrr::map(out, function(x) {
                    m <- x$unit_id != 19112
                    x <- x[m, ]
                    x})

#out$Cephalopoda %>% group_by(bin, taxon) %>% tally()


# get the data in stan format
# each taxon group individually
for(ii in seq(length(out))) {
  bpod <- out[[ii]]
  
  #litmat <- full.lithology(bpod)
  litmat <- strict.lithology(bpod)
  bpod <- bpod[match(rownames(litmat), bpod$unit_id), ]

  standata <- list()
  standata$N <- length(bpod$diversity)
  standata$y <- bpod$diversity
  standata$t <- bpod$bin
  standata$T <- max(bpod$bin)

  # make X
  K <- 3  # all plus intercept; initial
  # initially just intercept
  X <- matrix(1, nrow = standata$N, ncol = K)
  X[, 2] <- arm::rescale(log1p(bpod$max_thick))
  X[, 3] <- arm::rescale(log1p(bpod$col_area))
  #X[, 4] <- ifelse(bpod$units_above != 0, 1, 0)
  #X[, 5] <- ifelse(bpod$units_below != 0, 1, 0)

  ## change in space from bottom to top
  #topcoord <- bpod[, c('t_plng', 't_plat')]
  #botcoord <- bpod[, c('b_plng', 'b_plat')]
  #ch <- distGeo(topcoord, botcoord) / 1000  # km units
  #X[, 4] <- arm::rescale(ch)

  #toptemp <- ifelse(bpod$t_plat > 20 | bpod$t_plat < -2, 1, 0)
  #bottemp <- ifelse(bpod$b_plat > 20 | bpod$b_plat < -2, 1, 0)
  ## initially tropical?
  #X[, 5] <- bottemp
  

  X <- cbind(X, ilr(litmat))
  K <- ncol(X)
  standata$X <- X
  standata$K <- K

  #print(mean(standata$y))
  temp.name <- paste0('../data/data_dump/diversity_data_', shelly[ii], '.data.R')
  with(standata, {stan_rdump(list = alply(names(standata), 1),
                             file = temp.name)})
  temp.name <- paste0('../data/data_dump/diversity_image_', shelly[ii], '.rdata')
  save(standata, file = temp.name)
}
