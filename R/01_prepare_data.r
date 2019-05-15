# taxon variable is shelly
library(pacman)

p_load(here, tidyverse, readr, dplyr, purrr, reshape2,
       arm, rstan, rstanarm, 
       compositions, geosphere)

source(here::here('helper01_prepare_foo.r'))
source(here::here('helper02_rock_process.r'))

#   fossil.ord # fossil occurrences in the ordovician
#   strat.ord # fossil occurrences in the ordovician

# formerly `strat`
unit_data <- read_csv(here::here('data', 'macrostrat_units.csv'))

# formerly `fossil`
fossil_data <- read_csv(here::here('data', 'macrostrat_fossils.csv'))

# formerly `taxon
pbdb_data <- read_csv(here::here('data', 'pbdb_occurrences.csv'))


# useful constants
constant <- 20
shelly <- c('Brachiopoda', 'Anthozoa', 'Trilobita', 
            'Bivalvia', 'Gastropoda')#, 'Cephalopoda')
ord <- c(460.4, 427.4)
hirnantian <- 445.6


# join units with their fossils
# multiple fossils per unit
# some units have no fossils
# lets attach the information to the fossils
fossil_unit <- left_join(x = fossil_data, y = unit_data, 
                         by = c('unit_id', 'col_id', 't_age', 'b_age')) %>%
  dplyr::select(-(genus_no:taxon_no), refs.y) %>%
  mutate(m_age = (t_age + b_age) / 2)  # mid age of unit

fossil_unit


# filter to just the taxa i want
pbdb_data %>%
  filter(phylum %in% shelly | class %in% shelly)

### here



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
