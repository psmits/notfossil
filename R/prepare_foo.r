# extract the important count information
get_values <- function(taxon, strat, shelly) {
  # which occurrences correspond to focal group
  focus <- taxon[taxon$group == shelly, ]
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

  oc <- stratfocus %>%
    group_by(unit_id) %>%
    dplyr::transmute(occurrence = sum(pbdb_occurrences))
  stratfocus <- left_join(stratfocus, oc, by = 'unit_id')
 
  unitdiv <- llply(split(focus, focus$unit), nrow)
  mm <- match(stratfocus$unit_id, names(unitdiv))
  stratfocus$diversity <- unlist(unitdiv)[mm]

  stratfocus
}

# export focus to walk on
export_standata <- function(x, name, type = c('diversity', 'occurrence')) {
  litmat <- strict.lithology(x)
  bpod <- x[match(rownames(litmat), x$unit_id), ]

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
  

  X <- cbind(X, ilr(litmat))
  K <- ncol(X)
  standata$X <- X
  standata$K <- K

  if(name %in% c('Anthozoa', 'Bivalvia')) { 
    standata$prior_intercept_location <- 1
    standata$prior_intercept_scale <- 2
    standata$prior_phi_scale <- 3
  } else {
    standata$prior_intercept_location <- 2
    standata$prior_intercept_scale <- 2
    standata$prior_phi_scale <- 5
  }

  if(type == 'occurrence') {
    standata$prior_intercept_location <- standata$prior_intercept_location * 10
    standata$prior_intercept_location <- standata$prior_intercept_scale * 10
    standata$prior_phi_scale <- standata$prior_phi_scale * 10
  }


  temp.name <- paste0('../data/data_dump/diversity_data_', 
                      name, '_', type, '.data.R')
  with(standata, {stan_rdump(list = alply(names(standata), 1),
                             file = temp.name)})
  temp.name <- paste0('../data/data_dump/diversity_image_', 
                      name, '_', type, '.rdata')
  save(standata, file = temp.name)
}
