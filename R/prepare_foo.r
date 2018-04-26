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
  stratfocus$bin <- c()
  for(ii in seq(nrow(brks))) {
    mm <- stratfocus$m_age <= brks[ii, 1] & stratfocus$m_age > brks[ii, 2]
    stratfocus$bin[mm] <- ii
  }

  oc <- stratfocus %>%
    group_by(unit_id) %>%
    dplyr::transmute(collections = sum(pbdb_collections))
  stratfocus <- left_join(stratfocus, oc, by = 'unit_id')
 
  unitdiv <- llply(split(focus, focus$unit), 
                   function(x) length(unique(x$genus)))
  mm <- match(stratfocus$unit_id, names(unitdiv))
  stratfocus$diversity <- unlist(unitdiv)[mm]
  
  stratfocus
}

# export focus to walk on
export_standata <- function(x, name, type = c('diversity', 'occurrence')) {
  x <- out[[1]]
  type <- 'occurrence'
  litmat <- strict.lithology(x)
  bpod <- x[match(rownames(litmat), x$unit_id), ]

  standata <- list()
  if(type == 'diversity') {
    standata$N <- length(bpod$diversity)
    standata$y <- bpod$diversity
  } else if(type == 'occurrence') {
    standata$N <- length(bpod$collections)
    standata$y <- bpod$collections
  }


  standata$t <- bpod$bin
  standata$T <- max(bpod$bin)

  # make X
  K <- 4  # all plus intercept; initial
  # initially just intercept
  X <- matrix(1, nrow = standata$N, ncol = K)
  X[, 2] <- arm::rescale(log1p(bpod$max_thick))
  X[, 3] <- arm::rescale(log1p(bpod$col_area))

  nt <- pmap_dbl(list(bln = bpod$b_plng,
                      bpl = bpod$b_plat,
                      tln = bpod$t_plng,
                      tpl = bpod$t_plat),
                 .f = function(bln, bpl, tln, tpl) 
                   geomean(rbind(c(bln, bpl), c(tln, tpl)))[2])
  X[, 4] <- arm::rescale(nt)

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
    #standata$prior_phi_scale <- 5
  }

  if(type == 'occurrence') {
    standata$prior_intercept_location <- 3
    standata$prior_intercept_scale <- 3
    #standata$prior_phi_scale <- 50
  }


  temp.name <- paste0('../data/data_dump/diversity_data_', 
                      name, '_', type, '.data.R')
  with(standata, {stan_rdump(list = alply(names(standata), 1),
                             file = temp.name)})
  temp.name <- paste0('../data/data_dump/diversity_image_', 
                      name, '_', type, '.rdata')
  save(standata, file = temp.name)
}
