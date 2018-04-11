# plot of diversity through time compared to mean estimated from posterior
# lots of io but puts out a plot
plot_divtime <- function(shelly, brks, vert) {
  midpoint <- apply(brks, 1, mean)
  # plot up unit div through time vs our estimate of average
  cc <- list()
  for(ii in seq(length(shelly))) {
    load(paste0('../data/data_dump/diversity_image_', shelly[ii], '.rdata'))
    cc[[ii]] <- data.frame(x = standata$t, y = standata$y, g = shelly[ii])
  }
  cc <- Reduce(rbind, cc)
  # put in terms of Mya
  cc$x <- mapvalues(cc$x, sort(unique(cc$x)), midpoint)

  cg <- ggplot(cc, aes(x = x, y = y)) 
  cg <- cg + geom_hline(yintercept = 0, colour = 'darkgrey')
  cg <- cg + geom_vline(xintercept = vert, alpha = 0.5, linetype = 'dashed')
  cg <- cg + geom_vline(xintercept = 443.8, alpha = 0.5, linetype = 'dotdash')
  cg <- cg + geom_count(alpha = 0.5, 
                        position = position_jitter(width = 0.1, height = 0))
  cg <- cg + facet_grid(g ~ .)

  mean.time <- get_postpredstat(shelly, nsim, mean)
  mean.time <- purrr::reduce(mean.time, rbind)

  by.time <- group_by(mean.time, time, g)
 
  time.mean <- summarise(by.time, mean = mean(est), 
                         low = quantile(est, 0.1), 
                         high = quantile(est, 0.9))

  # put in terms of Mya
  time.mean$time <- mapvalues(time.mean$time, 
                              sort(unique(time.mean$time)), 
                              midpoint)
  cg <- cg + geom_pointrange(data = time.mean, 
                             mapping = aes(x = time, y = mean, 
                                           ymin = low, ymax = high),
                             colour = 'blue', size = 1.5, 
                             fatten = 2, alpha = 0.75)
  cg <- cg + scale_x_reverse()
  cg <- cg + labs(x = 'Time (Mya)', y = 'geological unit diversity')
  cg
}



# covariates through time
plot_covtime <- function(shelly, brks, covname, vert) {
  # covariate effects
  out <- out2 <- list()
  for(ii in seq(length(shelly))) {
    load(paste0('../data/data_dump/diversity_image_', shelly[ii], '.rdata'))
    pat <- paste0('trunc\\_[0-9]\\_', shelly[ii])
    files <- list.files('../data/mcmc_out', pattern = pat, full.names = TRUE)
    fit <- read_stan_csv(files)
    post <- rstan::extract(fit, permuted = TRUE)


    # violins
    mm <- melt(post$beta)
    mm[, 3] <- mapvalues(mm[, 3], unique(mm[, 3]), covname)
    names(mm) <- c('iter', 'time', 'covariate', 'value')
    mm$g <- shelly[ii]


    # point range
    betamean <- apply(post$beta, 2:3, mean)
    betalow <- apply(post$beta, 2:3, function(x) quantile(x, 0.1))
    betahigh <- apply(post$beta, 2:3, function(x) quantile(x, 0.9))
    betaest <- list(betamean, betalow, betahigh)
    betaest <- llply(betaest, function(x) {
                       colnames(x) <- covname
                       x})

    betaest <- llply(betaest, melt)
    betaest <- data.frame(betaest[[1]], 
                          low = betaest[[2]]$value, 
                          high = betaest[[3]]$value,
                          g = shelly[ii])
    names(betaest)[1:2] <- c('time', 'covariate')

    midpoint <- apply(brks, 1, mean)
    betaest$time <- mapvalues(betaest$time, 
                              sort(unique(betaest$time)), 
                              midpoint)
    mm$time <- mapvalues(mm$time, sort(unique(mm$time)), midpoint)

    out[[ii]] <- betaest
    out2[[ii]] <- mm
  }
  betaest <- Reduce(rbind, out)
  betaviol <- Reduce(rbind, out2)
  betaviol$covariate <- factor(betaviol$covariate, 
                               levels = levels(betaest$covariate))
  betaviol <- betaviol %>%
    group_by(covariate, g, time) %>%
    dplyr::mutate(p = sum(value > 0) / length(value)) %>%
    ungroup()

  mg <- ggplot(betaest, aes(x = time, y = value))
  mg <- mg + geom_hline(yintercept = 0, colour = 'darkgrey')
  mg <- mg + geom_vline(xintercept = vert, alpha = 0.5, linetype = 'dashed')
  mg <- mg + geom_vline(xintercept = 443.8, alpha = 0.5, linetype = 'dotdash')
  mg <- mg + geom_violin(data = betaviol, 
                         mapping = aes(x = time, y = value, group = time, 
                                       fill = p, colour = p), 
                         alpha = 0.5)
  mg <- mg + geom_pointrange(mapping = aes(ymin = low, ymax = high), fatten = 2)
  mg <- mg + facet_grid(g ~ covariate, scales = 'free_y')
  mg <- mg + scale_x_reverse()
  mg <- mg + scale_fill_distiller(name = 'Probability > 0', 
                                  palette = 'RdBu', limits = c(0, 1))
  mg <- mg + scale_colour_distiller(name = 'Probability > 0', 
                                    palette = 'RdBu', limits = c(0, 1))
  mg <- mg + labs(x = 'Time (Mya)', y = 'estimated regression coefficient')
  mg
}


# look at differences in effects through time 
plot_diffbeta <- function(shelly, covname) {

  get_diffbeta <- function(x, cova = 1) {
    dd <- dim(x)
    ds1 <- seq(dd[2] - 1)
    ds2 <- ds1 + 1

    nc <- seq(length(ds1))
    o <- purrr::map(nc, ~ x[, ds1[.x], cova] - x[, ds2[.x], cova])
    o
  }
  get_covdiff <- function(shelly) {
    out <- list()
    for(ii in seq(length(shelly))) {
      pat <- paste0('trunc\\_[0-9]\\_', shelly[ii])
      files <- list.files('../data/mcmc_out', pattern = pat, full.names = TRUE)
      fit <- read_stan_csv(files)
      post <- rstan::extract(fit, permuted = TRUE)

      nc <- seq(dim(post$beta)[3])
      out[[ii]] <- purrr::map(nc, ~ get_diffbeta(post$beta, cova = .x))
    }
    names(out) <- shelly
    out
  }

  cov_timepairs <- get_covdiff(shelly)

  mctp <- melt(cov_timepairs)
  names(mctp) <- c('value', 'timepair', 'covariate', 'taxon')
  mctp <- mctp %>% 
    group_by(timepair, covariate, taxon) %>%
    dplyr::summarize(p = sum(value > 0) / length(value))
  
  mctp$covariate <- with(mctp, {
                           plyr::mapvalues(covariate, 
                                           from = unique(covariate),
                                           to = covname)})

  prsss <- as.character(interaction(seq(from = 1, to = nrow(brks) - 1), 
                                    seq(from = 2, to = nrow(brks))))
  mctp$timepair <- with(mctp, {
                          plyr::mapvalues(timepair, 
                                          from = sort(unique(timepair)),
                                          to = prsss)})
  mctp$timepair <- factor(mctp$timepair, levels = prsss)

  # value, time pair, covariate, taxon
  mctp
  mct <- ggplot(mctp, aes(x = timepair, y = p, group = taxon)) + 
    geom_line() +
    geom_hline(yintercept = 0.9, linetype = 'dashed', alpha = 0.25) +
    geom_hline(yintercept = 0.1, linetype = 'dashed', alpha = 0.25) +
    facet_grid(taxon ~ covariate) +
    labs(x = 'time pair', y = 'probability')

  mct
}


# covariate effects at the hirnantian
compare_hirbeta <- function(shelly, hirnantian = 445.6, brks) {
  tc <- which(brks == hirnantian)[1]

  ordprob <- silprob <- list()
  for(ii in seq(length(shelly))) {
    pat <- paste0('trunc\\_[0-9]\\_', shelly[ii])
    files <- list.files('../data/mcmc_out', pattern = pat, full.names = TRUE)
    fit <- read_stan_csv(files)
    post <- rstan::extract(fit, permuted = TRUE)

    dd <- dim(post$beta) # sim, time, covariate
    betahir <- post$beta[, tc, ]
    betaord <- post$beta[, seq(tc - 1), ]
    betasil <- post$beta[, seq(from = tc + 1, to = dd[2]), ]
    # which are which? 

    # compare to hirnantian
    op <- sp <- c()
    for(jj in seq(dd[3])) {
      # for each covariate
      # ordovician
      tisp <- dim(betaord)
      compa <- purrr::map(seq(tisp[2]), ~ betaord[, .x, jj] > betahir[, jj])
      op[jj] <- sum(purrr::map_dbl(compa, ~ sum(.x))) / length(betaord[, , jj])

      # silurian
      tisp <- dim(betasil)
      compa <- purrr::map(seq(tisp[2]), ~ betasil[, .x, jj] > betahir[, jj])
      sp[jj] <- sum(purrr::map_dbl(compa, ~ sum(.x))) / length(betasil[, , jj])
    }
    ordprob[[ii]] <- op
    silprob[[ii]] <- sp
  }
  names(ordprob) <- names(silprob) <- shelly
  out <- transpose(list(ordivician = ordprob, 
                        silurian = silprob))
  out
}



plot_diffdiv <- function(shelly, nsim, foo = mean, brks) {
  # simulate from posterior, calculate mean for each time point for each sim
  mm <- get_postpredstat(shelly, nsim, foo)
  
  #mm <- purrr::reduce(mm, rbind)
  # g is taxonomic group

  get_diffdiv <- function(x) {
    xt <- split(x, x$time)
    nn <- x %>% dplyr::summarize(n = n_distinct(time)) %>% unlist()
    d1 <- seq(from = 1, to = nn - 1)
    d2 <- seq(from = 2, to = nn)
    nc <- seq(length(d1))
    out <- purrr::map(nc, ~ xt[[d1[.x]]]$est - xt[[d2[.x]]]$est) %>%
      purrr::map_dbl(., ~ sum(.x > 0) / length(.x))

    out
  }
  o <- purrr::map(mm, get_diffdiv)
  
  # give the times for plotting
  nn <- nrow(brks)
  d1 <- seq(from = 1, to = nn - 1)
  d2 <- seq(from = 2, to = nn)
  prsss <- as.character(interaction(d1, d2))
  fp <- purrr::map(o, ~ tibble(p = .x, comp = prsss)) %>% 
    bind_rows(.id = 'taxon')

  fp$taxon <- factor(fp$taxon, levels = unique(shelly))
  fp$comp <- factor(fp$comp, levels = prsss)
  fpg <- ggplot(fp, aes(x = comp, y = p, group = taxon)) +
    geom_line() + 
    geom_hline(yintercept = 0.9, linetype = 'dashed', alpha = 0.25) +
    geom_hline(yintercept = 0.1, linetype = 'dashed', alpha = 0.25) +
    facet_grid(taxon ~ .) +
    labs(x = 'time pair', y = 'probability')

  fpg

}

compare_hirdiv <- function(shelly, hirnantian = 445.6, brks, nsim, foo = mean) {
#compare_hirbeta <- function(shelly, hirnantian = 445.6, brks) {
  mm <- get_postpredstat(shelly, nsim, foo)
  
  # useful values to have around
  tc <- which(brks == hirnantian)[1] # the hirnantian
  dd <- nrow(brks) # time steps

  get_pval <- function(x, divhir) {
    xs <- split(x, x$time)
    compa <- purrr::map(xs, ~ .x$est > divhir$est)
    ca <- purrr::map_dbl(compa, ~ sum(.x))
    sum(ca) / length(unlist(compa))
  }
 
  # use get_pval to get ord vs hir, sil vs hir
  split_extract <- function(x, tc, dd) {
    divhir <- x[x$time == tc, ]
    divord <- x[x$time %in% seq(tc - 1), ]
    divsil <- x[x$time %in% seq(from = tc + 1, to = dd), ]

    op <- get_pval(divord, divhir)
    os <- get_pval(divsil, divhir)
    out <- list(ordovician = op, 
                silurian = os)
    out
  }

  # get the (unlabeled) pvalues
  pv <- purrr::map(mm, ~ split_extract(.x, tc, dd))
  pv
}
