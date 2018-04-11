# posterior predictive simulations
postpred <- function(post, sim) {
  N <- ncol(post$location)
  grab <- sample(4000, nsim)
  ppc <- list()
  for(jj in seq(nsim)) {
    gg <- grab[jj]
    oo <- c()
    for(ii in seq(N)) {
      oo[ii] <- rztnbinom(1, mu = post$location[gg, ii], theta = post$phi[gg])
    }
    ppc[[jj]] <- oo
  }
  ppc
}

# summary statistic for time points from posterior predictive dist
get_postpredstat <- function(shelly, nsim, foo = mean) {
  out <- list()
  for(ii in seq(length(shelly))) {
    load(paste0('../data/data_dump/diversity_image_', shelly[ii], '.rdata'))
    pat <- paste0('trunc\\_[0-9]\\_', shelly[ii])
    files <- list.files('../data/mcmc_out', pattern = pat, full.names = TRUE)
    fit <- read_stan_csv(files)
    post <- rstan::extract(fit, permuted = TRUE)
    pp <- postpred(post, nsim)

    by.time <- llply(pp, function(x) split(x, standata$t))
    mean.time <- llply(by.time, function(x) laply(x, foo))

    mean.time <- llply(mean.time, function(x) {
                         x <- cbind(x, seq(nrow(brks)))
                         x})

    mean.time <- Reduce(rbind, 
                        Map(function(x, y) 
                            cbind(x, y), mean.time, seq(nsim)))
    mean.time <- data.frame(mean.time, g = shelly[ii])
    names(mean.time) <- c('est', 'time', 'sim', 'g')
    out[[ii]] <- mean.time
  }
  names(out) <- shelly

  out                                  # list by taxonomic group
}


# posterior predictive checks
# lots of internal IO
postchecks<- function(shelly, nsim, silent = FALSE) {
  load(paste0('../data/data_dump/diversity_image_', shelly, '.rdata'))
  pat <- paste0('trunc\\_[0-9]\\_', shelly)
  files <- list.files('../data/mcmc_out', pattern = pat, full.names = TRUE)
  fit <- read_stan_csv(files)
  if(!silent) {
    check_all_diagnostics(fit, max_depth = 15)
  }
  post <- rstan::extract(fit, permuted = TRUE)

  ppc <- postpred(post, nsim)

  # posterior predictive checks
  checks <- checks.single(standata$y, ppc)
  checks.time <- checks.group(standata$y, ppc, group = standata$t)
  out <- list(data = standata, post = post,
              checks = checks, checks.time = checks.time)
  out
}




q75 <- function(x) quantile(x, 0.75)
q25 <- function(x) quantile(x, 0.25)

# internal checks
checks.single <- function(y, ppc) {
  ppc <- Reduce(rbind, ppc)

  # density
  ppc.dens <- ppc_dens_overlay(y, ppc[1:50, ])
  ppc.hist <- ppc_hist(y, ppc[1:5, ])

  # point estimates
  ppc.mean <- ppc_stat(y, ppc, stat = 'mean')
  ppc.sd <- ppc_stat(y, ppc, stat = 'sd')
  ppc.max <- ppc_stat(y, ppc, stat = 'max')
  #prop_zero <- function(x) mean(x == 0)
  #ppc.zero <- ppc_stat(y, ppc, stat = 'prop_zero')
  ppc.q25 <- ppc_stat(y, ppc, stat = 'q25')
  ppc.q75 <- ppc_stat(y, ppc, stat = 'q75')

  # error
  ppc.err <- ppc_error_scatter(y, ppc[1:6, ])
  ppc.avgerr <- ppc_error_scatter_avg(y, ppc)
  ppc.ecdf <- ppc_ecdf_overlay(y, ppc[1:50, ])

  # rootograms because i find them easy to read
  root.gg <- ppc_rootogram(y, ppc, style = 'hanging', prob = 0.8)

  # bars
  ppc.bars <- ppc_bars(y, ppc)

  # output
  out <- list(mean = ppc.mean, 
              sd = ppc.sd, 
              max = ppc.max, 
              #zero = ppc.zero, 
              q25 = ppc.q25,
              q75 = ppc.q75,
              err = ppc.err, 
              avgerr = ppc.avgerr, 
              ecdf = ppc.ecdf, 
              root = root.gg,
              bar = ppc.bars,
              dens = ppc.dens,
              hist = ppc.hist)
  out
}

# internal checks
checks.group <- function(y, ppc, group) {
  ppc <- Reduce(rbind, ppc)

  # by group
  ppc.bars.group <- ppc_bars_grouped(y, ppc, group = group)
  ppc.mean.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'mean')
  ppc.sd.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'sd')
  ppc.max.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'max')
  ppc.q25.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'q25')
  ppc.q75.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'q75')
  ppc.avgerr.group <- ppc_scatter_avg_grouped(y, ppc, group = group)
  ppc.violin.group <- ppc_violin_grouped(y, ppc, group = group, 
                                         y_draw = 'points')

  # all the plots
  out <- list(bar.group = ppc.bars.group,
              mean.group = ppc.mean.group,
              sd.group = ppc.sd.group,
              max.group = ppc.max.group,
              q25.group = ppc.q25.group,
              q75.group = ppc.q75.group,
              avgerr.group = ppc.avgerr.group,
              violin.group = ppc.violin.group
              )
  out
}


