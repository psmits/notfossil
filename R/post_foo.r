# posterior predictive simulations
# lots of internal IO
postchecks<- function(shelly, nsim, silent = FALSE) {
  grab <- sample(4000, nsim)
  
  load(paste0('../data/data_dump/diversity_image_', shelly, '.rdata'))
  pat <- paste0('trunc\\_[0-9]\\_', shelly)
  files <- list.files('../data/mcmc_out', pattern = pat, full.names = TRUE)
  fit <- read_stan_csv(files)
  if(!silent) {
    check_all_diagnostics(fit, max_depth = 15)
  }
  post <- extract(fit, permuted = TRUE)

  ppc <- list()
  for(jj in seq(nsim)) {
    gg <- grab[jj]
    oo <- c()
    for(ii in seq(standata$N)) {
      oo[ii] <- rztnbinom(1, mu = post$location[gg, ii], theta = post$phi[gg])
    }
    ppc[[jj]] <- oo
  }

  # posterior predictive checks
  checks <- single.checks(standata$y, ppc)
  checks.time <- group.checks(standata$y, ppc, group = standata$t)
  out <- list(data = standata, post = post,
                    checks = checks, checks.time = checks.time)
  out
}




q75 <- function(x) quantile(x, 0.75)
q25 <- function(x) quantile(x, 0.25)

# internal checks
single.checks <- function(y, ppc) {
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
group.checks <- function(y, ppc, group) {
  ppc <- Reduce(rbind, ppc)

  # by group
  ppc.bars.group <- ppc_bars_grouped(y, ppc, group = group)
  ppc.mean.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'mean')
  ppc.sd.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'sd')
  ppc.max.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'max')
  ppc.q25.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'q25')
  ppc.q75.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'q75')
  ppc.avgerr.group <- ppc_scatter_avg_grouped(y, ppc, group = group)

  # all the plots
  out <- list(bar.group = ppc.bars.group,
              mean.group = ppc.mean.group,
              sd.group = ppc.sd.group,
              max.group = ppc.max.group,
              q25.group = ppc.q25.group,
              q75.group = ppc.q75.group,
              avgerr.group = ppc.avgerr.group
              )
  out
}
