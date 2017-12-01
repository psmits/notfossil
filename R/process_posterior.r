# manipulation
library(reshape2)
library(plyr)

# analysis
library(compositions)
library(arm)
library(rstan)
library(coda)
library(loo)

# plotting
library(ggplot2)
library(scales)
library(grid)
library(ggridges)
library(bayesplot)

# helpful functions
source('stan_utility.R')
source('sim_hurdle.r')
source('post_foo.r')

# set up data
nsim <- 1000
grab <- sample(4000, nsim)
shelly <- c('Brachiopoda', 'Arthropoda', 'Mollusca')

out <- list()
for(kk in seq(length(shelly))) {

  load(paste0('../data/data_dump/unit_image_', shelly[kk], '.rdata'))

  files <- list.files('../data/mcmc_out', pattern = shelly[kk], 
                      full.names = TRUE)

  # pois
  fit <- read_stan_csv(files[5:8])
  check_all_diagnostics(fit)
  check_treedepth(fit, max_depth = 15)
  post <- rstan::extract(fit, permuted = TRUE)

  # nb
  fit2 <- read_stan_csv(files[1:4])
  check_all_diagnostics(fit2)
  check_treedepth(fit2, max_depth = 15)
  post2 <- rstan::extract(fit2, permuted = TRUE)

  # training set
  # loo and waic
  postlik <- extract_log_lik(fit)
  postloo <- loo(postlik)
  postwaic <- waic(postlik)

  post2lik <- extract_log_lik(fit2)
  post2loo <- loo(post2lik)
  post2waic <- waic(post2lik)

  #hurdleloo <- compare(postloo, post2loo)
  #hurdlewaic <- compare(postwaic, post2waic)


  # posterior predictive simulations for train set
  ppc.p <- list()
  for(jj in seq(nsim)) {
    gg <- grab[jj]
    oo <- c()
    for(ii in seq(standata$N_train)) {
      oo[ii] <- rhurdle(1, 
                        theta = post$theta[gg, ii], 
                        lambda = post$lambda_est[gg, ii])
    }
    ppc.p[[jj]] <- oo
  }
  ppc.nb <- list()
  for(jj in seq(nsim)) {
    gg <- grab[jj]
    oo <- c()
    for(ii in seq(standata$N_train)) {
      oo[ii] <- roverhurdle(1, 
                            theta = post2$theta[gg, ii], 
                            mu = post2$lambda_est[gg, ii],
                            omega = post2$phi[gg])
    }
    ppc.nb[[jj]] <- oo
  }

  # internal checks
  psis <- psislw(-extract_log_lik(fit), cores = 2)
  lw = psis$lw_smooth[grab, ]
  psis2 <- psislw(-extract_log_lik(fit2), cores = 2)
  lw2 = psis2$lw_smooth[grab, ]
  po.check <- series.checks(standata$y_train, ppc.p, lw)
  nb.check <- series.checks(standata$y_train, ppc.nb, lw2)


  # visualize regression coefs
  po.vis <- post.vis(post, unit.info)
  nb.vis <- post.vis(post2, unit.info)


  # testing set
  # posterior predictive simulations for test set
  pre.p <- list()
  for(jj in seq(nsim)) {
    gg <- grab[jj]
    oo <- c()
    for(ii in seq(standata$N_test)) {
      oo[ii] <- rhurdle(1, 
                        theta = post$theta_test[gg, ii], 
                        lambda = post$lambda_test[gg, ii])
    }
    pre.p[[jj]] <- oo
  }
  pre.nb <- list()
  for(jj in seq(nsim)) {
    gg <- grab[jj]
    oo <- c()
    for(ii in seq(standata$N_test)) {
      oo[ii] <- roverhurdle(1, 
                            theta = post2$theta_test[gg, ii], 
                            mu = post2$lambda_test[gg, ii],
                            omega = post2$phi[gg])
    }
    pre.nb[[jj]] <- oo
  }
  po.test <- series.checks(standata$y_test, pre.p)
  nb.test <- series.checks(standata$y_test, pre.nb)

  out[[kk]] <- list(data = standata,
                    posteriors = list(po = post, nb = post2),
                    loo = list(po = postloo, nb = post2loo), 
                    waic = list(po = postwaic, nb = post2waic),
                    po.check = po.check, nb.check = nb.check,
                    po.vis = po.vis, nb.vis = nb.vis,
                    po.test = po.test, nb.test = nb.test)
}
names(out) <- shelly

pdf(file = '../doc/figure/ppc_mean.pdf', height = 10, width = 8)
bayesplot_grid(out$Arthropoda$po.check$mean, out$Arthropoda$nb.check$mean,
               out$Brachiopoda$po.check$mean, out$Brachiopoda$nb.check$mean,
               out$Mollusca$po.check$mean, out$Mollusca$nb.check$mean,
               grid_args = list(nrow = 3, ncol = 2),
               titles = c('Arth_po', 'Arth_nb', 'Brach_po', 'Brach_nb',
                          'Mol_po', 'Mol_nb'))
dev.off()

pdf(file = '../doc/figure/ppc_sd.pdf', height = 10, width = 8)
bayesplot_grid(out$Arthropoda$po.check$sd, out$Arthropoda$nb.check$sd,
               out$Brachiopoda$po.check$sd, out$Brachiopoda$nb.check$sd,
               out$Mollusca$po.check$sd, out$Mollusca$nb.check$sd,
               grid_args = list(nrow = 3, ncol = 2),
               titles = c('Arth_po', 'Arth_nb', 'Brach_po', 'Brach_nb',
                          'Mol_po', 'Mol_nb'))
dev.off()

pdf(file = '../doc/figure/ppc_ecdf.pdf', height = 10, width = 8)
bayesplot_grid(out$Arthropoda$po.check$ecdf, out$Arthropoda$nb.check$ecdf,
               out$Brachiopoda$po.check$ecdf, out$Brachiopoda$nb.check$ecdf,
               out$Mollusca$po.check$ecdf, out$Mollusca$nb.check$ecdf,
               grid_args = list(nrow = 3, ncol = 2),
               titles = c('Arth_po', 'Arth_nb', 'Brach_po', 'Brach_nb',
                          'Mol_po', 'Mol_nb'))
dev.off()

pdf(file = '../doc/figure/ppc_root.pdf', height = 10, width = 8)
bayesplot_grid(out$Arthropoda$po.check$root, out$Arthropoda$nb.check$root,
               out$Brachiopoda$po.check$root, out$Brachiopoda$nb.check$root,
               out$Mollusca$po.check$root, out$Mollusca$nb.check$root,
               grid_args = list(nrow = 3, ncol = 2),
               titles = c('Arth_po', 'Arth_nb', 'Brach_po', 'Brach_nb',
                          'Mol_po', 'Mol_nb'))
dev.off()

pdf(file = '../doc/figure/ppc_avgerr.pdf', height = 10, width = 8)
bayesplot_grid(out$Arthropoda$po.check$avgerr, out$Arthropoda$nb.check$avgerr,
               out$Brachiopoda$po.check$avgerr, out$Brachiopoda$nb.check$avgerr,
               out$Mollusca$po.check$avgerr, out$Mollusca$nb.check$avgerr,
               grid_args = list(nrow = 3, ncol = 2),
               titles = c('Arth_po', 'Arth_nb', 'Brach_po', 'Brach_nb',
                          'Mol_po', 'Mol_nb'))
dev.off()

pdf(file = '../doc/figure/ppc_avgerr.pdf', height = 10, width = 8)
bayesplot_grid(out$Arthropoda$po.check$loo.pit, out$Arthropoda$nb.check$loo.pit,
               out$Brachiopoda$po.check$loo.pit, out$Brachiopoda$nb.check$loo.pit,
               out$Mollusca$po.check$loo.pit, out$Mollusca$nb.check$loo.pit,
               grid_args = list(nrow = 3, ncol = 2),
               titles = c('Arth_po', 'Arth_nb', 'Brach_po', 'Brach_nb',
                          'Mol_po', 'Mol_nb'))
dev.off()
