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
load(file = '../data/data_dump/unit_image.rdata')

nsim <- 1000
grab <- sample(4000, nsim)

files <- list.files('../data/mcmc_out', pattern = 'hurdle', full.names = TRUE)

# pois
fit <- read_stan_csv(files[13:16])
check_all_diagnostics(fit)
check_treedepth(fit, max_depth = 15)
post <- rstan::extract(fit, permuted = TRUE)

# nb
fit2 <- read_stan_csv(files[9:12])
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

hurdleloo <- compare(postloo, post2loo)
hurdlewaic <- compare(postwaic, post2waic)


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
po.check <- series.checks(standata$y_train, ppc.p)
nb.check <- series.checks(standata$y_train, ppc.nb)


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
