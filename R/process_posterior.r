# manipulation
library(reshape2)
library(plyr)

# analysis
library(compositions)
library(arm)
library(rstan)
library(coda)

# plotting
library(ggplot2)
library(scales)
library(grid)
library(ggridges)
library(bayesplot)

# helpful functions
source('stan_utility.R')
source('sim_hurdle.r')

# set up data
load(file = '../data/data_dump/unit_image.rdata')

nsim <- 1000
grab <- sample(standata$N, nsim)

files <- list.files('../data/mcmc_out', pattern = 'hurdle', full.names = TRUE)

# pois
fit <- read_stan_csv(files[1:4])
check_all_diagnostics(fit)
check_treedepth(fit, max_depth = 15)
post <- rstan::extract(fit, permuted = TRUE)

# nb
fit2 <- read_stan_csv(files[5:8])
check_all_diagnostics(fit2)
check_treedepth(fit2, max_depth = 15)
post2 <- rstan::extract(fit2, permuted = TRUE)

# posterior predictive simulations
ppc.p <- list()
for(jj in seq(nsim)) {
  gg <- grab[jj]
  oo <- c()
  for(ii in seq(standata$N)) {
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
  for(ii in seq(standata$N)) {
    oo[ii] <- roverhurdle(1, 
                          theta = post2$theta[gg, ii], 
                          mu = post2$lambda_est[gg, ii],
                          omega = post2$phi[gg])
  }
  ppc.nb[[jj]] <- oo
}

series.checks <- function(standata, ppc) {
  # point estimates
  ppc <- Reduce(rbind, ppc)
  ppc.mean <- ppc_stat(standata$y, ppc, stat = 'mean')
  ppc.sd <- ppc_stat(standata$y, ppc, stat = 'sd')
  ppc.max <- ppc_stat(standata$y, ppc, stat = 'max')
  prop_zero <- function(x) mean(x == 0)
  ppc.zero <- ppc_stat(standata$y, ppc, stat = 'prop_zero')
  q25 <- function(x) quantile(x, 0.25)
  ppc.q25 <- ppc_stat(standata$y, ppc, stat = 'q25')
  q75 <- function(x) quantile(x, 0.75)
  ppc.q75 <- ppc_stat(standata$y, ppc, stat = 'q75')

  # error
  ppc.err <- ppc_error_scatter(standata$y, ppc[1:6, ])
  ppc.avgerr <- ppc_error_scatter_avg(standata$y, ppc)
  ppc.ecdf <- ppc_ecdf_overlay(standata$y, ppc[1:50, ])

  # rootograms because i find them easy to read
  root.gg <- ppc_rootogram(standata$y, ppc, style = 'hanging', prob = 0.8)

  # all the plots
  out <- list(mean = ppc.mean, 
              sd = ppc.sd, 
              max = ppc.max, 
              zero = ppc.zero, 
              q25 = ppc.q25,
              q75 = ppc.q75,
              err = ppc.err, 
              avgerr = ppc.avgerr, 
              ecdf = ppc.ecdf, 
              root = root.gg)
  out
}
po.check <- series.checks(standata, ppc.p)
nb.check <- series.checks(standata, ppc.nb)


# visualize regression coefs
post.vis <- function(post, unit.info) {
  # theta regression coefficients
  bet.the <- post$beta_the
  colnames(bet.the) <- c(colnames(unit.info$lithology$raw)[-1], 
                         'thickness', 'area', 'contact above', 
                         'contact below', 'subsurface')
  bet.the <- melt(bet.the)

  the.gg <- ggplot(bet.the, aes(x = value, y = Var2))
  the.gg <- the.gg + geom_density_ridges(rel_min_height = 0.01)
  the.gg <- the.gg + theme_ridges()
  the.gg <- the.gg + labs(x = 'estimate', y = 'predictor of theta')

  # lambda regression coefficients
  bet.lam <- post$beta_lam
  colnames(bet.lam) <- c(colnames(unit.info$lithology$raw)[-1], 
                         'thickness', 'area', 'contact above', 
                         'contact below', 'subsurface')
  bet.lam <- melt(bet.lam)

  lam.gg <- ggplot(bet.lam, aes(x = value, y = Var2))
  lam.gg <- lam.gg + geom_density_ridges(rel_min_height = 0.01)
  lam.gg <- lam.gg + theme_ridges()
  lam.gg <- lam.gg + labs(x = 'estimate', y = 'predictor of lambda')


  # back transform the compositional variables
  inv.the <- alply(post$beta_the[, 1:22], 1, function(x) ilrInv(x, orig = unit.info$lithology$raw))
  the.comp.max <- table(laply(inv.the, which.max))
  inv.the.m <- melt(Reduce(cbind, inv.the))[, c(1, 3)]
  inv.the.m$Var1 <- factor(inv.the.m$Var1)
  inv.the.gg <- ggplot(inv.the.m, aes(x = value, y = Var1))
  inv.the.gg <- inv.the.gg + geom_density_ridges(rel_min_height = 0.01)
  inv.the.gg <- inv.the.gg + theme_ridges()


  inv.lam <- alply(post$beta_lam[, 1:22], 1, function(x) ilrInv(x, orig = unit.info$lithology$raw))
  lam.comp.max <- table(laply(inv.lam, which.max))
  inv.lam.m <- melt(Reduce(cbind, inv.lam))[, c(1, 3)]
  inv.lam.m$Var1 <- factor(inv.lam.m$Var1)
  inv.lam.gg <- ggplot(inv.lam.m, aes(x = value, y = Var1))
  inv.lam.gg <- inv.lam.gg + geom_density_ridges(rel_min_height = 0.01)
  inv.lam.gg <- inv.lam.gg + theme_ridges()

  out <- list(theta = the.gg,
              inv.theta = inv.the.gg,
              lambda = lam.gg,
              inv.lambda = inv.lam.gg)
  out
}
po.vis <- post.vis(post, unit.info)
nb.vis <- post.vis(post2, unit.info)
