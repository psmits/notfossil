library(reshape2)
library(plyr)

library(ggplot2)
library(scales)
library(grid)
library(ggridges)

library(compositions)
library(arm)
library(rstan)
library(coda)
library(bayesplot)

source('stan_utility.R')
source('sim_hurdle.r')

nsim <- 1000
grab <- sample(standata$N, nsim)

load(file = '../data/data_dump/unit_image.rdata')

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
post2 <- rstan::extract(fit, permuted = TRUE)

# posterior predictive simulations
ppc <- list()
for(jj in seq(nsim)) {
  gg <- grab[jj]
  oo <- c()
  for(ii in seq(standata$N)) {
    oo[ii] <- rhurdle(1, 
                      theta = post$theta[gg, ii], 
                      lambda = post$lambda_est[gg, ii])
  }
  ppc[[jj]] <- oo
}



# point estimates
ppc.mean <- ppc_stat(standata$y, Reduce(rbind, ppc), stat = 'mean')
ppc.sd <- ppc_stat(standata$y, Reduce(rbind, ppc), stat = 'sd')
ppc.max <- ppc_stat(standata$y, Reduce(rbind, ppc), stat = 'max')
prop_zero <- function(x) mean(x == 0)
ppc.zero <- ppc_stat(standata$y, Reduce(rbind, ppc), stat = 'prop_zero')

# error
ppc.err <- ppc_error_scatter(standata$y, Reduce(rbind, ppc[1:6]))
ppc.avgerr <- ppc_error_scatter_avg(standata$y, Reduce(rbind, ppc))
ppc.ecdf <- ppc_ecdf_overlay(standata$y, Reduce(rbind, ppc[1:50]))

# rootograms because i find them easy to read
rot.gg <- ppc_rootogram(standata$y, 
                        Reduce(rbind, ppc), 
                        style = 'hanging',
                        prob = 0.8)
#### enough posterior predictive analyses





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
