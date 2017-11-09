library(reshape2)
library(plyr)

library(ggplot2)
library(scales)
library(grid)
library(ggridges)

library(arm)
library(rstan)
library(coda)

source('sim_hurdle.r')

nsim <- 1000

load(file = '../data/data_dump/unit_image.rdata')

files <- list.files('../data/mcmc_out', pattern = 'hurdle', full.names = TRUE)
fit <- read_stan_csv(files)
fit.rhat <- stan_rhat(fit)
fit.ess <- stan_ess(fit)

post <- rstan::extract(fit, permuted = TRUE)
grab <- sample(standata$N, nsim)


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


# empirical values
em <- mean(standata$y)
esd <- sd(standata$y)
ee <- data.frame(emp = c(em, esd), variable = c('mean', 'sd'))

# point estimates from sims
ppc.m <- laply(ppc, mean)
ppc.sd <- laply(ppc, sd)

# histogram plots
ppc.sum <- data.frame(mean = ppc.m, sd = ppc.sd)
ppc.sum <- melt(ppc.sum)

ppc.gg <- ggplot(ppc.sum, aes(x = value))
ppc.gg <- ppc.gg + geom_histogram()
ppc.gg <- ppc.gg + facet_grid(. ~ variable, scales = 'free_x')
ppc.gg <- ppc.gg + geom_vline(mapping = aes(xintercept = em),
                              data = ee,
                              size = 1.5, colour = 'blue')

# regression coefficients
post$beta_the <- c(colnames(unit.info$lithology$raw)[-1], 
                   'thickness', 'area', 'contact above', 
                   'contact below', 'subsurface')
names(post$beta_the)
