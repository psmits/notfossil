library(reshape2)
library(plyr)

library(ggplot2)
library(scales)
library(grid)
library(ggridges)

library(arm)
library(rstan)
library(coda)

source('stan_utility.R')
source('sim_hurdle.r')

nsim <- 1000

load(file = '../data/data_dump/unit_image.rdata')

files <- list.files('../data/mcmc_out', pattern = 'hurdle', full.names = TRUE)
fit <- read_stan_csv(files[1:4])
check_all_diagnostics(fit)

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

# residuals and standardized residuals
resd <- llply(ppc, function(x) standata$y - x)
resd <- laply(resd, mean)
sdre <- llply(ppc, function(x) (standata$y - x) / sd(standata$y))
sdre <- laply(sdre, mean)

ppc.res <- data.frame(resid = resd, stan.resid = sdre)
ppc.res <- melt(ppc.res)

res.gg <- ggplot(ppc.res, aes(x = value))
res.gg <- res.gg + geom_histogram()
res.gg <- res.gg + facet_grid(. ~ variable, scales = 'free_x')


# theta regression coefficients
bet.the <- post$beta_the
colnames(bet.the) <- c(colnames(unit.info$lithology$raw)[-1], 
                       'thickness', 'area', 'contact above', 
                       'contact below', 'subsurface')
bet.the <- melt(bet.the)

the.gg <- ggplot(bet.the, aes(x = value, y = Var2))
the.gg <- the.gg + geom_density_ridges(rel_min_height = 0.01)
the.gg <- the.gg + theme_ridges()

# lambda regression coefficients
bet.lam <- post$beta_lam
colnames(bet.lam) <- c(colnames(unit.info$lithology$raw)[-1], 
                       'thickness', 'area', 'contact above', 
                       'contact below', 'subsurface')
bet.lam <- melt(bet.lam)

lam.gg <- ggplot(bet.lam, aes(x = value, y = Var2))
lam.gg <- lam.gg + geom_density_ridges(rel_min_height = 0.01)
lam.gg <- lam.gg + theme_ridges()

