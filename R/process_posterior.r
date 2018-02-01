# manipulation
library(reshape2)
library(plyr)
library(stringr)

# analysis
library(countreg)
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
library(xtable)

# helpful functions
source('../R/stan_utility.R')
source('../R/post_foo.r')

theme_set(theme_bw())
theme_update(axis.text = element_text(size = 10),
             axis.title = element_text(size = 12),
             legend.text = element_text(size = 5),
             legend.title = element_text(size = 8),
             legend.key.size = unit(0.75, 'cm'),
             strip.text = element_text(size = 8))

# set up data
shelly <- c('Brachiopoda', 'Trilobita', 'Bivalvia', 'Gastropoda')
#shelly <- c('Brachiopoda', 'Arthropoda', 'Mollusca')
nsim <- 1000
grab <- sample(4000, nsim)


# for each of the taxonomic groups
load('../data/data_dump/diversity_image_full.rdata')

pat <- 'trunc\\_[0-9]\\_full'
files <- list.files('../data/mcmc_out', pattern = pat, full.names = TRUE)
fit <- read_stan_csv(files)
check_all_diagnostics(fit)
post <- extract(fit, permuted = TRUE)

ppc <- list()
for(jj in seq(nsim)) {
  gg <- grab[jj]
  oo <- c()
  for(ii in seq(standata$N)) {
    oo[ii] <- rztnbinom(1, mu = post$mu[gg], theta = post$phi[gg])
  }
  ppc[[jj]] <- oo
}

# posterior predictive checks
#   
checks <- single.checks(standata$y, ppc)
checks.time <- group.checks(standata$y, ppc, group = standata$t)
checks.taxon <- group.checks(standata$y, ppc, group = standata$d)


names(post)
# plot by time unit
#   observed values: mean + sd
#   estimated values: mean + sd

