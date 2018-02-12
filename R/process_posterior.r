# manipulation
library(reshape2)
library(plyr)
library(stringr)
library(dplyr)

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
constant <- 10
ord <- c(460.4, 443.8)
shelly <- c('Brachiopoda', 'Trilobita', 'Bivalvia', 'Gastropoda')
#shelly <- shelly[-1]
#shelly <- c('Brachiopoda', 'Arthropoda', 'Mollusca')
nsim <- 1000

# time bins
timerange <- abs(diff(ord))
brks <- timerange / constant
brks <- seq(from = ord[2], to = ord[1], by = brks)
brks <- cbind(brks[-1], brks[-length(brks)])
brks <- brks[rev(seq(nrow(brks))), ]

# posterior predictive /checks
# lots of internal IO
check.result <- Map(function(x) postchecks(x, nsim), shelly)

# unit div through time vs estimated div from model
dg <- divtime.plot(shelly, brks)

# covariate effects
out <- list()
for(ii in seq(length(shelly))) {
  load(paste0('../data/data_dump/diversity_image_', shelly[ii], '.rdata'))
  pat <- paste0('trunc\\_[0-9]\\_', shelly[ii])
  files <- list.files('../data/mcmc_out', pattern = pat, full.names = TRUE)
  fit <- read_stan_csv(files)
  post <- extract(fit, permuted = TRUE)

  # covariates are : 
  #   intercept
  #   (max) thickness
  #   areal extent
  #   subsurface
  #   composition siliciclastic (carbonate is base)
  betamean <- apply(post$beta, 2:3, mean)
  betalow <- apply(post$beta, 2:3, function(x) quantile(x, 0.1))
  betahigh <- apply(post$beta, 2:3, function(x) quantile(x, 0.9))

  betaest <- list(betamean, betalow, betahigh)
  betaest <- llply(betaest, function(x) {
                     colnames(x) <- c('intercept', 
                                      'thickness', 
                                      'area', 
                                      'subsurface', 
                                      'siliciclastic')
                     x})

  betaest <- llply(betaest, melt)
  betaest <- data.frame(betaest[[1]], 
                        low = betaest[[2]]$value, 
                        high = betaest[[3]]$value,
                        g = shelly[ii])
  names(betaest)[1:2] <- c('time', 'covariate')

  midpoint <- apply(brks, 1, mean)
  betaest$time <- mapvalues(betaest$time, sort(unique(betaest$time)), midpoint)
  out[[ii]] <- betaest
}
betaest <- Reduce(rbind, out)


mg <- ggplot(betaest, aes(x = time, y = value, ymin = low, ymax = high))
mg <- mg + geom_pointrange()
mg <- mg + facet_grid(g ~ covariate)
mg <- mg + scale_x_reverse()
mg <- mg + labs(x = 'Time (Mya)', y = 'estimated regression coefficient')
