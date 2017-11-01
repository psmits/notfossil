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

load(file = '../data/data_dump/unit_image.rdata')

files <- list.files('../data/mcmc_out', pattern = 'hurdle', full.names = TRUE)
fit <- read_stan_csv(files[5:8])

post <- rstan::extract(fit, permuted = TRUE)

# what are the types of plots?
# posterior predictive checks
nsim <- 1000
out <- list()
temp.theta <- mean(post$theta)
temp.lambda <- colMeans(post$lambda)



for(ii in seq(length(temp.lambda))) {
  hold <- rhurdle(nsim, temp.theta, temp.lambda[ii])
  out[[ii]] <- hold
}
# what about tables?
