library(reshape2)
library(plyr)

library(ggplot2)
library(scales)
library(grid)
library(ggridges)

library(arm)
library(rstan)
library(coda)

load(file = '../data/data_dump/unit_image.rdata')

files <- list.files('../data/mcmc_out/', pattern = 'hurdle', full.names = TRUE)
fit <- read_stan_csv(files)

post <- rstan::extract(fit, permuted = TRUE)
