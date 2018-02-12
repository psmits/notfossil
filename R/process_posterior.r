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

# posterior predictive /checks
# lots of internal IO
postpred <- Map(function(x) postchecks(x, nsim), shelly)




load(paste0('../data/data_dump/diversity_image_', shelly[1], '.rdata'))
pat <- paste0('trunc\\_[0-9]\\_', shelly[1])
files <- list.files('../data/mcmc_out', pattern = pat, full.names = TRUE)
fit <- read_stan_csv(files)
post <- extract(fit, permuted = TRUE)



