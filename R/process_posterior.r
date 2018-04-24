# analysis
library(countreg)
library(compositions)
library(arm)
library(rstan)
library(coda)
library(loo)

# plotting
library(scales)
library(grid)
library(ggridges)
library(bayesplot)
library(xtable)

# manipulation
library(reshape2)
library(plyr)
library(stringr)
library(tidyverse)

# helpful functions
source('../R/stan_utility.R')
source('../R/post_foo.r')
source('../R/post_checks.r')
source('../R/post_div.r')

brks <- read_rds('../data/breaks.rds')  
# buts brks into namespace
# needed for plotting

# basic plotting requirements
theme_set(theme_bw())
theme_update(axis.text = element_text(size = 10),
             axis.title = element_text(size = 12),
             legend.text = element_text(size = 5),
             legend.title = element_text(size = 8),
             legend.key.size = unit(0.75, 'cm'),
             strip.text = element_text(size = 8))

# set up data
constant <- 20
ord <- c(460.4, 427.4)
hirnantian <- 445.6
nsim <- 1000
shelly <- c(# 'Anthozoa', # divergences
            # 'Bivalvia', # divergences
            'Brachiopoda', 
            # 'Gastropoda', # bonus data
            # 'Mollusca', # bonus data
            'Trilobita')
type <- c('diversity', 'occurrence')

# posterior predictive /checks
# series of plots for the ones i really want to look at
check.result <- purrr::map(shelly, ~ postchecks(.x, type = type[1], nsim))
check.result.oc <- purrr::map(shelly, ~ postchecks(.x, type = type[2], nsim)) 
names(check.result) <- names(check.result.oc) <- shelly


# walk over the posterior predictive plots
# partials make this easier, imo
plot_postchecks(check.result, type = type[1])
plot_postchecks(check.result, type = type[2])


# plot all the inference plots (effects, est div, sigs, etc)
# only 
# don't need to walk because internal loops/walks/etc
covname <- c('intercept (carbonate)', 'thickness', 'area', 'latitude',
             'dolomite', 'fine silic.', 'coarse silic.')

mit <- partial(plot_infertests, 
               brks = brks, vert = hirnantian, 
               foo = mean, nsim = nsim, covname = covname)
mitd <- partial(mit, type = type[1])
mitd(shelly)
mito <- partial(mit, type = type[2])
mito(shelly)
