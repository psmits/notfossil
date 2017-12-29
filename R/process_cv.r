# manipulation
library(reshape2)
library(plyr)
library(stringr)

# analysis
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

# helpful functions
source('../R/stan_utility.R')
source('../R/sim_hurdle.r')
source('../R/post_foo.r')

# set up data
shelly <- c('Brachiopoda', 'Arthropoda', 'Mollusca')
nsim <- 1000
grab <- sample(4000, nsim)
kfold <- 5
rnds <- 5


# cross-validation stuff
# memory intensive so split by model type in hopes of using less RAM
errorest.po <- analyze.cv(shelly, nsim, grab, kfold, rnds, 'pois')
errorest.nb <- analyze.cv(shelly, nsim, grab, kfold, rnds, 'ngbn')

# print out some cross-validation graphs
errorest.po <- llply(errorest.po, function(x) {
                       names(x) <- c('rd1', 'rd2', 'rd3', 'rd4', 'rd5')
                       x})
errorest.nb <- llply(errorest.nb, function(x) {
                       names(x) <- c('rd1', 'rd2', 'rd3', 'rd4', 'rd5')
                       x})
errorest <- list(pois = errorest.po, negbin = errorest.nb)
erest <- melt(errorest)
names(erest)


ergg <- ggplot(erest, aes(x = value))
ergg <- ergg + geom_histogram()
ergg <- ergg + facet_grid(L1 ~ L2, scales = 'free_x')
ergg <- ergg + labs(x = 'RMSE')




