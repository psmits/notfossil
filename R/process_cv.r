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
kfold <- 5
rnds <- 5


# cross-validation stuff
# memory intensive so split by model type in hopes of using less RAM

errorest.po <- errorest.nb <- list()
for(ii in seq(length(shelly))) {
  errorest.po[ii] <- analyze.cv(shelly[ii], nsim, grab, kfold, rnds, 'pois')
  errorest.po[ii] <- llply(errorest.po[ii], function(x) {
                             names(x) <- c('rd1', 'rd2', 'rd3', 'rd4', 'rd5')
                             x})
  errorest.nb[ii] <- analyze.cv(shelly[ii], nsim, grab, kfold, rnds, 'ngbn')
  errorest.nb[ii] <- llply(errorest.nb[ii], function(x) {
                             names(x) <- c('rd1', 'rd2', 'rd3', 'rd4', 'rd5')
                             x})
}
names(errorest.po) <- names(erroest.nb) <- shelly

# print out some cross-validation graphs
errorest <- list(pois = errorest.po, negbin = errorest.nb)
erest <- melt(errorest)
names(erest) <- c('value', 'fold', 'round', 'taxa', 'model')

ergg <- ggplot(erest, aes(x = value))
ergg <- ergg + geom_histogram()
ergg <- ergg + facet_grid(model ~ taxa, scales = 'free_x', shrink = TRUE)
ergg <- ergg + labs(x = 'RMSE')
#ggsave(filename = '../doc/figure/cv_rmse.pdf', plot = ergg,
#       height = 6, width = 8)
