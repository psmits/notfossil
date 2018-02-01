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
out <- list
for(kk in seq(length(shelly))) {
  load(paste0('../data/data_dump/diversity_image_', shelly[kk], '.rdata'))

  pat <- paste0('trunc\\_[0-9]\\_', shelly[kk])
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
  out[[kk]] <- list(data = standata, checks = checks, checks.time = checks.time)
}






# relict of multi model
#checks.taxon <- group.checks(standata$y, ppc, group = standata$d)
#
#
#names(post)
## plot by time unit
##   observed values: mean + sd
##   estimated values: mean + sd
#observed <- data.frame(Reduce(cbind, standata[c('y', 't', 'u', 'd')]))
#names(observed) <- c('count', 'time', 'unit', 'taxon')
#observed$taxon <- mapvalues(observed$taxon, unique(observed$taxon), shelly)
#
#bytax <- split(observed, observed$taxon)
#taxtime <- llply(bytax, function(x) split(x, x$time))
#taxtime <- llply(taxtime, function(x) 
#                 llply(x, function(y) 
#                       quantile(y$count, c(0.1, 0.5, 0.9))))
#names(taxtime) <- shelly
#taxtime <- llply(taxtime, function(x) Reduce(rbind, x))
#taxtime <- llply(taxtime, function(x) {
#                   x <- cbind(x, 1:10)
#                   x})
#taxtime <- Map(function(x, y) data.frame(x, y), taxtime, shelly)
#taxtime <- Reduce(rbind, taxtime)
#names(taxtime) <- c('low', 'med', 'hgh', 'time', 'taxon')
#
#gw <- ggplot(taxtime, aes(x = time, y = med))
##gw <- gw + geom_point(data = observed, mapping = aes(x = time, y = count), shape = 4)
#gw <- gw + geom_pointrange(mapping = aes(ymin = low, ymax = hgh), 
#                           colour = 'blue')
#
#
## get averages from simulation
#grow <- llply(ppc, function(x) data.frame(count = x, 
#                                          time = observed$time, 
#                                          taxon = observed$taxon))
#growtax <- llply(grow, function(x) split(x, x$taxon))
#growtt <- llply(growtax, function(x) llply(x, function(y) split(y, y$time)))
#
#growtt <- llply(growtt, function(a) 
#                llply(a, function(x) 
#                      laply(x, function(y) median(y$count))))
#growtt <- llply(growtt, function(y) 
#                llply(y, function(x) data.frame(count = x, time = 1:10)))
#
#growtt <- llply(growtt, function(z) 
#                Map(function(x, y) cbind(x, taxon = y), z, shelly))
#growtt <- llply(growtt, function(x) Reduce(rbind, x))
#growtt <- Reduce(rbind, Map(function(x, y) cbind(x, sim = y), growtt, 1:nsim))
#
## summary statistic approach median + 80% credible
#bt <- split(growtt, growtt$taxon)
#btt <- llply(bt, function(x) split(x, x$time))
#
#btt <- llply(btt, function(y) 
#             laply(y, function(x) quantile(x$count, c(0.1, 0.5, 0.9))))
#btt <- llply(btt, function(x) cbind(x, time = 1:10))
#btt <- Map(function(x, y) data.frame(x, taxon = y), btt, shelly)
#btt <- Reduce(rbind, btt)
#names(btt) <- c('low', 'med', 'hgh', 'time', 'taxon')
#
#gw <- gw + geom_pointrange(data = btt, 
#                           mapping = aes(x = time, y = med, 
#                                         ymin = low, ymax = hgh))
#gw
## way overestimating
