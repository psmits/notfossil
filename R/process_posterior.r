# manipulation
library(reshape2)
library(plyr)
library(stringr)
library(tidyverse)

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
shelly <- c('Brachiopoda', 'Anthozoa', 'Trilobita', 
            'Bivalvia', 'Gastropoda', 'Cephalopoda', 'Mollusca')


# posterior predictive /checks
# series of plots for the ones i really want to look at
check.result <- Map(function(x) postchecks(x, nsim), shelly)
chckm <- bayesplot_grid(check.result$Brachiopoda$checks$mean,
                        check.result$Trilobita$checks$mean,
                        check.result$Bivalvia$checks$mean,
                        check.result$Gastropoda$checks$mean,
                        check.result$Mollusca$checks$mean,
                        check.result$Anthozoa$checks$mean,
                        grid_args = list(ncol = 2),
                        titles = c('Brac mean', 'Tri mean',
                                   'Biv mean', 'Gas mean',
                                   'Mol mean', 'Anth mean'))
ggsave(plot = chckm, filename = '../doc/figure/ppc_mean.png',
       width = 10, height = 8)

chcks <- bayesplot_grid(check.result$Brachiopoda$checks$sd,
                        check.result$Trilobita$checks$sd,
                        check.result$Bivalvia$checks$sd,
                        check.result$Gastropoda$checks$sd,
                        check.result$Mollusca$checks$sd,
                        check.result$Anthozoa$checks$sd,
                        grid_args = list(ncol = 2),
                        titles = c('Brac sd', 'Tri sd',
                                   'Biv sd', 'Gas sd',
                                   'Mol sd', 'Anth sd'))
ggsave(plot = chcks, filename = '../doc/figure/ppc_sd.png',
       width = 10, height = 8)

chckroot <- bayesplot_grid(check.result$Brachiopoda$checks$root,
                           check.result$Trilobita$checks$root,
                           check.result$Bivalvia$checks$root,
                           check.result$Gastropoda$checks$root,
                           check.result$Mollusca$checks$root,
                           check.result$Anthozoa$checks$root,
                           grid_args = list(ncol = 2),
                           titles = c('Brac root', 'Tri root',
                                      'Biv root', 'Gas root',
                                      'Mol root', 'Anth root'))
ggsave(plot = chckroot, filename = '../doc/figure/ppc_root.png',
       width = 10, height = 8)

d1 <- check.result$Brachiopoda$checks$root + coord_cartesian(xlim = c(-1, 75))
d2 <- check.result$Trilobita$checks$root + coord_cartesian(xlim = c(-1, 75))
d3 <- check.result$Bivalvia$checks$root + coord_cartesian(xlim = c(-1, 75))
d4 <- check.result$Gastropoda$checks$root + coord_cartesian(xlim = c(-1, 75))
d5 <- check.result$Mollusca$checks$root + coord_cartesian(xlim = c(-1, 75))
d6 <- check.result$Anthozoa$checks$root + coord_cartesian(xlim = c(-1, 75))
chckroot <- bayesplot_grid(d1, d2, d3, d4, d5, d6, grid_args = list(ncol = 2),
                           titles = c('Brac root', 'Tri root',
                                      'Biv root', 'Gas root',
                                      'Mol root', 'Anth root'))
ggsave(plot = chckroot, filename = '../doc/figure/ppc_root_zoom.png',
       width = 10, height = 8)

chckerr <- bayesplot_grid(check.result$Brachiopoda$checks$err,
                          check.result$Trilobita$checks$err,
                          check.result$Bivalvia$checks$err,
                          check.result$Gastropoda$checks$err,
                          check.result$Mollusca$checks$err,
                          check.result$Anthozoa$checks$err,
                          grid_args = list(ncol = 2),
                          titles = c('Brac err', 'Tri err',
                                     'Biv err', 'Gas err',
                                     'Mol err', 'Anth err'))
ggsave(plot = chckerr, filename = '../doc/figure/ppc_err.png',
       width = 10, height = 8)

chckecdf <- bayesplot_grid(check.result$Brachiopoda$checks$ecdf,
                           check.result$Trilobita$checks$ecdf,
                           check.result$Bivalvia$checks$ecdf,
                           check.result$Gastropoda$checks$ecdf,
                           check.result$Mollusca$checks$ecdf,
                           check.result$Anthozoa$checks$ecdf,
                           grid_args = list(ncol = 2),
                           titles = c('Brac ecdf', 'Tri ecdf',
                                      'Biv ecdf', 'Gas ecdf',
                                      'Mol ecdf', 'Anth ecdf'))
ggsave(plot = chckecdf, filename = '../doc/figure/ppc_ecdf.png',
       width = 10, height = 8)

chckdens <- bayesplot_grid(check.result$Brachiopoda$checks$dens,
                           check.result$Trilobita$checks$dens,
                           check.result$Bivalvia$checks$dens,
                           check.result$Gastropoda$checks$dens,
                           check.result$Mollusca$checks$dens,
                           check.result$Anthozoa$checks$dens,
                           grid_args = list(ncol = 2),
                           titles = c('Brac dens', 'Tri dens',
                                      'Biv dens', 'Gas dens',
                                      'Mol dens', 'Anth dens'))
ggsave(plot = chckdens, filename = '../doc/figure/ppc_dens.png',
       width = 10, height = 8)

d1 <- check.result$Brachiopoda$checks$dens + coord_cartesian(xlim = c(-1, 75))
d2 <- check.result$Trilobita$checks$dens + coord_cartesian(xlim = c(-1, 75))
d3 <- check.result$Bivalvia$checks$dens + coord_cartesian(xlim = c(-1, 75))
d4 <- check.result$Gastropoda$checks$dens + coord_cartesian(xlim = c(-1, 75))
d5 <- check.result$Mollusca$checks$dens + coord_cartesian(xlim = c(-1, 75))
d6 <- check.result$Anthozoa$checks$dens + coord_cartesian(xlim = c(-1, 75))
chckdens <- bayesplot_grid(d1, d2, d3, d4, d5, d6, grid_args = list(ncol = 2),
                           titles = c('Brac dens', 'Tri dens',
                                      'Biv dens', 'Gas dens',
                                      'Mol dens', 'Anth dens'))
ggsave(plot = chckdens, filename = '../doc/figure/ppc_dens_zoom.png',
       width = 10, height = 8)


# group ppc-s
pm <- pv <- list()
for(ii in seq(length(shelly))) {
  pm[[ii]] <- check.result[[ii]]$checks.time$mean.group
  pv[[ii]] <- check.result[[ii]]$checks.time$violin.group
}



shelly <- shelly[c(1:5, 7)]
# unit div through time vs estimated div from model
dg <- plot_divtime(shelly, brks, vert = hirnantian)
ggsave(plot = dg, filename = '../doc/figure/unitdiv_time.png',
       width = 11, height = 8.5)

# covariate effects through time
covname <- c('intercept (c. sili)', 'thickness', 'area', 
             'fine siliciclastic', 'carbonate')
cg <- plot_covtime(shelly, brks, covname = covname, vert = hirnantian)
ggsave(plot = cg, filename = '../doc/figure/cov_time.png',
       width = 11, height = 8.5)

# step differenecs 
# coefs
bg <- plot_diffbeta(shelly, covname)
ggsave(plot = bg, filename = '../doc/figure/cov_diff.png',
       width = 11, height = 8.5)
# expected unit diversity
#plot_diffdiv(shelly, nsim, foo = mean)


compare_pvals_beta <- compare_hirbeta(shelly, hirnantian, brks)

# ste
