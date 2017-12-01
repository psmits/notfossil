# manipulation
library(reshape2)
library(plyr)

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
source('stan_utility.R')
source('sim_hurdle.r')
source('post_foo.r')

# set up data
shelly <- c('Brachiopoda', 'Arthropoda', 'Mollusca')
nsim <- 1000
grab <- sample(4000, nsim)

out <- analyze.posterior(shelly, nsim, grab)


# print out some posterior predictive checks
pdf(file = '../doc/figure/ppc_mean.pdf', height = 10, width = 8)
bayesplot_grid(out$Arthropoda$po.check$mean, out$Arthropoda$nb.check$mean,
               out$Brachiopoda$po.check$mean, out$Brachiopoda$nb.check$mean,
               out$Mollusca$po.check$mean, out$Mollusca$nb.check$mean,
               grid_args = list(ncol = 2),
               titles = c('Arth_po', 'Arth_nb', 'Brach_po', 'Brach_nb',
                          'Mol_po', 'Mol_nb'))
dev.off()

pdf(file = '../doc/figure/ppc_sd.pdf', height = 10, width = 8)
bayesplot_grid(out$Arthropoda$po.check$sd, out$Arthropoda$nb.check$sd,
               out$Brachiopoda$po.check$sd, out$Brachiopoda$nb.check$sd,
               out$Mollusca$po.check$sd, out$Mollusca$nb.check$sd,
               grid_args = list(ncol = 2),
               titles = c('Arth_po', 'Arth_nb', 'Brach_po', 'Brach_nb',
                          'Mol_po', 'Mol_nb'))
dev.off()

pdf(file = '../doc/figure/ppc_ecdf.pdf', height = 10, width = 8)
bayesplot_grid(out$Arthropoda$po.check$ecdf, out$Arthropoda$nb.check$ecdf,
               out$Brachiopoda$po.check$ecdf, out$Brachiopoda$nb.check$ecdf,
               out$Mollusca$po.check$ecdf, out$Mollusca$nb.check$ecdf,
               grid_args = list(ncol = 2),
               titles = c('Arth_po', 'Arth_nb', 'Brach_po', 'Brach_nb',
                          'Mol_po', 'Mol_nb'))
dev.off()

pdf(file = '../doc/figure/ppc_root.pdf', height = 10, width = 8)
bayesplot_grid(out$Arthropoda$po.check$root, out$Arthropoda$nb.check$root,
               out$Brachiopoda$po.check$root, out$Brachiopoda$nb.check$root,
               out$Mollusca$po.check$root, out$Mollusca$nb.check$root,
               grid_args = list(ncol = 2),
               titles = c('Arth_po', 'Arth_nb', 'Brach_po', 'Brach_nb',
                          'Mol_po', 'Mol_nb'))
dev.off()

pdf(file = '../doc/figure/ppc_avgerr.pdf', height = 10, width = 8)
bayesplot_grid(out$Arthropoda$po.check$avgerr, out$Arthropoda$nb.check$avgerr,
               out$Brachiopoda$po.check$avgerr, out$Brachiopoda$nb.check$avgerr,
               out$Mollusca$po.check$avgerr, out$Mollusca$nb.check$avgerr,
               grid_args = list(ncol = 2),
               titles = c('Arth_po', 'Arth_nb', 'Brach_po', 'Brach_nb',
                          'Mol_po', 'Mol_nb'))
dev.off()

pdf(file = '../doc/figure/ppc_avgerr.pdf', height = 10, width = 8)
bayesplot_grid(out$Arthropoda$po.check$loo.pit, out$Arthropoda$nb.check$loo.pit,
               out$Brachiopoda$po.check$loo.pit, out$Brachiopoda$nb.check$loo.pit,
               out$Mollusca$po.check$loo.pit, out$Mollusca$nb.check$loo.pit,
               grid_args = list(ncol = 2),
               titles = c('Arth_po', 'Arth_nb', 'Brach_po', 'Brach_nb',
                          'Mol_po', 'Mol_nb'))
dev.off()



# visualize regression estimates (incl invIlr coefs)
pdf(file = '../doc/figure/arth_coefs.pdf', height = 10, width = 8)
bayesplot_grid(out$Arthropoda$nb.vis$theta, out$Arthropoda$nb.vis$inv.theta, 
               out$Arthropoda$nb.vis$lambda, out$Arthropoda$nb.vis$inv.lambda, 
               grid_args = list(ncol = 2),
               titles = c('Arth_theta', 'Arth_inv.theta',
                          'Arth_lambda', 'Arth_inv.lamba'))
dev.off()

pdf(file = '../doc/figure/brach_coefs.pdf', height = 10, width = 8)
bayesplot_grid(out$Brachiopoda$nb.vis$theta, out$Brachiopoda$nb.vis$inv.theta, 
               out$Brachiopoda$nb.vis$lambda, out$Brachiopoda$nb.vis$inv.lambda,
               grid_args = list(ncol = 2),
               titles = c('Brach_theta', 'Brach_inv.theta',
                          'Brach_lambda', 'Brach_inv.lamba'))
dev.off()

pdf(file = '../doc/figure/mol_coefs.pdf', height = 10, width = 8)
bayesplot_grid(out$Mollusca$nb.vis$theta, out$Mollusca$nb.vis$inv.theta,
               out$Mollusca$nb.vis$lambda, out$Mollusca$nb.vis$inv.lambda,
               grid_args = list(ncol = 2),
               titles = c('Mol_theta', 'Mol_inv.theta',
                          'Mol_lambda', 'Mol_inv.lamba'))
dev.off()
