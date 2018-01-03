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



# analysis of posterior estimated from full dataset
out <- analyze.posterior(shelly, nsim, grab)

# print out some posterior predictive checks
gg <- bayesplot_grid(out$Arthropoda$po.check$mean, 
                     out$Arthropoda$nb.check$mean,
                     out$Brachiopoda$po.check$mean, 
                     out$Brachiopoda$nb.check$mean,
                     out$Mollusca$po.check$mean, 
                     out$Mollusca$nb.check$mean,
                     grid_args = list(ncol = 2),
                     titles = c('Arth_po_mean', 'Arth_nb_mean', 
                                'Brach_po_mean', 'Brach_nb_mean',
                                'Mol_po_mean', 'Mol_nb_mean'))
ggsave(filename = '../doc/figure/ppc_mean.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(out$Arthropoda$po.check$sd, 
                     out$Arthropoda$nb.check$sd,
                     out$Brachiopoda$po.check$sd, 
                     out$Brachiopoda$nb.check$sd,
                     out$Mollusca$po.check$sd, 
                     out$Mollusca$nb.check$sd,
                     grid_args = list(ncol = 2),
                     titles = c('Arth_po_sd', 'Arth_nb_sd', 
                                'Brach_po_sd', 'Brach_nb_sd',
                                'Mol_po_sd', 'Mol_nb_sd'))
ggsave(filename = '../doc/figure/ppc_sd.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(out$Arthropoda$po.check$ecdf, 
                     out$Arthropoda$nb.check$ecdf,
                     out$Brachiopoda$po.check$ecdf, 
                     out$Brachiopoda$nb.check$ecdf,
                     out$Mollusca$po.check$ecdf, 
                     out$Mollusca$nb.check$ecdf,
                     grid_args = list(ncol = 2),
                     titles = c('Arth_po_ecdf', 'Arth_nb_ecdf', 
                                'Brach_po_ecdf', 'Brach_nb_ecdf',
                                'Mol_po_ecdf', 'Mol_nb_ecdf'))
ggsave(filename = '../doc/figure/ppc_ecdf.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(out$Arthropoda$po.check$root, 
                     out$Arthropoda$nb.check$root,
                     out$Brachiopoda$po.check$root, 
                     out$Brachiopoda$nb.check$root,
                     out$Mollusca$po.check$root, 
                     out$Mollusca$nb.check$root,
                     grid_args = list(ncol = 2),
                     titles = c('Arth_po_root', 'Arth_nb_root', 
                                'Brach_po_root', 'Brach_nb_root',
                                'Mol_po_root', 'Mol_nb_root'))
ggsave(filename = '../doc/figure/ppc_root.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(out$Arthropoda$po.check$avgerr, 
                     out$Arthropoda$nb.check$avgerr,
                     out$Brachiopoda$po.check$avgerr, 
                     out$Brachiopoda$nb.check$avgerr,
                     out$Mollusca$po.check$avgerr, 
                     out$Mollusca$nb.check$avgerr,
                     grid_args = list(ncol = 2),
                     titles = c('Arth_po_avgerr', 'Arth_nb_avgerr', 
                                'Brach_po_avgerr', 'Brach_nb_avgerr',
                                'Mol_po_avgerr', 'Mol_nb_avgerr'))
ggsave(filename = '../doc/figure/ppc_avgerr.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(out$Arthropoda$po.check$loo.pit, 
                     out$Arthropoda$nb.check$loo.pit,
                     out$Brachiopoda$po.check$loo.pit, 
                     out$Brachiopoda$nb.check$loo.pit,
                     out$Mollusca$po.check$loo.pit, 
                     out$Mollusca$nb.check$loo.pit,
                     grid_args = list(ncol = 2),
                     titles = c('Arth_po_loopit', 'Arth_nb_loopit', 
                                'Brach_po_loopit', 'Brach_nb_loopit',
                                'Mol_po_loopit', 'Mol_nb_loopit'))
ggsave(filename = '../doc/figure/ppc_loopit.pdf', plot = gg,
       height = 10, width = 8)



# visualize regression estimates (incl invIlr coefs)
gg <- bayesplot_grid(out$Arthropoda$nb.vis$theta, 
                     out$Arthropoda$nb.vis$inv.theta, 
                     out$Arthropoda$nb.vis$lambda, 
                     out$Arthropoda$nb.vis$inv.lambda, 
                     grid_args = list(ncol = 2),
                     titles = c('Arth_theta', 'Arth_inv.theta',
                                'Arth_lambda', 'Arth_inv.lamba'))
ggsave(filename = '../doc/figure/arth_coefs.pdf', plot = gg,
       height = 6, width = 10)

gg <- bayesplot_grid(out$Brachiopoda$nb.vis$theta, 
                     out$Brachiopoda$nb.vis$inv.theta, 
                     out$Brachiopoda$nb.vis$lambda, 
                     out$Brachiopoda$nb.vis$inv.lambda,
                     grid_args = list(ncol = 2),
                     titles = c('Brach_theta', 'Brach_inv.theta',
                                'Brach_lambda', 'Brach_inv.lamba'))
ggsave(filename = '../doc/figure/brach_coefs.pdf', plot = gg,
       height = 6, width = 10)

gg <- bayesplot_grid(out$Mollusca$nb.vis$theta, 
                     out$Mollusca$nb.vis$inv.theta,
                     out$Mollusca$nb.vis$lambda, 
                     out$Mollusca$nb.vis$inv.lambda,
                     grid_args = list(ncol = 2),
                     titles = c('Mol_theta', 'Mol_inv.theta',
                                'Mol_lambda', 'Mol_inv.lamba'))
ggsave(filename = '../doc/figure/mol_coefs.pdf', plot = gg,
       height = 6, width = 10)



# results of predicting the test set
gg <- bayesplot_grid(out$Arthropoda$po.test$mean, 
                     out$Arthropoda$nb.test$mean, 
                     out$Brachiopoda$po.test$mean, 
                     out$Brachiopoda$nb.test$mean,
                     out$Mollusca$po.test$mean, 
                     out$Mollusca$nb.test$mean,
                     grid_args = list(ncol = 2),
                     titles = c('Arth_po_predmean', 'Arth_nb_predmean', 
                                'Brach_po_predmean', 'Brach_nb_predmean', 
                                'Mol_po_predmean', 'Mol_nb_predmean'))
ggsave(filename = '../doc/figure/pred_mean.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(out$Arthropoda$po.test$sd, 
                     out$Arthropoda$nb.test$sd, 
                     out$Brachiopoda$po.test$sd, 
                     out$Brachiopoda$nb.test$sd,
                     out$Mollusca$po.test$sd, 
                     out$Mollusca$nb.test$sd,
                     grid_args = list(ncol = 2),
                     titles = c('Arth_po_predsd', 'Arth_nb_predsd', 
                                'Brach_po_predsd', 'Brach_nb_predsd', 
                                'Mol_po_predsd', 'Mol_nb_predsd'))
ggsave(filename = '../doc/figure/pred_sd.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(out$Arthropoda$po.test$ecdf, 
                     out$Arthropoda$nb.test$ecdf, 
                     out$Brachiopoda$po.test$ecdf, 
                     out$Brachiopoda$nb.test$ecdf,
                     out$Mollusca$po.test$ecdf, 
                     out$Mollusca$nb.test$ecdf,
                     grid_args = list(ncol = 2),
                     titles = c('Arth_po_predecdf', 'Arth_nb_predecdf', 
                                'Brach_po_predecdf', 'Brach_nb_predecdf', 
                                'Mol_po_predecdf', 'Mol_nb_predecdf'))
ggsave(filename = '../doc/figure/pred_ecdf.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(out$Arthropoda$po.test$root, 
                     out$Arthropoda$nb.test$root, 
                     out$Brachiopoda$po.test$root, 
                     out$Brachiopoda$nb.test$root,
                     out$Mollusca$po.test$root, 
                     out$Mollusca$nb.test$root,
                     grid_args = list(ncol = 2),
                     titles = c('Arth_po_predroot', 'Arth_nb_predroot', 
                                'Brach_po_predroot', 'Brach_nb_predroot', 
                                'Mol_po_predroot', 'Mol_nb_predroot'))
ggsave(filename = '../doc/figure/pred_root.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(out$Arthropoda$po.test$avgerr, 
                     out$Arthropoda$nb.test$avgerr, 
                     out$Brachiopoda$po.test$avgerr, 
                     out$Brachiopoda$nb.test$avgerr,
                     out$Mollusca$po.test$avgerr, 
                     out$Mollusca$nb.test$avgerr,
                     grid_args = list(ncol = 2),
                     titles = c('Arth_po_predavgerr', 'Arth_nb_predavgerr', 
                                'Brach_po_predavgerr', 'Brach_nb_predavgerr', 
                                'Mol_po_predavgerr', 'Mol_nb_predavgerr'))
ggsave(filename = '../doc/figure/pred_avgerr.pdf', plot = gg,
       height = 10, width = 8)
