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
gg <- bayesplot_grid(out$Brachiopoda$po.check$mean, 
                     out$Brachiopoda$nb.check$mean,
                     out$Trilobita$po.check$mean,
                     out$Trilobita$nb.check$mean,
                     out$Bivalvia$po.check$mean,
                     out$Bivalvia$nb.check$mean,
                     out$Gastropoda$po.check$mean,
                     out$Gastropoda$nb.check$mean,
                     grid_args = list(ncol = 2),
                     titles = c('Brach_po_mean', 'Brach_nb_mean',
                                'Tril_po_mean', 'Tril_nb_mean',
                                'Biv_po_mean', 'Biv_nb_mean',
                                'Gas_po_mean', 'Gas_nb_mean'))
ggsave(filename = '../doc/figure/ppc_mean.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(out$Brachiopoda$po.check$sd, 
                     out$Brachiopoda$nb.check$sd,
                     out$Trilobita$po.check$sd,
                     out$Trilobita$nb.check$sd,
                     out$Bivalvia$po.check$sd,
                     out$Bivalvia$nb.check$sd,
                     out$Gastropoda$po.check$sd,
                     out$Gastropoda$nb.check$sd,
                     grid_args = list(ncol = 2),
                     titles = c('Brach_po_sd', 'Brach_nb_sd',
                                'Tril_po_sd', 'Tril_nb_sd',
                                'Biv_po_sd', 'Biv_nb_sd',
                                'Gas_po_sd', 'Gas_nb_sd'))
ggsave(filename = '../doc/figure/ppc_sd.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(
                     out$Brachiopoda$po.check$ecdf, 
                     out$Brachiopoda$nb.check$ecdf,
                     out$Trilobita$po.check$ecdf,
                     out$Trilobita$nb.check$ecdf,
                     out$Bivalvia$po.check$ecdf,
                     out$Bivalvia$nb.check$ecdf,
                     out$Gastropoda$po.check$ecdf,
                     out$Gastropoda$nb.check$ecdf,
                     grid_args = list(ncol = 2),
                     titles = c('Brach_po_ecdf', 'Brach_nb_ecdf',
                                'Tril_po_ecdf', 'Tril_nb_ecdf',
                                'Biv_po_ecdf', 'Biv_nb_ecdf',
                                'Gas_po_ecdf', 'Gas_nb_ecdf'))
ggsave(filename = '../doc/figure/ppc_ecdf.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(
                     out$Brachiopoda$po.check$root, 
                     out$Brachiopoda$nb.check$root,
                     out$Trilobita$po.check$root,
                     out$Trilobita$nb.check$root,
                     out$Bivalvia$po.check$root,
                     out$Bivalvia$nb.check$root,
                     out$Gastropoda$po.check$root,
                     out$Gastropoda$nb.check$root,
                     grid_args = list(ncol = 2),
                     titles = c('Brach_po_root', 'Brach_nb_root',
                                'Tril_po_root', 'Tril_nb_root',
                                'Biv_po_root', 'Biv_nb_root',
                                'Gas_po_root', 'Gas_nb_root'))
ggsave(filename = '../doc/figure/ppc_root.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(out$Brachiopoda$po.check$avgerr, 
                     out$Brachiopoda$nb.check$avgerr,
                     out$Trilobita$po.check$avgerr,
                     out$Trilobita$nb.check$avgerr,
                     out$Bivalvia$po.check$avgerr,
                     out$Bivalvia$nb.check$avgerr,
                     out$Gastropoda$po.check$avgerr,
                     out$Gastropoda$nb.check$avgerr,
                     grid_args = list(ncol = 2),
                     titles = c('Brach_po_avgerr', 'Brach_nb_avgerr',
                                'Tril_po_avgerr', 'Tril_nb_avgerr',
                                'Biv_po_avgerr', 'Biv_nb_avgerr',
                                'Gas_po_avgerr', 'Gas_nb_avgerr'))
ggsave(filename = '../doc/figure/ppc_avgerr.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(out$Brachiopoda$po.check$loo.pit, 
                     out$Brachiopoda$nb.check$loo.pit,
                     out$Trilobita$po.check$loo.pit,
                     out$Trilobita$nb.check$loo.pit,
                     out$Bivalvia$po.check$loo.pit,
                     out$Bivalvia$nb.check$loo.pit,
                     out$Gastropoda$po.check$loo.pit,
                     out$Gastropoda$nb.check$loo.pit,
                     grid_args = list(ncol = 2),
                     titles = c('Brach_po_loo.pit', 'Brach_nb_loo.pit',
                                'Tril_po_loo.pit', 'Tril_nb_loo.pit',
                                'Biv_po_loo.pit', 'Biv_nb_loo.pit',
                                'Gas_po_loo.pit', 'Gas_nb_loo.pit'))
ggsave(filename = '../doc/figure/ppc_loopit.pdf', plot = gg,
       height = 10, width = 8)



# visualize regression estimates (incl invIlr coefs)
gg <- bayesplot_grid(out$Brachiopoda$nb.vis$theta, 
                     out$Brachiopoda$nb.vis$inv.theta, 
                     out$Brachiopoda$nb.vis$lambda, 
                     out$Brachiopoda$nb.vis$inv.lambda,
                     grid_args = list(ncol = 2),
                     titles = c('Brach_theta', 'Brach_inv.theta',
                                'Brach_lambda', 'Brach_inv.lamba'))
ggsave(filename = '../doc/figure/brach_coefs.pdf', plot = gg,
       height = 6, width = 10)

gg <- bayesplot_grid(out$Trilobita$nb.vis$theta, 
                     out$Trilobita$nb.vis$inv.theta, 
                     out$Trilobita$nb.vis$lambda, 
                     out$Trilobita$nb.vis$inv.lambda,
                     grid_args = list(ncol = 2),
                     titles = c('Tril_theta', 'Tril_inv.theta',
                                'Tril_lambda', 'Tril_inv.lamba'))
ggsave(filename = '../doc/figure/tril_coefs.pdf', plot = gg,
       height = 6, width = 10)

gg <- bayesplot_grid(out$Bivalvia$nb.vis$theta, 
                     out$Bivalvia$nb.vis$inv.theta, 
                     out$Bivalvia$nb.vis$lambda, 
                     out$Bivalvia$nb.vis$inv.lambda,
                     grid_args = list(ncol = 2),
                     titles = c('Biv_theta', 'Biv_inv.theta',
                                'Biv_lambda', 'Biv_inv.lamba'))
ggsave(filename = '../doc/figure/biv_coefs.pdf', plot = gg,
       height = 6, width = 10)

gg <- bayesplot_grid(out$Gastropoda$nb.vis$theta, 
                     out$Gastropoda$nb.vis$inv.theta, 
                     out$Gastropoda$nb.vis$lambda, 
                     out$Gastropoda$nb.vis$inv.lambda,
                     grid_args = list(ncol = 2),
                     titles = c('Gas_theta', 'Gas_inv.theta',
                                'Gas_lambda', 'Gas_inv.lamba'))
ggsave(filename = '../doc/figure/gas_coefs.pdf', plot = gg,
       height = 6, width = 10)



# results of predicting the test set
gg <- bayesplot_grid(out$Brachiopoda$po.test$mean, 
                     out$Brachiopoda$nb.test$mean,
                     out$Trilobita$po.test$mean,
                     out$Trilobita$nb.test$mean,
                     out$Bivalvia$po.test$mean,
                     out$Bivalvia$nb.test$mean,
                     out$Gastropoda$po.test$mean,
                     out$Gastropoda$nb.test$mean,
                     grid_args = list(ncol = 2),
                     titles = c('Brach_po_predmean', 'Brach_nb_predmean', 
                                'Tril_po_predmean', 'Tril_nb_predmean',
                                'Biv_po_predmean', 'Biv_nb_predmean',
                                'Gas_po_predmean', 'Gas_nb_predmean'))
ggsave(filename = '../doc/figure/pred_mean.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(out$Brachiopoda$po.test$sd, 
                     out$Brachiopoda$nb.test$sd,
                     out$Trilobita$po.test$sd,
                     out$Trilobita$nb.test$sd,
                     out$Bivalvia$po.test$sd,
                     out$Bivalvia$nb.test$sd,
                     out$Gastropoda$po.test$sd,
                     out$Gastropoda$nb.test$sd,
                     grid_args = list(ncol = 2),
                     titles = c('Brach_po_predsd', 'Brach_nb_predsd', 
                                'Tril_po_predsd', 'Tril_nb_predsd',
                                'Biv_po_predsd', 'Biv_nb_predsd',
                                'Gas_po_predsd', 'Gas_nb_predsd'))
ggsave(filename = '../doc/figure/pred_sd.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(out$Brachiopoda$po.test$ecdf, 
                     out$Brachiopoda$nb.test$ecdf,
                     out$Trilobita$po.test$ecdf,
                     out$Trilobita$nb.test$ecdf,
                     out$Bivalvia$po.test$ecdf,
                     out$Bivalvia$nb.test$ecdf,
                     out$Gastropoda$po.test$ecdf,
                     out$Gastropoda$nb.test$ecdf,
                     grid_args = list(ncol = 2),
                     titles = c('Brach_po_predecdf', 'Brach_nb_predecdf', 
                                'Tril_po_predecdf', 'Tril_nb_predecdf',
                                'Biv_po_predecdf', 'Biv_nb_predecdf',
                                'Gas_po_predecdf', 'Gas_nb_predecdf'))
ggsave(filename = '../doc/figure/pred_ecdf.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(out$Brachiopoda$po.test$root, 
                     out$Brachiopoda$nb.test$root,
                     out$Trilobita$po.test$root,
                     out$Trilobita$nb.test$root,
                     out$Bivalvia$po.test$root,
                     out$Bivalvia$nb.test$root,
                     out$Gastropoda$po.test$root,
                     out$Gastropoda$nb.test$root,
                     grid_args = list(ncol = 2),
                     titles = c('Brach_po_predroot', 'Brach_nb_predroot', 
                                'Tril_po_predroot', 'Tril_nb_predroot',
                                'Biv_po_predroot', 'Biv_nb_predroot',
                                'Gas_po_predroot', 'Gas_nb_predroot'))
ggsave(filename = '../doc/figure/pred_root.pdf', plot = gg,
       height = 10, width = 8)

gg <- bayesplot_grid(out$Brachiopoda$po.test$avgerr, 
                     out$Brachiopoda$nb.test$avgerr,
                     out$Trilobita$po.test$avgerr,
                     out$Trilobita$nb.test$avgerr,
                     out$Bivalvia$po.test$avgerr,
                     out$Bivalvia$nb.test$avgerr,
                     out$Gastropoda$po.test$avgerr,
                     out$Gastropoda$nb.test$avgerr,
                     grid_args = list(ncol = 2),
                     titles = c('Brach_po_predavgerr', 'Brach_nb_predavgerr', 
                                'Tril_po_predavgerr', 'Tril_nb_predavgerr',
                                'Biv_po_predavgerr', 'Biv_nb_predavgerr',
                                'Gas_po_predavgerr', 'Gas_nb_predavgerr'))
ggsave(filename = '../doc/figure/pred_avgerr.pdf', plot = gg,
       height = 10, width = 8)
