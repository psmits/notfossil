# manipulation
library(reshape2)
library(plyr)
library(stringr)
library(dplyr)

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
constant <- 10
ord <- c(460.4, 443.8)
shelly <- c('Brachiopoda', 'Trilobita', 'Bivalvia', 'Gastropoda')
#shelly <- shelly[-1]
#shelly <- c('Brachiopoda', 'Arthropoda', 'Mollusca')
nsim <- 1000


partition <- partition_div(fit)
div_params <- partition[[1]]
nondiv_params <- partition[[2]]
png('../doc/figure/div_transitions.png', width = 480, height = 480)
plot(nondiv_params$'mu[1,3]', log(nondiv_params$'sigma_mu[3]'),
     col='red', pch=16, cex=0.8, xlab="mu[1,3]", ylab="log(sigma_mu[3])")
points(div_params$'mu[1,3]', log(div_params$'sigma_mu[3]'),
       col='green', pch=16, cex=0.8)
dev.off()



# time bins
timerange <- abs(diff(ord))
brks <- timerange / constant
brks <- seq(from = ord[2], to = ord[1], by = brks)
brks <- cbind(brks[-1], brks[-length(brks)])
brks <- brks[rev(seq(nrow(brks))), ]

# posterior predictive /checks
# lots of internal IO
check.result <- Map(function(x) postchecks(x, nsim), shelly)
chckm <- bayesplot_grid(check.result$Brachiopoda$checks$mean,
                        check.result$Trilobita$checks$mean,
                        check.result$Bivalvia$checks$mean,
                        check.result$Gastropoda$checks$mean,
                        grid_args = list(ncol = 2),
                        titles = c('Brac mean', 'Tri mean',
                                   'Biv mean', 'Gas mean'))
ggsave(plot = chckm, filename = '../doc/figure/ppc_mean.png',
       width = 10, height = 8)
chcks <- bayesplot_grid(check.result$Brachiopoda$checks$sd,
                        check.result$Trilobita$checks$sd,
                        check.result$Bivalvia$checks$sd,
                        check.result$Gastropoda$checks$sd,
                        grid_args = list(ncol = 2),
                        titles = c('Brac sd', 'Tri sd',
                                   'Biv sd', 'Gas sd'))
ggsave(plot = chcks, filename = '../doc/figure/ppc_sd.png',
       width = 10, height = 8)
chckroot <- bayesplot_grid(check.result$Brachiopoda$checks$root,
                        check.result$Trilobita$checks$root,
                        check.result$Bivalvia$checks$root,
                        check.result$Gastropoda$checks$root,
                        grid_args = list(ncol = 2),
                        titles = c('Brac root', 'Tri root',
                                   'Biv root', 'Gas root'))
ggsave(plot = chckroot, filename = '../doc/figure/ppc_root.png',
       width = 10, height = 8)
chckerr <- bayesplot_grid(check.result$Brachiopoda$checks$err,
                        check.result$Trilobita$checks$err,
                        check.result$Bivalvia$checks$err,
                        check.result$Gastropoda$checks$err,
                        grid_args = list(ncol = 2),
                        titles = c('Brac err', 'Tri err',
                                   'Biv err', 'Gas err'))
ggsave(plot = chckerr, filename = '../doc/figure/ppc_err.png',
       width = 10, height = 8)
chckecdf <- bayesplot_grid(check.result$Brachiopoda$checks$ecdf,
                        check.result$Trilobita$checks$ecdf,
                        check.result$Bivalvia$checks$ecdf,
                        check.result$Gastropoda$checks$ecdf,
                        grid_args = list(ncol = 2),
                        titles = c('Brac ecdf', 'Tri ecdf',
                                   'Biv ecdf', 'Gas ecdf'))
ggsave(plot = chckecdf, filename = '../doc/figure/ppc_ecdf.png',
       width = 10, height = 8)
chckdens <- bayesplot_grid(check.result$Brachiopoda$checks$dens,
                        check.result$Trilobita$checks$dens,
                        check.result$Bivalvia$checks$dens,
                        check.result$Gastropoda$checks$dens,
                        grid_args = list(ncol = 2),
                        titles = c('Brac dens', 'Tri dens',
                                   'Biv dens', 'Gas dens'))
ggsave(plot = chckdens, filename = '../doc/figure/ppc_dens.png',
       width = 10, height = 8)

pg <- list()
for(ii in seq(length(shelly))) 
  pg[[ii]] <- check.result[[ii]]$checks.time$mean.group



# unit div through time vs estimated div from model
dg <- divtime.plot(shelly, brks)
ggsave(plot = dg, filename = '../doc/figure/unitdiv_time.png',
       width = 10, height = 8)

# covariate effects through time
covname <- c('intercept', 'thickness', 'area', 'subsurface', 'siliciclastic')
cg <- covtime.plot(shelly, brks, covname = covname)
ggsave(plot = cg, filename = '../doc/figure/cov_time.png',
       width = 10, height = 8)
