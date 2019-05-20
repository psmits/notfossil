## code for managing all the posterior predictive checks
## simulation,
## individual checks,
## group checks
## comparison plots that are called as sideeffects
#
#
## posterior predictive simulations
#postpred <- function(post, sim) {
#  N <- ncol(post$location)
#  grab <- sample(4000, nsim)
#  ppc <- list()
#  for(jj in seq(nsim)) {
#    gg <- grab[jj]
#    oo <- c()
#    for(ii in seq(N)) {
#      oo[ii] <- rztnbinom(1, mu = post$location[gg, ii], theta = post$phi[gg])
#    }
#    ppc[[jj]] <- oo
#  }
#  ppc
#}
#
## summary statistic for time points from posterior predictive dist
#get_postpredstat <- function(shelly, type, nsim, foo = mean) {
#  out <- list()
#  for(ii in seq(length(shelly))) {
#    load(paste0('../data/data_dump/diversity_image_', 
#                shelly[ii], '_', type, '.rdata'))
#    post <- read_mcmcout(shelly[ii], type)
#    pp <- postpred(post, nsim)
#
#    by.time <- llply(pp, function(x) split(x, standata$t))
#    mean.time <- llply(by.time, function(x) laply(x, foo))
#
#    mean.time <- llply(mean.time, function(x) {
#                         x <- cbind(x, seq(nrow(brks)))
#                         x})
#
#    mean.time <- Reduce(rbind, 
#                        Map(function(x, y) 
#                            cbind(x, y), mean.time, seq(nsim)))
#    mean.time <- data.frame(mean.time, g = shelly[ii])
#    names(mean.time) <- c('est', 'time', 'sim', 'g')
#    out[[ii]] <- mean.time
#  }
#  names(out) <- shelly
#
#  out                                  # list by taxonomic group
#}
#
#
## posterior predictive checks
## lots of internal IO
#postchecks<- function(shelly, type, nsim, silent = FALSE) {
#  post <- read_mcmcout(shelly, type, silent = silent)
#
#  load(paste0('../data/data_dump/diversity_image_', 
#              shelly, 
#              '_', 
#              type, 
#              '.rdata'))
#
#  ppc <- postpred(post, nsim)
#
#  # posterior predictive checks
#  checks <- checks.single(standata$y, ppc)
#  checks.time <- checks.group(standata$y, ppc, group = standata$t)
#  out <- list(data = standata, post = post,
#              checks = checks, checks.time = checks.time)
#  out
#}
#
#
#
#
#q75 <- function(x) quantile(x, 0.75)
#q25 <- function(x) quantile(x, 0.25)
#
## internal checks
#checks.single <- function(y, ppc) {
#  ppc <- Reduce(rbind, ppc)
#
#  # density
#  ppc.dens <- ppc_dens_overlay(y, ppc[1:50, ])
#  ppc.hist <- ppc_hist(y, ppc[1:5, ])
#
#  # point estimates
#  ppc.mean <- ppc_stat(y, ppc, stat = 'mean')
#  ppc.sd <- ppc_stat(y, ppc, stat = 'sd')
#  ppc.max <- ppc_stat(y, ppc, stat = 'max')
#  #prop_zero <- function(x) mean(x == 0)
#  #ppc.zero <- ppc_stat(y, ppc, stat = 'prop_zero')
#  ppc.q25 <- ppc_stat(y, ppc, stat = 'q25')
#  ppc.q75 <- ppc_stat(y, ppc, stat = 'q75')
#
#  # error
#  ppc.err <- ppc_error_scatter(y, ppc[1:6, ])
#  ppc.avgerr <- ppc_error_scatter_avg(y, ppc)
#  ppc.ecdf <- ppc_ecdf_overlay(y, ppc[1:50, ])
#
#  # rootograms because i find them easy to read
#  #root.gg <- ppc_rootogram(y, ppc, style = 'hanging', prob = 0.8)
#
#  # bars
#  ppc.bars <- ppc_bars(y, ppc)
#
#  # output
#  out <- list(mean = ppc.mean, 
#              sd = ppc.sd, 
#              max = ppc.max, 
#              #zero = ppc.zero, 
#              q25 = ppc.q25,
#              q75 = ppc.q75,
#              err = ppc.err, 
#              avgerr = ppc.avgerr, 
#              ecdf = ppc.ecdf, 
#              #root = root.gg,
#              bar = ppc.bars,
#              dens = ppc.dens,
#              hist = ppc.hist)
#  out
#}
#
## internal checks
#checks.group <- function(y, ppc, group) {
#  ppc <- Reduce(rbind, ppc)
#
#  # by group
#  ppc.bars.group <- ppc_bars_grouped(y, ppc, group = group)
#  ppc.mean.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'mean')
#  ppc.sd.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'sd')
#  ppc.max.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'max')
#  ppc.q25.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'q25')
#  ppc.q75.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'q75')
#  ppc.avgerr.group <- ppc_scatter_avg_grouped(y, ppc, group = group)
#  ppc.violin.group <- ppc_violin_grouped(y, ppc, group = group, 
#                                         y_draw = 'points')
#
#  # all the plots
#  out <- list(bar.group = ppc.bars.group,
#              mean.group = ppc.mean.group,
#              sd.group = ppc.sd.group,
#              max.group = ppc.max.group,
#              q25.group = ppc.q25.group,
#              q75.group = ppc.q75.group,
#              avgerr.group = ppc.avgerr.group,
#              violin.group = ppc.violin.group
#              )
#  out
#}
#
#
## plot out all the posterior predictive checks
#plot_postchecks <- function(check.result, type) {
#  # make plots for the posterior predictive tests
#  chckm <- bayesplot_grid(check.result$Anthozoa$checks$mean,
#                          check.result$Bivalvia$checks$mean,
#                          check.result$Brachiopoda$checks$mean,
#                          check.result$Gastropoda$checks$mean,
#                          check.result$Mollusca$checks$mean,
#                          check.result$Trilobita$checks$mean,
#                          grid_args = list(ncol = 2),
#                          titles = c( 'Anth mean',
#                                      'Biv mean', 
#                                      'Brac mean', 
#                                      'Gas mean',
#                                      'Mol mean',
#                                      'Tri mean'))
#  tn <- paste0('../doc/figure/ppc_mean_', type, '.png')
#  ggsave(plot = chckm, filename = tn, width = 10, height = 8)
#
#  chcks <- bayesplot_grid(check.result$Anthozoa$checks$sd,
#                          check.result$Bivalvia$checks$sd,
#                          check.result$Brachiopoda$checks$sd,
#                          check.result$Gastropoda$checks$sd,
#                          check.result$Mollusca$checks$sd,
#                          check.result$Trilobita$checks$sd,
#                          grid_args = list(ncol = 2),
#                          titles = c( 'Anth sd',
#                                      'Biv sd', 
#                                      'Brac sd', 
#                                      'Gas sd',
#                                      'Mol sd', 
#                                      'Tri sd'))
#  tn <- paste0('../doc/figure/ppc_sd_', type, '.png')
#  ggsave(plot = chcks, filename = tn, width = 10, height = 8)
#
#  #chckroot <- bayesplot_grid(check.result$Anthozoa$checks$root,
#  #                           check.result$Bivalvia$checks$root,
#  #                           check.result$Brachiopoda$checks$root,
#  #                           check.result$Gastropoda$checks$root,
#  #                           check.result$Mollusca$checks$root,
#  #                           check.result$Trilobita$checks$root,
#  #                           grid_args = list(ncol = 2),
#  #                           titles = c( 'Anth root',
#  #                                       'Biv root', 
#  #                                       'Brac root', 
#  #                                       'Gas root',
#  #                                       'Mol root', 
#  #                                       'Tri root'))
#  #tn <- paste0('../doc/figure/ppc_root_', type, '.png')
#  #ggsave(plot = chckroot, filename = tn, width = 10, height = 8)
#
#
#
#
#  #d1 <- check.result$Anthozoa$checks$root + coord_cartesian(xlim = c(-1, 65))
#  #d2 <- check.result$Bivalvia$checks$root + coord_cartesian(xlim = c(-1, 65))
#  #d3 <- check.result$Brachiopoda$checks$root + coord_cartesian(xlim = c(-1, 65))
#  #d4 <- check.result$Gastropoda$checks$root + coord_cartesian(xlim = c(-1, 65))
#  #d5 <- check.result$Mollusca$checks$root + coord_cartesian(xlim = c(-1, 65))
#  #d6 <- check.result$Trilobita$checks$root + coord_cartesian(xlim = c(-1, 65))
#  #chckroot <- bayesplot_grid(d1, 
#  #                           d2, 
#  #                           d3, 
#  #                           d4, 
#  #                           d5, 
#  #                           d6, 
#  #                           grid_args = list(ncol = 2),
#  #                           titles = c( 'Anth root',
#  #                                       'Biv root', 
#  #                                       'Brac root', 
#  #                                       'Gas root',
#  #                                       'Mol root', 
#  #                                       'Tri root'))
#  #tn <- paste0('../doc/figure/ppc_root_zoom_', type, '.png')
#  #ggsave(plot = chckroot, filename = tn, width = 10, height = 8)
#
#  chckerr <- bayesplot_grid(check.result$Anthozoa$checks$err,
#                            check.result$Bivalvia$checks$err,
#                            check.result$Brachiopoda$checks$err,
#                            check.result$Gastropoda$checks$err,
#                            check.result$Mollusca$checks$err,
#                            check.result$Trilobita$checks$err,
#                            grid_args = list(ncol = 2),
#                            titles = c( 'Anth err',
#                                        'Biv err', 
#                                        'Brac err', 
#                                        'Gas err',
#                                        'Mol err', 
#                                        'Tri err'))
#  tn <- paste0('../doc/figure/ppc_err_', type, '.png')
#  ggsave(plot = chckerr, filename = tn, width = 10, height = 8)
#
#  chckecdf <- bayesplot_grid(check.result$Anthozoa$checks$ecdf,
#                             check.result$Bivalvia$checks$ecdf,
#                             check.result$Brachiopoda$checks$ecdf,
#                             check.result$Gastropoda$checks$ecdf,
#                             check.result$Mollusca$checks$ecdf,
#                             check.result$Trilobita$checks$ecdf,
#                             grid_args = list(ncol = 2),
#                             titles = c( 'Anth ecdf',
#                                         'Biv ecdf', 
#                                         'Brac ecdf', 
#                                         'Gas ecdf',
#                                         'Mol ecdf', 
#                                         'Tri ecdf'))
#  tn <- paste0('../doc/figure/ppc_ecdf_', type, '.png')
#  ggsave(plot = chckecdf, filename = tn, width = 10, height = 8)
#
#  chckdens <- bayesplot_grid(check.result$Anthozoa$checks$dens,
#                             check.result$Bivalvia$checks$dens,
#                             check.result$Brachiopoda$checks$dens,
#                             check.result$Gastropoda$checks$dens,
#                             check.result$Mollusca$checks$dens,
#                             check.result$Trilobita$checks$dens,
#                             grid_args = list(ncol = 2),
#                             titles = c( 'Anth dens',
#                                         'Biv dens', 
#                                         'Brac dens', 
#                                         'Gas dens',
#                                         'Mol dens', 
#                                         'Tri dens'))
#  tn <- paste0('../doc/figure/ppc_dens_', type, '.png')
#  ggsave(plot = chckdens, filename = tn, width = 10, height = 8)
#
#  d1 <- check.result$Anthozoa$checks$dens + coord_cartesian(xlim = c(-1, 65))
#  d2 <- check.result$Bivalvia$checks$dens + coord_cartesian(xlim = c(-1, 65))
#  d3 <- check.result$Brachiopoda$checks$dens + coord_cartesian(xlim = c(-1, 65))
#  d4 <- check.result$Gastropoda$checks$dens + coord_cartesian(xlim = c(-1, 65))
#  d5 <- check.result$Mollusca$checks$dens + coord_cartesian(xlim = c(-1, 65))
#  d6 <- check.result$Trilobita$checks$dens + coord_cartesian(xlim = c(-1, 65))
#  chckdens <- bayesplot_grid(d1, 
#                             d2, 
#                             d3, 
#                             d4, 
#                             d5, 
#                             d6, 
#                             grid_args = list(ncol = 2),
#                             titles = c( 'Anth dens',
#                                         'Biv dens', 
#                                         'Brac dens', 
#                                         'Gas dens',
#                                         'Mol dens', 
#                                         'Tri dens'))
#  tn <- paste0('../doc/figure/ppc_dens_zoom_', type, '.png')
#  ggsave(plot = chckdens, filename = tn, width = 10, height = 8)
#
#
#  ## group ppc-s
#  #pm <- pv <- list()
#  #for(ii in seq(length(shelly))) {
#  #  pm[[ii]] <- check.result[[ii]]$checks.time$mean.group
#  #  pv[[ii]] <- check.result[[ii]]$checks.time$violin.group
#  #}
#
#}
