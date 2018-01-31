# internal checks
series.checks <- function(y, ppc, group = NULL) {
  ppc <- Reduce(rbind, ppc)

  # density
  ppc.dens <- ppc_dens_overlay(y, ppc[1:50, ])
  ppc.hist <- ppc_hist(y, ppc[1:5, ])

  # point estimates
  ppc.mean <- ppc_stat(y, ppc, stat = 'mean')
  ppc.sd <- ppc_stat(y, ppc, stat = 'sd')
  ppc.max <- ppc_stat(y, ppc, stat = 'max')
  #prop_zero <- function(x) mean(x == 0)
  #ppc.zero <- ppc_stat(y, ppc, stat = 'prop_zero')
  q25 <- function(x) quantile(x, 0.25)
  ppc.q25 <- ppc_stat(y, ppc, stat = 'q25')
  q75 <- function(x) quantile(x, 0.75)
  ppc.q75 <- ppc_stat(y, ppc, stat = 'q75')

  # error
  ppc.err <- ppc_error_scatter(y, ppc[1:6, ])
  ppc.avgerr <- ppc_error_scatter_avg(y, ppc)
  ppc.ecdf <- ppc_ecdf_overlay(y, ppc[1:50, ])

  # rootograms because i find them easy to read
  root.gg <- ppc_rootogram(y, ppc, style = 'hanging', prob = 0.8)


  # now by group
  ppc.mean.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'mean')
  ppc.sd.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'sd')
  ppc.max.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'max')
  ppc.q25.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'q25')
  ppc.q75.group <- ppc_stat_grouped(y, ppc, group = group, stat = 'q75')
  ppc.avgerr.group <- ppc_scatter_avg_grouped(y, ppc, group = group)


  # all the plots
  out <- list(mean = ppc.mean, 
              sd = ppc.sd, 
              max = ppc.max, 
              #zero = ppc.zero, 
              q25 = ppc.q25,
              q75 = ppc.q75,
              err = ppc.err, 
              avgerr = ppc.avgerr, 
              ecdf = ppc.ecdf, 
              root = root.gg,
              mean.group = ppc.mean.group,
              sd.group = ppc.sd.group,
              max.group = ppc.max.group,
              q25.group = ppc.q25.group,
              q75.group = ppc.q75.group,
              avgerr.group = ppc.avgerr.group
              )
  out
}
