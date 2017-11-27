# internal checks
series.checks <- function(y, ppc) {
  # point estimates
  ppc <- Reduce(rbind, ppc)
  ppc.mean <- ppc_stat(y, ppc, stat = 'mean')
  ppc.sd <- ppc_stat(y, ppc, stat = 'sd')
  ppc.max <- ppc_stat(y, ppc, stat = 'max')
  prop_zero <- function(x) mean(x == 0)
  ppc.zero <- ppc_stat(y, ppc, stat = 'prop_zero')
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

  # all the plots
  out <- list(mean = ppc.mean, 
              sd = ppc.sd, 
              max = ppc.max, 
              zero = ppc.zero, 
              q25 = ppc.q25,
              q75 = ppc.q75,
              err = ppc.err, 
              avgerr = ppc.avgerr, 
              ecdf = ppc.ecdf, 
              root = root.gg)
  out
}



# visualize regression coefs
post.vis <- function(post, unit.info) {
  # theta regression coefficients
  bet.the <- post$beta_the
  colnames(bet.the) <- c(colnames(unit.info$lithology)[-1], 
                         'thickness', 'area', 'contact above', 
                         'contact below', 'subsurface')
  bet.the <- melt(bet.the)

  the.gg <- ggplot(bet.the, aes(x = value, y = Var2))
  the.gg <- the.gg + geom_density_ridges(rel_min_height = 0.01)
  the.gg <- the.gg + theme_ridges()
  the.gg <- the.gg + labs(x = 'estimate', y = 'predictor of theta')

  # lambda regression coefficients
  bet.lam <- post$beta_lam
  colnames(bet.lam) <- c(colnames(unit.info$lithology)[-1], 
                         'thickness', 'area', 'contact above', 
                         'contact below', 'subsurface')
  bet.lam <- melt(bet.lam)

  lam.gg <- ggplot(bet.lam, aes(x = value, y = Var2))
  lam.gg <- lam.gg + geom_density_ridges(rel_min_height = 0.01)
  lam.gg <- lam.gg + theme_ridges()
  lam.gg <- lam.gg + labs(x = 'estimate', y = 'predictor of lambda')


  # back transform the compositional variables
  inv.the <- alply(post$beta_the[, 1:22], 1, function(x) ilrInv(x, orig = unit.info$lithology))
  the.comp.max <- table(laply(inv.the, which.max))
  inv.the.m <- melt(Reduce(cbind, inv.the))[, c(1, 3)]
  inv.the.m$Var1 <- factor(inv.the.m$Var1)
  inv.the.gg <- ggplot(inv.the.m, aes(x = value, y = Var1))
  inv.the.gg <- inv.the.gg + geom_density_ridges(rel_min_height = 0.01)
  inv.the.gg <- inv.the.gg + theme_ridges()


  inv.lam <- alply(post$beta_lam[, 1:22], 1, function(x) ilrInv(x, orig = unit.info$lithology))
  lam.comp.max <- table(laply(inv.lam, which.max))
  inv.lam.m <- melt(Reduce(cbind, inv.lam))[, c(1, 3)]
  inv.lam.m$Var1 <- factor(inv.lam.m$Var1)
  inv.lam.gg <- ggplot(inv.lam.m, aes(x = value, y = Var1))
  inv.lam.gg <- inv.lam.gg + geom_density_ridges(rel_min_height = 0.01)
  inv.lam.gg <- inv.lam.gg + theme_ridges()

  out <- list(theta = the.gg,
              inv.theta = inv.the.gg,
              lambda = lam.gg,
              inv.lambda = inv.lam.gg)
  out
}
