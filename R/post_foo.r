# internal checks
series.checks <- function(y, ppc, lw = NULL) {
  ppc <- Reduce(rbind, ppc)

  # density
  ppc.dens <- ppc_dens_overlay(y, ppc[1:50, ])
  ppc.hist <- ppc_hist(y, ppc[1:5, ])

  # point estimates
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
  if(!is.null(lw)) {
    loo.pit <- ppc_loo_pit(y, ppc, lw = lw, compare = 'normal')
    #loo.int <- ppc_loo_intervals(y, ppc, lw = lw)
    #loo.rib <- ppc_loo_ribbon(y, ppc, lw = lw)
    out$loo.pit <- loo.pit
  }

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
  the.gg <- the.gg + labs(x = 'estimate (log-odds)', y = 'predictor of theta')

  # lambda regression coefficients
  bet.lam <- post$beta_lam
  colnames(bet.lam) <- c(colnames(unit.info$lithology)[-1], 
                         'thickness', 'area', 'contact above', 
                         'contact below', 'subsurface')
  bet.lam <- melt(bet.lam)

  lam.gg <- ggplot(bet.lam, aes(x = value, y = Var2))
  lam.gg <- lam.gg + geom_density_ridges(rel_min_height = 0.01)
  lam.gg <- lam.gg + theme_ridges()
  lam.gg <- lam.gg + labs(x = 'estimate (E[log diversity])', y = 'predictor of lambda')


  # back transform the compositional variables
  gr <- ncol(unit.info$lithology) - 1
  inv.the <- alply(post$beta_the[, 1:gr], 1, function(x) ilrInv(x, orig = unit.info$lithology))
  the.comp.max <- table(laply(inv.the, which.max))
  inv.the.m <- melt(Reduce(cbind, inv.the))[, c(1, 3)]
  inv.the.m$Var1 <- mapvalues(inv.the.m$Var1, 
                              from = unique(inv.the.m$Var1), 
                              to = colnames(unit.info$lithology))
  inv.the.m$Var1 <- factor(inv.the.m$Var1)
  inv.the.gg <- ggplot(inv.the.m, aes(x = value, y = Var1))
  inv.the.gg <- inv.the.gg + geom_density_ridges(rel_min_height = 0.01)
  inv.the.gg <- inv.the.gg + theme_ridges()
  inv.the.gg <- inv.the.gg + labs(x = 'estimate (composition)', y = 'predictor of theta')


  inv.lam <- alply(post$beta_lam[, 1:gr], 1, function(x) ilrInv(x, orig = unit.info$lithology))
  lam.comp.max <- table(laply(inv.lam, which.max))
  inv.lam.m <- melt(Reduce(cbind, inv.lam))[, c(1, 3)]
  inv.lam.m$Var1 <- mapvalues(inv.lam.m$Var1, 
                              from = unique(inv.lam.m$Var1), 
                              to = colnames(unit.info$lithology))
  inv.lam.m$Var1 <- factor(inv.lam.m$Var1)
  inv.lam.gg <- ggplot(inv.lam.m, aes(x = value, y = Var1))
  inv.lam.gg <- inv.lam.gg + geom_density_ridges(rel_min_height = 0.01)
  inv.lam.gg <- inv.lam.gg + theme_ridges()
  inv.lam.gg <- inv.lam.gg + labs(x = 'estimate (composition)', y = 'predictor of theta')

  out <- list(theta = the.gg,
              inv.theta = inv.the.gg,
              lambda = lam.gg,
              inv.lambda = inv.lam.gg)
  out
}




# analysis of full dataset
analyze.posterior <- function(shelly, nsim, grab) {
  out <- list()
  for(kk in seq(length(shelly))) {

    load(paste0('../data/data_dump/unit_image_', shelly[kk], '.rdata'))

    pat <- paste0('train\\_[0-9]\\_', shelly[kk])
    files <- list.files('../data/mcmc_out', pattern = pat, full.names = TRUE)

    # pois
    fit <- read_stan_csv(files[5:8])
    check_all_diagnostics(fit)
    check_treedepth(fit, max_depth = 15)
    post <- rstan::extract(fit, permuted = TRUE)

    # nb
    fit2 <- read_stan_csv(files[1:4])
    check_all_diagnostics(fit2)
    check_treedepth(fit2, max_depth = 15)
    post2 <- rstan::extract(fit2, permuted = TRUE)

    # training set
    # loo and waic
    postlik <- extract_log_lik(fit)
    postloo <- loo(postlik)
    postwaic <- waic(postlik)

    post2lik <- extract_log_lik(fit2)
    post2loo <- loo(post2lik)
    post2waic <- waic(post2lik)

    #hurdleloo <- compare(postloo, post2loo)
    #hurdlewaic <- compare(postwaic, post2waic)


    # posterior predictive simulations for train set
    ppc.p <- list()
    for(jj in seq(nsim)) {
      gg <- grab[jj]
      oo <- c()
      for(ii in seq(standata$N_train)) {
        oo[ii] <- rhurdle(1, 
                          theta = post$theta[gg, ii], 
                          lambda = post$lambda_est[gg, ii])
      }
      ppc.p[[jj]] <- oo
    }
    ppc.nb <- list()
    for(jj in seq(nsim)) {
      gg <- grab[jj]
      oo <- c()
      for(ii in seq(standata$N_train)) {
        oo[ii] <- roverhurdle(1, 
                              theta = post2$theta[gg, ii], 
                              mu = post2$lambda_est[gg, ii],
                              omega = post2$phi[gg])
      }
      ppc.nb[[jj]] <- oo
    }

    # internal checks
    psis <- psislw(-extract_log_lik(fit), cores = 2)
    lw = psis$lw_smooth[grab, ]
    psis2 <- psislw(-extract_log_lik(fit2), cores = 2)
    lw2 = psis2$lw_smooth[grab, ]
    po.check <- series.checks(standata$y_train, ppc.p, lw)
    nb.check <- series.checks(standata$y_train, ppc.nb, lw2)


    # visualize regression coefs
    po.vis <- post.vis(post, unit.info)
    nb.vis <- post.vis(post2, unit.info)


    # testing set
    # posterior predictive simulations for test set
    pre.p <- list()
    for(jj in seq(nsim)) {
      gg <- grab[jj]
      oo <- c()
      for(ii in seq(standata$N_test)) {
        oo[ii] <- rhurdle(1, 
                          theta = post$theta_test[gg, ii], 
                          lambda = post$lambda_test[gg, ii])
      }
      pre.p[[jj]] <- oo
    }
    pre.nb <- list()
    for(jj in seq(nsim)) {
      gg <- grab[jj]
      oo <- c()
      for(ii in seq(standata$N_test)) {
        oo[ii] <- roverhurdle(1, 
                              theta = post2$theta_test[gg, ii], 
                              mu = post2$lambda_test[gg, ii],
                              omega = post2$phi[gg])
      }
      pre.nb[[jj]] <- oo
    }
    po.test <- series.checks(standata$y_test, pre.p)
    nb.test <- series.checks(standata$y_test, pre.nb)

    out[[kk]] <- list(data = standata,
                      posteriors = list(po = post, nb = post2),
                      loo = list(po = postloo, nb = post2loo), 
                      waic = list(po = postwaic, nb = post2waic),
                      po.check = po.check, nb.check = nb.check,
                      po.vis = po.vis, nb.vis = nb.vis,
                      po.test = po.test, nb.test = nb.test)
  }
  names(out) <- shelly
  out
}








analyze.cv <- function(shelly, nsim, grab, kfold) {
  #
  out <- list()
  for(mm in seq(length(shelly))) {
    targets <- list()
    for(kk in seq(kfold)) {
      pat <- paste0('../data/data_dump/unit_image_', shelly[mm], '_fold', kk, '.rdata')
      load(pat)
      targets[[kk]] <- list(y = standata$y_test, N = standata$N_test)
    }

    pat <- paste0('train\\_fold[0-9]\\_[0-9]\\_', shelly[mm])
    files <- list.files('../data/mcmc_out', pattern = pat, full.names = TRUE)
    bymodel <- str_detect(files, pattern = 'over_train')
    pomodel <- files[!bymodel]
    nbmodel <- files[bymodel]
    # k-folds exist
    pofold <- split(pomodel, rep(seq(kfold), each = 4))
    nbfold <- split(nbmodel, rep(seq(kfold), each = 4))

    pofit <- Map(read_stan_csv, pofold)
    popost <- Map(function(x) rstan::extract(x, permuted = TRUE), pofit)
    nbfit <- Map(read_stan_csv, nbfold)
    nbpost <- Map(function(x) rstan::extract(x, permuted = TRUE), nbfit)


    # estimate RMSE for each fold
    # dist of 1000 RMSE for each fold
    # average across folds
    pormse <- list()
    for(kk in seq(kfold)) {
      rmse <- list()
      for(jj in seq(nsim)) {
        gg <- grab[jj]
        oo <- c()
        for(ii in seq(targets[[kk]]$N)) {
          oo[ii] <- rhurdle(1, 
                            theta = popost[[kk]]$theta_test[gg, ii], 
                            lambda = popost[[kk]]$lambda_test[gg, ii])
        }
        rmse[[jj]] <- sqrt(mean((oo - targets[[kk]]$y)^2))
      }
      pormse[[kk]] <- rmse
    }
    nbrmse <- list()
    for(kk in seq(kfold)) {
      rmse <- list()
      for(jj in seq(nsim)) {
        gg <- grab[jj]
        oo <- c()
        for(ii in seq(targets[[kk]]$N)) {
          oo[ii] <- roverhurdle(1, 
                                theta = nbpost[[kk]]$theta_test[gg, ii], 
                                mu = nbpost[[kk]]$lambda_test[gg, ii],
                                omega = nbpost[[kk]]$phi[gg])
        }
        rmse[[jj]] <- sqrt(mean((oo - targets[[kk]]$y)^2))
      }
      nbrmse[[kk]] <- rmse
    }

    out[[mm]] <- list(poisson = pormse, negbin = nbrmse)
  }
  names(out) <- shelly
  out
}
