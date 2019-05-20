#library(ggplot2)
#library(ggridges)
#library(plyr)
#library(reshape2)
##install.packages("countreg", repos="http://R-Forge.R-project.org")
#library(countreg)
#
##rtpois <- function(n, lambda) {
##  # simulate from zero-truncated poisson
##  x <- qpois(runif(n, dpois(0, lambda), 1), lambda)
##  x
##}
#
#rhurdle <- function(n, theta, lambda) {
#  # theta: probability of generating a 0
#  # lambda: poisson expectation
#
#  out <- array(dim = n)
#
#  # first theta
#  is.zero <- rbinom(n, 1, theta)
#
#  # then lambda
#  not.zero <- !is.zero
#  nz <- rztpois(sum(not.zero), lambda)
#
#  # put it together
#  out[is.zero == 1] <- 0
#  out[not.zero] <- nz
#
#  out
#}
#roverhurdle <- function(n, theta, mu, omega) {
#  # theta: probability of generating a 0
#  # lambda: poisson expectation
#
#  out <- array(dim = n)
#
#  # first theta
#  is.zero <- rbinom(n, 1, theta)
#
#  # then lambda
#  not.zero <- !is.zero
#  nz <- rztnbinom(sum(not.zero), mu = mu, theta = omega)
#
#  # put it together
#  out[is.zero == 1] <- 0
#  out[not.zero] <- nz
#
#  out
#}
#
#
## predict rhurdle
#predict.hurdle <- function(theta, lambda) {
#  ppc.p <- list()
#  for(jj in seq(nsim)) {
#    gg <- grab[jj]
#    oo <- c()
#    for(ii in seq(standata$N_train)) {
#      oo[ii] <- rhurdle(1, 
#                        theta = post$theta[gg, ii], 
#                        lambda = post$lambda_est[gg, ii])
#    }
#    ppc.p[[jj]] <- oo
#  }
#}
#
#
#
#
### hurdle with poisson
##
##n <- 1000
##theta <- c(0.1, 0.25, 0.5, 0.75, 0.9)
##lambda <- c(1, 2, 5, 7, 10)
##
##out <- list()
##for(ii in seq(length(lambda))) {
##  out[[ii]] <- alply(theta, 1, function(x) rhurdle(n, x, lambda[ii]))
##  names(out[[ii]]) <- as.character(theta)
##}
##names(out) <- as.character(lambda)
##out <- melt(out)[, -1]
##names(out) <- c('count', 'theta', 'lambda')
##out$theta <- factor(out$theta, levels = theta)
##out$lambda <- factor(out$lambda, levels = lambda)
##
##sim <- ggplot(out, aes(x = count, y = ..density..))
##sim <- sim + geom_histogram()
##sim <- sim + facet_grid(theta ~ lambda)
##sim <- sim + labs(y = 'Density of simultaed counts', x = 'Count')
##sim <- sim + theme_bw()
##sim <- sim + theme(strip.text = element_text(size = 25),
##                   axis.title = element_text(size = 30),
##                   axis.text = element_text(size = 20))
##ggsave(filename = '../doc/figure/hurdle_simulation.png',
##       plot = sim, height = 8, width = 10)
##
##
##
### hurdle with neg binom
##size <- c(1, 2, 5, 7, 10)
##prob <- theta
##
##out <- list()
##for(ii in seq(length(lambda))) {
##  out[[ii]] <- alply(prob, 1, function(x) 
##                     roverhurdle(n, theta = 0.5, size[ii], x))
##  names(out[[ii]]) <- as.character(prob)
##}
##names(out) <- as.character(lambda)
##out <- melt(out)[, -1]
##names(out) <- c('count', 'prob', 'size')
##out$prob <- factor(out$prob, levels = prob)
##out$size <- factor(out$size, levels = size)
##
##sim2 <- ggplot(out, aes(x = count, y = ..density..))
##sim2 <- sim2 + geom_histogram()
##sim2 <- sim2 + facet_grid(prob ~ size)
##sim2 <- sim2 + labs(y = 'Density of simultaed counts', x = 'Count')
##sim2 <- sim2 + theme_bw()
##sim2 <- sim2 + theme(strip.text = element_text(size = 25),
##                     axis.title = element_text(size = 30),
##                     axis.text = element_text(size = 20))
##ggsave(filename = '../doc/figure/overhurdle_simulation.png',
##       plot = sim2, height = 8, width = 10)
##
