library(ggplot2)
library(ggridges)
library(plyr)
library(reshape2)

rtpois <- function(n, lambda) {
  # simulate from zero-truncated poisson
  x <- qpois(runif(n, dpois(0, lambda), 1), lambda)
  x
}

rhurdle <- function(n, theta, lambda) {
  # theta: probability of generating a 0
  # lambda: poisson expectation

  out <- array(dim = n)
  
  # first theta
  is.zero <- rbinom(n, 1, theta)
  
  # then lambda
  not.zero <- !is.zero
  nz <- rtpois(sum(not.zero), lambda)

  # put it together
  out[is.zero == 1] <- 0
  out[not.zero] <- nz

  out
}
  

n <- 5000
theta <- c(0.1, 0.25, 0.5, 0.75, 0.9)
lambda <- 5 

sim <- alply(theta, 1, function(x) rhurdle(n, x, lambda))
names(sim) <- theta
sim <- melt(sim)

ds <- data.frame(sim)
names(ds) <- c('n', 'count', 'sim')



sim.out <- ggplot(ds, aes(x = count, y = ..count..))
sim.out <- sim.out + geom_histogram()
sim.out <- sim.out + facet_grid(sim ~ ., switch = 'y')
sim.out <- sim.out + labs(y = expression(paste('Simulated value of ', Theta)))
sim.out <- sim.out + scale_x_continuous(breaks = seq(0, max(ds$count), 1))
sim.out <- sim.out + theme_minimal()
sim.out <- sim.out + theme(axis.text.y = element_blank(),
                           strip.text = element_text(size = 25))
