library(reshape2)
library(plyr)

library(ggplot2)
library(scales)
library(grid)
library(ggridges)

library(arm)
library(rstan)
library(coda)

library(brms)
library(pscl)


load(file = '../data/data_dump/unit_image.rdata')


ord.units <- data.frame(x = standata$y)

op <- ggplot(ord.units, aes(x = x, y = ..density..))
op <- op + geom_histogram()
op <- op + labs(y = 'Density of counts', x = 'Count')
op <- op + theme_bw()
op <- op + theme(axis.title = element_text(size = 30),
                 axis.text = element_text(size = 20))
ggsave(filename = '../doc/figure/ordovician_counts.png',
       plot = op, height = 8, width = 10)


dfs <- data.frame(y = standata$y, standata$X)
brm(y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10
    + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19
    + X20 + X21 + X22 + X23 + X24 + X25 + X26
    , data = dfs, family = hurdle_poisson())
