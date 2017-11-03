library(reshape2)
library(plyr)

library(ggplot2)
library(scales)
library(grid)
library(ggridges)

library(arm)
library(rstan)
library(coda)

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
