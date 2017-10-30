library(arm)
library(plyr)
library(stringr)
library(compositions)
library(geosphere)
library(reshape2)
library(rstan)
#library(glmnet)
#library(rpart)

source('fossil_functions.r')
source('fossil_mung.r')
source('rock_functions.r')
source('rock_mung.r')

source('download_scrap.r')
# FYI: match(fossil.ord$unit_id, strat.ord$unit_id)  # match fossil occurrence to strat unit
standata <- list()

foss.info <- process.fossil(fossil.ord)
unit.info <- process.strat(strat.ord)  # everything is already transformed

# for some reason there area a few misaligns
match.match <- foss.info$unit %in% unit.info$unit.id
foss.info <- foss.info[match.match, ]

# now it is about making an output file for stan

# first set up the response variable
# zero responses
zero.count <- unit.info$unit.id[!(unit.info$unit.id %in% foss.info$unit)]  
standata$yz <- rep(0, length(zero.count))  # zero response
standata$Nz <- length(zero.count)

# nonzero responses
standata$ynz <- foss.info$count
standata$Nnz <- length(foss.info$count)


# for unit membership, have to standardize for all counts
unit.info$unit.stan <- seq(length(unit.info$unit.id))
# unit membership for zero counts
standata$uz <- unit.info$unit.stan[!(unit.info$unit.id %in% foss.info$unit)]

foss.info$unit.stan <- 
  unit.info$unit.stan[match(foss.info$unit, unit.info$unit.id)]
# unit memership for nonzero counts
standata$unz <- foss.info$unit.stan


# X will be unit-level covariates
#   (ilr transform) lithology
#   thickness high
#   thickness low
#   area
#   contact above
#   contact below
#   tropical vs temperate bottom
#   tropical vs temperate top
#   tropical/temperate change
#   cross equator
standata$X <- cbind(unit.info$lithology$ilr.trans, 
                    unit.info$thickness$high, 
                    unit.info$column.area, 
                    unit.info$contact$above, 
                    unit.info$contact$below, 
                    #unit.info$change, 
                    #unit.info$location$top.temp, 
                    #unit.info$location$bot.temp, 
                    #unit.info$location$cross.eq, 
                    #unit.info$location$switches, 
                    #unit.info$duration,
                    unit.info$subsurface)
standata$K <- ncol(standata$X)
standata$Nu <- nrow(standata$X)


# for when there are fossils
# taxon information
# taxon hierachy
#   num orders
#   order membership
#   num class
#   class membership
#   num phylum
#   phylum membership
tn <- apply(foss.info[, 1:3], 2, function(x) as.numeric(as.factor(x)))
foss.info <- cbind(tn, foss.info)
names(foss.info)[1:3] <- c('phylum.num', 'class.num', 'order.num')
standata$o <- foss.info$order.num
standata$O <- max(standata$o)

# match orders in classes
uc <- unique(foss.info[2:3])
uc <- uc[order(uc[, 2]), ]
standata$c <- uc[, 1]
standata$C <- max(standata$c)

# match classes in phyla
up <- unique(foss.info[1:2])
up <- up[order(up[, 2]), ]
standata$p <- up[, 1]
standata$P <- max(standata$p)


# export the data
# for stan
with(standata, {stan_rdump(list = alply(names(standata), 1), 
                           file = '../data/data_dump/unit_data.data.R')})
# in less manipuated form
save(standata, unit.info, foss.info, 
     file = '../data/data_dump/unit_image.rdata')
