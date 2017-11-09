library(arm)
library(plyr)
library(stringr)
library(compositions)
library(geosphere)
library(reshape2)
library(rstan)
library(caret)
#library(glmnet)
#library(rpart)

source('fossil_functions.r')
source('fossil_mung.r')
source('rock_functions.r')
source('rock_mung.r')



# bring in data
source('download_scrap.r')

# clean data
#shelly <- c('Arthropoda', 'Brachiopoda', 'Mollusca', 'Echinodermata', 
#            'Hemichordata', 'Bryozoa', 'Cnidaria')
shelly <- 'Brachiopoda'
foss.info <- process.fossil(fossil.ord, shelly = shelly)
unit.info <- process.strat(strat.ord)  # everything is already transformed

# for some reason there area a few misaligns
match.match <- foss.info$unit %in% unit.info$unit.id
foss.info <- foss.info[match.match, ]

# now it is about making an output file for stan
standata <- list()
#unit.info$unit.id %in% foss.info$unit
by.unit <- split(foss.info, foss.info$unit)
by.unit <- data.frame(unit = as.character(names(by.unit)),
                      count = laply(by.unit, function(x) sum(x$count)))

# set up every unit for every shelly
by.phyl <- split(foss.info, foss.info$phylum)
by.phyl <- llply(by.phyl, function(x) split(x, x$unit))
by.phyl <- melt(llply(by.phyl, function(x) llply(x, function(y) sum(y$count))))
names(by.phyl) <- c('count', 'unit', 'phyl')
by.phyl <- split(by.phyl, by.phyl$phyl)

# response matrix!
ymat <- matrix(0, nrow = length(unit.info$unit.id), ncol = length(shelly))
for(ii in seq(ncol(ymat))) {
  mm <- match(by.phyl[[ii]]$unit, unit.info$unit.id)
  ymat[mm, ii] <- by.phyl[[ii]]$count
}


#out <- list()
#nz <- c()
#for(ii in seq(length(by.ord))) {
u.count <- rep(0, length(unit.info$unit.id))
u.count[unit.info$unit %in% by.unit$unit] <- by.unit$count

#
standata$y <- u.count # count of order in unit
#standata$o <- out[, 2]  # order being looked at
#standata$O <- length(by.ord)
#standata$u <- rep(seq(length(unit.info$unit.id)), times = length(by.ord))
#standata$U <- length(unit.info$unit.id)
standata$N <- length(standata$y)


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
                    unit.info$subsurface)
                    #unit.info$change, 
                    #unit.info$location$top.temp, 
                    #unit.info$location$bot.temp, 
                    #unit.info$location$cross.eq, 
                    #unit.info$location$switches, 
                    #unit.info$duration,
standata$K <- ncol(standata$X)

inc <- standata$y ==0
standata$zi <- inc * 1
standata$Nz <- sum(inc)
standata$Nnz <- sum(!inc)
standata$Xnz <- standata$X[!inc, ]
standata$ynz <- standata$y[!inc]



# export the data
# for stan
with(standata, {stan_rdump(list = alply(names(standata), 1), 
                           file = '../data/data_dump/unit_data.data.R')})
# in less manipuated form
save(standata, unit.info, foss.info, 
     file = '../data/data_dump/unit_image.rdata')
