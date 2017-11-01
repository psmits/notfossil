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


standata <- list()

foss.info <- process.fossil(fossil.ord)
unit.info <- process.strat(strat.ord)  # everything is already transformed

# for some reason there area a few misaligns
match.match <- foss.info$unit %in% unit.info$unit.id
foss.info <- foss.info[match.match, ]

# now it is about making an output file for stan

# first set up the response variable
# how many fossils in each unit???
unit.count <- laply(split(foss.info, foss.info$unit), function(x) 
                    c(unit = x$unit[1], count = sum(x$count)))
unit.count <- apply(unit.count, 2, as.numeric)
exposure <- rep(0, length(unit.info$unit.id))
exposure[match(unit.count[, 1], unit.info$unit.id)] <- unit.count[, 2]
standata$exposure <- exposure + 1

# set up every unit for every order
by.ord <- split(foss.info, foss.info$order)
by.ord <- by.ord[laply(by.ord, nrow) > 5]

out <- list()
nz <- c()
for(ii in seq(length(by.ord))) {
  o.count <- rep(0, length(unit.info$unit.id))
  splts <- match(by.ord[[ii]]$unit, unit.info$unit.id)
  o.count[splts] <- by.ord[[ii]]$count
  out[[ii]] <- o.count
  nz[ii] <- sum(o.count == 0)

}
out <- melt(out)

standata$y <- out[, 1]  # count of order in unit
standata$o <- out[, 2]  # order being looked at
standata$O <- length(by.ord)
standata$u <- rep(seq(length(unit.info$unit.id)), times = length(by.ord))
standata$U <- length(unit.info$unit.id)
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
                    #unit.info$change, 
                    #unit.info$location$top.temp, 
                    #unit.info$location$bot.temp, 
                    #unit.info$location$cross.eq, 
                    #unit.info$location$switches, 
                    #unit.info$duration,
                    unit.info$subsurface)
standata$K <- ncol(standata$X)



# export the data
# for stan
with(standata, {stan_rdump(list = alply(names(standata), 1), 
                           file = '../data/data_dump/unit_data.data.R')})
# in less manipuated form
save(standata, unit.info, foss.info, 
     file = '../data/data_dump/unit_image.rdata')
