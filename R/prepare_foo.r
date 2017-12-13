# complete dataset
export.standata <- function(fossil.ord, strat.ord, bracket, shelly) {
  foss.info <- process.fossil(fossil.ord, shelly = shelly)  # grabs PBDB information
  unit.info <- process.strat(strat.ord, bracket = bracket)  # relevant properties of the units

  # there area a few misaligns
  match.match <- foss.info$unit %in% unit.info$unit.id
  foss.info <- foss.info[match.match, ]

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


  #out <- list()
  #nz <- c()
  #for(ii in seq(length(by.ord))) {
  u.count <- rep(0, length(unit.info$unit.id))
  u.count[unit.info$unit %in% by.unit$unit] <- by.unit$count


  # now it is about making an output file for stan
  standata <- list()
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
  standata$X <- cbind(ilr(unit.info$lithology),
                      arm::rescale(log1p(unit.info$thickness$high)), 
                      arm::rescale(log1p(unit.info$column.area)), 
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



  # train vs test version of the dataset
  standata$y_train <- standata$y[unit.info$test == 1]
  standata$y_test <- standata$y[unit.info$test != 1]

  # two X matrices, train and test
  thick <- log1p(unit.info$thickness$high)
  thick.m <- mean(thick[unit.info$test == 1])
  thick.s <- sd(thick[unit.info$test == 1])
  thick.train <- thick[unit.info$test == 1]
  thick.train <- (thick.train - thick.m) / (2 * thick.s)
  thick.test <- thick[unit.info$test != 1]
  thick.test <- (thick.test - thick.m) / (2 * thick.s)

  colar <- log1p(unit.info$column.area)
  colar.m <- mean(colar[unit.info$test == 1])
  colar.s <- sd(colar[unit.info$test == 1])
  colar.train <- colar[unit.info$test == 1]
  colar.train <- (colar.train - colar.m) / (2 * colar.s)
  colar.test <- colar[unit.info$test != 1]
  colar.test <- (colar.test - colar.m) / (2 * colar.s)

  lith.train <- unit.info$lithology[unit.info$test == 1, ]
  lith.test <- unit.info$lithology[unit.info$test != 1, ]

  X_train <- cbind(ilr(lith.train),
                   thick.train,
                   colar.train,
                   unit.info$contact$above[unit.info$test == 1], 
                   unit.info$contact$below[unit.info$test == 1],
                   unit.info$subsurface[unit.info$test == 1])
  X_test <- cbind(ilr(lith.test),
                  thick.test,
                  colar.test,
                  unit.info$contact$above[unit.info$test != 1], 
                  unit.info$contact$below[unit.info$test != 1],
                  unit.info$subsurface[unit.info$test != 1])
  standata$X_train <- X_train
  standata$X_test <- X_test

  standata$N_train <- sum(unit.info$test == 1)
  standata$N_test <- sum(unit.info$test != 1)

  inc <- standata$y_train == 0
  standata$zi_train <- inc * 1
  standata$Nz_train <- sum(inc)
  standata$Nnz_train <- sum(!inc)
  standata$ynz_train <- standata$y_train[!inc]
  standata$Xnz_train <- standata$X_train[!inc, ]

  # export the data
  # for stan
  temp.name <- paste0('../data/data_dump/unit_data_', shelly, '.data.R')
  with(standata, {stan_rdump(list = alply(names(standata), 1), 
                             file = temp.name)})
 
  # in less manipuated form
  temp.name <- paste0('../data/data_dump/unit_image_', shelly, '.rdata')
  save(standata, unit.info, foss.info, 
       file = temp.name)
}




# k-fold datasets
export.stanfold <- function(fossil.ord, strat.ord, bracket, shelly, kfold) {
  foss.info <- process.fossil(fossil.ord, shelly = shelly)  # grabs PBDB information
  unit.info <- process.strat(strat.ord, bracket = bracket)  # relevant properties of the units

  # there area a few misaligns
  match.match <- foss.info$unit %in% unit.info$unit.id
  foss.info <- foss.info[match.match, ]

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

  u.count <- rep(0, length(unit.info$unit.id))
  u.count[unit.info$unit %in% by.unit$unit] <- by.unit$count

  standata <- list()
  u.count <- u.count[unit.info$test == 1] # count of order in unit

  thick <- log1p(unit.info$thickness$high)
  colar <- log1p(unit.info$column.area)
 
  # KF-folds
  ntr <- length(u.count)
  flds <- createFolds(u.count, k = kfold, returnTrain = TRUE)
  for(ff in seq(kfold)) {
    tester <- which(!(seq(ntr) %in% flds[[ff]]))
    
    # isolate the train set (4/5th of data)
    standata$y_train <- u.count[flds[[ff]]]
    standata$y_test <- u.count[tester]
    
    thick.m <- mean(thick[flds[[ff]]])
    thick.s <- sd(thick[flds[[ff]]])
    thick.train <- thick[flds[[ff]]]
    thick.train <- (thick.train - thick.m) / (2 * thick.s)
    thick.test <- thick[tester]
    thick.test <- (thick.test - thick.m) / (2 * thick.s)

    colar.m <- mean(colar[flds[[ff]]])
    colar.s <- sd(colar[flds[[ff]]])
    colar.train <- colar[flds[[ff]]]
    colar.train <- (colar.train - colar.m) / (2 * colar.s)
    colar.test <- colar[tester]
    colar.test <- (colar.test - colar.m) / (2 * colar.s)

    lith.train <- unit.info$lithology[flds[[ff]], ]
    lith.test <- unit.info$lithology[tester, ]
    
    X_train <- cbind(ilr(lith.train),
                     thick.train,
                     colar.train,
                     unit.info$contact$above[flds[[ff]]],
                     unit.info$contact$below[flds[[ff]]],
                     unit.info$subsurface[flds[[ff]]])
    X_test <- cbind(ilr(lith.test),
                    thick.test,
                    colar.test,
                     unit.info$contact$above[tester],
                     unit.info$contact$below[tester], 
                     unit.info$subsurface[tester])
    standata$X_train <- X_train
    standata$X_test <- X_test
    
    inc <- standata$y_train == 0
    standata$zi_train <- inc * 1
    standata$ynz_train <- standata$y_train[!inc]
    
    standata$Xnz_train <- standata$X_train[!inc, ]
    
    standata$Nz_train <- sum(inc)
    standata$Nnz_train <- sum(!inc)
    
    standata$N_train <- length(standata$y_train)
    standata$N_test<- length(tester)
    standata$K <- ncol(standata$X_train)
  
    temp.name <- paste0('../data/data_dump/unit_data_', shelly, 
                        '_fold', ff, '.data.R')
    with(standata, {stan_rdump(list = alply(names(standata), 1), 
                               file = temp.name)})
  }
}
