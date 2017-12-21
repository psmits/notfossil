# this script is called entirely for the side effects 
# taxon variable is shelly
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

source('../R/fossil_functions.r')
source('../R/fossil_mung.r')
source('../R/rock_functions.r')
source('../R/rock_mung.r')

source('../R/prepare_foo.r')

# bring in data
#   fossil.ord # fossil occurrences in the ordovician
#   strat.ord # fossil occurrences in the ordovician
source('../R/download_scrap.r')  # just macrostrat

# clean data
#shelly <- c('Arthropoda', 'Brachiopoda', 'Mollusca', 'Echinodermata', 
#            'Hemichordata', 'Bryozoa', 'Cnidaria')
#shelly <- c('Brachiopoda', 'Trilobita', 'Bivalvia', 'Gastropoda')
shelly <- c('Brachiopoda', 'Arthropoda', 'Mollusca')
ord <- c(460.4, 443.8)
#mid <- ord[1] - abs((diff(ord) / 4) * 3)
#mid <- ord[1] - abs((diff(ord) / 10) * 9)
mid <- 445.6
bracket <- c(ord[1], mid, ord[2])

rnds <- 5
kfold <- 5


for (kk in seq(length(shelly))) {
  export.standata(fossil.ord, strat.ord, bracket, shelly[kk])
  export.stanfold(fossil.ord, strat.ord, bracket, shelly[kk], 
                  kfold = kfold, rnds = rnds)
}
