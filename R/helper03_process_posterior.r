# # helper function to read in data
# # this way things only have to be changed in one easy to find place
# read_mcmcout <- function(shelly, type, silent = TRUE) {
#   pat <- paste0('trunc\\_[0-9]\\_', shelly, '_', type)
#   files <- list.files('../data/mcmc_out', pattern = pat, full.names = TRUE)
#   fit <- read_stan_csv(files)
#   if(!silent) {
#     check_all_diagnostics(fit, max_depth = 20)
#   }
#   post <- rstan::extract(fit, permuted = TRUE)
#   post
# }
