settest <- function(unit.info, foss.info, p = 0.8) {
  # training set of units
  keep <- base::sample(unit.info$unit.id, size = length(unit.info$unit.id) * 0.8)

  index.keep <- unit.info$unit.id %in% keep
  
  # test set of units
  hold <- unit.info$unit.id[!index.keep]
}
