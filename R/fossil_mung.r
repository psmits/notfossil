source('fossil_functions.r')

process.fossil <- function(fossil.ord) {

  # genus id numbers
  genus.ids <- laply(fossil.ord$genus_no, function(x) str_split(x, '\\|'))
  genus.unit <- genus.ids
  names(genus.unit) <- fossil.ord$unit_id  # match genus_no w/ unit_id
  genus.ids <- genus.ids[laply(genus.ids, function(x) all(x != ''))]
  genus.ids <- sort(unique(unlist(genus.ids)))

  # collection id numbers
  colec.ids <- laply(fossil.ord$cltn_id, function(x) str_split(x, '\\|'))
  colec.unit <- colec.ids
  names(colec.unit) <- fossil.ord$unit_id
  colec.ids <- colec.ids[laply(colec.ids, function(x) all(x != ''))]
  colec.ids <- sort(unique(unlist(colec.ids)))

  # read in all specimens based on their collection numbers
  make.url <- paste0('https://paleobiodb.org/data1.2/occs/list.txt?coll_id=', 
                     paste0(colec.ids,collapse=','), '&show=full')
  fossils <- read.csv(make.url, stringsAsFactors = FALSE)

  ## lithology stuff
  #fossils$lithology1
  #fossils$lithology2
  #fossils$fossilsfrom1
  #fossils$fossilsfrom2

  ## collection type
  #fossils$collection_type
  #fossils$collection_coverage


  # curiosity: how many non-zero ids
  taxonomy <- c('phylum', 'class', 'order', 'family', 'genus')
  how.many <- apply(fossils[, taxonomy], 2, function(x) sum(x != ''))

  # deal with subgenus nonsense
  fossils$genus <- Reduce(c, str_match_all(fossils$genus, '^\\S*'))
  fossils <- fossils[fossils$order != '', ]

  # give new column that is unit_id
  fossils$unit_id <- NA
  nn <- as.numeric(names(colec.unit))
  for(ii in seq(length(colec.unit))) {
    fossils$unit_id[fossils$collection_no %in% colec.unit[[ii]]] <- nn[ii]
  }

  # some of the orders missing phylum/class are easy to fix
  fossils <- clean.taxon(fossils)


  by.unit <- split(fossils, fossils$unit_id)
  unit.counts <- llply(by.unit, function(x) table(x$order))

  taxon.count.unit <- melt(unit.counts)
  taxon.count.unit[, 1] <- as.character(taxon.count.unit[, 1])

  mm <- match(taxon.count.unit[, 1], fossils$order)
  tcu <- data.frame(fossils[mm, taxonomy[1:2]], taxon.count.unit, 
                    stringsAsFactors = FALSE)

  names(tcu) <- c('phylum', 'class', 'order', 'count', 'unit')
  tcu
}
