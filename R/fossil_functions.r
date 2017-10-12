# fix synonyms
clean.taxon <- function(fossils) {
  to.fix <- fossils$phylum == '' | fossils$class == ''

  ord.synon <- c('Ellesmerocerida', 'Cyrtocerinida')
  fossils[fossils$order %in% ord.synon, 'order'] <- ord.synon[1]

  fixes <- rbind(c('Hyolitha', 'Hyolitha', 'Hyolithida'),
                 c('Hyolitha', 'Hyolitha', 'Orthothecida'),
                 c('Arthropoda', 'Trilobita', 'Harpetida'),
                 c('Echinodermata', 'Pelmatoza', 'Parablastoidea'),
                 c('Echinodermata', 'Carpoidea', 'Soluta'),
                 c('Mollusca', 'Tentaculita', 'Tentaculitida'),
                 c('Mollusca', 'Polyplacophora', 'Hemithecellitina'),
                 c('Arthropoda', 'Merostomata', 'Eurypterida'),
                 c('Mollusca', 'Cephalopoda', 'Ellesmerocerida'),
                 c('Retaria', 'Radiolaria', 'Spumellaria'),
                 c('Retaria', 'Radiolaria', 'Archaeospicularia'),
                 c('Retaria', 'Radiolaria', 'Entactinaria'),
                 c('Retaria', 'Radiolaria', 'Albaillellaria'),
                 c('Rhodophyta', 'Rhodophyceae', 'Corallinales'))

  for(ii in seq(nrow(fixes))) {
    tt <- which(fossils$order %in% fixes[ii, 3])
    for(jj in seq(length(tt))) {
      fossils[tt[jj], c('phylum', 'class')] <- fixes[ii, 1:2]
    }
  }

  fossils
}
