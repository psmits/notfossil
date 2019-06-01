#' Extract summary of lithological composition (strict)
#'
#' Given a textual description of a geological unit's lithology, extract the key terms.
#'
#' @param lith a vector of lithological descriptions like thos exported by Macrostrat.
#' @return list each element is a unit's simple lithological description
#' @export
simplify_lithology <- function(lith) {

  ## some words are functional equivalents. synonymize!
  #dup_words <- list(c('green', 'greenish'), 
  #                  c('limestone', 'lime'),
  #                  c('red', 'reddish'), 
  #                  c('blue', 'bluish'),
  #                  c('brown', 'brownish'),
  #                  c('yellow', 'yellowish'),
  #                  c('thin', 'thinly'),
  #                  c('thick', 'thickly'),
  #                  c('bedded', 'laminated'),
  #                  c('dolomite', 'dolomitic'),
  #                  c('chert', 'cherty'),
  #                  c('sandstone', 'sand', 'sandy'),
  #                  c('mudstone', 'mud'),
  #                  c('laminated', 'laminations'),
  #                  c('shale', 'shaly', 'shaley'),
  #                  c('siltstone', 'silty', 'siliceous'),
  #                  c('arkose', 'arkosic'),
  #                  c('argillite', 'argillaceous'))

  ## some words aren't useful: too common or too rare
  #too_common <- c('sedimentary', 'light', 'dark', 'white', 'grey', 
  #                'phosphatic', 'tan', 'yellow', 'medium', 'red', 'black')
  
  # fine type siliciclastics
  fine_sil <- c('siltstone', 'claystone', 'mudstone', 'shale', 'argillite')
  
  # clean up the lithological descriptions
  lith_explode <- 
    lith %>%
    # list elements are vector of words from each description
    future_map(., ~ str_split(.x, '\\|')) %>%
    future_map(., function(x) map(x, str_trim)) %>%
    future_map(., function(x) map(x, ~ str_split(.x, '  ~ ', simplify = TRUE)))

  # work with just the text description
  lith_words <- 
    lith_explode %>%
    future_map(., function(x) map(x, ~ str_split(.x[, 1], ' '))) %>%
    # some words are too common or unhelpful, get rid of them.
    #wordrm(., too_common) %>%
    # some words are duplicated. get rid of those.
    #worddup(.) %>%
    # now that we've cleaned the lithological descriptions
    # assign to one of the categories
    future_map(., function(tt)
               map(tt, function(x) 
                   map(x, ~ case_when(any(.x == 'siliciclastic') & 
                                        any(.x %in% fine_sil) ~ 
                                        'fine_siliciclastic',
                                      any(.x == 'siliciclastic') & 
                                        !(any(.x %in% fine_sil)) ~ 
                                        'coarse_siliciclastic',
                                      any(.x == 'carbonate') ~ 'carbonate',
                                      TRUE ~ 'other')))) %>%
    future_map(., ~ unlist(.x))

  # lith_words has all the words
  # lith_explode has both words [1] and numeric [2]
  # replace lith_explode words with lith_words

  lith_explode <- flatten(lith_explode) # house keeping

  lith_recombo <- future_map2(lith_words, lith_explode,
                              function(x, y) {
                                o <- tibble(words = x, value = as.numeric(y[, 2]))
                                o})

  # aggregate duplicates by adding their numerics together
  lith_ready <- future_map(lith_recombo, ~ .x %>%
                           group_by(words) %>%
                           dplyr::summarize(value = sum(value)) %>%
                           mutate(value = value / sum(value))) %>%
    future_map(., ~ .x %>% spread(words, value))

  lith_ready <- 
    tibble(x = lith_ready) %>%
    unnest() %>%
    mutate_all(.funs = ~ replace_na(.x, replace = 0)) %>%
    split(., seq(nrow(.)))

  lith_ready

}



