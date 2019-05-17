# bin data
# figure out logical breaks
ra <- range(strat$m_age)
timerange <- abs(diff(ra))
#timerange <- abs(diff(ord))
rr <- timerange / constant
brks1 <- seq(from = hirnantian, to = ra[2] + 1, by = rr) # 
brks2 <- seq(from = hirnantian, to = ra[1] - 1, by = -rr) # 
brks <- c(rev(brks2[-1]), brks1)
brks <- cbind(brks[-1], brks[-length(brks)])
brks <- brks[rev(seq(nrow(brks))), ]
write_rds(brks, path = '../data/breaks.rds')

# one pesky observation that doesn't play nice with bins
strat <- strat[strat$m_age > min(brks), ]


# get the count information from the data
out <- purrr::map(shelly, ~ get_values(taxon, strat, .x))
names(out) <- shelly


# make a mollusc category after the fact
tt <- full_join(out$Bivalvia, out$Gastropoda)
tt <- tt %>%
  group_by(unit_id, bin) %>%
  dplyr::mutate(diversity = sum(diversity),
                collections = sum(collections)) %>%
  ungroup() %>%
  distinct(unit_id, .keep_all = TRUE)
out$Mollusca <- tt
shelly <- names(out)

out <- purrr::map(out, function(x) {
                    m <- x$unit_id != 19112
                    x <- x[m, ]
                    x})

# do all the exporting
# partials make this easier, imo
esd <- partial(export_standata, type = 'diversity')
purrr::walk2(out, shelly, esd)
eso <- partial(export_standata, type = 'occurrence')
purrr::walk2(out, shelly, eso)
