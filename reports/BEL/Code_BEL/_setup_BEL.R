# Setup -------------------------------------------------------------------

library(dplyr)
#if(!exists(c("country", "year"))) {
#  stop("Please specify country and year.")
#}

# Prepare Data ------------------------------------------------------------

# Download data
silc.p <- tbl(pg, "pp") %>%
  filter(pb020 == 'BE') %>%
  select(pb010, pb020, pb030, pb040, pb150, py010g, py050g, px010, px030, 
         py080g, py010n, py050n, py080n, py140n, py090n, py100n, py110n, 
         py120n, py130n, hy040g, hy030g ) %>%
  collect(n = Inf)


silc.h <- tbl(pg, "hh") %>%
  filter(hb020 == 'BE') %>%
  select(hb020, hb030, hy010, hx010, hy040g, hy030g, hy170g, hy040n, 
         hy030n, hy170n,hy020) %>%
  collect(n = Inf)

silc.d <- tbl(pg, "dd") %>%
  filter(db020 == 'BE') %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

silc.r <- tbl(pg, "rr") %>% 
  filter(rb020 == 'BE') %>%
  select(rb010, rb020, rb030, rb050) %>%
  collect(n = Inf)

# Create unique IDs for merging, merge country and household ID
silc.p <- silc.p %>% mutate(id_h = paste0(pb020, px030))

silc.h <- silc.h %>% mutate(id_h = paste0(hb020, hb030))

silc.d <- silc.d %>% mutate(id_h = paste0(db020, db030))

# Merge the datasets
silc.pd <- left_join(silc.p, silc.d %>% select(id_h, db020, db090))

silc.hd <- left_join(silc.h, silc.d)


# Create total personal income --------------------------------------------

# Find string "py" (i.e. income variables) for summing up total personal income. 
silc.pd <- silc.pd %>% 
  mutate(total.inc = rowSums(silc.pd[, grep("py", colnames(silc.pd))], 
                             na.rm = TRUE)) 

# Fin ---------------------------------------------------------------------

message("Prepared data for ", country, " in ", year, ".")
