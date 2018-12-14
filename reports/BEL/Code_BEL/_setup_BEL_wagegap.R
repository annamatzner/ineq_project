# -----------------------------------------------------------------------------

# Setup Data Belgium - Immigrant Wage Gap
# prepare data frame and generate income variables
# time frame: 2016

# -----------------------------------------------------------------------------

library(dplyr)

# Prepare Data ----------------------------------------------------------------

# Download data
silc.p <- tbl(pg, "c13p") %>%
  filter(pb010 == 2013 & pb020 == 'BE') %>%
  select(pb030, pb210, pb220a, pe040, pl200, pl160, pl111, py010g, 
         py050g, py021g, py080g, py090g, py100g, py110g, py120g, 
         py130g, py140g) %>%
  collect(n = Inf)
# pb210_num, pb220a_num, pl111_num im 2016 Datensatz verfügbar

silc.h <- tbl(pg, "c13h") %>%
  filter(hb010 == 2013 & hb020 == 'BE') %>%
  select(hb010, hb020, hb030, hy020, hy030g, hy040g, hy050g, hy060g, hy070g, 
         hy080g, hy090g, hy110g, hy120g, hy130g, hy140g, hx040, hx050) %>%
  collect(n = Inf)

silc.d <- tbl(pg, "c13d") %>%
  filter(db010 == 2013 & db020 == 'BE') %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

silc.r <- tbl(pg, "c13r") %>% 
  filter(rb010 == 2013 & rb020 == 'BE') %>%
  select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)

# Rename rb030, pb030 to personal_id
silc.r <- silc.r %>% rename(personal_id = rb030)
silc.p <- silc.p %>% rename(personal_id = pb030)

# merge silc.r and silc.p
silc.rp <- left_join(silc.r, silc.p)

# Create age, household ID, gender variables
silc.rp <- silc.rp %>% 
  mutate(age = rb010 - rb080,
         gender = factor(rb090, labels = c('Male','Female')),
         id_h = paste0(rb020, rx030)) 

# Create unique IDs for merging, merge country and household ID h,d

silc.h <- silc.h %>% mutate(id_h = paste0(hb020, hb030))

silc.d <- silc.d %>% mutate(id_h = paste0(db020, db030))

# Merge silc.rp and silc.h
silc.rph <- left_join(silc.rp, silc.h)

# Replace NA's in silc.rph by 0
silc.rph[is.na(silc.rph)] <- 0

# P1 EUROSTAT -----------------------------------------------------------------

# Pre-tax factor income (Canberra: primary income) ----------------------------

# Sum up personal income
silc.rph <- silc.rph %>% mutate(pincome1 = py010g + py050g + py080g + py021g)

# Sum up personal income of HH
silc.rph <- silc.rph %>% group_by(id_h) %>%
  mutate(sum_pincome1 = sum(pincome1))

# Equivalised HH income per person
silc.rph <- silc.rph %>% 
  mutate(income_p1_1 = ((sum_pincome1 + hy110g + hy040g + hy090g) / hx050))

# Pre-tax national income -----------------------------------------------------

# Sum up personal income 
silc.rph <- silc.rph %>% mutate(pincome2 = py090g + py100g)

# Sum up personal income of HH
silc.rph <- silc.rph %>% group_by(id_h) %>%
  mutate(sum_pincome2 = sum(pincome2))

# Equivalised HH income per person
silc.rph <- silc.rph %>%
  mutate(income_p1_2 = (income_p1_1 + sum_pincome2 / hx050))

# Post-tax national income ----------------------------------------------------

# Sum up personal income 
silc.rph <- silc.rph %>% mutate(pincome3 = py110g + py120g + py130g + py140g)

# Sum up personal income of HH
silc.rph <- silc.rph %>% group_by(id_h) %>%
  mutate(sum_pincome3 = sum(pincome3))

# Equivalised HH income per person
silc.rph <- silc.rph %>%
  mutate(income_p1_3 = (income_p1_2 + 
                          (sum_pincome3 + hy050g + hy060g + hy070g + hy080g 
                           - hy120g - hy130g - hy140g) / hx050))

silc.rph$hy020/silc.rph$hx050


# Subsetting ------------------------------------------------------------------

# To get useful results we subset to income >= 0
silc.pos.p1 <- silc.rph %>% filter(income_p1_1 > 0, income_p1_2 > 0, 
                                   income_p1_3 > 0)

# Fin -------------------------------------------------------------------------



