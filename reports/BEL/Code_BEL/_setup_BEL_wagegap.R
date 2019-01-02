# -----------------------------------------------------------------------------

# Setup Data Belgium - Immigrant Wage Gap
# prepare data frame and generate income variables
# time frame: 2017

# -----------------------------------------------------------------------------

library(dplyr)

# Prepare Data ----------------------------------------------------------------

# Download data
silc.p <- tbl(pg, "c17p") %>%
  filter(pb020 == 'BE') %>%
  select(pb030, pb040, pb210, pb220a, pe040, ph010, pl040, pl060, pl100, pl073, 
         pl074, pl150, pl200, pl160, pl111, py010g) %>%
  collect(n = Inf)

silc.h <- tbl(pg, "c17h") %>%
  filter(hb020 == 'BE') %>%
  select(hb010, hb020, hb030) %>%
  collect(n = Inf)

silc.d <- tbl(pg, "c17d") %>%
  filter(db020 == 'BE') %>%
  select(db010, db020, db030, db040, db090, db100) %>%
  collect(n = Inf)

silc.r <- tbl(pg, "c17r") %>% 
  filter(rb020 == 'BE') %>%
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
silc.rph <- left_join(silc.rph, silc.d)

# Replace NA's in silc.rph by 0
silc.rph[is.na(silc.rph)] <- 0

# Subsetting ------------------------------------------------------------------

# To get useful results we subset to income >= 0
silc <- silc.rph %>% filter(py010g > 0)

# Recoding & Renaming Variables -----------------------------------------------

# experience
silc <- silc %>% rename(experience = pl200)

# filter only observations with income and working time 
silc <- silc %>% filter(pl060>0 & (pl073+pl074)>0 & pl040 == 3)
# 4619 observations

# calculate hourly wage
silc <- silc %>% mutate(hwages = py010g / 
                          ((pl060 + pl100)*52/12*pmin(12,(pl073 + pl074))))

# Recode Education Variable
silc$edu <- as.factor(silc$pe040)
levels(silc$edu) <- c("1","1","2","3","3","3","3","4")

# Managerial Position Dummy
# 0 wenn keine leitende Position, 1 wenn in einer leitenden Position
silc$pl150[silc$pl150 == 2] <- 0
silc$pl150 <- as.numeric(silc$pl150)
silc <- silc %>% rename(position = pl150)

# Origin of Birth
# How many observations where country of birth = Belgium, EU, other
table <- as.data.frame(table(silc$pb210))
# EU: 378, BEL: 3787, OTH: 408

# We treat EU + Belgium as one group
silc$pb210[silc$pb210 == "LOC"] <- 0
silc$pb210[silc$pb210 == "EU"] <- 0
silc$pb210[silc$pb210 == "OTH"] <- 1
silc$pb210 <- as.factor(silc$pb210)
silc <- na.omit(silc)
silc <- silc %>% rename(migration = pb210)

# Degree of urbanisation 
silc$db100 <-factor(silc$db100, levels=c("1","2","3"))
silc <- silc %>% rename(urb = db100)

# Health Dummy
silc$ph010[silc$ph010 == 1] <- 0
silc$ph010[silc$ph010 == 2] <- 0
silc$ph010[silc$ph010 == 3] <- 0
silc$ph010[silc$ph010 == 4] <- 1
silc$ph010[silc$ph010 == 5] <- 1
silc <- silc %>% rename(health = ph010)
# 0 = good health, 1 = bad health

# Recent job change Dummy
silc$pl160[silc$pl160 == 2] <- 0
silc$pl160 <- as.factor(silc$pl160)
silc <- silc %>% rename(jobchange = pl160)

# Weights
silc <- silc %>% rename(weights = pb040)

# New Data Frame: only variables we need --------------------------------------

data <- silc %>% select(id_h, personal_id, migration, edu, experience, age, 
                        gender, hwages, position, weights, urb, health, 
                        jobchange)


# Fin -------------------------------------------------------------------------

