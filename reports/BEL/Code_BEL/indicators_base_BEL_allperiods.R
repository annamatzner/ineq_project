# -----------------------------------------------------------------------------
#
# Indicators R-Script Belgium 2004 - 2017
#
# -----------------------------------------------------------------------------

library(dplyr)
library(survey)
library(convey)

# -----------------------------------------------------------------------------

data <- readRDS("belgium_allperiods.RData")

# To get useful results we subset to income >= 0
silc.pos.p1 <- data %>% filter(income_p1_1 > 0, income_p1_2 > 0, 
                                   income_p1_3 > 0)


# Also subset to age >=20
silc.pos.p2 <- data %>% filter(income_p2_1 > 0, income_p2_2 > 0, 
                                   income_p2_3 > 0, age >= 20)  

###to do: inflation correction

# Creating Survey Objects -----------------------------------------------------

silc.p1.svy <- svydesign(ids =  ~ id_h,
                         weights = ~rb050,
                         data = silc.pos.p1) %>% convey_prep()

silc.p2.svy <- svydesign(ids =  ~ id_h,
                         weights = ~rb050,
                         data = silc.pos.p2) %>% convey_prep()

# Indicators ------------------------------------------------------------------

# P1 EUROSTAT -----------------------------------------------------------------

# Pre-tax factor income (Canberra: primary income) ----------------------------

# Mean
mean_p1_1 <- svyby(~income_p1_1, by=~rb010, design=silc.p1.svy, FUN=svymean)


# Median
median_p1_1 <- svyby(~income_p1_1, by=~rb010, design=silc.p1.svy, F
                     UN=svyquantile, c(0.5), ci=TRUE)

# Gini
#gini_p1_1 <- svygini(~income_p1_1, silc.p1.svy)
gini_p1_1_all <- svyby(~income_p1_1, by=~rb010, design=silc.p1.svy, FUN=svygini)

# P80/P20
p80p20_p1_1 <- svyby(~income_p1_1, by=~rb010, design=silc.p1.svy, FUN=svyqsr)

# Top 10% share
#top10_p1_1 <- svytotal(~income_p1_1,subset(silc.p1.svy, income_p1_1>=
#as.numeric(svyquantile(~income_p1_1,silc.p1.svy,quantile=c(0.9)))),
#na.rm=TRUE)/svytotal(~income_p1_1,silc.p1.svy,na.rm=TRUE)

#top10_p1_1_all <- svyby(~income_p1_1, by=~rb010, design=silc.p1.svy, 
# FUN=svyquantile, c(0.9), ci=TRUE)

top_p1_1 <- subset(silc.p1.svy, income_p1_1 >= as.numeric(
  svyquantile(~income_p1_1, silc.p1.svy, quantile=c(0.9))))

topnum_p1_1 <- svyby(~income_p1_1, ~rb010, top_p1_1, svytotal)

topden_p1_1 <- svyby(~income_p1_1, ~rb010, silc.p1.svy, svytotal)

years_top10_p1_1 <- topnum_p1_1 / topden_p1_1

# Table preparation -----------------------------------------------------------

table_p1_1 <- data.frame(mean_p1_1$rb010, mean_p1_1$income_p1_1, 
                 median_p1_1$income_p1_1, gini_p1_1_all$income_p1_1,
                 p80p20_p1_1$income_p1_1, years_top10_p1_1$income_p1_1)

colnames(table_p1_1)<- c("Jahr", "Mittelwert" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p1_1


# Pre-tax national income -----------------------------------------------------
# Mean
#mean_p1_2 <- svymean(~income_p1_2,silc.p1.svy)
mean_p1_2 <- svyby(~income_p1_2, by=~rb010, design=silc.p1.svy, FUN=svymean)

# Median
median_p1_2 <- svyby(~income_p1_2, by=~rb010, design=silc.p1.svy, 
                     FUN=svyquantile, c(0.5), ci=TRUE)

# Gini
#gini_p1_1 <- svygini(~income_p1_2, silc.p1.svy)
gini_p1_2_all <- svyby(~income_p1_2, by=~rb010, design=silc.p1.svy, FUN=svygini)

# P80/P20
p80p20_p1_2 <- svyby(~income_p1_2, by=~rb010, design=silc.p1.svy, FUN=svyqsr)

# Top 10% share
#top10_p1_2 <- svytotal(~income_p1_2,subset(silc.p1.svy, income_p1_2>=
#    as.numeric(svyquantile(~income_p1_2,silc.p1.svy,quantile=c(0.9)))),
#    na.rm=TRUE)/svytotal(~income_p1_2,silc.p1.svy,na.rm=TRUE)

#top10_p1_2_all <- svyby(~income_p1_1, by=~rb010, design=silc.p1.svy, 
#FUN=svyquantile, c(0.9), ci=TRUE)

top_p1_2 <- subset(silc.p1.svy, income_p1_2 >= as.numeric(
  svyquantile(~income_p1_2, silc.p1.svy, quantile=c(0.9))))

topnum_p1_2 <- svyby(~income_p1_2, ~rb010, top_p1_2, svytotal)

topden_p1_2 <- svyby(~income_p1_2, ~rb010, silc.p1.svy, svytotal)

years_top10_p1_2 <- topnum_p1_2 / topden_p1_2

# Table preparation -----------------------------------------------------------

table_p1_2 <- data.frame(mean_p1_2$rb010, mean_p1_2$income_p1_2, 
                         median_p1_2$income_p1_2, gini_p1_2_all$income_p1_2,
                         p80p20_p1_2$income_p1_2, years_top10_p1_2$income_p1_2)

colnames(table_p1_2)<- c("Jahr", "Mittelwert" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p1_2


# Post-tax disposable income --------------------------------------------------
# Mean
#mean_p1_1 <- svymean(~income_p1_1,silc.p1.svy.allp)
mean_p1_3 <- svyby(~income_p1_3, by=~rb010, design=silc.p1.svy, FUN=svymean)


# Median
median_p1_3 <- svyby(~income_p1_3, by=~rb010, design=silc.p1.svy, FUN=svyquantile, c(0.5), ci=TRUE)

# Gini
#gini_p1_3 <- svygini(~income_p1_3, silc.p1.svy)
gini_p1_3_all <- svyby(~income_p1_3, by=~rb010, design=silc.p1.svy, FUN=svygini)

# P80/P20
p80p20_p1_3 <- svyby(~income_p1_3, by=~rb010, design=silc.p1.svy, FUN=svyqsr)

# Top 10% share
#top10_p1_3 <- svytotal(~income_p1_3, subset(silc.p1.svy, income_p1_3>=
#as.numeric(svyquantile(~income_p1#_3,silc.p1.svy,quantile=c(0.9)))),
#na.rm=TRUE)/svytotal(~income_p1_3,silc.p1.svy,na.rm=TRUE)

# top10_p1_3_all <- svyby(~income_p1_3, by=~rb010, design=silc.p1.svy, 
# FUN=svyquantile, c(0.9), ci=TRUE)

top_p1_3 <- subset(silc.p1.svy, income_p1_3 >= as.numeric(
  svyquantile(~income_p1_3, silc.p1.svy, quantile=c(0.9))))

topnum_p1_3 <- svyby(~income_p1_3, ~rb010, top_p1_3, svytotal)

topden_p1_3 <- svyby(~income_p1_3, ~rb010, silc.p1.svy, svytotal)

years_top10_p1_3 <- topnum_p1_3 / topden_p1_3

# Table preparation -----------------------------------------------------------

table_p1_3 <- data.frame(mean_p1_3$rb010, mean_p1_3$income_p1_3, 
                         median_p1_3$income_p1_3, gini_p1_3_all$income_p1_3,
                         p80p20_p1_3$income_p1_3, years_top10_p1_3$income_p1_3)

colnames(table_p1_3)<- c("Jahr", "Mittelwert" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p1_3

# -----------------------------------------------------------------------------

# P2 WID WORLD ---------------------------------------------------------------

# Pre-tax factor income (Canberra: primary income) ---------------------------

# Mean
#mean_p2_1 <- svymean(~income_p2_1,silc.p2.svy.allp)
mean_p2_1 <- svyby(~income_p2_1, by=~rb010, design=silc.p2.svy, FUN=svymean)


# Median
median_p2_1 <- svyby(~income_p2_1, by=~rb010, design=silc.p2.svy, FUN=svyquantile, c(0.5), ci=TRUE)

# Gini
#gini_p2_1 <- svygini(~income_p2_1, silc.p2.svy)
gini_p2_1_all <- svyby(~income_p2_1, by=~rb010, design=silc.p2.svy, FUN=svygini)

# P80/P20
p80p20_p2_1 <- svyby(~income_p2_1, by=~rb010, design=silc.p2.svy, FUN=svyqsr)

# Top 10% share
# top10_p2_1 <- svytotal(~income_p2_1, subset(silc.p2.svy, income_p2_1>=
# as.numeric(svyquantile(~income_p1_3,silc.p2.svy,quantile=c(0.9)))),
# na.rm=TRUE)/svytotal(~income_p1_3,silc.p2.svy,na.rm=TRUE)

#top10_p2_1_all <- svyby(~income_p2_1, by=~rb010, design=silc.p2.svy, 
#FUN=svyquantile, c(0.9), ci=TRUE)

top_p2_1 <- subset(silc.p2.svy, income_p2_1 >= as.numeric(
  svyquantile(~income_p2_1, silc.p2.svy, quantile=c(0.9))))

topnum_p2_1 <- svyby(~income_p2_1, ~rb010, top_p2_1, svytotal)

topden_p2_1 <- svyby(~income_p2_1, ~rb010, silc.p2.svy, svytotal)

years_top10_p2_1 <- topnum_p2_1 / topden_p2_1

# Table preparation -----------------------------------------------------------

table_p2_1 <- data.frame(mean_p2_1$rb010, mean_p2_1$income_p2_1, 
                         median_p2_1$income_p2_1, gini_p2_1_all$income_p2_1,
                         p80p20_p2_1$income_p2_1, years_top10_p2_1$income_p2_1)

colnames(table_p2_1)<- c("Jahr", "Mittelwert" ,"Median", "Gini", "P80/P20", "Top10%")

table_p2_1

# Pre-tax national income -----------------------------------------------------

# Mean
#mean_p2_2 <- svymean(~income_p2_2,silc.p2.svy.allp)
mean_p2_2 <- svyby(~income_p2_2, by=~rb010, design=silc.p2.svy, FUN=svymean)


# Median
median_p2_2 <- svyby(~income_p2_2, by=~rb010, design=silc.p2.svy, FUN=svyquantile, c(0.5), ci=TRUE)

# Gini
gini_p2_2 <- svygini(~income_p2_2, silc.p2.svy)
gini_p2_2_all <- svyby(~income_p2_2, by=~rb010, design=silc.p2.svy, FUN=svygini)

# P80/P20
p80p20_p2_2 <- svyby(~income_p2_2, by=~rb010, design=silc.p2.svy, FUN=svyqsr)

# Top 10% share
#top10_p2_2 <- svytotal(~income_p2_2, subset(silc.p2.svy, income_p2_2>=
#as.numeric(svyquantile(~income_p2_2,silc.p2.svy,quantile=c(0.9)))),
#na.rm=TRUE)/svytotal(~income_p2_2,silc.p2.svy,na.rm=TRUE)

#top10_p2_2_all <- svyby(~income_p2_2, by=~rb010, design=silc.p2.svy, 
#FUN=svyquantile, c(0.9), ci=TRUE)

top_p2_2 <- subset(silc.p2.svy, income_p2_2 >= as.numeric(
  svyquantile(~income_p2_2, silc.p2.svy, quantile=c(0.9))))

topnum_p2_2 <- svyby(~income_p2_2, ~rb010, top_p2_2, svytotal)

topden_p2_2 <- svyby(~income_p2_2, ~rb010, silc.p2.svy, svytotal)

years_top10_p2_2 <- topnum_p2_2 / topden_p2_2

# Table preparation -----------------------------------------------------------

table_p2_2 <- data.frame(mean_p2_2$rb010, mean_p2_2$income_p2_2, 
                         median_p2_2$income_p2_2, gini_p2_2_all$income_p2_2,
                         p80p20_p2_2$income_p2_2, years_top10_p2_2$income_p2_2)

colnames(table_p2_2)<- c("Jahr", "Mittelwert" ,"Median", "Gini", "P80/P20", "Top10%")

table_p2_2

# Post-tax disposable income -------------------------------------------------

# Mean
#mean_p2_2 <- svymean(~income_p2_2,silc.p2.svy.allp)
mean_p2_3 <- svyby(~income_p2_3, by=~rb010, design=silc.p2.svy, FUN=svymean)


# Median
median_p2_3 <- svyby(~income_p2_3, by=~rb010, design=silc.p2.svy, FUN=svyquantile, c(0.5), ci=TRUE)

# Gini
#gini_p2_3 <- svygini(~income_p2_3, silc.p2.svy)
gini_p2_3_all <- svyby(~income_p2_3, by=~rb010, design=silc.p2.svy, FUN=svygini)

# P80/P20
p80p20_p2_3 <- svyby(~income_p2_3, by=~rb010, design=silc.p2.svy, FUN=svyqsr)

# Top 10% share
#top10_p2_3 <- svytotal(~income_p2_3, subset(silc.p2.svy, income_p2_3>=
#as.numeric(svyquantile(~income_p2_3,silc.p2.svy,quantile=c(0.9)))),
#na.rm=TRUE)/svytotal(~income_p2_3,silc.p2.svy,na.rm=TRUE)

# top10_p2_3_all <- svyby(~income_p2_3, by=~rb010, design=silc.p2.svy, 
#FUN=svyquantile, c(0.9), ci=TRUE)

top_p2_3 <- subset(silc.p2.svy, income_p2_3 >= as.numeric(
  svyquantile(~income_p2_3, silc.p2.svy, quantile=c(0.9))))

topnum_p2_3 <- svyby(~income_p2_3, ~rb010, top_p2_3, svytotal)

topden_p2_3 <- svyby(~income_p2_3, ~rb010, silc.p2.svy, svytotal)

years_top10_p2_3 <- topnum_p2_3 / topden_p2_3

# Table preparation -----------------------------------------------------------

table_p2_3 <- data.frame(mean_p2_3$rb010, mean_p2_3$income_p2_3, 
                         median_p2_3$income_p2_3, gini_p2_3_all$income_p2_3,
                         p80p20_p2_3$income_p2_3, years_top10_p2_3$income_p2_3)

colnames(table_p2_3)<- c("Jahr", "Mittelwert" ,"Median", "Gini", "P80/P20", "Top10%")

table_p2_3


#------------------------------------------------------------
####andere Methode f. Indikatoren bzw. Zusammenfassung####

------------########P1: Eurostat#######--------------------------

#MEAN
mean_p1 <- svyby(~income_p1_1 + income_p1_2 + income_p1_3, ~rb010, silc.p1.svy, svymean, keep.var = FALSE)

#median
median_p1 <- svyby(~income_p1_1 + income_p1_2 + income_p1_3, ~rb010, silc.p1.svy, svyquantile, quantiles=0.5, keep.var = FALSE)

#GINI

gini_p11 <- svyby(~income_p1_1, ~rb010, silc.p1.svy, svygini, keep.var = FALSE)
gini_p12 <- svyby(~income_p1_2, ~rb010, silc.p1.svy, svygini, keep.var = FALSE)
gini_p13 <- svyby(~income_p1_3, ~rb010, silc.p1.svy, svygini, keep.var = FALSE)

gini_p1 <- as.data.frame(cbind(gini_p11[,2], gini_p12[,2], gini_p13[,2]))

#p80/20

p80_p1 <- svyby(~income_p1_1 + income_p1_2 + income_p1_3, ~rb010, silc.p1.svy, svyqsr)

####top 10 % share
#create svydesign for top 10

svy_p1_top10 <- subset(silc.p1.svy, income_p1_1 >= as.numeric(svyquantile(~income_p1_1, silc.p1.svy, quantile=c(0.9))))

top_num <- svyby(~income_p1_1, ~rb010, svy_p1_top10, svytotal)

top_den <- svyby(~income_p1_1, ~rb010, silc.p1.svy, svytotal)

top10_p1_1 <- top_num / top_den


####P2: Partial sharing, restricted sample (>=20 years)-----


#GINI

gini_p21 <- svyby(~income_p1_1, ~rb010, silc.p2.svy, svygini, keep.var = FALSE)
gini_p22 <- svyby(~income_p1_2, ~rb010, silc.p2.svy, svygini, keep.var = FALSE)
gini_p23 <- svyby(~income_p1_3, ~rb010, silc.p2.svy, svygini, keep.var = FALSE)

gini_p2 <- as.data.frame(cbind(gini_p21[,2], gini_p22[,2], gini_213[,2]))

#p80/20

p80_p2 <- svyby(~income_p2_1 + income_p2_2 + income_p2_3, ~rb010, silc.p2.svy, svyqsr)

####top 10 % share
#create svydesign for top 10

svy_p2_top10 <- subset(silc.p2.svy, income_p2_1 >= as.numeric(svyquantile(~income_p2_1, silc.p1.svy, quantile=c(0.9))))

top_num <- svyby(~income_p2_1, ~rb010, svy_p2_top10, svytotal)

top_den <- svyby(~income_p2_1, ~rb010, silc.p2.svy, svytotal)

top10_p2_1 <- top_num / top_den


####Overview - create tables####-----