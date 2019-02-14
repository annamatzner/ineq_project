# -----------------------------------------------------------------------------
#
# Indicators R-Script Belgium 2004 - 2017
#
# -----------------------------------------------------------------------------

library(dplyr)
library(survey)
library(convey)
library(eurostat)
library(ggplot2)
library(xtable)

# -----------------------------------------------------------------------------

# inflation <- get_eurostat("prc_hicp_aind", time_format = "raw")
# inflation <- inflation %>% filter(unit == "INX_A_AVG", coicop == "CP00", 
#                                 geo == "BE", time %in% 2004:2017) %>% 
#  select(time, values) %>% arrange(time)


# -----------------------------------------------------------------------------

silc.p1 <- readRDS("belgium_allperiods_p1.RData")
silc.p2 <- readRDS("belgium_allperiods_p2.RData")


# Creating Survey Objects -----------------------------------------------------

silc.p1.svy <- svydesign(ids =  ~ id_h,
                         weights = ~rb050,
                         data = silc.p1) %>% convey_prep()

silc.p2.svy <- svydesign(ids =  ~ id_h,
                         weights = ~rb050,
                         data = silc.p2) %>% convey_prep()

# Indicators ------------------------------------------------------------------

# P1 EUROSTAT -----------------------------------------------------------------

# Pre-tax factor income (Canberra: primary income) ----------------------------

# Mean
mean_p1_1 <- svyby(~income_p1_1, ~rb010, silc.p1.svy, svymean)
# mean_p1_1$mean <- mean_p1_1$income_p1_1/inflation$values*100

# Median
median_p1_1 <- svyby(~income_p1_1, ~rb010, silc.p1.svy, svyquantile, 
                     quantiles = c(0.5), keep.var = FALSE)
# median_p1_1$median <- median_p1_1$income_p1_1/inflation$values*100

# Gini
gini_p1_1_all <- svyby(~income_p1_1, ~rb010, silc.p1.svy, svygini)

# P80/P20
p80p20_p1_1 <- svyby(~income_p1_1, ~rb010, silc.p1.svy, svyqsr)

# Top 10% share

share <- vector("numeric", length(2004:2017))
j <- 1

for(year in 2004:2017) {
  svy_subset <- subset(silc.p1.svy, rb010 == year)
  svy_top_10 <- subset(svy_subset, income_p1_1 >= as.numeric(svyquantile(~income_p1_1, svy_subset, quantile = c(0.9))))
  share[j] <- svytotal(~income_p1_1, svy_top_10) / svytotal(~income_p1_1, svy_subset)
  j <- j + 1
}

years_top10_p1_1 <- data.frame(top_p1_1 = share)

# Table preparation -----------------------------------------------------------

table_p1_1 <- data.frame(mean_p1_1$rb010, round(mean_p1_1$income_p1_1), 
                 round(median_p1_1$statistic), round(gini_p1_1_all$income_p1_1*100,1),
                 round(p80p20_p1_1$income_p1_1,1), round(years_top10_p1_1$top_p1_1*100,1))

colnames(table_p1_1)<- c("Jahr", "Mittelwert" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p1_1
print(xtable(table_p1_1), include.rownames = FALSE)

# Pre-tax national income -----------------------------------------------------

# Mean
mean_p1_2 <- svyby(~income_p1_2, ~rb010, silc.p1.svy, svymean)
# mean_p1_2$mean <- mean_p1_2$income_p1_2/inflation$values*100

# Median
median_p1_2 <- svyby(~income_p1_2, ~rb010, silc.p1.svy, 
                     svyquantile, quantiles = c(0.5), keep.var = FALSE)
# median_p1_2$median <- median_p1_2$income_p1_2/inflation$values*100

# Gini
gini_p1_2_all <- svyby(~income_p1_2, ~rb010, silc.p1.svy, svygini)

# P80/P20
p80p20_p1_2 <- svyby(~income_p1_2, ~rb010, silc.p1.svy, svyqsr)

# Top 10% share
share <- vector("numeric", length(2004:2017))
j <- 1

for(year in 2004:2017) {
  svy_subset <- subset(silc.p1.svy, rb010 == year)
  svy_top_10 <- subset(svy_subset, income_p1_2 >= as.numeric(svyquantile(~income_p1_2, svy_subset, quantile = c(0.9))))
  share[j] <- svytotal(~income_p1_2, svy_top_10) / svytotal(~income_p1_2, svy_subset)
  j <- j + 1
}

years_top10_p1_2 <- data.frame(top_p1_2 = share)

# Table preparation -----------------------------------------------------------

table_p1_2 <- data.frame(mean_p1_2$rb010, round(mean_p1_2$income_p1_2), 
                         round(median_p1_2$statistic), round(gini_p1_2_all$income_p1_2*100,1),
                         round(p80p20_p1_2$income_p1_2,1), round(years_top10_p1_2$top_p1_2*100,1))

colnames(table_p1_2)<- c("Jahr", "Mittelwert" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p1_2
print(xtable(table_p1_2), include.rownames = FALSE)

# Post-tax disposable income --------------------------------------------------

# Mean
mean_p1_3 <- svyby(~income_p1_3, ~rb010, silc.p1.svy, svymean)
# mean_p1_3$mean <- mean_p1_3$income_p1_3/inflation$values*100

# Median
median_p1_3 <- svyby(~income_p1_3, ~rb010, silc.p1.svy, 
                     svyquantile, quantiles = c(0.5), keep.var = FALSE)
# median_p1_3$median <- median_p1_3$income_p1_3/inflation$values*100

# Gini
gini_p1_3_all <- svyby(~income_p1_3, ~rb010, silc.p1.svy, svygini)

# P80/P20
p80p20_p1_3 <- svyby(~income_p1_3, ~rb010, silc.p1.svy, svyqsr)

# Top 10% share

share <- vector("numeric", length(2004:2017))
j <- 1

for(year in 2004:2017) {
  svy_subset <- subset(silc.p1.svy, rb010 == year)
  svy_top_10 <- subset(svy_subset, income_p1_3 >= as.numeric(svyquantile(~income_p1_3, svy_subset, quantile = c(0.9))))
  share[j] <- svytotal(~income_p1_3, svy_top_10) / svytotal(~income_p1_3, svy_subset)
  j <- j + 1
}

years_top10_p1_3 <- data.frame(top_p1_3 = share)

# Table preparation -----------------------------------------------------------

table_p1_3 <- data.frame(mean_p1_3$rb010, round(mean_p1_3$income_p1_3), 
                         round(median_p1_3$statistic), round(gini_p1_3_all$income_p1_3*100,1),
                         round(p80p20_p1_3$income_p1_3,1), round(years_top10_p1_3$top_p1_3*100,1))

colnames(table_p1_3)<- c("Jahr", "Mittelwert" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p1_3
print(xtable(table_p1_3), include.rownames = FALSE)

# -----------------------------------------------------------------------------

# P2 WID WORLD ---------------------------------------------------------------

# Pre-tax factor income (Canberra: primary income) ---------------------------

# Mean
mean_p2_1 <- svyby(~income_p2_1, ~rb010, silc.p2.svy, svymean)
# mean_p2_1$mean <- mean_p2_1$income_p2_1/inflation$values*100

# Median
median_p2_1 <- svyby(~income_p2_1, ~rb010, silc.p2.svy, 
                     svyquantile, quantiles = c(0.5), keep.var = FALSE)
# median_p2_1$median <- median_p2_1$income_p2_1/inflation$values*100

# Gini
gini_p2_1_all <- svyby(~income_p2_1, ~rb010, silc.p2.svy, svygini)

# P80/P20
p80p20_p2_1 <- svyby(~income_p2_1, ~rb010, silc.p2.svy, svyqsr)

# Top 10% share
share <- vector("numeric", length(2004:2017))
j <- 1

for(year in 2004:2017) {
  svy_subset <- subset(silc.p2.svy, rb010 == year)
  svy_top_10 <- subset(svy_subset, income_p2_1 >= as.numeric(svyquantile(~income_p2_1, svy_subset, quantile = c(0.9))))
  share[j] <- svytotal(~income_p2_1, svy_top_10) / svytotal(~income_p2_1, svy_subset)
  j <- j + 1
}

years_top10_p2_1 <- data.frame(top_p2_1 = share)

# Table preparation -----------------------------------------------------------

table_p2_1 <- data.frame(mean_p2_1$rb010, round(mean_p2_1$income_p2_1), 
                         round(median_p2_1$statistic), round(gini_p2_1_all$income_p2_1*100,1),
                         round(p80p20_p2_1$income_p2_1,1), round(years_top10_p2_1$top_p2_1*100, 1))

colnames(table_p2_1)<- c("Jahr", "Mittelwert" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p2_1
print(xtable(table_p2_1), include.rownames = FALSE)

# Pre-tax national income -----------------------------------------------------

# Mean
mean_p2_2 <- svyby(~income_p2_2, ~rb010, silc.p2.svy, svymean)
#mean_p2_2$mean <- mean_p2_2$income_p2_2/inflation$values*100

# Median
median_p2_2 <- svyby(~income_p2_2, ~rb010, silc.p2.svy, 
                     svyquantile, quantiles = c(0.5), keep.var = FALSE)
#median_p2_2$median <- median_p2_2$income_p2_2/inflation$values*100

# Gini
gini_p2_2_all <- svyby(~income_p2_2, ~rb010, silc.p2.svy, svygini)

# P80/P20
p80p20_p2_2 <- svyby(~income_p2_2, ~rb010, silc.p2.svy, svyqsr)

# Top 10% share
share <- vector("numeric", length(2004:2017))
j <- 1

for(year in 2004:2017) {
  svy_subset <- subset(silc.p2.svy, rb010 == year)
  svy_top_10 <- subset(svy_subset, income_p2_2 >= as.numeric(svyquantile(~income_p2_2, svy_subset, quantile = c(0.9))))
  share[j] <- svytotal(~income_p2_2, svy_top_10) / svytotal(~income_p2_2, svy_subset)
  j <- j + 1
}

years_top10_p2_2 <- data.frame(top_p2_2 = share)

# Table preparation -----------------------------------------------------------

table_p2_2 <- data.frame(mean_p2_2$rb010, round(mean_p2_2$income_p2_2), 
                         round(median_p2_2$statistic), round(gini_p2_2_all$income_p2_2*100, 1),
                         round(p80p20_p2_2$income_p2_2,1), round(years_top10_p2_2$top_p2_2*100,1))

colnames(table_p2_2)<- c("Jahr", "Mittelwert" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p2_2
xtable(table_p2_2)

# Post-tax disposable income -------------------------------------------------

# Mean
mean_p2_3 <- svyby(~income_p2_3, ~rb010, silc.p2.svy, svymean)
# mean_p2_3$mean <- mean_p2_3$income_p2_3/inflation$values*100

# Median
median_p2_3 <- svyby(~income_p2_3, ~rb010, silc.p2.svy, 
                     svyquantile, quantiles = c(0.5), keep.var = FALSE)
# median_p2_3$median <- median_p2_3$income_p2_3/inflation$values*100

# Gini
gini_p2_3_all <- svyby(~income_p2_3, ~rb010, silc.p2.svy, svygini)

# P80/P20
p80p20_p2_3 <- svyby(~income_p2_3, ~rb010, silc.p2.svy, svyqsr)

# Top 10% share
share <- vector("numeric", length(2004:2017))
j <- 1

for(year in 2004:2017) {
  svy_subset <- subset(silc.p2.svy, rb010 == year)
  svy_top_10 <- subset(svy_subset, income_p2_3 >= as.numeric(svyquantile(~income_p2_3, svy_subset, quantile = c(0.9))))
  share[j] <- svytotal(~income_p2_3, svy_top_10) / svytotal(~income_p2_3, svy_subset)
  j <- j + 1
}

years_top10_p2_3 <- data.frame(top_p2_3 = share)


# Table preparation -----------------------------------------------------------

table_p2_3 <- data.frame(mean_p2_3$rb010, round(mean_p2_3$income_p2_3), 
                         round(median_p2_3$statistic), round(gini_p2_3_all$income_p2_3*100,1),
                         round(p80p20_p2_3$income_p2_3,1), round(years_top10_p2_3$top_p2_3*100,1))

colnames(table_p2_3)<- c("Jahr", "Mittelwert" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p2_3
xtable(table_p2_3)


# Plots -----------------------------------------------------------------------

# MEAN -----------------------------------------------------------------------

# Mittelwert vor und nach Steuern P1 Equal Sharing
mean_plot_p1 <- ggplot() +
  geom_line(mapping = aes(y = table_p1_1$Mittelwert, x = table_p1_1$Jahr,
                          color = "Mittelwert vor Steuern (factor)"),
            size = 1 ) +
  geom_line(mapping = aes(y = table_p1_3$Mittelwert,x = table_p1_3$Jahr,
                          color = "Mittelwert nach Steuern"), size = 1) +
  geom_line(mapping = aes(y = table_p1_2$Mittelwert,x = table_p1_2$Jahr,
                          color = "Mittelwert vor Steuern (national)"), 
            size = 1) + scale_color_manual(values = 
   c('Mittelwert vor Steuern (factor)' = 'darkred', 
     'Mittelwert nach Steuern' = 'darkblue', 
     'Mittelwert vor Steuern (national)' = 'pink')) +
  labs(color = '', x = "Jahr", y = "Einkommen in Euro", 
       title = "Mittelwert vor und nach Steuern",
       subtitle = "Equal Sharing der Haushaltsmitglieder") +
  expand_limits(y = c(12000, 32000)) + 
  ggsave(file='mean_plot_p1.svg',height=4,width=7)
mean_plot_p1

# Mittelwert vor und nach Steuern P2 Partial Sharing
mean_plot_p2 <- ggplot() +
  geom_line(mapping = aes(y = table_p2_1$Mittelwert,x = table_p2_1$Jahr,
                          color = "Mittelwert vor Steuern (factor)"),
            size = 1 ) +
  geom_line(mapping = aes(y = table_p2_3$Mittelwert,x = table_p2_3$Jahr,
                          color = "Mittelwert nach Steuern"), size = 1) +
  geom_line(mapping = aes(y = table_p2_2$Mittelwert,x = table_p2_2$Jahr,
                          color = "Mittelwert vor Steuern (national)"), 
            size = 1) +
  scale_color_manual(values = c('Mittelwert vor Steuern (factor)' = 'darkred',
                                'Mittelwert nach Steuern' = 'darkblue', 
                                'Mittelwert vor Steuern (national)' = 'pink')) +
  labs(color = '', x = "Jahr", y = "Einkommen in Euro", 
       title = "Mittelwert vor und nach Steuern",
       subtitle = "Partial Sharing der Haushaltsmitglieder") +
  expand_limits(y = c(8000, 30000)) + 
  ggsave(file='mean_plot_p2.svg',height=4,width=7)
mean_plot_p2

# GINI -----------------------------------------------------------------------

# Gini vor und nach Steuern P1
gini_plot_p1 <- ggplot() +
  geom_line(mapping = aes(y = table_p1_1$Gini,x = table_p1_1$Jahr,
                          color = "Gini vor Steuern (factor)"), size = 1 ) +
  geom_line(mapping = aes(y = table_p1_2$Gini,x = table_p1_2$Jahr,
                          color = "Gini vor Steuern (national)"), size = 1) +
  geom_line(mapping = aes(y = table_p1_3$Gini,x = table_p1_3$Jahr,
                          color = "Gini nach Steuern"), size = 1) +
  scale_color_manual(values = c('Gini vor Steuern (factor)' = 'darkred',
                                'Gini nach Steuern' = 'darkblue', 
                                'Gini vor Steuern (national)' = 'pink'))+
  labs(color = '', x = "Jahr", y = "Gini", 
       title = "Gini Koeffizient vor und nach Steuern",
       subtitle = "Equal Sharing der Haushaltsmitglieder") +
  expand_limits(y = c(0.10, 0.60)) + 
  ggsave(file='gini_plot_p1.svg',height=4,width=7)
gini_plot_p1

# Gini vor und nach Steuern P2
gini_plot_p2 <- ggplot() +
  geom_line(mapping = aes(y = table_p2_1$Gini,x = table_p2_1$Jahr,
                          color = "Gini vor Steuern (factor)"), size = 1 ) +
  geom_line(mapping = aes(y = table_p2_2$Gini,x = table_p2_2$Jahr,
                          color = "Gini vor Steuern (national)"), size = 1) +
  geom_line(mapping = aes(y = table_p2_3$Gini,x = table_p2_3$Jahr,
                          color = "Gini nach Steuern"), size = 1) +
  scale_color_manual(values = c('Gini vor Steuern (factor)' = 'darkred', 
                                'Gini nach Steuern' = 'darkblue',
                                'Gini vor Steuern (national)' = 'pink'))+
  labs(color = '', x = "Jahr", y = "Gini", 
       title = "Gini Koeffizient vor und nach Steuern",
       subtitle = "Partial Sharing der Haushaltsmitglieder")+
  expand_limits(y = c(0.10, 0.60)) + 
  ggsave(file='gini_plot_p2.svg',height=4,width=7)
gini_plot_p2

# TOP10% ----------------------------------------------------------------------

# Gini vor und nach Steuern P1
top10_plot_p1 <- ggplot() +
  geom_line(mapping = aes(y = table_p1_1$`Top10%`,x = table_p1_1$Jahr,
                          color = "Top10% vor Steuern (factor)"), size = 1 ) +
  geom_line(mapping = aes(y = table_p1_2$`Top10%`,x = table_p1_2$Jahr,
                          color = "Top10% vor Steuern (national)"), size = 1) +
  geom_line(mapping = aes(y = table_p1_3$`Top10%`,x = table_p1_3$Jahr,
                          color = "Top10% nach Steuern"), size = 1) +
  scale_color_manual(values = c('Top10% vor Steuern (factor)' = 'darkred',
                                'Top10% nach Steuern' = 'darkblue', 
                                'Top10% vor Steuern (national)' = 'pink'))+
  labs(color = '', x = "Jahr", y = "Anteil der Top 10%", 
       title = "Anteil am Einkommen der Top 10% vor und nach Steuern",
       subtitle = "Equal Sharing der Haushaltsmitglieder") +
  expand_limits(y = c(0, 0.5)) + 
  ggsave(file='top10_plot_p1.svg',height=4,width=7)
top10_plot_p1

# Gini vor und nach Steuern P1
top10_plot_p2 <- ggplot() +
  geom_line(mapping = aes(y = table_p2_1$`Top10%`,x = table_p2_1$Jahr,
                          color = "Top10% vor Steuern (factor)"), size = 1 ) +
  geom_line(mapping = aes(y = table_p2_2$`Top10%`,x = table_p2_2$Jahr,
                          color = "Top10% vor Steuern (national)"), size = 1) +
  geom_line(mapping = aes(y = table_p2_3$`Top10%`,x = table_p2_3$Jahr,
                          color = "Top10% nach Steuern"), size = 1) +
  scale_color_manual(values = c('Top10% vor Steuern (factor)' = 'darkred',
                                'Top10% nach Steuern' = 'darkblue', 
                                'Top10% vor Steuern (national)' = 'pink'))+
  labs(color = '', x = "Jahr", y = "Anteil der Top 10%", 
       title = "Anteil am Einkommen der Top 10% vor und nach Steuern",
       subtitle = "Partial Sharing der Haushaltsmitglieder") +
  expand_limits(y = c(0.1, 0.7)) + 
  ggsave(file='top10_plot_p2.svg',height=4,width=7)
top10_plot_p2

