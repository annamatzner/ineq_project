# -----------------------------------------------------------------------------
#
# Indicators with Eurostat Data
#
# -----------------------------------------------------------------------------


library(tidyverse)
library(eurostat)
library(svglite)


# DATA ------------------------------------------------------------------------

pov <- get_eurostat("ilc_li02") # Quote der von Armut bedrohten Personen nach 
# Armutsgefährdungsgrenze, Alter und Geschlecht

quants <- get_eurostat("ilc_di01") # Einkommensverteilung nach Quantilen

qsr <- get_eurostat("ilc_di11") # S80/S20 Einkommensquintilverhältnis 
# nach Geschlecht und nach Altersklassen

gini <- get_eurostat("ilc_di12") # Gini-Koeffizient des verfügbaren 
# Äquivalenzeinkommens 

inc <- get_eurostat("ilc_di04", time_format="num") # Mean and median income by household type

inflation <- get_eurostat("prc_hicp_aind" ,time_format = "num")
inflation <- inflation %>% filter(unit == "INX_A_AVG", coicop == "CP00", 
                          geo == "BE", time %in% c(1996:2001, 2003:2017)) %>% 
                        select(time, values) %>% rename(hvpi = "values")


# GINI ------------------------------------------------------------------------

gini <- gini %>% filter(geo == "BE") %>% select(time, values) %>% 
  rename(Gini = "values")

# plot
ggplot() +
  geom_line(aes(y = gini$Gini,x = gini$time), color = "pink", size = 1) +
            labs(color = '', x = "Jahr", y = "Gini Koeffizient", 
            title = "Gini-Koeffizient des verfügbaren Äquivalenzeinkommens",
            subtitle = "1995 - 2017",
            caption = "Quelle: Eurostat") +
  expand_limits(y = c(22, 33)) + ggsave(file='gini_plot.svg',height=4,width=7)


# MEDIAN/MEAN -----------------------------------------------------------------

incdf <- inc %>% filter(geo == "BE", hhtyp == "TOTAL", unit == "EUR", 
                        time %in% 1996:2017) %>% 
  select(time, indic_il, values) %>% 
  mutate(indic_il = plyr::revalue(indic_il, c("MED_E"="Median","MEI_E"="Mittelwert")))

incdf <- incdf %>% spread(indic_il,values) %>% left_join(., inflation) %>% 
  mutate('Mittelwert (bereinigt)' = Mittelwert/hvpi*100, 'Median (bereinigt)'  = Median/hvpi*100) %>% 
  select(-hvpi) %>% gather(indicator, values, Median:'Median (bereinigt)')

# plot
ggplot(incdf, aes(x=time,y=values, group=indicator, color=indicator)) +
  geom_line(aes(linetype = indicator, color = indicator)) +
  scale_linetype_manual(values = c("solid","dashed","solid","dashed")) +
 scale_color_manual(values = c('darkcyan','darkcyan','chocolate1', 'chocolate1')) +
  theme(legend.position = c(0.8,0.2), legend.title = element_blank()) +
  labs(title = "Mittelwert und Median der Einkommen in Belgien 1996 - 2017", 
       subtitle = "Bereinigt mittels HVPI", caption = "Quelle: Eurostat", 
       x = "Jahr", y = "Einkommen in Euro") + 
  ggsave(file='mean_median_bel.svg',height=4,width=7)


# Top 10% ---------------------------------------------------------------------

quants <- quants %>% filter(geo == "BE", indic_il == "SHARE", currency == "EUR",
                            quantile == "D10") %>% select(time, values) %>%
  rename("Anteil Top 10%" = "values")

# plot
ggplot() +
  geom_line(aes(y = quants$"Anteil Top 10%",x = quants$time), color = "pink", size = 1) +
  labs(color = '', x = "Jahr", y = "Anteil in %", 
       title = "Anteil des oberen Dezils am nationalen Äquivalenznationaleinkommen ",
       subtitle = "1995 - 2017",
       caption = "Quelle: Eurostat") +
  expand_limits(y = c(15, 30))  + 
  ggsave(file='top10_plot.svg',height=4,width=7)


# ARMUTSGEFÄHRDUNG ------------------------------------------------------------

pov <- pov %>% filter(geo == "BE", age == "TOTAL", indic_il == "LI_R_MD60", 
                      sex == "T", unit == "PC") %>% select(time, values) %>%
  rename(Armutsgefährdungsquote = "values")

# plot
ggplot() +
  geom_line(aes(y = pov$Armutsgefährdungsquote,x = pov$time), color = "pink", size = 1) +
  labs(color = '', x = "Jahr", y = "Armutsgefährdungsquote", 
       title = "Armutsgefährdungsquote (Grenze: 60% des medianen Äquivalenzeinkommens nach Sozialleistungen)",
       subtitle = "1995 - 2017",
       caption = "Quelle: Eurostat") +
  expand_limits(y = c(10, 20)) + ggsave(file='armut_plot.svg',height=4,width=7)


# S80/20 ----------------------------------------------------------------------

qsr <- qsr %>% filter(geo == "BE", age == "TOTAL", sex == "T") %>% 
  select(values, time) %>% rename( "S80/20" = "values")

  
# TABELLE 1 -------------------------------------------------------------------
# alle Indikatoren

indikatoren_bel <- data.frame(gini$time, mean$Mittelwert, median$Median, 
                              gini$Gini, quants$`Anteil Top 10%`, qsr$`S80/20`, 
                              pov$Armutsgefährdungsquote)

colnames(indikatoren_bel) <- c("Jahr", "Mittelwert", "Median", "Gini", 
                               "Anteil der Top 10%", "S80/20", 
                               "Armutsgefährdungsquote")

indikatoren_bel$Jahr <- substr(indikatoren_bel$Jahr, 1, 4)
write.csv(indikatoren_bel, file = "table_bel", row.names = F)


# TABELLE 2 -------------------------------------------------------------------
# Mittelwerte und Mediane bereinigt und unbereinigt und hvpi Index

incdf <- inc %>% filter(geo == "BE", hhtyp == "TOTAL", unit == "EUR", 
                        time %in% 1996:2017) %>% 
  select(time, indic_il, values) %>% 
  mutate(indic_il = plyr::revalue(indic_il, c("MED_E"="Median","MEI_E"="Mittelwert")))

incdf <- incdf %>% spread(indic_il,values) %>% left_join(., inflation) %>% 
  mutate('Mittelwert (bereinigt)' = Mittelwert/hvpi*100, 'Median (bereinigt)'  = Median/hvpi*100) 

indikatoren_bel_2 <- incdf %>% select(time, hvpi, Mittelwert,
                                      `Mittelwert (bereinigt)`, Median, 
                                      `Median (bereinigt)`) %>% arrange(desc(time))

indikatoren_bel_2$`Mittelwert (bereinigt)` <- round(indikatoren_bel_2$`Mittelwert (bereinigt)`,0)
indikatoren_bel_2$`Median (bereinigt)` <- round(indikatoren_bel_2$`Median (bereinigt)`,0)

colnames(indikatoren_bel_2) <- c("Jahr", "HVPI (2015 = 100)", "Mittelwert", 
                                 "Mittelwert (bereinigt)", "Median", 
                                 "Median (bereinigt)")

write.csv(indikatoren_bel_2, file = "table_bel_2", row.names = F)
