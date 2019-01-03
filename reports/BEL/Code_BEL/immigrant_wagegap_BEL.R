# -----------------------------------------------------------------------------

# Belgium - Immigrant Wage Gap
# Descriptive Statistics & Oaxaca Blinder Decomposition
# time frame: 2017

# -----------------------------------------------------------------------------

library(oaxaca)
library(ggplot2)
library(dplyr)
library(survey)
library(convey)
library(Hmisc)
library(xtable)
library(stargazer)

# -----------------------------------------------------------------------------

# Generate Subsets for EU borns and migrants

eu <- data %>% filter(migration == 0) 
migrants <- data %>% filter(migration == 1) 


# Descriptive Statistics ------------------------------------------------------

# Plot Education for both groups

p_eu <- ggplot(eu, aes(x="", y=eu$edu, fill=eu$edu))+
  geom_bar(width = 1, stat = "identity") + 
  labs(x = "", y = "", 
       title = "Übersicht Bildungsstufen von EU-BürgerInnen in Belgien") + 
   scale_fill_discrete(name = "Bildungsstufen", 
   labels = c("Maximal Primarabschluss","Untere Sekundarstufe", 
   "Höhere Sekundarstufe und nicht tertiäre höhere Abschlüsse", 
   "Tertiärer Abschluss"))
p_eu

p_mig <- ggplot(migrants, aes(x = "", y = edu, fill = edu))+
  geom_bar(width = 1, stat = "identity") + 
  labs(x = "", y = "", 
       title = "Übersicht Bildungsstufen von MigrantInnen in Belgien") + 
  scale_fill_discrete(name = "Bildungsstufen", 
   labels = c("Maximal Primarabschluss","Untere Sekundarstufe", 
   "Höhere Sekundarstufe und nicht tertiäre höhere Abschlüsse", 
    "Tertiärer Abschluss"))
p_mig

# Income Density Plots
eu_limited <- eu %>% filter(py010g <= 260000) # to make scales comparable
income_density_eu <- ggplot(eu_limited) + geom_density(aes(x=py010g)) + 
  labs(x = "Brutto-Jahreseinkommen EU-BürgerInnen", y = "Dichte", title = "")

income_density_mig <- ggplot(migrants) + geom_density(aes(x=py010g)) + 
  labs(x = "Brutto-Jahreseinkommen MigrantInnen", y = "Dichte", title = "")

# Basic Analysis of Income Data -----------------------------------------------

# Creating Survey Objects -----------------------------------------------------

data.svy.eu <- svydesign(ids =  ~ id_h,
                         weights = ~ weights,
                         data = eu) %>% convey_prep()

data.svy.mig <- svydesign(ids =  ~ id_h,
                         weights = ~ weights,
                         data = migrants) %>% convey_prep()

# Mean and Median -------------------------------------------------------------

# Mean
mean_eu <- svymean(~hwages, data.svy.eu)
mean_mig <- svymean(~hwages, data.svy.mig)
mean_gap <- (mean_eu-mean_mig)/mean_eu

# Median
median_eu <- svyquantile(~hwages, data.svy.eu, quantile=c(0.5))
median_mig <- svyquantile(~hwages, data.svy.mig, quantile=c(0.5))
median_gap <- (median_eu-median_mig)/median_eu

# creating a data frame with values for means and medians
df <- data.frame(names=c("Mean income EU", "Mean income Migrants", 
                         "Median income EU", "Median income Migrants"),
                 values=c(mean_eu, mean_mig, median_eu, median_mig))
df

# check variable classes in order for ggplot to work properly
class(df$names) # factor
class(df$values) # numeric

# Basic barplot
p <- ggplot(data=df, aes(x=factor(names,levels=names) , y=values, fill=names)) +
  geom_bar(position="dodge",stat="identity") + labs(x="", y="Hourly wages", 
  title="") + 
  scale_fill_manual(values=alpha(c("blue","red", "blue", "red"), .5)) + 
  guides(fill="none")
p


# -----------------------------------------------------------------------------

# Some basic OLS regressions --------------------------------------------------

# OLS whole sample
ols_0 <- (lm(log(hwages) ~ gender + I(experience) + I(experience^2) + edu + 
         migration + position + urb + health + jobchange, data = data, 
         weights = weights))
summary(ols_0)

ols_1 <- (lm(log(hwages) ~ gender + I(age) + I(age^2) + edu + 
          migration + position + urb + health + jobchange, data = data, 
          weights = weights))
summary(ols_1) # better

# health, urbanisation insignificant 

ols_2 <- (lm(log(hwages) ~ gender + I(age) + I(age^2) + edu + 
             migration + position + jobchange, data = data, weights = weights))
summary(ols_2)

# Stargazer Output für Latex
stargazer(ols_2, title="Results", align=TRUE, dep.var.label=("Log- Bruttostundenlohn"), covariate.labels=c("Gender", "Alter", "Alter^2", "Untere Sekundarstufe", "Höhere Sekundarstufe/nicht tertiäre höhere Abschlüsse", "Tertiärer Abschluss", "Migration", "Position", "Job Wechsel"), omit.stat=c("LL","ser","f"),no.space = TRUE)


# -----------------------------------------------------------------------------

# OLS for each sample ---------------------------------------------------------

ols_eu <- (lm(log(hwages) ~ gender + I(age) + I(age^2) + edu 
                , data = eu, weights = weights))
summary(ols_eu)

ols_mig <- (lm(log(hwages) ~ gender + I(age) + I(age^2) + edu
               , data = migrants, weights = weights))
summary(ols_mig)


# Oaxaca Blinder Decomposition ------------------------------------------------

oaxaca <- oaxaca(hwages ~ gender + I(age) + I(age^2) + edu 
                    | migration, data = data)
summary(oaxaca)
oaxaca$y # shows the means and difference in means, leaving the difference of
# ... to be explained by the Oaxaca Blinder decomposition
oaxaca$twofold$overall

oaxacadf <- data.frame(oaxaca$twofold$overall)
oaxacadf <- oaxacadf[,1:5]
xtable(oaxacadf) # Latex Output

plot_oaxaca <- plot(oaxaca, decomposition = "twofold", group.weight = -1)
plot_oaxaca


# Still have to decide which group weight we use but in general around 
# 1.0 - 1.3 explained, rest is unexplained ( R package documentation)
# ab seite 69 bei schnetzer

# 

