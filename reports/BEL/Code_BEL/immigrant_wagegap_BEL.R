# -----------------------------------------------------------------------------

# Belgium - Immigrant Wage Gap
# Descriptive Statistics & Oaxaca Blinder Decomposition
# time frame: 20

# -----------------------------------------------------------------------------

library(oaxaca)
library(ggplot2)
library(dplyr)
library(survey)
library(convey)
library(Hmisc)

# -----------------------------------------------------------------------------

# Generate Subsets for EU borns and migrants

eu <- data %>% filter(migration == 0) # 4331 observations
migrants <- data %>% filter(migration == 1) # 370 observations


# Descriptive Statistics ------------------------------------------------------

# Basic Analysis of Income Data

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
mean_gap <- round((1 - mean_mig / mean_eu) * 100, digits = 2) # 22.4
median_gap <- round((1 - median_mig / median_eu) * 100, digits = 2) # 31.5

# Median
median_eu <- svyquantile(~hwages, data.svy.eu, quantile=c(0.5))
median_mig <- svyquantile(~hwages, data.svy.mig, quantile=c(0.5))

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
  title="Mean and median income of EU citizen and migrants in Belgium") + 
  scale_fill_manual(values=c("blue","orange", "blue", "orange")) + 
  guides(fill="none")
p


# -----------------------------------------------------------------------------

# Some basic OLS regressions --------------------------------------------------

# OLS whole sample
ols_0 <- (lm(log(hwages) ~ gender + I(experience) + I(experience^2) + edu + 
         migration + position + urb + health, data = data, weights = weights))
summary(ols_0)

ols_1 <- (lm(log(hwages) ~ gender + I(age) + I(age^2) + edu + 
          migration + position + urb + health, data = data, weights = weights))
summary(ols_1)

# health and urbanisation insignificant

ols_2 <- (lm(log(hwages) ~ gender + I(experience) + I(experience^2) + edu + 
             migration + position, data = data, weights = weights))
summary(ols_2)

ols_3 <- (lm(log(hwages) ~ gender + I(age) + I(age^2) + edu + migration + 
               position, data = data, weights = weights))
summary(ols_3)


# -----------------------------------------------------------------------------

# Generate Subsets for EU borns and migrants

eu <- data %>% filter(migration == 0) # 4331 observations
migrants <- data %>% filter(migration == 1) # 370 observations

# OLS for each sample ---------------------------------------------------------

ols_eu <- (lm(log(hwages) ~ gender + I(experience) + I(experience^2) + edu + 
                position, data = data, weights = weights))
summary(ols_eu)

ols_mig <- (lm(log(hwages) ~ gender + I(experience) + I(experience^2) + edu + 
                position, data = data, weights = weights))
summary(ols_mig)


# Oaxaca Blinder Decomposition ------------------------------------------------

oaxaca <- oaxaca(hwages ~ gender + I(experience) + I(experience^2) + edu + 
                   position | migration, data = data)
summary(oaxaca)
oaxaca$y # shows the means and difference in means, leaving the difference of
# 5,60 to be explained by the Oaxaca Blinder decomposition
oaxaca$twofold$overall
plot <- plot(oaxaca, decomposition = "twofold", group.weight = -1)

# Still have to decide which group weight we use but in general around 
# 1.0 - 1.3 explained, rest is unexplained ( R package documentation)
# ab seite 69 bei schnetzer

# 

