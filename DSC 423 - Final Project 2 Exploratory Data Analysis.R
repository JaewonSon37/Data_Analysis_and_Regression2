# Read the CSV file
summer_olympic <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Final Project\\Data File\\final_summer.csv")
head(summer_olympic)

# Structure of the dataset
str(summer_olympic)

# Summary
## Summary for GDP
summary(summer_olympic$GDP)

## Summary for Population
summary(summer_olympic$Population)

## Summary for Number of Medals
summary(summer_olympic$NumberofMedals)

# Line plot
## Load the library
library(ggplot2)

## Subset for the USA and Russia
usa <- summer_olympic[summer_olympic$Country == "USA", ]
rus <- summer_olympic[summer_olympic$Country == "RUS", ]

## Create plots for USA's GDP over time
ggplot(data = usa, aes(x = Year, y = GDP)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "USA's GDP Over Time", x = "Year", y = "GDP (in Billions)")

## Create plots for USA's Population over time
ggplot(data = usa, aes(x = Year, y = Population)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "USA's Population", x = "Year", y = "Population")

## Create plots for USA's Number of Medals over time
ggplot(data = usa, aes(x = Year, y = NumberofMedals)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "USA's Number of Medals", x = "Year", y = "Number of Medals")

## Create plots for Russia's GDP over time
ggplot(data = rus, aes(x = Year, y = GDP)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Russia's GDP Over Time", x = "Year", y = "GDP (in Billions)")

## Create plots for Russia's Population over time
ggplot(data = rus, aes(x = Year, y = Population)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Russia's Population", x = "Year", y = "Population")

## Create plots for Russia's Number of Medals over time
ggplot(data = rus, aes(x = Year, y = NumberofMedals)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Russia's Number of Medals", x = "Year", y = "Number of Medals")

# Box plot
## Box plot for GDP
boxplot(summer_olympic$GDP, main = "Boxplot of GDP")

## Box plot for GDP with log transformation
summer_olympic$log_GDP <- log(summer_olympic$GDP)
boxplot(summer_olympic$log_GDP, main = "Boxplot of log(GDP)")

## Box plot for Population
boxplot(summer_olympic$Population, main = "Boxplot of Population")

## Box plot for Population with log transformation
summer_olympic$log_Population <- log(summer_olympic$Population)
boxplot(summer_olympic$log_Population, main = "Boxplot of log(Population)")

## Box plot for Number of Medals
boxplot(summer_olympic$NumberofMedals, main = "Boxplot of Number of Medals")

# Bar Plot
## Bar Plot for Host Continent
ggplot(summer_olympic, aes(x = HostContinent)) +
  geom_bar()

## Bar Plot for Political System
ggplot(summer_olympic, aes(x = PoliticalSystem)) +
  geom_bar()

# Scatter plot and correlation
## Subset for the numeric variables
summer_olympic_numeric <- subset(summer_olympic,select = -c(Country, Olympic, Year, Climate,
                                                            PoliticalSystem, HostContinent,
                                                            HostCountry, Participation))

## Define a function to display correlation coefficients on pairs plot
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor = 1, ...)
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  text(0.5, 0.5, txt, cex = cex.cor)
}

## Create a pairs plot with correlation coefficients
pairs( ~ ., data = summer_olympic_numeric,
       lower.panel = panel.smooth, upper.panel = panel.cor)