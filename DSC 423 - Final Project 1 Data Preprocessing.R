# Data preprocessing - summer.csv
summer <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Final Project\\Data File\\summer.csv")

summer1 <- summer[!duplicated(summer[, -5]), ] 
head(summer)

summer2 <- summer1[, c('Year', 'Country')]
head(summer2)

top15_country_list <- c('AUS', 'CAN', 'CHN', 'FIN', 'FRA', 'GBR', 'GER', 'ITA',
                        'JPN', 'KOR', 'NED', 'RUS', 'SUI', 'SWE', 'USA')
summer3 <- summer2[summer2$Country %in% top15_country_list, ]
head(summer3)

summer_medal_counts <- table(summer3$Year, summer3$Country)
print(summer_medal_counts)

write.csv(summer_medal_counts, "cleaned_summer.csv", row.names = TRUE)

# Data preprocessing - winter.csv
winter <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Final Project\\Data File\\winter.csv")

winter1 <- winter[!duplicated(winter[, -5]), ] 
head(winter)

winter2 <- winter1[, c('Year', 'Country')]
head(winter2)

top15_country_list <- c('AUT', 'CAN', 'CHN', 'FIN', 'FRA', 'GER', 'ITA', 'JPN',
                        'KOR', 'NED', 'NOR', 'RUS', 'SUI', 'SWE', 'USA')
winter3 <- winter2[winter2$Country %in% top15_country_list, ]
head(winter3)

winter_medal_counts <- table(winter3$Year, winter3$Country)
print(winter_medal_counts)

write.csv(winter_medal_counts, "cleaned_winter.csv", row.names = TRUE)

# Data preprocessing - gdp_1960_2022.csv
gdp <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Final Project\\Data File\\gdp_1960_2022.csv")

gdp1 <- subset(gdp, select = - c(Country.Name, Indicator.Name, Indicator.Code))
head(gdp1)

country_list <- c('AUS', 'AUT', 'CAN', 'CHN', 'FIN', 'FRA', 'GBR', 'GER', 'ITA',
                  'JPN', 'KOR', 'NED', 'NOR', 'RUS', 'SUI', 'SWE', 'USA')
gdp2 <- gdp1[gdp1$Country.Code %in% country_list, ]
head(gdp2)

write.csv(gdp2, "cleaned_gdp.csv", row.names = TRUE)

# Data preprocessing - population.csv
population <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Final Project\\Data File\\population.csv")

population1 <- subset(population, select = - c(Country.Name, Indicator.Name, Indicator.Code))
head(population1)

country_list <- c('AUS', 'AUT', 'CAN', 'CHN', 'FIN', 'FRA', 'GBR', 'GER', 'ITA',
                  'JPN', 'KOR', 'NED', 'NOR', 'RUS', 'SUI', 'SWE', 'USA')
population2 <- population1[population1$Country.Code %in% country_list, ]
head(population2)

write.csv(population2, "cleaned_population.csv", row.names = TRUE)


# Data preprocessing - health.csv
health <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Final Project\\Data File\\health.csv")

country_list <- c('AUS', 'AUT', 'CAN', 'CHN', 'FIN', 'FRA', 'GBR', 'GER', 'ITA',
                  'JPN', 'KOR', 'NED', 'NOR', 'RUS', 'SUI', 'SWE', 'USA')
year_list <- c('1960', '1964', '1968', '1972', '1976', '1980', '1984', '1988', '1992', '1994', '1996', '1998',
               '2000', '2002', '2004', '2006', '2008', '2010', '2012', '2014', '2016', '2018', '2020', '2022')
health1 <- health[health$CountryCode %in% country_list & health$Year %in% year_list, ]
head(health1)

write.csv(health1, "cleaned_health.csv", row.names = TRUE)