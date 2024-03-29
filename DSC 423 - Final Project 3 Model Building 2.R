# Data Loading and Preprocessing
## Read the CSV file
winter <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Final Project\\Data File\\final_winter.csv")

## Remove unnecessary columns
drop <- c("Olympic", "Year", "Country", "Participation")
final <- winter[, !names(winter) %in% drop]

# Model Building 1 - with backward method
## Load the library
library(MASS)
library(car)

## Initial model
m <- lm(NumberofMedals ~ ., data = final)
summary(m)

## Model selection
set.seed(123)
model_full <- lm(NumberofMedals ~ ., data = final)
step <- stepAIC(model_full, direction = "backward")

m <- lm(NumberofMedals ~ GDP + Population + Climate + PoliticalSystem + 
          HostCountry + PopMalePct + PopGrowth + POP_65. + POP_15.64 + DeathRate + 
          BirthRate + MortalityRateMale + MortalityRateFemale + NetMigration, 
        data = final)
summary(m)

## Remove the most insignificant variable based on the p-value
m <- lm(NumberofMedals ~ GDP + Population + Climate + HostCountry + 
          PopMalePct + PopGrowth + POP_65. + POP_15.64 + DeathRate + 
          BirthRate + MortalityRateMale + MortalityRateFemale + NetMigration, 
        data = final)
summary(m)

## Remove the most insignificant variable based on the p-value
m <- lm(NumberofMedals ~ GDP + Population + Climate + HostCountry + 
          PopMalePct + PopGrowth + POP_65. + DeathRate + BirthRate + 
          MortalityRateMale + MortalityRateFemale + NetMigration, 
        data = final)
summary(m)

## Remove the most insignificant variable based on the p-value
m <- lm(NumberofMedals ~ GDP + Population + Climate + HostCountry + 
          PopMalePct + PopGrowth + DeathRate + BirthRate + 
          MortalityRateMale + MortalityRateFemale + NetMigration, 
        data = final)
summary(m)

## Check multicollinearity
vif(m)

## Remove the variable with the highest VIF value
m <- lm(NumberofMedals ~ GDP + Population + HostCountry + 
          PopMalePct + PopGrowth + DeathRate + BirthRate + 
          MortalityRateMale + MortalityRateFemale + NetMigration, 
        data = final)
summary(m)

## Remove the most insignificant variable based on the p-value
m <- lm(NumberofMedals ~ GDP + HostCountry + PopMalePct + 
          PopGrowth + DeathRate + BirthRate + MortalityRateMale + 
          MortalityRateFemale + NetMigration, 
        data = final)
summary(m)

## Remove the most insignificant variable based on the p-value
m <- lm(NumberofMedals ~ GDP + HostCountry + PopMalePct + PopGrowth + 
          DeathRate + BirthRate + MortalityRateMale + MortalityRateFemale, 
        data = final)
summary(m)

## Remove the most insignificant variable based on the p-value
m <- lm(NumberofMedals ~ GDP + HostCountry + PopMalePct + PopGrowth + 
          DeathRate + BirthRate + MortalityRateFemale, 
        data = final)
summary(m)

## Remove the most insignificant variable based on the p-value
m <- lm(NumberofMedals ~ GDP + HostCountry + PopMalePct + 
          PopGrowth + DeathRate + BirthRate, 
        data = final)
summary(m)

## Remove the most insignificant variable based on the p-value
m <- lm(NumberofMedals ~ GDP + PopMalePct + 
          PopGrowth + DeathRate + BirthRate,
        data = final)
summary(m)

## Check multicollinearity
vif(m)

## Plot graphs for diagnosing the regression model
plot(m)

## Plot graphs for diagnosing the regression model with specific columns
graph_cols <- c("NumberofMedals", "GDP", "PopMalePct", "PopGrowth", "DeathRate", "BirthRate")
plot(winter[, graph_cols])

## Create interaction and second-order terms 
final$G2 <- final$GDP * final$GDP
final$PM2 <- final$PopMalePct * final$PopMalePct
final$PG2 <- final$PopGrowth * final$PopGrowth
final$DR2 <- final$DeathRate * final$DeathRate
final$BR2 <- final$BirthRate * final$BirthRate
final$BRDR <- final$BirthRate * final$DeathRate
final$GPG <- final$GDP * final$PopGrowth

## Generate a new model with interaction and second-order terms
m <- lm(NumberofMedals ~ GDP + PopMalePct + 
          PopGrowth + DeathRate + BirthRate + 
          BRDR + GPG + G2 + PM2 + PG2 + DR2 + BR2, 
        data = final)
summary(m)

## Remove the most insignificant variable based on the p-value
m <- lm(NumberofMedals ~ GDP + PopMalePct + 
          PopGrowth + DeathRate + BirthRate + 
          BRDR + GPG + PM2 + PG2 + DR2 + BR2, 
        data = final)
summary(m)

## Remove the most insignificant variable based on the p-value
m <- lm(NumberofMedals ~ GDP + PopMalePct + 
          PopGrowth + DeathRate + BirthRate + 
          BRDR + GPG + PM2 + PG2 + BR2, 
        data = final)
summary(m)

## Remove the most insignificant variable based on the p-value
m <- lm(NumberofMedals ~ GDP + PopMalePct + 
          PopGrowth + DeathRate + BirthRate + 
          BRDR + GPG + PM2 + BR2, 
        data = final)
summary(m)

## Plot graphs for diagnosing the regression model
plot(m)

# Checking residuals 1
## Load the library
library(caret)

## Set up cross-validation parameters
train_control <- trainControl(method = 'cv', number = 10)

## Train a model using cross-validation
m_cv <- train(NumberofMedals ~ GDP + PopMalePct + 
                PopGrowth + DeathRate + BirthRate + 
                BRDR + GPG + PM2 + BR2, 
              data = final, method = 'lm', trControl = train_control)
print(m_cv)

# Model Building 2 - with forward method
## Model selection
set.seed(123)
model_empty <- lm(NumberofMedals ~ 1, data = final)
step <- stepAIC(model_empty, direction = "forward", scope = list(upper = model_full, lower = model_empty))

m <- lm(NumberofMedals ~ POP_0.14 + PoliticalSystem + NetMigration + 
          Climate + GDP + HostCountry + POP_65., 
        data = final)
summary(m)

## Remove insignificant variables based on the p-value and domain knowledge
m <- lm(NumberofMedals ~ POP_0.14 + PoliticalSystem + 
          NetMigration + Climate + GDP, 
        data = final)

## Check multicollinearity
vif(m)

## Create interaction and second-order terms
final$P2 <- final$POP_0.14 * final$POP_0.14
final$G2 <- final$GDP * final$GDP
final$GP <- final$GDP * final$POP_0.14

## Generate a new model with interaction and second-order terms
m <- lm(NumberofMedals ~ POP_0.14 + PoliticalSystem + 
          NetMigration + Climate + GDP + 
          P2 + G2 + GP, 
        data = final)
summary(m)

## Remove the most insignificant variable based on the p-value
m <- lm(NumberofMedals ~ POP_0.14 + PoliticalSystem + 
          NetMigration + Climate + GDP + P2 + GP, 
        data = final)
summary(m)

## Remove the most insignificant variable based on the p-value
m <- lm(NumberofMedals ~ POP_0.14 + PoliticalSystem + 
          NetMigration + Climate + GDP + P2, 
        data = final)
summary(m)

## Plot graphs for diagnosing the regression model with specific columns
graph_df <- final[, c("NumberofMedals", "POP_0.14",  "PoliticalSystem", "NetMigration", "Climate", "GDP", "P2")]
plot(graph_df)

## Log transformation
m <- lm(NumberofMedals ~ POP_0.14 + PoliticalSystem + 
          NetMigration + Climate + log(GDP) + P2, 
        data = final)
summary(m)

## Plot graphs for diagnosing the regression model
plot(m)

# Checking residuals 2
## Set up cross-validation parameters
train_control <- trainControl(method = 'cv', number = 10)

## Train a model using cross-validation
m_cv <- train(NumberofMedals ~ POP_0.14 + PoliticalSystem + 
                NetMigration + Climate + log(GDP) + P2,
              data = final, method = 'lm', trControl = train_control)
print(m_cv)

# Prediction
## Final model
m <- lm(NumberofMedals ~ GDP + PopMalePct + PopGrowth + DeathRate + BirthRate + 
          DeathRate:BirthRate + GDP:PopGrowth + I(PopMalePct^2) +  I(BirthRate^2), 
        data = final)
summary(m)

## Read the CSV file
predict_winter <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Final Project\\Data File\\predict_winter.csv")

## Predict the number of medals
predict_medals <- predict(m, newdata = predict_winter)
print(predict_medals)