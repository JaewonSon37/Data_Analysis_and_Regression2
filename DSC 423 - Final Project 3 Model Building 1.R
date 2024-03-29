# Data Loading and Preprocessing
## Read the CSV file
summer_olympic <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Final Project\\Data File\\final_summer.csv")

## Remove unnecessary columns
summer_olympic <- subset(summer_olympic, select = -c(Country, Olympic, Year))

## Convert categorical variables into dummy variables
dummy_summer_olympic <- as.data.frame(model.matrix(NumberofMedals ~ . -1, data = summer_olympic))
dummy_summer_olympic <- subset(dummy_summer_olympic, select = -c(ClimateDwa))
dummy_summer_olympic$NumberofMedals <- summer_olympic$NumberofMedals

# Model Building 1
## Load the library
library(car)

## Initial model
model1 <- lm(NumberofMedals ~ ., data = dummy_summer_olympic)
summary(model1)

## Check multicollinearity
vif(model1)

## Remove insignificant variables based on the p-value and domain knowledge
model2 <- lm(NumberofMedals ~ GDP + Population + ClimateBWh + ClimateCfa + ClimateCfb + 
               ClimateCsa + ClimateDfa + ClimateDfb + ClimateDfc + PoliticalSystemcommunism + 
               PoliticalSystemdemocracy + PoliticalSystemmilitarydictatorship + HostCountryyes + 
               Participationyes + LifeExp + DeathRate + BirthRate + MortalityRateMale + 
               MortalityRateFemale + YoungFertilityRate + NetMigration, 
             data = dummy_summer_olympic)
summary(model2)

## Model selection
stepwise_model2 <- step(model2, direction = "both", trace = FALSE)
summary(stepwise_model2)

## Check multicollinearity
vif(stepwise_model2)

## Remove the variable with the highest VIF value
model3 <- lm(NumberofMedals ~ GDP + Population + ClimateCsa + ClimateDfc + PoliticalSystemcommunism + 
               HostCountryyes + Participationyes + LifeExp + BirthRate + 
               MortalityRateMale + YoungFertilityRate + NetMigration + PoliticalSystemdemocracy, 
             data = dummy_summer_olympic)
summary(model3)

## Check multicollinearity
vif(model3)

## Remove the variable with the highest VIF value
model4 <- lm(NumberofMedals ~ GDP + Population + ClimateCsa + ClimateDfc + PoliticalSystemcommunism + 
               HostCountryyes + Participationyes + BirthRate + MortalityRateMale + 
               YoungFertilityRate + NetMigration + PoliticalSystemdemocracy, 
             data = dummy_summer_olympic)
summary(model4)

## Check multicollinearity
vif(model4)

## Remove the variable with the highest VIF value
model5 <- lm(NumberofMedals ~ GDP + Population + ClimateCsa + ClimateDfc + 
               PoliticalSystemcommunism + HostCountryyes + Participationyes + BirthRate + 
               MortalityRateMale + YoungFertilityRate + NetMigration, 
             data = dummy_summer_olympic)
summary(model5)

## Check multicollinearity
vif(model5)

## Remove the most insignificant variable based on the p-value
model6 <- lm(NumberofMedals ~ GDP + Population + ClimateDfc + PoliticalSystemcommunism + HostCountryyes + 
               Participationyes +BirthRate + MortalityRateMale + YoungFertilityRate + NetMigration, 
             data = dummy_summer_olympic)
summary(model6)

## Check multicollinearity
vif(model6)

## Remove the most insignificant variable based on the p-value
model7 <- lm(NumberofMedals ~ GDP + Population + ClimateDfc + PoliticalSystemcommunism + HostCountryyes + 
               Participationyes + BirthRate + YoungFertilityRate + NetMigration, 
             data = dummy_summer_olympic)
summary(model7)

## Check multicollinearity
vif(model7)

## Create interaction term variable
model8 <- lm(NumberofMedals ~ (GDP + Population + ClimateDfc + PoliticalSystemcommunism + HostCountryyes + 
                                 Participationyes + BirthRate + YoungFertilityRate + NetMigration)^2, 
             data = dummy_summer_olympic)
summary(model8)

## Model selection
stepwise_model8 <- step(model8, direction = "both", trace = FALSE)
summary(stepwise_model8)

## Create second-order term variable
model9 <- lm(NumberofMedals ~ GDP + Population + ClimateDfc + PoliticalSystemcommunism + HostCountryyes + 
               Participationyes + BirthRate + YoungFertilityRate + NetMigration +
               I(GDP^2) + I(Population^2) + I(ClimateDfc^2) + I(PoliticalSystemcommunism^2) + I(HostCountryyes^2) + 
               I(Participationyes^2) + I(BirthRate^2) + I(YoungFertilityRate^2) + I(NetMigration^2), 
             data = dummy_summer_olympic)
summary(model9)

## Generate a new model with significant interaction and second-order terms
model10 <- lm(NumberofMedals ~ GDP + Population + ClimateDfc + PoliticalSystemcommunism +
                HostCountryyes + Participationyes + BirthRate + YoungFertilityRate + NetMigration + 
                GDP:Population + GDP:PoliticalSystemcommunism + GDP:Participationyes + GDP:BirthRate + 
                GDP:YoungFertilityRate + GDP:NetMigration + Population:ClimateDfc + 
                Population:PoliticalSystemcommunism + Population:HostCountryyes + Population:Participationyes + 
                Population:BirthRate + Population:YoungFertilityRate + Population:NetMigration + 
                ClimateDfc:HostCountryyes + ClimateDfc:Participationyes + ClimateDfc:NetMigration + 
                PoliticalSystemcommunism:HostCountryyes + PoliticalSystemcommunism:Participationyes +
                PoliticalSystemcommunism:BirthRate + HostCountryyes:YoungFertilityRate + HostCountryyes:NetMigration + 
                Participationyes:BirthRate + Participationyes:YoungFertilityRate + 
                BirthRate:YoungFertilityRate + YoungFertilityRate:NetMigration + I(Population^2),
              data = dummy_summer_olympic)
summary(model10)

## Model selection
stepwise_model10 <- step(model10, direction = "both", trace = FALSE)
summary(stepwise_model10)

## Remove insignificant variables based on the p-value
model11 <- lm(NumberofMedals ~ GDP + Population + ClimateDfc + PoliticalSystemcommunism + HostCountryyes + 
                Participationyes + BirthRate + YoungFertilityRate + NetMigration + GDP:PoliticalSystemcommunism + 
                GDP:Participationyes + GDP:BirthRate + Population:BirthRate + Population:YoungFertilityRate + 
                Population:NetMigration + ClimateDfc:HostCountryyes + ClimateDfc:Participationyes + 
                ClimateDfc:NetMigration + PoliticalSystemcommunism:HostCountryyes + 
                PoliticalSystemcommunism:Participationyes + PoliticalSystemcommunism:BirthRate + 
                HostCountryyes:YoungFertilityRate + HostCountryyes:NetMigration + Participationyes:BirthRate + 
                Participationyes:YoungFertilityRate + YoungFertilityRate:NetMigration + I(Population^2),
              data = dummy_summer_olympic)
summary(model11)

## Remove insignificant variables based on the p-value
model12 <- lm(NumberofMedals ~ GDP + Population + ClimateDfc + PoliticalSystemcommunism + HostCountryyes + 
                Participationyes + BirthRate + YoungFertilityRate + NetMigration + GDP:PoliticalSystemcommunism + 
                GDP:Participationyes + GDP:BirthRate + Population:BirthRate + Population:YoungFertilityRate + 
                ClimateDfc:HostCountryyes + ClimateDfc:Participationyes + PoliticalSystemcommunism:HostCountryyes + 
                PoliticalSystemcommunism:Participationyes + PoliticalSystemcommunism:BirthRate + 
                HostCountryyes:YoungFertilityRate + HostCountryyes:NetMigration + Participationyes:BirthRate + 
                Participationyes:YoungFertilityRate + YoungFertilityRate:NetMigration + I(Population^2),
              data = dummy_summer_olympic)
summary(model12)

# Cross-validation 1
## Load the library
library(caret)

## Set up cross-validation parameters
train_control12 <- trainControl(method = "cv", number = 10)

## Train a model using cross-validation
cv_model12 <- train(NumberofMedals ~ GDP + Population + ClimateDfc + PoliticalSystemcommunism + HostCountryyes +
                      Participationyes + BirthRate + YoungFertilityRate + NetMigration + GDP:PoliticalSystemcommunism + 
                      GDP:Participationyes + GDP:BirthRate + Population:BirthRate + Population:YoungFertilityRate + 
                      ClimateDfc:HostCountryyes + ClimateDfc:Participationyes + PoliticalSystemcommunism:HostCountryyes + 
                      PoliticalSystemcommunism:Participationyes + PoliticalSystemcommunism:BirthRate + 
                      HostCountryyes:YoungFertilityRate + HostCountryyes:NetMigration + Participationyes:BirthRate + 
                      Participationyes:YoungFertilityRate + YoungFertilityRate:NetMigration + I(Population^2), 
                    data = dummy_summer_olympic, method = "lm", trControl = train_control12)
print(cv_model12)

# Checking residuals 1
## Calculate residuals and predicted values
residuals12 <- residuals(model12)
predicted_values12 <- predict(model12)

## Q-Q plot of residuals
qqnorm(residuals12)
qqline(residuals12)

## Scatter plot of residuals against predicted values
plot(predicted_values12, residuals12, xlab = "Predicted values", ylab = "Residuals")

## Box plot of residuals
boxplot(residuals12)

## Histogram of residuals
hist(residuals12, main = "Histogram of Residuals")

# Model Building 2
## Remove insignificant variable based on domain knowledge
model13 <- lm(NumberofMedals ~ GDP + Population + ClimateDfc + PoliticalSystemcommunism + 
                HostCountryyes + Participationyes + BirthRate + YoungFertilityRate + NetMigration + 
                GDP:PoliticalSystemcommunism + GDP:Participationyes + GDP:BirthRate + 
                Population:BirthRate + Population:YoungFertilityRate + ClimateDfc:HostCountryyes + 
                ClimateDfc:Participationyes + PoliticalSystemcommunism:HostCountryyes + 
                PoliticalSystemcommunism:Participationyes + PoliticalSystemcommunism:BirthRate + 
                HostCountryyes:YoungFertilityRate + HostCountryyes:NetMigration + 
                Participationyes:BirthRate + Participationyes:YoungFertilityRate + I(Population^2), 
              data = dummy_summer_olympic)
summary(model13)

## Remove insignificant variable based on the p-value
model14 <- lm(NumberofMedals ~ GDP + Population + ClimateDfc + PoliticalSystemcommunism + 
                HostCountryyes + Participationyes + BirthRate + YoungFertilityRate + NetMigration + 
                GDP:PoliticalSystemcommunism + GDP:Participationyes + Population:BirthRate + 
                Population:YoungFertilityRate + ClimateDfc:HostCountryyes + ClimateDfc:Participationyes +
                PoliticalSystemcommunism:HostCountryyes + PoliticalSystemcommunism:Participationyes + 
                PoliticalSystemcommunism:BirthRate + HostCountryyes:YoungFertilityRate + HostCountryyes:NetMigration + 
                Participationyes:BirthRate + Participationyes:YoungFertilityRate + I(Population^2), 
              data = dummy_summer_olympic)
summary(model14)

## Remove insignificant variable based on the p-value
model15 <- lm(NumberofMedals ~ GDP + Population + ClimateDfc + PoliticalSystemcommunism + 
                HostCountryyes + Participationyes + BirthRate + YoungFertilityRate + NetMigration + 
                GDP:PoliticalSystemcommunism + Population:BirthRate + Population:YoungFertilityRate + 
                ClimateDfc:HostCountryyes + ClimateDfc:Participationyes + PoliticalSystemcommunism:HostCountryyes + 
                PoliticalSystemcommunism:Participationyes + PoliticalSystemcommunism:BirthRate + 
                HostCountryyes:YoungFertilityRate + HostCountryyes:NetMigration + Participationyes:BirthRate + 
                Participationyes:YoungFertilityRate + I(Population^2), 
              data = dummy_summer_olympic)
summary(model15)

## Remove insignificant variable based on the p-value
model16 <- lm(NumberofMedals ~ GDP + Population + ClimateDfc + PoliticalSystemcommunism + 
                HostCountryyes + Participationyes + BirthRate + YoungFertilityRate + NetMigration + 
                GDP:PoliticalSystemcommunism + Population:BirthRate + ClimateDfc:HostCountryyes + 
                ClimateDfc:Participationyes + PoliticalSystemcommunism:Participationyes + 
                PoliticalSystemcommunism:BirthRate + HostCountryyes:YoungFertilityRate + 
                HostCountryyes:NetMigration + Participationyes:BirthRate + 
                Participationyes:YoungFertilityRate + I(Population^2), 
              data = dummy_summer_olympic)
summary(model16)

## Remove insignificant variable based on the p-value
model17 <- lm(NumberofMedals ~ GDP + Population + ClimateDfc + PoliticalSystemcommunism + 
                HostCountryyes + Participationyes + BirthRate + YoungFertilityRate + NetMigration + 
                GDP:PoliticalSystemcommunism + Population:BirthRate + ClimateDfc:Participationyes +
                PoliticalSystemcommunism:Participationyes + PoliticalSystemcommunism:BirthRate + 
                HostCountryyes:YoungFertilityRate + HostCountryyes:NetMigration + 
                Participationyes:BirthRate + Participationyes:YoungFertilityRate + I(Population^2), 
              data = dummy_summer_olympic)
summary(model17)

# Cross-validation 2
## Set up cross-validation parameters
train_control17 <- trainControl(method = "cv", number = 10)

## Train a model using cross-validation
cv_model17 <- train(NumberofMedals ~ GDP + Population + ClimateDfc + PoliticalSystemcommunism + 
                      HostCountryyes + Participationyes + BirthRate + YoungFertilityRate + NetMigration + 
                      GDP:PoliticalSystemcommunism + Population:BirthRate + ClimateDfc:Participationyes +
                      PoliticalSystemcommunism:Participationyes + PoliticalSystemcommunism:BirthRate + 
                      HostCountryyes:YoungFertilityRate + HostCountryyes:NetMigration + 
                      Participationyes:BirthRate + Participationyes:YoungFertilityRate + I(Population^2), 
                    data = dummy_summer_olympic, method = "lm", trControl = train_control17)
print(cv_model17)

# Checking residuals 2
## Calculate residuals and predicted values
residuals17 <- residuals(model17)
predicted_values17 <- predict(model17)

## Q-Q plot of residuals
qqnorm(residuals17)
qqline(residuals17)

## Scatter plot of residuals against predicted values
plot(predicted_values17, residuals17, xlab = "Predicted values", ylab = "Residuals")

## Box plot of residuals
boxplot(residuals17)

## Histogram of residuals
hist(residuals17, main = "Histogram of Residuals")

# Model Building 3
## Log transformation
model18 <- lm(NumberofMedals ~ log(GDP) + log(Population) + ClimateDfc + PoliticalSystemcommunism + 
                HostCountryyes + Participationyes + BirthRate + YoungFertilityRate + NetMigration + 
                GDP:PoliticalSystemcommunism + Population:BirthRate + ClimateDfc:Participationyes +
                PoliticalSystemcommunism:Participationyes + PoliticalSystemcommunism:BirthRate + 
                HostCountryyes:YoungFertilityRate + HostCountryyes:NetMigration + 
                Participationyes:BirthRate + Participationyes:YoungFertilityRate + I(Population^2), 
              data = dummy_summer_olympic)
summary(model18)

## Remove insignificant variables based on the p-value
model19 <- lm(NumberofMedals ~ log(GDP) + log(Population) + ClimateDfc + PoliticalSystemcommunism + 
                HostCountryyes + Participationyes + BirthRate + YoungFertilityRate + NetMigration + 
                GDP:PoliticalSystemcommunism + PoliticalSystemcommunism:BirthRate + 
                HostCountryyes:YoungFertilityRate + HostCountryyes:NetMigration + 
                Participationyes:BirthRate + Participationyes:YoungFertilityRate + I(Population^2), 
              data = dummy_summer_olympic)
summary(model19)

# Cross-validation 3
## Set up cross-validation parameters
train_control19 <- trainControl(method = "cv", number = 10)

## Train a model using cross-validation
cv_model19 <- train(NumberofMedals ~ log(GDP) + log(Population) + ClimateDfc + PoliticalSystemcommunism + 
                      HostCountryyes + Participationyes + BirthRate + YoungFertilityRate + NetMigration + 
                      GDP:PoliticalSystemcommunism + PoliticalSystemcommunism:BirthRate + 
                      HostCountryyes:YoungFertilityRate + HostCountryyes:NetMigration + 
                      Participationyes:BirthRate + Participationyes:YoungFertilityRate + I(Population^2), 
                    data = dummy_summer_olympic, method = "lm", trControl = train_control19)
print(cv_model19)

# Checking residuals 3
## Calculate residuals and predicted values
residuals19 <- residuals(model19)
predicted_values19 <- predict(model19)

## Q-Q plot of residuals
qqnorm(residuals19)
qqline(residuals19)

## Scatter plot of residuals against predicted values
plot(predicted_values19, residuals19, xlab = "Predicted values", ylab = "Residuals")

## Box plot of residuals
boxplot(residuals19)

## Histogram of residuals
hist(residuals19, main = "Histogram of Residuals")

# Prediction
## Read the CSV file
predict_summer <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Final Project\\Data File\\predict_summer.csv")

## Predict the number of medals
predict_medals <- predict(model19, newdata = predict_summer)
print(predict_medals)