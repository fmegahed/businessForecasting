setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load(tidyverse, magrittr, readxl, fpp2)

thermSales = read_excel("Data/Weekly_Therm_Sales.xlsx")

Time = thermSales$Time[1:26] # 
Sales = thermSales$WeeklyThermSales[1:26]

regModel = lm(thermSales$WeeklyThermSales[1:26] ~ thermSales$Time[1:26])
coefs = regModel$coefficients # Approach 1
summary(regModel) # Approach 2: Printing the summary for the model

l0 = 202.6246 
l0 = coefs[1]
l0
b0 = -0.3682 # coefs[2]


# Using R to Compute the LES

les = holt(thermSales$WeeklyThermSales, alpha = 0.2, beta = 0.1, h = 10) # les smoothing
summary(les) # summary of the model

thermSales$Holt =  les$fitted # extracting the smoothed values

autoplot(les) + theme_bw() +
  labs(y="Weekly Sales")

metrics = accuracy(les)


## Optimizing the LES Model (alpha, beta)

WJF = read_excel("Data/WFJ_sales.xlsx")

trainData = WJF[1:26, ] # training data used for estimating the optimal model

lesOpt = holt(trainData$`WFJ Sales`, h = 36) # how many time periods we want to forecast
summary(lesOpt)
alpha = lesOpt$model$par['alpha']
beta = lesOpt$model$par['beta']

metricsWJF = accuracy(lesOpt)

# Use them over the entire dataset to get the validation results
lesValidation = holt(WJF$`WFJ Sales`, 
                     alpha = alpha , beta = beta)
WJF$LESoptimal = lesValidation$fitted

validationData = WJF[27:62, ] # so I can calculate the accuracy metrics only for the validation data

metricsValidation = accuracy(validationData$`WFJ Sales`, validationData$LESoptimal)
metricsValidation



