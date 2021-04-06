setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically setting my working directory

pacman::p_load(tidyverse, 
               magrittr, 
               fpp2, # accuracy() function, Arima(), auto.arima(),
               astsa, # package is for the required data
               plotly)

birthData = birth # saving the data to an object titled birthData

autoplot(birthData) + theme_bw() -> birthPlot

# Some ways to know/infer that we have seasonality in the data
ggplotly(birthPlot) # interactive from the cursor
decomposed = decompose(birthData) # trying to compute seasonal factors
autoplot(decomposed) # visually from the plot

# Auto.arima
autoBirthModel = auto.arima(birthData)
summary(autoBirthModel)
checkresiduals(autoBirthModel)


#How do we fit a seasonal model by hand?
## Precursor: knowing when to fit a seasonal model

# For now, we will intentionally ignore the past evidence for the need for a seasonal model

ndiffs(birthData) # should be 1

birthDataDiff = diff(birthData, differences = 1)
birthDataDiff

acf(birthDataDiff, lag.max = 60)
pacf(birthDataDiff, lag.max = 60)

# ignore our discussion from the ACF and PACF
# I will fit a nonseasonal model

nonseasonal = Arima(birthData, order = c(0, 1, 2))
summary(nonseasonal)
checkresiduals(nonseasonal)

acf(nonseasonal$residuals, lag.max = 60)
pacf(nonseasonal$residuals, lag.max = 60)

manualSeasonalModel = Arima(birthData, order = c(0, 1, 2), seasonal = c(2,1,0))
summary(manualSeasonalModel)
checkresiduals(manualSeasonalModel)

manualSeasonalModel2 = Arima(birthData, order = c(0, 1, 2), seasonal = c(1,1,0))
summary(manualSeasonalModel2)
checkresiduals(manualSeasonalModel2)

manualSeasonalModel3 = Arima(birthData, order = c(0, 1, 2), seasonal = c(1,1,2))
summary(manualSeasonalModel3)
checkresiduals(manualSeasonalModel3)

# As a conclusion, we fitted three reasonable models for the dataset
## autoBirthModel -- (0,1,2)(1,1,1) ---> AICc = 2420.03 : Ranked 1
# manualSeasonalModel -- (0,1,2)(2,1,0) --> AICc = 2458.43 : Ranked 3
# manualSeasonalModel3 -- (0,1,2)(1,1,2) --> AICc = 2420.67 Ranked 2