setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically setting the working directory

pacman::p_load(tidyverse, magrittr, fpp2)

visc = read.csv("Data/17 - viscosity.csv") %>% 
  ts() # convert it to a time-series, I am using the defaults for ts (freq = 1, start at obs 1)

# Step 1: Plot the data
visc %>% autoplot()

# Step 2: Confirm that it is stationary
ndiffs(visc) # we can conclude that it is reasonable to assume that the ts is stationary since ndiffs -> 0

# Step 3: Plot the ACF and PACF
acf(visc, plot = F) %>% autoplot()
pacf(visc, plot = F) %>% autoplot()


## Potential Models
### MA(1) model, which is based on the assumption that the ACF cuts off at Lag 1 (lags3, 4 were sampling errors)
### AR(2) model eventually

# Step 4: For the sake of argument, lets fit the MA(1) -- we suspect that is likely not the correct model
### based on the sinsoudial pattern that we saw in the ACF plot
### based on potentially not wanting to ignore what we saw for Lags 3 and 4 in the ACF plot
### PACF plot may be cutting off at lag 2


ma1 = Arima(visc, order = c(0, 0, 1)) # p is the number of lags (also the order) in the AR model, 
#d is the number of differences to bring the ts to stationarity,
# q is the number of lags (order of) the MA model
class(ma1)
names(ma1)
summary(ma1) #
accuracy(ma1)

checkresiduals(ma1) # this is not a good model based on the Ljung-Box test and the plot
## Since the p-value < 0.05 --> the residuals from the MA1 model are correlated over time
acf(ma1$residuals, plot = F) %>% autoplot()
pacf(ma1$residuals, plot = F) %>% autoplot()

# If we were to ignore the fact that this is not a good model

# Step 5: Using the model for forecasting

ma1Forecast = forecast(ma1, h = 10, level = 95)
class(ma1Forecast)
print(ma1Forecast)
accuracy(ma1Forecast) # identical to what we have from lines 34-35

autoplot(ma1Forecast)

# You need to investigate is how well ARMA and AR(2) models work
