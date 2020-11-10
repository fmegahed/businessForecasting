setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load(tidyverse, # for dplyr, ggplot, etc
               magrittr, # for the two-way pipe
               fpp2, # this loads the forecast package
               astsa, # for the gnp data
               ggfortify) # for the fitted function to work

gnpData = gnp # saving the built-in data to an object gnpData
gnpData # printing the time-series

# Plotting the data
autoplot(gnpData) + theme_bw() + labs(y= 'GNP', x= 'year')

# From the plot, the data is obviously not stationary
ndiffs(gnpData) # from the output, two differences are required to bring data to stationarity


# We will difference the data twice
gnp_diff2 = diff(gnpData, differences = 2)
gnp_diff2 # quick printout to make sure that the differences are correct

# Visualizations of the differenced data to determine p and q
autoplot(gnp_diff2) # just confirming that it is stationary
acf(gnp_diff2, plot = FALSE) %>% autoplot() # hoping that the acf cuts off
pacf(gnp_diff2, plot = FALSE) %>% autoplot()

# Start by fitting an ARIMA(0,2,1)
# This assumes that you assumed that ACF plot cuts off after lag(1)
handModel1 = Arima(gnpData, order = c(0,2,1)) # fitting the Arima model
class(handModel1)
summary(handModel1) # summary of the model
checkresiduals(handModel1) # residuals are correlated based on plot and test

# Diagnostics based on the fact that the residuals of model were found to be correlated
acf(handModel1$residuals, plot = FALSE) %>% autoplot()
pacf(handModel1$residuals, plot = FALSE) %>% autoplot() + labs(y= 'Partial ACF')

# This is not the only conclusion that you could have made based on both ACF and PACF plots
handModel2 = Arima(gnpData, order = c(1,2,1))
summary(handModel2)
checkresiduals(handModel2)
# Residuals are correlated so I am violating a key assumption behind the suitability of 
# the ARIMA model


# Diagnostics
pacf(handModel2$residuals, plot = FALSE) %>% autoplot() + labs(y= 'PACF')


# Third Model
# Similar to above, ARMA with d=2 so we will just increment either p and/or q by 1
handModel3 = Arima(gnpData, order = c(2,2,1))
summary(handModel3)
checkresiduals(handModel3)

# Just picking an overly complex model (Please DO NOT FIT)
handModel4 = Arima(gnpData, order = c(6,2,6))
summary(handModel4)
checkresiduals(handModel4)

# This is also reasonable
handModel5 = Arima(gnpData, order = c(2,2,2))
summary(handModel5)
checkresiduals(handModel5)


# How does that compare to the auto.arima() for this specific data?
autoModel = auto.arima(gnpData)
class(autoModel)
summary(autoModel)
checkresiduals(autoModel)


# Plotting the Data
forecast(autoModel, h = 10) %>% autoplot() +
  geom_line(aes(y = fitted(autoModel)), col = 'blue', linetype = 'dashed', size = 1.25) +
  geom_line(aes(y = fitted(handModel1)), col = 'red', linetype = 'dashed', size = 1.25)


# What you would need to do to evaluate the predictive performance on a holdout dataset
gnp2 = gnpData[1:180] # roughly 80% percent of the data for training
ndiffs(gnp2)
