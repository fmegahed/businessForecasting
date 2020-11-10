setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically setting the working directory

pacman::p_load(tidyverse) # loading  the required packages


# Non-Graded Class Activity
classActivity = read.csv('Data/18 - inclass ARMA Practice.csv') # reading the data

# Series 1: Investigation
series1 = classActivity$series1 # extracting the information for Series1

series1 %>% acf(lag.max = 25, plot = FALSE) %>% # computing the autocorrelation function for 25 lags and rejecting the default plot
  autoplot() + # use of autoplot from ggplot (loaded from tidyverse)
  theme_bw() + # using my preferred black and white theme
  labs(title = 'ACF Plot of Series1') # giving the plot a title

series1 %>% pacf(lag.max = 25, plot = FALSE) %>% autoplot() + theme_bw() +
  labs(title = 'PACF Plot of Series1') # repeating the above for the PACF of the Series

# Based on the ACF: The series has only 3 significant lags (everything after seems to be not significant --
# this ignored minor increases above the upper limit)

# Based on the PACF: It seems that we have a sinusoidal pattern

# Check Slide 4 for more details on why we concluded an MA(3) for Series 1



# Series 5:
series5 = classActivity$series5

series5 %>% acf(lag.max = 25, plot = FALSE) %>% # computing the autocorrelation function for 25 lags and rejecting the default plot
  autoplot() + # use of autoplot from ggplot (loaded from tidyverse)
  theme_bw() + # using my preferred black and white theme
  labs(title = 'ACF Plot of Series5') # giving the plot a title

series5 %>% pacf(lag.max = 25, plot = FALSE) %>% # computing the partial autocorrelation function for 25 lags and rejecting the default plot
  autoplot() + # use of autoplot from ggplot (loaded from tidyverse)
  theme_bw() + # using my preferred black and white theme
  labs(title = 'PACF Plot of Series5') # giving the plot a title


# Series 5:
series6 = classActivity$series6

series6 %>% acf(lag.max = 25, plot = FALSE) %>% # computing the autocorrelation function for 25 lags and rejecting the default plot
  autoplot() + # use of autoplot from ggplot (loaded from tidyverse)
  theme_bw() + # using my preferred black and white theme
  labs(title = 'ACF Plot of Series6') # giving the plot a title

series6 %>% pacf(lag.max = 25, plot = FALSE) %>% # computing the partial autocorrelation function for 25 lags and rejecting the default plot
  autoplot() + # use of autoplot from ggplot (loaded from tidyverse)
  theme_bw() + # using my preferred black and white theme
  labs(title = 'PACF Plot of Series6') # giving the plot a title




#---------------------------------------------------------------------------------------------------------------------------------
## The Viscosity Data

pacman::p_load(tidyverse, fpp2, ggfortify)

visc = read.csv('Data/18 - viscosity.csv') %>% # reading the data
  ts() # converting it to a time-series object (to make plotting it easier)

# Step 1: Plotting the Data
visc %>% autoplot() + labs(x = 'Observation Number', y = 'Viscosity') + theme_bw()

# Step 2: Confirming that the data is indeed stationary
ndiffs(visc)
# From the output = 0, then this ts is stationary

# Step 3: Plot the acf
acf(visc, lag.max = 20, plot = FALSE) %>% autoplot() + theme_bw()
pacf(visc, lag.max = 20, plot = FALSE) %>% autoplot() + theme_bw()


# Step 4: Fit a MA(1) Model for the sake of argument
ma1 = Arima(y = visc, # has to be a ts() object
            order = c(0, 0, 1)) # (p, d, q)  AR order, the degree of differencing to bring it to stationarity, and the MA order.
class(ma1) # the object type is ARIMA
summary(ma1) # similar to what we have done with models before (e.g., with the lm models + ts models)
accuracy(ma1)

autoplot(ma1) + theme_bw() + labs(x= 'Obs. Number', y = 'Viscosity') # plotting the fitted (red) vs actual (black)

checkresiduals(ma1) # from the forecast package (loaded with fpp2)

# Step5: Using the model for forecasting
ma1Forecast = forecast(ma1, h = 10, level = c(90, 95)) # creating the forecast for 10 periods ahead
class(ma1Forecast) # the class of the object
print(ma1Forecast) #  print to print the forecasted values
accuracy(ma1Forecast) # note accuracy on the forecast object is identical to that of the original ma1 object

autoplot(ma1Forecast) + # plotting the forecast
  geom_line(aes(y = fitted(ma1Forecast)), col = 'blue') + # with fitted and ggfortify to show the fitted data (retrospective as well)
  theme_bw()
