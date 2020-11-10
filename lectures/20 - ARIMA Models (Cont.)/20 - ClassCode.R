setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load(tidyverse, magrittr, fpp2, lubridate, ggfortify)


# Step 0: Reading the Data
GNP = read.csv('https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=748&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GNP&scale=left&cosd=1947-01-01&coed=2020-04-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2020-10-28&revision_date=2020-10-28&nd=1947-01-01')

# Step 1: Plotting the data, fixing any columns
GNP$DATE %<>% ymd() # converts date from character to Date

# From Plotting the Data

# We plotted a line graph to examine whether the data is stationary
# If you suspect seasonlity-related issues, also create a seasonal plot
GNP %>% ggplot(aes(x = DATE, y = GNP)) + geom_line() +
  labs(x = 'Time', y = 'GNP in Billions of Dollars') + theme_bw()
# We can easily conclude that the data is not stationary

# We are converting this as a time-series; and giving it a starting date 
# (primarily if we print out the console it would make sense to you)
gnpTS = ts(GNP$GNP, frequency = 4, start = c(1947, 1)) 


# Our next step is to identify the number of differences needed to bring it to stationarity
ndiffs(gnpTS) # from the fpp2 package


# We are thinking about how we should fit an ARIMA type model 
# We have already concluded that d=2 is needed to bring the data to stationarity
# AR --> p: (are the number of lags were the partial correlation is significantly different from zero)
# MA --> q
gnpDiff2 = diff(gnpTS, differences = 2) # second difference -- last class I have proved that this is
# equal to diff(gnpTS) %>% diff()

# Given that none of the lags seemed significant -- our first model is actually an ARIMA c(0,2,0)
acf(gnpDiff2, plot = FALSE, lag.max = 40) %>% autoplot() + theme_bw()
pacf(gnpDiff2, plot = FALSE, lag.max = 40) %>% autoplot() + theme_bw()


# Fitting the model
model1 = Arima(gnpTS, order = c(0, 2, 0))
checkresiduals(model1)
# Diagnostics if the check residuals indicated that the residuals are correlated
acf(model1$residuals, plot = FALSE) %>% autoplot() + theme_bw()
pacf(model1$residuals, plot = FALSE) %>% autoplot() + theme_bw()
#########################################################################
accuracy(model1) # returns the accuracy of the actual vs fitted values
summary(model1) # summarises the model

# Lets see how the model fits on past data
autoplot(model1) # plots the time-series in black and the fitted values in red

# For forecasting
forecast(model1, h = 10) # create forecasts for the next h = 10 time periods

# Plotting the time-series, the fitted values, and the forecasted values

forecast(model1, h = 10) %>% autoplot() +
  geom_line(aes(y = fitted(model1)), col = 'red', linetype = 4, size = 1.5) + theme_bw()
