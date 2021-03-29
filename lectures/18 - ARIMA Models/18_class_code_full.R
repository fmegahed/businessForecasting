setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load(tidyverse, magrittr, fpp2, lubridate, plotly, broom) # dplyr, ggplot2

# Step 0: Loaded the data into R
GNP = read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GNP&scale=left&cosd=1947-01-01&coed=2020-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2021-03-29&revision_date=2021-03-29&nd=1947-01-01")

GNP2 = read.csv("Data/GNP.csv") # approach 2 (download the data and read it from your PC)
setdiff(GNP, GNP2) # to prove that they are exactly the same datasets

GNP$DATE %<>% ymd() # converts the date column into a Date type variable
str(GNP) # checking the classes of my two columns

# Step 1: Plotting the Data
GNP %>% ggplot(aes(x = DATE, y = GNP)) + geom_line() + theme_bw()

# From the graph, we can conclude that the data is DEF NOT Stationary
ndiffs(GNP$GNP) # from the output, we need to difference the data twice to make it stationary

# showing two different ways to obtain the second diff (use whatever makes sense to you)
gnp_diff2A = diff(GNP$GNP, differences = 2) 
gnp_diff2B = diff(GNP$GNP) %>% diff()

GNP %<>% mutate(secondDiff = c(NA, NA, diff(GNP, differences = 2)) ) # added the NAs to make the num obs = to the rest of the df

GNP %>% ggplot(aes(x = DATE, y = secondDiff)) + geom_line() + theme_bw() # plotting the second diff

ndiffs(GNP$secondDiff) # confirming that no additional differencing is needed per the 
# ndiffs(), i.e., the KPSS test


### A Manual Investigation of the differenced TS
### You will need to do it, if you are not using an auto fitting process

# Converting the gnp Data to a time-series
gnpTS = ts(GNP$GNP, start = c(1947, 1), frequency = 4)

gnpDiff2 = diff(gnpTS, differences = 2)

# Now we have concluded that gnpDiff2 is stationary
# I can examine both the ACF and PACF to determine what type of ARMA model to fit


# For the purpose of picking a simple model, we will (for NOW) ignore the lags that were sig
## Rationale:
# [A] Trying a simple model first
# [B] We would probably expect that if we were significant at lag 1, we should see that for
## that the 0.25, 0.5, 0.75 values should also be significant
acf(gnpDiff2, plot = T)
pacf(gnpDiff2, plot = T)


# Check fitting an ARIMA model (0, 2, 0)
model1 = Arima(gnpTS, order = c(0, 2, 0))
summary(model1)
checkresiduals(model1) # model assumption of uncorrelated residuals is violated

acf(model1$residuals, plot = F) %>% autoplot # trying to get insights about next model
pacf(model1$residuals) # trying to get insights about next model


# Assuming that the residuals cut-off at Lag 1 (i.e., lags 2 and 5 are sampling error)
## It is reasonable to try a MA1

model2 = Arima(gnpTS, order = c(0, 2, 1))
summary(model2)
checkresiduals(model2)

# Based on the (A) accuracy metrics,  (B) the p-value for the Ljung-Box test > 0.05 (we will
# NOT reject the null hypothesis), and hence we can conclude that we do NOT have enough evidence 
# to say that the residuals are correlated, and (C) the panel plot overall looks reasonable

# For the purpose of forecasting
forecast(model2, h = 8) # up to and including 8 quarters ahead

forecast(model2, h = 8) %>% autoplot() +
  autolayer(fitted(model2), series = "fitted")



# ----------------------------------------------------------------------------
# The Non-Graded Class Activity
series10 = read.csv("Data/18 - inclass ARIMA Practice.csv") # reading the data
series10 = series10$series10 # pulling the data for the column of interest

series10 = ts(series10) # no frequency is provided so we use 1 (default)
autoplot(series10) # using autoplot since we do not have time information

ndiffs(series10) # confirming that it is stationary

acf(series10, plot = F) %>% autoplot() # probably we can conclude that the ts cuts at 2 (i.e., an MA2)

series10M1 = Arima(series10, c(0,0,2))
checkresiduals(series10M1)
summary(series10M1) # MAPE will be very high since values are close to 0

tidy(series10M1)

# Not from the ACF (but just because this is how Fadel generated the data)
# You will obviously not have that piece of information available
series10M2 = Arima(series10, c(2,0,2))
checkresiduals(series10M2)
summary(series10M2) # MAPE will be very high since values are close to 0

resultsSeries10 = cbind(accuracy(series10M1), accuracy(series10M2))
resultsSeries10 # accuracies are not that much different

broom::tidy(series10M2) # another way of showing the model coefficients


