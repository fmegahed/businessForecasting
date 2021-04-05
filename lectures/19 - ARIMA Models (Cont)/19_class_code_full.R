setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically setting the working directory

pacman::p_load(tidyverse,
               fpp2,
               astsa,
               scales)

?auto.arima

gnpData = gnp
gnp

# Step 1: Plot the data to see if it is stationary
autoplot(gnpData) + scale_x_continuous(breaks = pretty_breaks(20)) + theme_bw()

# From the generated plot, the data is obviously NOT stationary as it has an increasing trend
ndiffs(gnpData) # from the output (console) --> 2

# We will difference the data twice based on the output above
gnp_diff2 = diff(gnpData, differences = 2)

acf(gnp_diff2) # FOR NOW, we will assume that it cuts off at lag 1
# MA(1) model is probably not going to work due to the cyclical nature of the acf at lags 1+


# What would be my initial values for p, d, and q? (based on the assumption that the ACF cuts off at lag1)
handModel1 = Arima(gnpData,
                   order = c(0, # because for now we are assuming that the ACF cuts off (not an AR process)
                             2, # because the output of ndiffs was equal to 2
                             1)) # the order of the MA process (based on the potentially naive interpretation of the ACF plot)
summary(handModel1) # MAPE 0.786
checkresiduals(handModel1) # from the plot and the Ljung-Box test, we conclude that the res are correlated


# Diagnositics based on this
acf(handModel1$residuals)
pacf(handModel1$residuals)


# A reasonable second model -- but that is not necessarily the only second model to run
handModel2 = Arima(gnpData, order = c(1,2,1))
summary(handModel2) # MAPE 0.721
checkresiduals(handModel2)


handModel3 = Arima(gnpData, order = c(2,2,1))
summary(handModel3)
checkresiduals(handModel3)


# Alternatively, we can also use the auto.arima()

autoModel = auto.arima(gnpData)
summary(autoModel)
broom::tidy(autoModel)
checkresiduals(autoModel)


# Ridiculous model (statistically valid model but overly complex)
handModel4 = Arima(gnpData, order = c(6,2,6))
summary(handModel4)
checkresiduals(handModel4) -> test


# For the purpose of checking if we went a different route, what decision we should have made?
handModel3b = Arima(gnpData, order = c(1,2,2))
summary(handModel3b) # 0.726 MAPE # BIC 2261.04
checkresiduals(handModel3b)

rbind(accuracy(handModel3), accuracy(handModel3b))
BIC(handModel3)
summary(handModel3)

# y_(t=500) = 20 (from Model 1)
# e_(t=500) --> -2
Forecast = 20 -2 = 18


# ---------------------------------------------------------------------------------------------
# The Crypto Example

pacman::p_load(tidyquant, magrittr, lubridate, broom)

crypto = tq_get(c('BTC-USD', 'ETH-USD', 'LTC-USD', 'ADA-USD', 'LINK-USD', 
                  'ZIL-USD'), 
                from = '2020-10-15', to = '2021-03-03') 

crypto %<>% select(c(symbol, date, adjusted))
is_grouped_df(crypto) # answer was FALSE (so we will group it)

crypto %<>% group_by(symbol) %>% mutate(adjustedLog = log(adjusted))

nestedCrypto = crypto %>% select(-c(date, adjusted)) %>% 
  nest(data = adjustedLog)

nestedCrypto %<>% mutate(dataTS = map(.x = data, .f = ts, start = c(2020, yday('2020-10-15')), freq = 366 ),
                         fitArima = map(.x = dataTS, .f = auto.arima ),
                         predMetrics = map(.x = fitArima, .f = accuracy),
                         MAPE = map_dbl(.x = predMetrics, .f = c(5)),
                         residuals = map(.x = fitArima, .f = checkresiduals), # needed to apply the check.residuals fun first
                         pValues = map_dbl(.x = residuals, .f = "p.value") ) # then extract using only the "name"

# Note all models are actually good

nestedCrypto %>% arrange(MAPE)



