setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically setting the working directory
pacman::p_load(tidyverse, tidyquant, magrittr, lubridate, fpp2, scales, sweep, readxl)


# Slide 05
#---------
crypto = tq_get(c('BTC-USD', 'ETH-USD', 'LTC-USD', 'ADA-USD', 'LINK-USD', 'ZIL-USD'), 
                from = '2020-10-15', to = '2021-03-03') 

crypto %<>% select(c(symbol, date, adjusted))

is_grouped_df(crypto) # answer was FALSE (so we will group it)

crypto %<>% group_by(symbol) %>% mutate(adjustedLog = log(adjusted)) # to make the data potentially more linear

nestedCrypto = crypto %>% select(-c(date, adjusted)) %>% nest(data = adjustedLog)
nestedCrypto


nestedCrypto %<>% mutate(data.ts = map(.x = data, .f = ts, start = c(2020, yday("2020-10-15")), freq = 366 ),
                         fitHolt = map(.x = data.ts, .f = holt, h = 7, alpha = 0.2, beta = 0.1),
                         accMetrics = map(.x = fitHolt, .f = accuracy),
                         MAPE = map_dbl(.x = accMetrics, .f = c(5)),
                         sweep = map(.x = fitHolt, .f = sw_sweep, fitted = T))

nestedCrypto %>% arrange(MAPE)

unnestedCrypto = nestedCrypto %>% unnest(sweep)

colnames(unnestedCrypto)

unnestedCrypto %>% ggplot(aes(x = date_decimal(index), y = adjustedLog, color = key)) +
  facet_wrap(~ symbol, scales = "free_y", ncol = 3) + geom_line() +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), alpha = 0.1) + theme_bw() +
  theme(legend.position = "top") + labs(x = "Month", y = 'Log(Adjusted Price)')



# Slide 18
bike = read_excel('Data/BikeSalesR.xlsx') # reading the file based on where I stored it locally
bike # printing it (given that it was only 16 observations and 2 variables)
bikeTS = ts(bike$`Bike Sales`, frequency = 4) # setting freq is important for seasonal data
bikeTS

decomposed = decompose(bikeTS, type = "additive")
decomposed

decomposed$figure %>% sum() # showing you that the sum of the seasonal factors in an additive model = 0

# Y = T + S + E 
# decomposed$x = decomposed$trend + decomposed$seasonal + decomposed$random

detrend = decomposed$x - decomposed$trend
detrend
# For Q1: mean of the detrended series for that time period - overall mean of the detrended series
## The purpose the subtraction is to standardize our seasonal factors.
mean(c(-15, -14.625, -14.375)) - mean(detrend, na.rm = T)
autoplot(decomposed) + theme_bw() 
plotly::ggplotly()

# Slide 27
hwAdd = hw(bikeTS, seasonal = "additive", level = 95,
           alpha = 0.2, beta = 0.1, gamma = 0.1)
summary(hwAdd) # printing information related to the call, the model and the results
autoplot(hwAdd) # plotting the actual and forecasted values
