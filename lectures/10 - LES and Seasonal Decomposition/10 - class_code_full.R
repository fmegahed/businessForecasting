setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically setting the working directory

pacman::p_load(tidyverse, tidyquant, magrittr, lubridate, fpp2, scales, sweep, readxl)

#-------------------------------------------------------------------------------------------------------------------------------
# Slides 11-14
thermSales = read_excel('Data/Weekly_Therm_Sales.xlsx') # the Data indicates a folder where I stored my xlsx

thermSales %>% ggplot(aes(x = Time, y = WeeklyThermSales)) + geom_point() + geom_line() + theme_bw()

lesFit = holt(thermSales$WeeklyThermSales, alpha = 0.2, beta = 0.1) # commonly used values in practice

names(lesFit) # to see the names of the sublists in LesFit
accuracy(lesFit) # using the accuracy function from fpp2 to compute accuracy measures (based on the entire data)

autoplot(lesFit) + autolayer(fitted(lesFit), series = 'Fitted') # plotting the actual and the forecast (auotlayer adds the fitted values)

thermSales$Fitted = lesFit$fitted # equivalent to lesFit %>% [['fitted']]

#-------------------------------------------------------------------------------------------------------------------------------
# Slides 15-17

wSales = read_excel('Data/WFJ_sales.xlsx') %>% select(c(1, 2))

trainData = wSales[1:26, ]

fitHolt = holt(trainData$`WFJ Sales`) # training data to fit the model

accuracy(fitHolt)

fitHoltEntireData = holt(wSales$`WFJ Sales`, h = 10, level = 95, 
                         alpha = fitHolt$model$par['alpha'],
                         beta = fitHolt$model$par['beta']) # fitting the model to the entire data

# I will add the fitted values to wSales object
wSales$fit = fitHoltEntireData$fitted

validationData = wSales[-c(1:26), ]

validationResults = accuracy(object = validationData$fit, x = validationData$`WFJ Sales`)  
validationResults

#------------------------------------------------------------------------------
# Slide 18
#---------
crypto = tq_get(c('BTC-USD', 'ETH-USD', 'LTC-USD', 'ADA-USD', 'LINK-USD', 'ZIL-USD'), 
                from = '2020-10-15') 

crypto %<>% select(c(symbol, date, adjusted))

is_grouped_df(crypto) # answer was FALSE (so we will group it)

crypto %<>% group_by(symbol) %>% mutate(adjustedLog = log(adjusted)) # to make the data potentially more linear

nestedCrypto = crypto %>% select(-c(date, adjusted)) %>% nest(data = adjustedLog)
nestedCrypto


nestedCrypto %<>% mutate(data.ts = map(.x = data, .f = ts, start = c(2020, yday("2020-10-15")), freq = 366 ),
                         fitHolt = map(.x = data.ts, .f = holt, h = 30, alpha = 0.2, beta = 0.1),
                         accMetrics = map(.x = fitHolt, .f = accuracy),
                         MAPE = map(.x = accMetrics, .f = c(5)),
                         sweep = map(.x = fitHolt, .f = sw_sweep, fitted = T))

nestedCrypto

unnestedCrypto = nestedCrypto %>% unnest(sweep)
