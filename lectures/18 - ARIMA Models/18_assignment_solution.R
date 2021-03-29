setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load(tidyverse, magrittr, fpp2)

toothpaste = read.csv("Data/18 - toothpaste.csv")
toothpaste = ts(toothpaste)

toothpaste %>% autoplot()
ndiffs(toothpaste)


ndiffs(toothpaste)  #get the number of differences to make the ts stationary
diff_tooth = diff(toothpaste,differences=2) #Take appropriate differencing
plot(diff_tooth,type="o")

#Fit model based on the ACF and PACF plots and iterate...
acf(diff_tooth)
pacf(diff_tooth)


fore_tooth = Arima(toothpaste,order=c(0,2,2))

#summarize and forecast with the model
summary(fore_tooth)

checkresiduals(fore_tooth)