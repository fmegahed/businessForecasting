setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically setting my working directory

pacman::p_load(tidyverse, 
               magrittr, 
               fpp2, # accuracy() function, Arima(), auto.arima(), autoplot
               astsa, # package is for the required data
               plotly)

netflix = read.csv("Data/20 - Netflix_growth_pct_2000.csv")
netflix = ts(netflix, start=c(2001,1), frequency=4)


ndiffs(netflix)

netflix_secDiff = diff(netflix, differences = 2)
acf(netflix_secDiff)

autoplot(netflix)

netflixModel = auto.arima(netflix)

summary(netflixModel)

checkresiduals(netflixModel)


# Fitting a nonseasonal ARIMA (0, 2, 2)
potentialModel = Arima(netflix, order = c(0, 2, 2))
summary(potentialModel)
summary(netflixModel)
checkresiduals(potentialModel)
