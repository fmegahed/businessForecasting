setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically setting my working directory

pacman::p_load(tidyverse, magrittr, 
               fpp2, # accuracy() function, Arima(), auto.arima(),
               ggfortify, # for plotting the fitted values in addition to the autoplot
               astsa, # package is for the required data
               plotly)

birthData = birth

# static plot
p = autoplot(birthData) + theme_bw()  + labs(x = 'Years', y = 'Births Per Month (Thousands)')

# interactive plot
ggplotly(p)

# Apply the auto.arima()
birthModel = auto.arima(birthData)

# Our typical Post Model
# [1] Plot the model to visually examine the fit
autoplot(birthModel) 
ggplotly()

 # [2] Print the model Summary --
 # This allows us to see the best model Arima(p,d, q), # accuracy measures on the training data
summary(birthModel)

  # [3] Checking the residuals
checkresiduals(birthModel)

  # [4] This is the forecast for the next 12 time periods
forecast(birthModel, h =12)

# [5] Visualizing the forecast (Optional but most people would like to do it)
forecast(birthModel, h =12) %>% autoplot() + theme_bw() +
  geom_line(y = fitted(birthModel), linetype = 'dashed', col = 'blue')

birthHand = Arima(birthData, order = c(0,1,2), seasonal = c(1,1,1))
autoplot(birthHand)

# As another example to illustrate what happens when you ignore seasonality
nonseasonalBirthModel = Arima(birthData, order = c(0,1,2))
checkresiduals(nonseasonalBirthModel)

# I am showing you how both the ACF and PACF plots can show that the data is seasonal
acf(nonseasonalBirthModel$residuals, plot = FALSE, lag.max = 60) %>% autoplot()


### The Netflix Data
netflix = read.csv('Data/22 - Netflix_growth_pct_2000.csv')
netflix = ts(data = netflix, start = c(2001,1), frequency = 4)
autoplot(netflix)

netflixModel = auto.arima(netflix)
summary(netflixModel)

netflixComplexModel = Arima(netflix, order = c(4,2,0))
summary(netflixComplexModel)
acf(netflixComplexModel$residuals, plot = FALSE) %>% autoplot()
pacf(netflixComplexModel$residuals, plot = FALSE) %>% autoplot()
checkresiduals(netflixComplexModel)
