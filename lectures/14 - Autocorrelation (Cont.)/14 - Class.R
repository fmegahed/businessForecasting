setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically set working dir to file location

pacman::p_load(tidyverse, fpp2, tidyquant, magrittr)


google = tq_get('GOOG', # tq_get function from tidyquant
                from = '2019-01-01', # starting date (if not trading day, you get the next possible trading day)
                to = '2020-09-30') # ending date (not included in our data pull)
googleStock = google$adjusted # just pulling the vector of adjusted closing prices
summary(googleStock) # printing out summary statistics


# From lines 14-16 (was to allow me to have an XTS object with a date instead of observation numbers)
googleStock %<>% data.frame() # makes it a data frame of one variable -- two way pipe comes from magrittr
row.names(googleStock) = google$date # will assign its row names to the dates
colnames(googleStock) = 'adjusted'


# Approach 1 for the ts plot 
as.xts(googleStock) %>% 
  autoplot() # comes from the forecast package (loaded from through the fpp2 package) -- reason: so I do not create a date col

# Approach 2 for the ts plot
google %>% ggplot(aes(x = date, y = adjusted)) + geom_line()


# ACF plot
googleStock$adjusted %>% # recommended approach in case of a data.frame to ensure that you know the input to ACF
  acf(lag.max = 15, plot = FALSE) %>% # acf (plot = FALSE -- we are only interested in using autoplot)
  autoplot() + theme_bw()


# PACF plot
googleStock$adjusted %>% # recommended approach in case of a data.frame to ensure that you know the input to ACF
  pacf(lag.max = 15, plot = FALSE) %>% # pacf (plot = FALSE -- we are only interested in using autoplot)
  autoplot() + theme_bw()


# Scatter Plot in Slide 18
google$lag1price = lag(google$adjusted) # creating a lag 1 variable for adjusted price
google %>% ggplot(aes(x = lag1price, y = adjusted)) + geom_point() + theme_bw() # scatter plot


# Scatter Plot in Slide 19
model = lm(data = google, adjusted ~ lag1price)
summary(model)
