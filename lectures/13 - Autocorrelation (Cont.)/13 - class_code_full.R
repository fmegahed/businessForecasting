setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically setting the working directory
pacman::p_load(tidyverse, tidyquant, magrittr, lubridate, fpp2, readxl, ggpubr,
               stargazer)

# Slide 5
retail = read.csv("http://tiny.cc/megahed")
retail$DATE %<>% ymd() # converting date column to date

# Plotting the Actual Data
retail %>% ggplot(aes(x = DATE, y = RSCCAS)) + 
  geom_point() + geom_line() + theme_bw() -> p1

# Compute the first difference
retail$DIFF = retail$RSCCAS - lag(retail$RSCCAS)

# ACF Comparisons
acf(retail$RSCCAS, plot = F) %>% autoplot() -> p2


retail %>% ggplot(aes(x = DATE, y = DIFF)) + 
  geom_point() + geom_line() + theme_bw() -> p3
acf(retail$DIFF, plot = F, na.action = na.pass) %>% autoplot() -> p4

acfDiff = acf(retail$DIFF, plot = F, na.action = na.pass)
acfDiff

ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2) # put all ggplots in one graph



# The GOOGLE Example
google = tq_get("GOOG", from = "2019-01-01", to = "2020-12-31") %>% select(date, adjusted)
google %>% ggplot(aes(x= date, y = adjusted)) + geom_line() + theme_bw() -> g1

acfGoogle = acf(google$adjusted, plot = F)
acfGoogle$acf

acfGoogle %>% autoplot() + theme_bw() -> g2
acf(google$adjusted, plot = T) -> g2b

pacf(google$adjusted, plot = F) -> pacfGoogle
pacfGoogle$acf # showing that it is the same for lag1 to acf

pacfGoogle %>% autoplot() + theme_bw() -> g3

ggarrange(g1, g2, g3, ncol = 1) # ggarrange is from ggpubr (requires everything to be a ggplot object)


google$lag1price = lag(google$adjusted) 

# Scatter plot of adjusted vs price
google %>% ggplot(aes(x = lag1price, y = adjusted)) + geom_point() + theme_bw()

# Fitting a linear model for price as a function of lag1 price
model = lm(adjusted ~ lag1price, data = google)
summary(model)
stargazer(model, type = "html", out = "googleLM.html")
