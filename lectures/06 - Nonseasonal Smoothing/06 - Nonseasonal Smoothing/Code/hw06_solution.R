pacman::p_load(tidyquant, tidyverse, magrittr, xtable, fpp2, lubridate, timetk, scales)

# Q1:
# ---

cardano = tq_get('ADA-USD', from = Sys.Date()-30, to = Sys.Date() - 1) %>% 
  select(date, adjusted)

cardano %<>%  mutate(naiveFC = lag(adjusted),
                     lower = NA,
                     upper = NA)

RMSE = accuracy(object = cardano$naiveFC, x = cardano$adjusted) %>% .[1,'RMSE']

cardanoFuture = data.frame(date = seq.Date(from = ymd( max(cardano$date) + 1), by = 1, length.out = 10), # avoid doing dates by hand
                           adjusted = NA, # we do not have true values
                           naiveFC = cardano$adjusted[nrow(cardano)], # automatically get last value
                           lower = cardano$adjusted[nrow(cardano)] - (qnorm(0.975)*RMSE), # lower 95% interval
                           upper = cardano$adjusted[nrow(cardano)] + (qnorm(0.975)*RMSE)) # upper

cardanoCombined = rbind(cardano, cardanoFuture)


cardanoCombined %>% ggplot(aes(x = date, y = adjusted)) + geom_point() + geom_line() +
  geom_line(aes(x = date, y = naiveFC), color = 'red', linetype = 'dashed') +
  geom_point(aes(x = date, y = naiveFC), color = 'red') +
  scale_x_date(breaks = pretty_breaks(12)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x = 'Dates', 
       y = 'Adjusted/Close Price', 
       title = 'Naive Forecast for Cardano (ADA-USD)', 
       caption = paste('Data Source: Yahoo Finance | Data from', cardano$date[1], 'to',
                       cardano$date[nrow(cardano)])) + theme_bw(base_size = 8)


# Q2:
# ---
cardano = tq_get('ADA-USD', from = '2020-01-01') %>% 
  select(date, adjusted) 

cardano %<>% mutate(ma7 = rollmeanr(adjusted, k = 7, na.pad = T),
                    fcMA7 = lag(ma7)) 
cardano %>% filter(date == '2021-02-06')


# Q4
# ---
cardano %>% filter(date >= '2021-01-28' & date <= '2021-02-10') %>% 
  ggplot(aes(x = date, y = adjusted)) +
  geom_point() + geom_line() +
  scale_x_date(breaks = pretty_breaks(6)) + 
  scale_y_continuous(labels = scales::dollar) + theme_bw(base_size = 18) +
  labs(x = 'Date', y = 'Closing Price of ADA-USD')
