setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load(tidyquant, tidyverse, magrittr)

usdEuro = tq_get("EUR=X", from = "2020-08-01") %>% select(date, adjusted) %>% 
  mutate(cuMean = cummean(adjusted))


ggplot(usdEuro, aes(x = date)) +
  geom_line(aes(y = adjusted, color = 'adjusted')) +
  geom_line(aes(y = cuMean, color = 'cumulativeMean')) +
  theme_bw() + 
  labs(x = '2020', y = 'USD/EURO', title = 'Smoothed TS Using Cumulative Mean', color = 'TS',
       caption = "Data is from FRED, extracted on 2020-09-09")

usdEuro %<>%  mutate(ma3 = rollmean(adjusted, 3, align = 'right', fill = NA),
                     ma7 = rollmeanr(adjusted, 7, fill = NA))

ggplot(usdEuro, aes(x = date)) +
  geom_line(aes(y = adjusted, color = 'adjusted')) +
  geom_line(aes(y = cuMean, color = 'cumulativeMean')) +
  geom_line(aes(y = ma3, color = 'ma3')) +
  geom_line(aes(y = ma7, color = 'ma7')) +
  theme_bw() + 
  labs(x = '2020', y = 'USD/EURO', title = 'Smoothed TS Using Cumulative Mean and MAs', 
       color = 'TS',
       caption = "Data is from FRED, extracted on 2020-09-09")
