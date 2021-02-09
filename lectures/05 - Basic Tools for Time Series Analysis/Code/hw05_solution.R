if(require(pacman) == FALSE) install.packages('pacman')

# Q1:
#----
pacman::p_load(tidyverse, magrittr, scales, COVID19, gridExtra, tidyquant, fpp2)

covid = covid19(country = c('EGY', 'IND', 'USA'), start = '2020-03-01', end = '2021-02-06')

covid %>% select(id, date, confirmed) %>%  
  mutate(newCases = confirmed - lag(confirmed)) %>% filter(id == 'EGY' & date == '2021-01-21')

# Q2:
#----
covid %>% select(id, date, confirmed, population) %>%  
  mutate(newCases = confirmed - lag(confirmed), 
         newCasesByPop = newCases/population) %>% filter(date == '2021-01-21')

# Q3:
#---
pacman::p_load(tidyverse, magrittr, scales, COVID19, gridExtra)

covid = covid19(country = c('EGY', 'IND', 'USA'), start = '2020-03-01', end = '2021-02-06')

covid %>% ggplot(aes(x= date, y = confirmed, group = id)) + geom_line() +
  scale_x_date(breaks = pretty_breaks(n= 12)) +
  facet_wrap(~id , scales = 'free_y', ncol = 1) + theme_bw()  -> p1

covid %>% ggplot(aes(x= date, y = log(confirmed), group = id)) + geom_line() +
  scale_x_date(breaks = pretty_breaks(n= 12)) +
  facet_wrap(~id , ncol = 1) + theme_bw() -> p2

grid.arrange(p1, p2, ncol=2) # did not discuss in class but used to put two ggplots side-by-side
# obviously you could have just printed each chart separately


# Q4:
#----
bitcoin = tq_get("BTC-USD", from = "2020-02-01", to = "2021-01-31") %>% select(symbol, date, adjusted)
bitcoin %<>% mutate(naiveFC = lag(adjusted))
accuracy(object = bitcoin$naiveFC, # forecast object is the first argument
         bitcoin$adjusted) %>% round(2)
