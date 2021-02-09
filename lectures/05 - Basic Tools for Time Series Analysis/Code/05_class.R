if(require(pacman) == FALSE) install.packages('pacman') # check and install pacman if needed
pacman::p_load(tidyverse, # load the tidyverse package for ggplot2 and dplyr functions
               scales, # we sometimes use this for pretty_breaks() or the commas() with ggplot2
               magrittr, # for pipe based operators
               lubridate, # we use this to fix dates,
               fpp2, # for measures of forecast accuracy
               tidyquant) # we use tidyquant to get stocks and economic data

coins = tq_get(c('ADA-USD', 'LINK-USD', 'ZIL-USD'), from = '2020-11-01', to ='2021-02-06')

coins %<>% group_by(symbol) %>% 
  mutate(lagClose = lag(close),
    DYt = close - lag(close),
         GR = DYt/lag(close), # (current - pastData)/ pastData
         logClose = log(close),
         firstDiffOfLogs = logClose - lag(logClose),
         minClose = min(close),
         maxClose = max(close),
         scaledClose = (close - minClose)/(maxClose - minClose))

coins %>% filter(date >= '2021-02-04')

100*(0.538 - 0.441)/0.441 # this means that there were a 22% increase in the price of cardano between both days

coins %>% ggplot(aes(x = date, y = logClose, group = symbol, color = symbol)) +
  geom_line() + facet_wrap(~symbol, scales = 'free_y', ncol=1)
