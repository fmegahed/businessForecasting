
# sets working directory to where the file is stored
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Checking to see whether the pacman package is installed
if(require(pacman) == FALSE) install.packages("pacman") # if pacman is not on your machine, install it

pacman::p_load(tidyverse, # tidyverse -- IMO you will need it for most of your data analysis
               tidyquant, # to be used for getting stock data
               magrittr) # we are loading to use the pipe operators

# tq_get is from the tidyquant package
FANG = tq_get(x = c('AMZN', 'FB', 'GOOG', 'NFLX'), # the stocks that we want to get
              from = '2020-01-01', # gets this date or the next trading date
              to = '2021-01-24') # get data up to BUT NOT including that date
str(FANG) # returns the internal structure of that object

max(FANG$date)


# let us build the plot
FANG %>% # layer 1: data (which gets passed as the first argument to the next function via the pipe (magrittr))
  ggplot(mapping = aes(x = date, y = adjusted, group = symbol)) + # layer 2: aesthetics
  geom_line() + # layer 3:
  facet_wrap(~ symbol, ncol = 2, scales = 'free_y') + # layer 4: facets (optional)
  stat_smooth(method = 'loess') + # layer 5: optional (here we are fitting a local regression to data)
  labs(x= 'Date', y = 'Adjusted Closing Price', 
       caption = 'Data from 2020-01-02 to 2021-01-22') +
  theme_bw()

ggsave('stocks_ggsaved.png', width = 6 , height = 4, units = 'in')


# getting the starting date
min(FANG$date)
FANG %>% 
  filter(date == '2020-01-02') %>%  # filter function comes from dplyr (tidyverse) and is a rowwise operation
  select(symbol, adjusted)  -> # select is a column wise operation (dplyr)
  baseFANG

# getting the ending date
max(FANG$date)
FANG %>% 
  filter(date == '2021-01-22') %>%  # filter function comes from dplyr (tidyverse) and is a rowwise operation
  select(symbol, adjusted)  -> # select is a column wise operation (dplyr)
  currentFANG

combinedFANG = left_join(x = baseFANG,
                         y = currentFANG,
                         by = 'symbol') # also comes from dplyr/ tidyverse  
colnames(combinedFANG)[2:3] = c('base', 'current')   

combinedFANG$pctChange = 100* (combinedFANG$current - combinedFANG$base)/ combinedFANG$base
  