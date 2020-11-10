setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically setting my working directory

pacman::p_load(tidyverse, magrittr, readxl, lubridate)


#### Going over Figures 4.2 and 4.3 from the textbook (due to my incomplete/ somewhat confusing solution)

walmart = read_excel('Data/Walmart_2.xlsx', sheet = 1) %>% select(c(6,7,8)) # reading data and keeping only 3 cols
colnames(walmart) = c('Year', 'Quarter', 'Sales') # simplyfing the names a bit

# Q1 01-01, Q2 04-01, Q3 07-01, Q4 10-01 (q starts at the first of month and they are seperated by 3 months)
walmart$Date = paste0(walmart$Year, '-', c('01', '04', '07', '10'), '-01' ) %>% # concatenate year-month-day
  ymd() # made it a date

# Plot 4.2
walmart %>% ggplot(aes(x = Date, y = Sales)) +
  geom_line() + geom_point() + theme_bw() +
  scale_x_date(breaks = scales::pretty_breaks(26))

# Plot 4.2 (modified/colored)
walmart %>% ggplot(aes(x = Date, y = Sales, color = Year)) +
  geom_line() + geom_point() + theme_bw()

# Plot 4.2 (modified/colored)
walmart %>% mutate(Year = as.factor(Year)) %>% 
  ggplot(aes(x = Date, y = Sales, color = Year)) +
  geom_line() + geom_point() + theme_bw()

# Plot 4.3
walmart %>% mutate(Year = as.factor(Year)) %>% # mutate optional to have diff colors for year 
  ggplot(aes(x = Quarter, y = Sales, group = Year, color = Year)) +
  geom_line() + geom_point() + theme_bw()




#### Autocorrelation
pacman::p_load(fpp2, zoo) # this also loads the forecast package

set.seed(10052020) # ensures that we all get the same looking plot (assuming version of R > 4)

wn = rnorm(500) # generating standard normal data (n=500), mu = 0, sd = 1

# plot the data
ts(wn) %>% autoplot() +  # autoplot needs the forecast package which we load when have fpp2
  theme_bw() + labs(y = 'White Noise Data') + ylim(c(-3,3))

acf(ts(wn), lag.max = 10, type = 'covariance') # shows autocovariance 

ts(wn) %>% acf(lag.max = 10, type = 'correlation') %>% autoplot() # advantage is it drops lag 0


# CMA of the WN model
cma = rollmean(wn, k = 3, align = 'center')
ts(cma) %>% autoplot() + theme_bw() + labs(y = 'CMA(3) of the WN Data') + 
  ylim(c(-3,3)) # added the ylim so we can easily compare between the plots in Lines 45 and 55

acf(ts(cma), lag.max = 10, type = 'covariance') # shows autocovariance (to document the 2/9 from Slide 12)

ts(cma) %>% acf(lag.max = 10, type = 'correlation') %>% autoplot() # advantage is it drops lag 0



# WFJ Sales
WFJ = read_excel('Data/WFJ_sales.xlsx', sheet = 1)
WFJ = WFJ$`WFJ Sales` # extracting the Y (we do not care about anything else for this demo)

ts(WFJ) %>% autoplot() + theme_bw() + labs( y = 'WFJ Sales') # create the figure in Slide 21

acf(WFJ, lag.max = 12) %>% autoplot() + theme_bw() # figure in Slide 22

WFJ = data.frame(WFJ, lag1 = lag(WFJ, n =1) ) # to ensure that I can put the data in a data frame

lm(data = WFJ, WFJ ~ .) %>% summary() # Slide 23
