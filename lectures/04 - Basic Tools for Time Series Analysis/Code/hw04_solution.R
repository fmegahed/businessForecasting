setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically setting the working directory

# [A] Computing the testPositivity Rate for Queens County on January 05, 2021
pacman::p_load(magrittr, tidyverse, lubridate, plotly)
df = read.csv('https://health.data.ny.gov/api/views/xdss-u53e/rows.csv?accessType=DOWNLOAD')

df$Test.Date %<>% mdy() # converting date to date format
queens = df %>% filter(County == 'Queens' & Test.Date == '2021-01-05')

queensTestPost = queens$New.Positives/queens$Total.Number.of.Tests.Performed
round(queensTestPost, digits = 3)

# [B] Compute aggregate test positivity rate for New York City
pacman::p_load(magrittr, tidyverse, lubridate, plotly)

df = read.csv('https://health.data.ny.gov/api/views/xdss-u53e/rows.csv?accessType=DOWNLOAD')
df$Test.Date %<>% mdy() # converting date to date format

nyc = df %>% filter(County %in% c('New York', 'Kings', 'Bronx', 'Richmond', 'Queens') & Test.Date == '2021-01-05')
nycTestPost = nyc %>% summarise(testPositivityRate = sum(New.Positives)/sum(Total.Number.of.Tests.Performed))
round(nycTestPost, digits = 3)

# [C] Plotting the aggregate test positivity rate for NY City
pacman::p_load(magrittr, tidyverse, lubridate, plotly)

df = read.csv('https://health.data.ny.gov/api/views/xdss-u53e/rows.csv?accessType=DOWNLOAD')
df$Test.Date %<>% mdy() # converting date to date format

nyc = df %>% filter(County %in% c('New York', 'Kings', 'Bronx', 'Richmond', 'Queens') & 
                      Test.Date >= '2020-12-01' & Test.Date <= '2021-01-31') %>% 
  group_by(Test.Date) %>% 
  summarise(testPositivityRate = sum(New.Positives)/sum(Total.Number.of.Tests.Performed))

nyc %>% ggplot(aes(x = Test.Date, y = testPositivityRate)) +
  geom_line() + geom_point() -> p

ggplotly(p)
