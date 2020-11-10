pacman::p_load(tidyverse, magrittr, lubridate, fpp2, ggfortify)

inflation = read.csv('https://tinyurl.com/y56hzetf')

inflation$DATE %<>% ymd() # to convert from char to date + ymd() from lubridate and two-way pipe from magrittr

trainInfl = inflation[1:24,] # will be used to train the ses model based on the first 24 obs
sesModel = ses(trainInfl$FPCPITOTLZGUSA, initial = 'optimal') # mainly doing this to get the optimal alpha
summary(sesModel) # seeing the optimal value
alphaOptimal = sesModel$model$par['alpha'] # getting the optimal value

# fitting the ses model with the optimal alpha over the entire ts
finalModel = ses(inflation$FPCPITOTLZGUSA, initial = 'optimal', alpha = alphaOptimal)
inflation$sesOpt = finalModel %>% .[['fitted']] # converts the fitted values to a new column in the data frame

accuracy(object = inflation$sesOpt[25:28], 
         x = inflation$FPCPITOTLZGUSA[25:28])


#### GDP Data
gdp = read.csv('https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDP&scale=left&cosd=1947-01-01&coed=2020-04-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2020-10-26&revision_date=2020-10-26&nd=1947-01-01')
gdp$cmean = cummean(gdp$GDP) # calculating the cumulative mean (dplyr)

gdp$ma3 = rollmeanr(gdp$GDP, k = 3, na.pad = TRUE) # creates the ma3 smoother
gdp$ma3Forecast = lag(gdp$ma3) # uses the smoothed values for forecasting

gdpHolt = holt(gdp$GDP, alpha =0.2, beta = 0.1, initial = 'optimal', h =3) # h needs to be greater than 2
summary(gdpHolt) # to print out the forecasts
autoplot(gdpHolt) + geom_line(aes(y = fitted(gdpHolt)), col = 'blue') # to visualize the forecasts


# AU Data
aut = austourists

aut
trainData = aut[1:52] %>% ts(frequency = 4)

auTrainModel = hw(trainData, initial = 'optimal')
summary(auTrainModel)

auFinalModel = hw(aut, alpha = auTrainModel$model$par['alpha'],
                  beta = auTrainModel$model$par['beta'],
                  gamma = auTrainModel$model$par['gamma']) %>% .[['fitted']]

accuracy(object = auFinalModel[53:68], x = aut[53:68])



########################################### ARIMA Models #######################################
pacman::p_load(tidyverse, lubridate, fpp2)

gnp = read.csv('https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GNP&scale=left&cosd=1947-01-01&coed=2020-04-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2020-10-26&revision_date=2020-10-26&nd=1947-01-01')
gnp$DATE %<>% ymd() # convert it from character to a Date

temp = GNP = gnp$GNP %>% ts(frequency = 4)

GNP = gnp$GNP %>% ts(frequency = 4, start = c(1947,1)) # note the start argument to make the dates make more sense
autplot(GNP) # always plot the data

ndiffs(GNP) # I expect a non-zero value (which means that we will need differencing to bring the data to stationarity)

gnpDiff2 = diff(GNP, differences = 2) # second difference
gnpDiff2Test = diff(diff(GNP)) # no need to do this -- we have only explored this as a visual check

acf(gnpDiff2) %>% autoplot()
pacf(gnpDiff2) %>% autoplot()
