# The purpose of today's class code is to walk you through the process of fitting:
## an autoML model on crypto data (with generating features)
# I believe that this is a good introduction to the use of more advanced (sometimes better) models
# for time-series prediction



# Loading the Required Packages -------------------------------------------

# * Installing the latest release for h2o ----


# Based on http://h2o-release.s3.amazonaws.com/h2o/rel-zipf/2/index.html
# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-zipf/2/R")



# * Loading the Other Packages --------------------------------------------
pacman::p_load(tidyquant, # for tq_get and it also loads zoo (package for ts data)
               tidyverse, # for ggplot2, dplyr, etc
               magrittr, # for the two way pipe
               fpp2, # loads the forecast package
               lubridate, # to manipulate "dates"
               DataExplorer, # for plotting missing
               imputeTS, # to impute the TS data
               h2o)

zil = tq_get('ZIL-USD', from = '2018-01-01', to = Sys.Date() -1 ) %>% 
  select(date, close, volume)

# * Exploratory Analysis of ZIL Data ----
# (A) Checking to see missing data
colSums(is.na(zil)) # returns the number of missing values per Column
plot_missing(zil, ggtheme = theme_bw(), theme_config = list(legend.position = c('none')))
7/1194

# (B) Impute the missing data
zil$close %<>% na.interpolation() 
zil$volume %<>% na.interpolation() 
colSums(is.na(zil)) # should be 0 0 0 if the above two lines worked correctly


# * Feature Engineering ----
# (C) Generate some features
df = zil %>%  
  mutate(year = year(date),
         quarter = quarter(date) %>% as.factor(),
         month = month(date, label = TRUE) %>% as.character() %>% as.factor(), # to not make it an ordered factor
         day = wday(date, label = T) %>% as.character() %>% as.factor(), # to not make it an ordered factor
         lag1Close = lag(close),
         lag7Close = lag(close, n =7),
         laggedMA7 = rollmeanr(close, k = 7, na.pad = T) %>% lag(),
         laggedMA30 = rollmeanr(close, k = 30, na.pad = T) %>% lag(),
         laggedMA50 = rollmeanr(close, k = 50, na.pad = T) %>% lag(),
         )
str(df) 

df %<>% na.omit() 

# * Model Fitting ---------------------------------------------------------
# * * Training Data ----
trainData = df %>% filter(year < 2020)
validData = df %>% filter(year == 2020)
testData = df %>% filter(year > 2020)

# * * Response and Predictor Variables
y = "close"
x = setdiff(colnames(df), c("close", "date") )


# * * Using h2o package ----
h2o.init()
trainDataH2o = trainData %>% as.h2o()
validDataH2o = validData %>% as.h2o()
testDataH2o = testData %>% as.h2o()

fittedModels = h2o.automl(x = x, 
                          y = y,
                          training_frame = trainDataH2o,
                          validation_frame = validDataH2o,
                          leaderboard_frame = testDataH2o,
                          nfolds = 5,
                          stopping_metric = "AUTO",
                          max_runtime_secs = 60)

# * * Printing the fitted Models
fittedModels

bestModel = fittedModels@leader
bestModel

predictions = h2o.predict(bestModel, newdata = testDataH2o)

testError = h2o.performance(bestModel, newdata = testDataH2o)
testError
