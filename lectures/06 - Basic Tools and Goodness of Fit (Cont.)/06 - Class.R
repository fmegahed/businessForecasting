setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse, lubridate, magrittr, tidyquant, fpp2)

aapl = tq_get("aapl", from = "2020-08-17", to = "2020-08-29")

aapl %<>% select(symbol, date, adjusted)

aapl$naiveFC = lag(aapl$adjusted)

aapl$e = aapl$adjusted - aapl$naiveFC
aapl$PE = 100* aapl$e / aapl$adjusted


e = aapl$adjusted - aapl$naiveFC

ME = mean(aapl$e, na.rm = TRUE)
MPE = mean(aapl$PE, na.rm = TRUE)
RMSE =  sqrt( mean(e^2, na.rm = TRUE) )

naiveFCfpp = snaive(aapl$adjusted)
names(naiveFCfpp)
naiveFCfpp = naiveFCfpp[['fitted']]


metrics = accuracy(naiveFCfpp, aapl$adjusted)

Z_score = qnorm(0.975)

PI_U =

PI_L =