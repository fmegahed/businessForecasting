setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically setting the working directory
pacman::p_load(tidyverse, tidyquant, magrittr, lubridate, fpp2, readxl)

bike = read_excel("Data/BikeSalesR.xlsx")

bikeTS = ts(bike$`Bike Sales`, frequency = 4)
bikeTS
autoplot(bikeTS)

hwadd = hw(bikeTS, # this allows the model to get the frequency
           seasonal = "additive", alpha = 0.2, beta = 0.1, gamma =0.1,
           level = 95)
hwmult = hw(bikeTS, # this allows the model to get the frequency
            seasonal = "multiplicative", alpha = 0.2, beta = 0.1, gamma =0.1,
            level = 95)
summary(hwmult)

results = rbind(accuracy(hwadd),
                accuracy(hwmult))
results
row.names(results) = c('A', 'M')
results

autoplot(bikeTS) +
  autolayer(fitted(hwadd), series = "A", color = "red") +
  autolayer(fitted(hwmult), series = "M", color = "gray") + theme_bw()


# Slide 7
pacman::p_load(tidyquant, timetk)
aapl = tq_get('AAPL', from = "2021-01-07", to = Sys.Date() -1) %>%
  select(date, adjusted)
str(aapl)
aapl_ts = timetk::tk_ts(aapl) # we captured the dates here
aapl_ts
tk_index(aapl_ts, timetk_idx = T) # gets the dates




# Slide 19
set.seed(2021)
wn = rnorm(500, mean = 0, sd = 1)

wnTS = ts(wn)
autoplot(wnTS) # plotting the generated data

acf_of_WhiteNoise = acf(wnTS, plot = F)
acf_of_WhiteNoise

# interval = pointEstimate +/- 1.96* 1/sqrt(500)
# 0 +- 1.96 * 1/ sqrt(500)
1.96/sqrt(500)

acf_of_WhiteNoise %>% autoplot()

cma = rollmean(wn, k =3, align = "center", na.pad = T)

ts(cma) %>% autoplot()
na.omit(cma) %>% acf() -> acfCMA
acfCMA


# Slides 20-22
df = read_excel("Data/WFJ_sales.xlsx")
df %>% ggplot(aes(x = Obs, y =`WFJ Sales`)) + geom_point() + geom_line()

acf(df$`WFJ Sales`, plot = F) %>% autoplot()
1.96/sqrt(62)
wfjACF = acf(df$`WFJ Sales`, plot = F)
wfjACF

df$lag1 = lag(df$`WFJ Sales`, n =1)
model = lm(data = df, `WFJ Sales` ~ lag1)
summary(model)
