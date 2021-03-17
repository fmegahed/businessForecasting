
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically setting the working directory

# Some required packages
pacman::p_load(tidyverse, # for dplyr, ggplots
               tidyquant, # getting data, it also loads zoo
               fpp2, # loads the forecast package 
               magrittr, # for the %<>% 
               ggpubr, # ggarrange
               tseries, # for adf.test and kpss.test
               lubridate) # for fixing dates

gnp = read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GNP&scale=left&cosd=1947-01-01&coed=2019-12-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2021-03-15&revision_date=2021-03-15&nd=1947-01-01")

gnp2 = tq_get("GNP", get = "economic.data",
              from = "1947-01-01", to = "2019-12-01")

# Approach 1 (using the gnp object)
str(gnp)
str(gnp2)

gnp$DATE %<>% ymd()
class(gnp$DATE)

gnp %<>% mutate(diff1a = c(NA, diff(GNP) ),
                diff1b = GNP - lag(GNP),
                diff2a = c(rep(NA, 2), diff(GNP, differences = 2)),
                diff2b = diff1b - lag(diff1b))

p1 = gnp %>% ggplot(aes(x = DATE, y = GNP)) + geom_line() + theme_bw()
gnp %>% ggplot(aes(x = DATE, y = diff1b)) + geom_line() + theme_bw() -> p2
gnp %>% ggplot(aes(x = DATE, y = diff2b)) + geom_line() + theme_bw() -> p3

ggarrange(p1, p2, p3, ncol=1)

colMeans(gnp[, -1], na.rm = T)


kpss.test(gnp$GNP) # our conclusion from the printed output is that the ts is NOT stationary
kpss.test(gnp$diff1b) # our conclusion from the printed output is that the ts is NOT stationary
kpss.test(gnp$diff2b)# our conclusion from the printed output is that the ts IS stationary

ndiffs(gnp$GNP) # second difference is needed to achieve stationarity based on the output
